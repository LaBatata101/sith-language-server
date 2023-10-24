use std::ops::{Deref, DerefMut};

use python_ast::{
    Alias, AnnAssignStmt, AssignStmt, ClassDefStmt, Expr, ForStmt, FunctionDefStmt, Identifier,
    Parameter, ParameterWithDefault,
};
use ruff_index::{newtype_index, IndexSlice, IndexVec};
use ruff_text_size::{Ranged, TextRange};

#[derive(Debug)]
pub enum Declaration<'a> {
    Other(&'a Expr),
    Import(&'a Alias),
    Assign(&'a AssignStmt),
    ClassDef(&'a ClassDefStmt),
    PatternMatch(&'a Identifier),
    AnnAssign(&'a AnnAssignStmt),
    Parameter(ParamDeclaration<'a>),
    FunctionDef(&'a FunctionDefStmt),
    For(&'a ForStmt),
    // TODO: add more declaration types
}

impl Ranged for Declaration<'_> {
    fn range(&self) -> TextRange {
        match self {
            Declaration::Other(node) => node.range(),
            Declaration::Import(node) => node.range(),
            Declaration::Assign(node) => node.range(),
            Declaration::ClassDef(node) => node.range(),
            Declaration::PatternMatch(node) => node.range(),
            Declaration::AnnAssign(node) => node.range(),
            Declaration::Parameter(node) => node.range(),
            Declaration::FunctionDef(node) => node.range(),
            Declaration::For(node) => node.range(),
        }
    }
}

#[derive(Debug)]
pub enum ParamDeclaration<'a> {
    Param(&'a Parameter),
    ParamWithDefault(&'a ParameterWithDefault),
}

impl Ranged for ParamDeclaration<'_> {
    fn range(&self) -> TextRange {
        match self {
            ParamDeclaration::Param(node) => node.range,
            ParamDeclaration::ParamWithDefault(node) => node.range,
        }
    }
}

#[newtype_index]
pub struct DeclarationId;

#[derive(Debug, Default)]
pub struct Declarations<'a>(IndexVec<DeclarationId, Declaration<'a>>);

impl<'a> Declarations<'a> {
    pub fn push(&mut self, symbol: Declaration<'a>) -> DeclarationId {
        self.0.push(symbol)
    }
}

impl<'a> Deref for Declarations<'a> {
    type Target = IndexSlice<DeclarationId, Declaration<'a>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a> DerefMut for Declarations<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
