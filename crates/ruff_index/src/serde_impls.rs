use std::marker::PhantomData;

use serde::{ser::SerializeSeq, Deserialize, Deserializer, Serialize};

use crate::{Idx, IndexVec};

impl<I: Idx, T: Serialize> Serialize for IndexVec<I, T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut seq = serializer.serialize_seq(Some(self.len()))?;
        for e in self {
            seq.serialize_element(e)?;
        }
        seq.end()
    }
}

impl<'de, I: Idx, T> Deserialize<'de> for IndexVec<I, T>
where
    T: Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        // Create a visitor to handle deserialization
        struct IndexVecVisitor<I, T>(PhantomData<I>, PhantomData<T>);

        impl<'de, I, T> serde::de::Visitor<'de> for IndexVecVisitor<I, T>
        where
            I: Idx,
            T: Deserialize<'de>,
        {
            type Value = IndexVec<I, T>;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("a sequence")
            }

            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: serde::de::SeqAccess<'de>,
            {
                let mut vec = Vec::new();

                while let Some(element) = seq.next_element()? {
                    vec.push(element);
                }

                Ok(IndexVec::from_raw(vec))
            }
        }

        deserializer.deserialize_seq(IndexVecVisitor(PhantomData, PhantomData))
    }
}
