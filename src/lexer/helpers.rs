pub fn is_string_prefix(prefix: &str) -> bool {
    matches!(
        prefix,
        "r" | "u"
            | "R"
            | "U"
            | "f"
            | "F"
            | "fr"
            | "Fr"
            | "fR"
            | "FR"
            | "rf"
            | "rF"
            | "Rf"
            | "RF"
            | "b"
            | "B"
            | "br"
            | "Br"
            | "bR"
            | "BR"
            | "rb"
            | "rB"
            | "Rb"
            | "RB"
    )
}

pub fn is_eol(char: u8) -> bool {
    char == b'\n' || char == b'\r'
}

pub fn is_whitespace(char: u8) -> bool {
    matches!(char, b' ' | b'\t' | 0x0C)
}

pub fn is_char_operator(char: u8) -> bool {
    matches!(
        char as char,
        '*' | '+' | '=' | '-' | '<' | '>' | '&' | '|' | '%' | '~' | '^' | '!' | '@' | '/'
    )
}

pub fn unicode_char_size(char: u8) -> usize {
    if (char & 0xE0) == 0xC0 {
        2
    } else if (char & 0xF0) == 0xE0 {
        3
    } else if (char & 0xF8) == 0xF0 {
        4
    } else {
        1
    }
}

fn convert_slice_to_unicode_codepoint(bytes: &[u8]) -> u32 {
    assert!(!bytes.is_empty() && bytes.len() <= 4, "Invalid slice size!");

    if bytes.len() == 1 {
        bytes[0] as u32
    } else if bytes.len() == 2 {
        ((bytes[0] & 0x1F) as u32) << 6 | ((bytes[1] & 0x3F) as u32)
    } else if bytes.len() == 3 {
        ((bytes[0] & 0x0F) as u32) << 12 | ((bytes[1] & 0x3F) as u32) << 6 | ((bytes[2] & 0x3F) as u32)
    } else {
        ((bytes[0] & 0x07) as u32) << 18
            | ((bytes[1] & 0x3F) as u32) << 12
            | ((bytes[2] & 0x3F) as u32) << 6
            | ((bytes[3] & 0x3F) as u32)
    }
}

pub fn convert_byte_to_unicode_codepoint(bytes: &[u8], char: u8, pos: usize) -> u32 {
    let char_size = unicode_char_size(char);
    convert_slice_to_unicode_codepoint(&bytes[pos..pos + char_size])
}

macro_rules! StateMachine {
    ($text:expr, START: $start_state:literal, $($state:literal, $input:pat => $next_state:literal;)* FINAL: $($final_state:literal),+) => {
        {
            let mut state = $start_state;
            let text: &str = $text;

            for ch in text.chars() {
                match state {
                    $($state if matches!(ch, $input) => {
                        state = $next_state;
                    },)*
                    _ => return false
                }
            }

            matches!(state, $($final_state)|+)
        }
    };
}

/// Verify if `str` is a valid decimal literal accordingly to the Python Lexer specification.
///
/// References:
///     - [https://docs.python.org/3/reference/lexical_analysis.html#integer-literals]
///
/// # Examples:
///
/// ```
///  assert!(is_integer_number_valid("1"));
///  assert!(is_integer_number_valid("123"));
///  assert!(is_integer_number_valid("1_2_3"));
///  assert!(!is_integer_number_valid("12_"));
///  assert!(!is_integer_number_valid("_12"));
///  assert!(!is_integer_number_valid("1_2_3__"));
///  assert!(!is_integer_number_valid("1_2__3"));
/// ```
pub fn is_decimal_number_valid(str: &str) -> bool {
    StateMachine!(str,
        START: 0,
        0, '0'..='9' => 1;

        1, '0'..='9' => 1;
        1, '_' => 2;

        2, '0'..='9' => 1;
        FINAL: 1
    )
}

/// Verify if `str` is a valid float literal accordingly to the Python Lexer specification.
/// **NOTE**: This function also verifies if `str` is a valid imaginary literal.
///
/// References:
///     - [https://docs.python.org/3/reference/lexical_analysis.html#floating-point-literals]
///     - [https://docs.python.org/3/reference/lexical_analysis.html#imaginary-literals]
///
/// # Examples:
///
/// ```
///  assert!(is_float_number_valid("3.14"));
///  assert!(is_float_number_valid("10."));
///  assert!(is_float_number_valid(".001"));
///  assert!(is_float_number_valid("1e100"));
///  assert!(is_float_number_valid("0e0"));
///  assert!(is_float_number_valid("3.14_15_93"));
///  assert!(is_float_number_valid("1_000.95"));
///  assert!(is_float_number_valid("3e-10"));
///  assert!(is_float_number_valid("3.14E-10"));
///  assert!(!is_float_number_valid("3._14"));
///  assert!(!is_float_number_valid("3.__14"));
///  assert!(!is_float_number_valid("3E1_4_"));
///  assert!(!is_float_number_valid("3.14e10e-10"));
///
///  // Imaginary Number
///     
///  assert!(is_float_number_valid("3.14j"));
///  assert!(is_float_number_valid("10.j"));
///  assert!(is_float_number_valid(".001J"));
///  assert!(is_float_number_valid("1e100j"));
///  assert!(is_float_number_valid("0e0j"));
///  assert!(is_float_number_valid("3.14_15_93J"));
///  assert!(is_float_number_valid("1_000.95J"));
///  assert!(is_float_number_valid("3e-10j"));
///  assert!(is_float_number_valid("3.14E+10J"));
///  assert!(!is_float_number_valid("3._14j"));
///  assert!(!is_float_number_valid("3.__14J"));
///  assert!(!is_float_number_valid("3E1_4_J"));
///  assert!(!is_float_number_valid("3.14e10e-10J"));
/// ```
pub fn is_float_number_valid(str: &str) -> bool {
    StateMachine!(str,
        START: 0,
        0, '0'..='9' | '.' => 1;
        1, '0'..='9' => 1;
        1, '_' => 2;
        1, '.' => 3;
        1, 'e' | 'E' => 5;
        1, 'j' | 'J' => 9;

        2, '0'..='9' => 1;

        3, '0'..='9' => 4;
        3, 'e' | 'E' => 5;
        3, 'j' | 'J' => 9;

        4, '_' => 8;
        4, '0'..='9' => 4;
        4, 'e' | 'E' => 5;
        4, 'j' | 'J' => 9;

        5, '-' | '+' | '0'..='9' => 6;

        6, '_' => 7;
        6, '0'..='9' => 6;
        6, 'j' | 'J' => 9;

        7, '0'..='9' => 6;

        8, '0'..='9' => 4;

        FINAL: 1, 3, 4, 6, 9
    )
}

/// Verify if `str` is a valid octal literal accordingly to the Python Lexer specification.
///
/// References:
///     - [https://docs.python.org/3/reference/lexical_analysis.html#integer-literals]
///
/// # Examples:
///
/// ```
///  assert!(is_octal_number_valid("0o1"));
///  assert!(is_octal_number_valid("0O1_0"));
///  assert!(is_octal_number_valid("0o_1_2_7"));
///  assert!(is_octal_number_valid("0o_1_2_3_4_5_6_7"));
///  assert!(!is_octal_number_valid("0O__0_1_2_34567"));
///  assert!(!is_octal_number_valid("0o_"));
///  assert!(!is_octal_number_valid("0o77__"));
/// ```
pub fn is_octal_number_valid(str: &str) -> bool {
    StateMachine!(str,
        START: 0,
        0, '0' => 1;
        1, 'o' | 'O' => 2;

        2, '0'..='7' => 3;
        2, '_' => 5;

        3, '0'..='7' => 3;
        3, '_' => 4;

        4, '0'..='7' => 3;

        5, '0'..='7' => 3;
        FINAL: 3
    )
}

/// Verify if `str` is a valid binary literal accordingly to the Python Lexer specification.
///
/// References:
///     - [https://docs.python.org/3/reference/lexical_analysis.html#integer-literals]
///
/// # Examples:
///
/// ```
///  assert!(is_binary_number_valid("0b1"));
///  assert!(is_binary_number_valid("0B1_0"));
///  assert!(is_binary_number_valid("0b_1_0_1"));
///  assert!(!is_binary_number_valid("0b__1_0_1"));
///  assert!(!is_binary_number_valid("0b_"));
///  assert!(!is_binary_number_valid("0b01__"));
///  assert!(!is_binary_number_valid("0b02"));
/// ```
pub fn is_binary_number_valid(str: &str) -> bool {
    StateMachine!(str,
        START: 0,
        0, '0' => 1;
        1, 'b' | 'B' => 2;

        2, '0' | '1' => 3;
        2, '_' => 5;

        3, '0' | '1' => 3;
        3, '_' => 4;

        4, '0' | '1' => 3;

        5, '0' | '1' => 3;
        FINAL: 3
    )
}

/// Verify if `str` is a valid hexadecimal literal accordingly to the Python Lexer specification.
///
/// References:
///     - [https://docs.python.org/3/reference/lexical_analysis.html#integer-literals]
///
/// # Examples:
///
/// ```
///  assert!(is_hex_number_valid("0x0123456789"));
///  assert!(is_hex_number_valid("0Xabcdef"));
///  assert!(is_hex_number_valid("0x0123456789_abcdef_ABCDEF"));
///  assert!(is_hex_number_valid("0x_a_B_c_D_e"));
///  assert!(is_hex_number_valid("0xDEAD_beef"));
///  assert!(is_hex_number_valid("0Xcafe_BABE"));
///  assert!(!is_hex_number_valid("0x__cafe_babe"));
///  assert!(!is_hex_number_valid("0x_"));
///  assert!(!is_hex_number_valid("0x_c_a_f_e_b_a_b_e_"));
/// ```
pub fn is_hex_number_valid(str: &str) -> bool {
    StateMachine!(str,
        START: 0,
        0, '0' => 1;
        1, 'x' | 'X' => 2;

        2, '0'..='9' | 'a'..='f' | 'A'..='F' => 3;
        2, '_' => 5;

        3, '0'..='9' | 'a'..='f' | 'A'..='F' => 3;
        3, '_' => 4;

        4, '0'..='9' | 'a'..='f' | 'A'..='F' => 3;

        5, '0'..='9' | 'a'..='f' | 'A'..='F' => 3;
        FINAL: 3
    )
}
