use unicode_ident::{is_xid_continue, is_xid_start};

pub const fn is_quote(c: char) -> bool {
    matches!(c, '\'' | '"')
}

pub const fn is_ascii_identifier_start(c: char) -> bool {
    matches!(c, 'a'..='z' | 'A'..='Z' | '_')
}

// Checks if the character c is a valid starting character as described
// in https://docs.python.org/3/reference/lexical_analysis.html#identifiers
pub fn is_unicode_identifier_start(c: char) -> bool {
    is_xid_start(c)
}

// Checks if the character c is a valid continuation character as described
// in https://docs.python.org/3/reference/lexical_analysis.html#identifiers
pub fn is_identifier_continuation(c: char) -> bool {
    match c {
        'a'..='z' | 'A'..='Z' | '_' | '0'..='9' => true,
        c => is_xid_continue(c),
    }
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
