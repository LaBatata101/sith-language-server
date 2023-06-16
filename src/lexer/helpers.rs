pub fn convert_slice_to_char(slice: &[u8]) -> char {
    std::str::from_utf8(slice).unwrap().chars().next().unwrap()
}

pub fn unicode_char_size(char: u8) -> u32 {
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
