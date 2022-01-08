use bitmatch::bitmatch;
use byteorder::ReadBytesExt;
use std::{env, fs, io};

// Sign-EXTend the least significant b bits of n
#[inline]
fn sext(n: u16, b: usize) -> i16 {
    (if (n >> (b - 1) & 1) != 0 {
        n | (0xffff << b)
    } else {
        n
    }) as i16
}

// TODO: Use u16::checked_add_signed() and print the destination address,
// instead of the offset, when mixed_integer_ops makes it to Rust stable!
#[inline]
fn format_br(inst: u16, flags: u16, offset: i16) -> String {
    format!(
        "BR{} #{}",
        match flags {
            0b000 =>
                if inst == 0 {
                    return String::from("NOP");
                } else {
                    return format!(".FILL x{:04x}", inst);
                },
            0b001 => "p",
            0b010 => "z",
            0b011 => "zp",
            0b100 => "n",
            0b101 => "np",
            0b110 => "nz",
            0b111 => "",
            _ => unreachable!("Bad branch flags"),
        },
        offset
    )
}

#[inline]
fn format_jmp(basereg: u16) -> String {
    match basereg {
        7 => String::from("RET"),
        _ => format!("JMP R{}", basereg),
    }
}

#[inline]
fn format_trap(trapvect: u16) -> String {
    match trapvect {
        0x20 => String::from("GETC"),
        0x21 => String::from("OUT"),
        0x22 => String::from("PUTS"),
        0x23 => String::from("IN"),
        0x24 => String::from("PUTSP"),
        0x25 => String::from("HALT"),
        _ => format!("TRAP x{:02x}", trapvect),
    }
}

#[bitmatch]
fn dis(i: u16) -> String {
    #[bitmatch]
    match i {
        "0001dddsss0??aaa" => format!("ADD R{}, R{}, R{}", d, s, a),
        "0001dddsss1mmmmm" => format!("ADD R{}, R{}, #{}", d, s, sext(m, 5)),
        "0101dddsss0??aaa" => format!("AND R{}, R{}, R{}", d, s, a),
        "0101dddsss1mmmmm" => format!("AND R{}, R{}, #{}", d, s, sext(m, 5)),
        "0000fffooooooooo" => format_br(i, f, sext(o, 9)),
        "1100???bbb??????" => format_jmp(b),
        "01001ooooooooooo" => format!("JSR #{}", sext(o, 11)),
        "01000??bbb??????" => format!("JSRR R{}", b),
        "0010dddooooooooo" => format!("LD R{}, #{}", d, sext(o, 9)),
        "1010dddooooooooo" => format!("LDI R{}, #{}", d, sext(o, 9)),
        "0110dddbbboooooo" => format!("LDR R{}, R{}, #{}", d, b, sext(o, 6)),
        "1110dddooooooooo" => format!("LEA R{}, #{}", d, sext(o, 9)),
        "1001dddsss??????" => format!("NOT R{}, R{}", d, s),
        "1101????????????" => format!("RTI"),
        "0011sssooooooooo" => format!("ST R{}, #{}", s, sext(o, 9)),
        "1011sssooooooooo" => format!("STI R{}, #{}", s, sext(o, 9)),
        "0111sssbbboooooo" => format!("STR R{}, R{}, #{}", s, b, sext(o, 6)),
        "1111????tttttttt" => format_trap(t),
        "1000????????????" => format!(".FILL x{:04x}", i), // illegal instruction, must be data
    }
}

fn format_char(c: char) -> Option<String> {
    match c {
        '\t' => Some(String::from("\\t")),
        '\n' => Some(String::from("\\n")),
        '\r' => Some(String::from("\\r")),
        '\x1b' => Some(String::from("\\e")),
        '"' => Some(String::from("\\\"")),
        '\'' => Some(String::from("\\'")),
        '\\' => Some(String::from("\\\\")),
        ' '..='~' => Some(String::from(c)),
        _ => None,
    }
}

// If the instruction is 0x00xx, where xx is a non-control ASCII character, or
// commonly escaped control character, print it as a single character 'c'. If
// the instruction could be two packed ASCII characters (such as for PUTSP),
// print it as two characters "cc". (Note that PUTSP reverses them)
fn format_as_char_pair(inst: u16) -> String {
    let hi = char::from_u32((inst >> 8) as u32).unwrap_or('\u{ff}');
    let lo = char::from_u32((inst & 0xff) as u32).unwrap_or('\u{ff}');
    if hi == '\0' {
        match format_char(lo) {
            None => String::from(""),
            Some(c) => format!("'{}'", c),
        }
    } else {
        let c1 = format_char(hi);
        let c2 = format_char(lo);
        if c1.is_none() || c2.is_none() {
            return String::from("");
        }
        format!("\"{}{}\"", c1.unwrap(), c2.unwrap())
    }
}

fn main() -> io::Result<()> {
    let arg = env::args()
        .nth(1)
        .unwrap_or_else(|| panic!("Requires one argument: file to load"));
    let mut file = fs::File::open(arg)?;
    let offset = file.read_u16::<byteorder::NetworkEndian>()?;
    println!("       .ORIG x{:04x}", offset);
    let mut pc = offset;
    while let Ok(inst) = file.read_u16::<byteorder::NetworkEndian>() {
        println!(
            "x{:04x}: {:18} ; {:04x} {}",
            pc,
            dis(inst),
            inst,
            format_as_char_pair(inst)
        );
        pc += 1;
    }
    println!("       .END");
    Ok(())
}
