#![allow(dead_code)]

use bitmatch::bitmatch;
use byteorder::WriteBytesExt;
use multimap::MultiMap;
use std::{error, fmt, io};

#[derive(Clone, Copy, Debug)]
pub enum Reg {
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
}

#[derive(Debug)]
#[rustfmt::skip]
pub enum Inst {
    Add1(/* dst: */ Reg, /* src1: */ Reg, /* src2: */ Reg),
    Add2(/* dst: */ Reg, /* src: */ Reg, /* imm: */ i8),
    And1(/* dst: */ Reg, /* src1: */ Reg, /* src2: */ Reg),
    And2(/* dst: */ Reg, /* src: */ Reg, /* imm: */ i8),
    Blkw(u16),
    Br(/* n: */ bool, /* z: */ bool, /* p: */ bool, /* label: */ &'static str),
    Fill(u16),
    Jmp(/* base: */ Reg),
    Jsr(/* label: */ &'static str),
    Jsrr(/* base: */ Reg),
    Ld(/* dst: */ Reg, /* label: */ &'static str),
    Ldi(/* dst: */ Reg, /* label: */ &'static str),
    Ldr(/* dst: */ Reg, /* base: */ Reg, /* offset: */ i8),
    Lea(/* dst: */ Reg, /* label: */ &'static str),
    Not(/* dst: */ Reg, /* src: */ Reg),
    Rti,
    St(/* src: */ Reg, /* label: */ &'static str),
    Sti(/* src: */ Reg, /* label: */ &'static str),
    Str(/* src: */ Reg, /* base: */ Reg, /* offset: */ i8),
    Stringz(&'static str),
    Trap(u8),
}

impl Inst {
    pub fn word_len(&self) -> u16 {
        match *self {
            Inst::Blkw(len) => len,
            Inst::Stringz(s) => u16::try_from(s.len()).unwrap(),
            _ => 1,
        }
    }
}

pub type SymbolTable = MultiMap<&'static str, u16>;

#[derive(Debug, Clone)]
pub struct AssemblerError {
    errors: Vec<(u16, String)>,
}

impl fmt::Display for AssemblerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Error assembling program")?;
        for (pc, message) in &self.errors {
            writeln!(f, "  x{:04x}: {}", pc, message)?;
        }
        Ok(())
    }
}

impl error::Error for AssemblerError {}

#[derive(Debug)]
pub struct Program {
    origin: u16,
    bytecode: Vec<u16>,
}

// MOV, ZERO, INC, DEC, RET provided as convenience instructions.
// RET is named as equivalent to JMP R7 in the LC3 specification.
// The other four are suggested in
// https://people.cs.georgetown.edu/~squier/Teaching/HardwareFundamentals/LC3-trunk/src/lc3pre_Language_Extensions.txt
#[macro_export]
macro_rules! asm {
    (@inst ADD $dst:expr, $src:expr, $imm:literal)  => { Add2($dst, $src, $imm) };
    (@inst ADD $dst:expr, $src1:expr, $src2:expr)   => { Add1($dst, $src1, $src2) };
    (@inst AND $dst:expr, $src:expr, $imm:literal)  => { And2($dst, $src, $imm) };
    (@inst AND $dst:expr, $src1:expr, $src2:expr)   => { And1($dst, $src1, $src2) };
    (@inst BLKW $len:expr)                          => { Blkw($len) };
    (@inst BR $lbl:literal)                         => { Br(true, true, true, $lbl) };
    (@inst BRn $lbl:literal)                        => { Br(true, false, false, $lbl) };
    (@inst BRnp $lbl:literal)                       => { Br(true, false, true, $lbl) };
    (@inst BRnz $lbl:literal)                       => { Br(true, true, false, $lbl) };
    (@inst BRnzp $lbl:literal)                      => { Br(true, true, true, $lbl) };
    (@inst BRp $lbl:literal)                        => { Br(false, false, true, $lbl) };
    (@inst BRz $lbl:literal)                        => { Br(false, true, false, $lbl) };
    (@inst BRzp $lbl:literal)                       => { Br(false, true, true, $lbl) };
    (@inst DEC $dst:expr)                           => { Add2($dst, $dst, -1) };
    (@inst FILL $data:expr)                         => { Fill($data) };
    (@inst GETC)                                    => { Trap(0x20) };
    (@inst HALT)                                    => { Trap(0x25) };
    (@inst IN)                                      => { Trap(0x23) };
    (@inst INC $dst:expr)                           => { Add2($dst, $dst, 1) };
    (@inst JMP $base:expr)                          => { Jmp($base) };
    (@inst JSR $lbl:literal)                        => { Jsr($lbl) };
    (@inst LD $dst:expr, $lbl:literal)              => { Ld($dst, $lbl) };
    (@inst LDI $dst:expr, $lbl:literal)             => { Ldi($dst, $lbl) };
    (@inst LDR $dst:expr, $base:expr, $offset:expr) => { Ldr($dst, $base, $offset) };
    (@inst LEA $dst:expr, $lbl:literal)             => { Lea($dst, $lbl) };
    (@inst MOV $dst:expr, $src:expr)                => { Add2($dst, $src, 0) };
    (@inst NOP)                                     => { Fill(0) };
    (@inst NOT $dst:expr, $src:expr)                => { Not($dst, $src) };
    (@inst OUT)                                     => { Trap(0x21) };
    (@inst PUTS)                                    => { Trap(0x22) };
    (@inst PUTSP)                                   => { Trap(0x24) };
    (@inst RET)                                     => { Jmp(R7) };
    (@inst RTI)                                     => { Rti };
    (@inst ST $src:expr, $lbl:literal)              => { St($src, $lbl) };
    (@inst STI $src:expr, $lbl:literal)             => { Sti($src, $lbl) };
    (@inst STR $src:expr, $base:expr, $offset:expr) => { Str($src, $base, $offset) };
    (@inst STRINGZ $str:literal)                    => { Stringz($str) };
    (@inst TRAP $vect:expr)                         => { Trap($vect) };
    (@inst ZERO $dst:expr)                          => { And2($dst, $dst, 0) };

    (
        .ORIG $orig:literal;
        $($(:$lbl:ident)? $op:ident $($t:expr),*;)+
        .END;
    ) => {{
        #[allow(unused_imports)]
        use $crate::assembler::{Inst::*, Program, Reg::*, SymbolTable};
        let mut code = Vec::new();
        #[allow(unused_mut)]
        let mut symtab: SymbolTable = Default::default();
        let origin: u16 = $orig;
        $(
            $(
                symtab.insert(
                    stringify!($lbl),
                    origin + code.iter().map(|i| i.word_len()).sum::<u16>(),
                );
            )*
            code.push(asm! {@inst $op $($t),*});
        )*
        Program::assemble(origin, &code, &symtab)
    }};
}

impl Program {
    fn integer_fits(integer: i32, bits: usize) -> Result<u16, String> {
        let shift = 32 - bits;
        if integer << shift >> shift != integer {
            Err(format!(
                "Value x{:04x} is too large to fit in {} bits",
                integer, bits
            ))
        } else {
            Ok(integer as u16)
        }
    }

    fn calc_offset(
        origin: u16,
        symtab: &SymbolTable,
        pc: u16,
        label: &'static str,
        bits: usize,
    ) -> Result<u16, String> {
        if let Some(v) = symtab.get_vec(label) {
            if v.len() != 1 {
                return Err(format!("Duplicate label \"{}\"", label));
            }
            let addr = v[0];
            let offset = addr as i32 - origin as i32 - pc as i32 - 1;
            Self::integer_fits(offset, bits).map_err(|_| {
                format!(
                    "Label \"{}\" is too far away from instruction ({} words)",
                    label, offset
                )
            })
        } else {
            Err(format!("Undefined label \"{}\"", label))
        }
    }

    pub fn origin(&self) -> usize {
        self.origin as usize
    }

    pub fn bytecode(&self) -> &[u16] {
        &self.bytecode
    }

    #[bitmatch]
    pub fn assemble(
        origin: u16,
        code: &[Inst],
        symtab: &SymbolTable,
    ) -> Result<Self, AssemblerError> {
        use Inst::*;
        let mut words = Vec::new();
        let mut errors = Vec::new();
        for inst in code {
            let pc = u16::try_from(words.len()).unwrap();
            let append_error = |e| {
                errors.push((origin + pc, e));
                0
            };
            match inst {
                Add1(dst, src1, src2) => {
                    let (d, s, a) = (*dst as u16, *src1 as u16, *src2 as u16);
                    words.push(bitpack!("0001_dddsss000aaa"));
                }
                Add2(dst, src, imm) => {
                    let (d, s) = (*dst as u16, *src as u16);
                    let i = Self::integer_fits((*imm).into(), 5).unwrap_or_else(append_error);
                    words.push(bitpack!("0001_dddsss1iiiii"));
                }
                And1(dst, src1, src2) => {
                    let (d, s, a) = (*dst as u16, *src1 as u16, *src2 as u16);
                    words.push(bitpack!("0101_dddsss000aaa"));
                }
                And2(dst, src, imm) => {
                    let (d, s) = (*dst as u16, *src as u16);
                    let i = Self::integer_fits((*imm).into(), 5).unwrap_or_else(append_error);
                    words.push(bitpack!("0101_dddsss1iiiii"));
                }
                Blkw(len) => words.extend(vec![0; *len as usize]),
                Br(n, z, p, label) => {
                    let (n, z, p) = (*n as u16, *z as u16, *p as u16);
                    let o = Self::calc_offset(origin, symtab, pc, label, 9)
                        .unwrap_or_else(append_error);
                    words.push(bitpack!("0000_nzpooooooooo"));
                }
                Fill(v) => words.push(*v),
                Jmp(base) => {
                    let b = *base as u16;
                    words.push(bitpack!("1100_000bbb000000"));
                }
                Jsr(label) => {
                    let o = Self::calc_offset(origin, symtab, pc, label, 11)
                        .unwrap_or_else(append_error);
                    words.push(bitpack!("0100_1ooooooooooo"));
                }
                Jsrr(base) => {
                    let b = *base as u16;
                    words.push(bitpack!("0100_000bbb000000"));
                }
                Ld(dst, label) => {
                    let d = *dst as u16;
                    let o = Self::calc_offset(origin, symtab, pc, label, 9)
                        .unwrap_or_else(append_error);
                    words.push(bitpack!("0010_dddooooooooo"));
                }
                Ldi(dst, label) => {
                    let d = *dst as u16;
                    let o = Self::calc_offset(origin, symtab, pc, label, 9)
                        .unwrap_or_else(append_error);
                    words.push(bitpack!("1010_dddooooooooo"));
                }
                Ldr(dst, base, offset) => {
                    let (d, b) = (*dst as u16, *base as u16);
                    let o = Self::integer_fits((*offset).into(), 6).unwrap_or_else(append_error);
                    words.push(bitpack!("0110_dddbbboooooo"));
                }
                Lea(dst, label) => {
                    let d = *dst as u16;
                    let o = Self::calc_offset(origin, symtab, pc, label, 9)
                        .unwrap_or_else(append_error);
                    words.push(bitpack!("1110_dddooooooooo"));
                }
                Not(dst, src) => {
                    let (d, s) = (*dst as u16, *src as u16);
                    words.push(bitpack!("1001_dddsss111111"));
                }
                Rti => words.push(0b1000_000000000000),
                St(src, label) => {
                    let s = *src as u16;
                    let o = Self::calc_offset(origin, symtab, pc, label, 9)
                        .unwrap_or_else(append_error);
                    words.push(bitpack!("0011_sssooooooooo"));
                }
                Sti(src, label) => {
                    let s = *src as u16;
                    let o = Self::calc_offset(origin, symtab, pc, label, 9)
                        .unwrap_or_else(append_error);
                    words.push(bitpack!("1011_sssooooooooo"));
                }
                Str(src, base, offset) => {
                    let (s, b) = (*src as u16, *base as u16);
                    let o = Self::integer_fits((*offset).into(), 6).unwrap_or_else(append_error);
                    words.push(bitpack!("0111_sssbbboooooo"));
                }
                Stringz(s) => {
                    words.extend(s.bytes().map(|b| b as u16));
                    words.push(0);
                }
                Trap(vect) => {
                    let t = *vect as u16;
                    words.push(bitpack!("1111_0000tttttttt"));
                }
            }
        }
        if words.len() > (0xffff - origin).into() {
            errors.push((0xffff, "Program is too large to fit in memory".to_string()));
        }
        if errors.is_empty() {
            Ok(Self {
                origin,
                bytecode: words,
            })
        } else {
            Err(AssemblerError { errors })
        }
    }

    pub fn write_buf(&self, out: &mut impl WriteBytesExt) -> io::Result<()> {
        out.write_u16::<byteorder::NetworkEndian>(self.origin)?;
        for inst in &self.bytecode {
            out.write_u16::<byteorder::NetworkEndian>(*inst)?;
        }
        Ok(())
    }

    pub fn write_img(&self, fname: &str) -> io::Result<()> {
        let mut file = std::fs::File::create(fname)?;
        self.write_buf(&mut file)
    }
}

#[test]
fn origin() {
    let program = asm! { .ORIG 0x1234; NOP; .END; }.unwrap();
    assert_eq!(program.origin(), 0x1234);
}

#[test]
fn invalid_offset() {
    asm! {
                .ORIG 0x4000;
                LD R1, "lbl";
                HALT;
                BLKW 255;
        :lbl    FILL 23;
                .END;
    }
    .unwrap_err();
}

#[test]
fn invalid_label() {
    asm! {
        .ORIG 0x3000;
        JSR "i don't exist";
        .END;
    }
    .unwrap_err();
}

#[test]
fn duplicate_label() {
    asm! {
                .ORIG 0x3000;
                LD R0, "data";
                OUT;
                HALT;
        :data   FILL 0x30;
        :data   FILL 0x31;
                .END;
    }
    .unwrap_err();
}

#[test]
fn out_of_range() {
    asm! { .ORIG 0x3000; ADD R0, R0, 127; .END; }.unwrap_err();
    asm! { .ORIG 0x3000; AND R0, R0, 127; .END; }.unwrap_err();
    asm! { .ORIG 0x3000; LDR R0, R1, 127; .END; }.unwrap_err();
    asm! { .ORIG 0x3000; STR R0, R1, 127; .END; }.unwrap_err();
}

#[test]
fn too_big() {
    asm! {
        .ORIG 1;
        BLKW 0xffff;
        .END;
    }
    .unwrap_err();
}
