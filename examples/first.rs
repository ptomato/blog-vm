// SPDX-License-Identifier: CC0
// SPDX-FileContributor: Authored by Philip Chimento, 2021-2022

// A bare-bones virtual machine for the LC-3 architecture, written in Rust,
// inspired by Andrei Ciobanu's blog post:
// https://www.andreinc.net/2021/12/01/writing-a-simple-vm-in-less-than-125-lines-of-c
// This is the first and simplest version, corresponding to the first blog post
// in my series.
// The full version is in src/main.rs.

use bitmatch::bitmatch;
use byteorder::ReadBytesExt;
use std::num::Wrapping;
use std::{env, fs, io};

type Word = Wrapping<u16>;
type Addr = usize;
type Inst = u16;

#[inline] #[rustfmt::skip] fn adr(w: Word) -> Addr { w.0.into() }

// Sign-EXTend the least significant b bits of n
#[inline]
fn sext(n: u16, b: usize) -> Word {
    assert!(b <= 15);
    let shift = 16 - b;
    Wrapping(((n as i16) << shift >> shift) as u16)
}

// Convert the lower 8 bytes of a machine word to an ASCII character
#[inline]
fn ascii(val: Word) -> char {
    char::from_u32(val.0 as u32 & 0xff).unwrap_or('?')
}

type Reg = u16;
const R0: Reg = 0;
const R7: Reg = 7;
const RPC: Reg = 8; // program counter
const RCND: Reg = 9; // flags register
const NREGS: usize = RCND as usize + 1;

type Flag = Word;
const FP: Flag = Wrapping(1 << 0);
const FZ: Flag = Wrapping(1 << 1);
const FN: Flag = Wrapping(1 << 2);

type TrapVect = u16;
const TGETC: TrapVect = 0x20;
const TOUT: TrapVect = 0x21;
const TPUTS: TrapVect = 0x22;
const TIN: TrapVect = 0x23;
const TPUTSP: TrapVect = 0x24;
const THALT: TrapVect = 0x25;

const MEM_SIZE: usize = u16::MAX as usize + 1;
const PC_START: Word = Wrapping(0x3000);

struct VM {
    mem: [Word; MEM_SIZE],
    reg: [Word; NREGS],
    running: bool,
    term: console::Term,
}

impl VM {
    fn new() -> Self {
        Self {
            mem: [Wrapping(0); MEM_SIZE],
            reg: [Wrapping(0); NREGS],
            running: false,
            term: console::Term::stdout(),
        }
    }

    // Read from and write to memory
    #[inline] #[rustfmt::skip]    fn ld(&mut self, addr: Addr) -> Word { self.mem[addr] }
    #[inline] #[rustfmt::skip]    fn st(&mut self, addr: Addr, val: Word) { self.mem[addr] = val; }

    // Read from and write to register
    #[inline] #[rustfmt::skip]    fn r(&self, r: Reg) -> Word { self.reg[r as usize] }
    #[inline] #[rustfmt::skip]    fn rw(&mut self, r: Reg, val: Word) { self.reg[r as usize] = val; }

    // Methods for manipulating the program counter
    #[inline] #[rustfmt::skip]    fn pc(&self) -> Word { self.r(RPC) }
    #[inline] #[rustfmt::skip]    fn jmp(&mut self, pc: Word) { self.rw(RPC, pc); }
    #[inline] #[rustfmt::skip]    fn jrel(&mut self, offset: Word) { self.jmp(self.pc() + offset); }
    #[inline] #[rustfmt::skip]    fn inc_pc(&mut self) { self.jrel(Wrapping(1)); }
    #[inline] #[rustfmt::skip]    fn iaddr(&mut self, o: u16) -> Addr { adr(self.ld(adr(self.pc() + sext(o, 9)))) }

    // Store value in destination register and update flags register
    #[inline]
    fn dst(&mut self, r: Reg, val: Word) {
        self.rw(r, val);
        let flags = match val.0 {
            0 => FZ,               // val is zero
            1..=0x7fff => FP,      // val is a positive number
            0x8000..=0xffff => FN, // val is a negative number
        };
        self.rw(RCND, flags);
    }

    fn crash(&mut self, message: &str) {
        println!("{}", message);
        self.running = false;
    }

    // Execute one instruction
    #[bitmatch]
    fn exec(&mut self, i: Inst) {
        // "d" = dest reg, "s" = src1 reg, "a" = additional src2 reg,
        // "m" = immediate val, "b" = base reg, "o" = offset (6, 9, or 11 bits),
        // "f" = flags, "t" = trap vector
        #[bitmatch]
        match i {
            "0000fffooooooooo" /* BR   */ => if (self.r(RCND).0 & f) != 0 { self.jrel(sext(o, 9)); },
            "0001dddsss0??aaa" /* ADD1 */ => self.dst(d, self.r(s) + self.r(a)),
            "0001dddsss1mmmmm" /* ADD2 */ => self.dst(d, self.r(s) + sext(m, 5)),
            "0010dddooooooooo" /* LD   */ => { let v = self.ld(adr(self.pc() + sext(o, 9))); self.dst(d, v); }
            "0011sssooooooooo" /* ST   */ => self.st(adr(self.pc() + sext(o, 9)), self.r(s)),
            "01000??bbb??????" /* JSRR */ => { self.rw(R7, self.pc()); self.jmp(self.r(b)); }
            "01001ooooooooooo" /* JSR  */ => { self.rw(R7, self.pc()); self.jrel(sext(o, 11)); }
            "0101dddsss0??aaa" /* AND1 */ => self.dst(d, self.r(s) & self.r(a)),
            "0101dddsss1mmmmm" /* AND2 */ => self.dst(d, self.r(s) & sext(m, 5)),
            "0110dddbbboooooo" /* LDR  */ => { let v = self.ld(adr(self.r(b) + sext(o, 6))); self.dst(d, v); }
            "0111sssbbboooooo" /* STR  */ => self.st(adr(self.r(b) + sext(o, 6)), self.r(s)),
            "1000????????????" /* n/a  */ => self.crash(&format!("Illegal instruction {:#04x}", i)),
            "1001dddsss??????" /* NOT  */ => self.dst(d, !self.r(s)),
            "1010dddooooooooo" /* LDI  */ => { let a = self.iaddr(o); let v = self.ld(a); self.dst(d, v); }
            "1011sssooooooooo" /* STI  */ => { let a = self.iaddr(o); self.st(a, self.r(s)); }
            "1100???bbb??????" /* JMP  */ => self.jmp(self.r(b)),
            "1101????????????" /* RTI  */ => self.crash("RTI not available in user mode"),
            "1110dddooooooooo" /* LEA  */ => self.dst(d, self.pc() + sext(o, 9)),
            "1111????tttttttt" /* TRAP */ => self.trap(t),
        }
    }

    // Wait for a keypress and store its ASCII code in R0
    fn getc(&mut self) -> char {
        let ch = self.term.read_char().unwrap_or('\0');
        let res = Wrapping(if ch.is_ascii() { ch as u16 & 0xff } else { 0 });
        self.rw(R0, res);
        ch
    }

    // Iterator starting at memory address in R0 and ending at first zero location
    fn strz_words(&self) -> impl Iterator<Item = &Word> {
        self.mem[adr(self.r(R0))..].iter().take_while(|&v| v.0 != 0)
    }

    // Software emulation of trap routines
    fn trap(&mut self, t: TrapVect) {
        match t {
            TGETC => {
                self.getc();
            }
            TOUT => print!("{}", ascii(self.r(R0))),
            TPUTS => {
                let string = self.strz_words().map(|&v| ascii(v)).collect::<String>();
                print!("{}", string);
            }
            TIN => {
                print!("> ");
                let ch = self.getc();
                print!("{}", ch);
            }
            TPUTSP => {
                let mut bytes = self
                    .strz_words()
                    .flat_map(|&v| v.0.to_ne_bytes())
                    .collect::<Vec<_>>();
                if bytes[bytes.len() - 1] == 0 {
                    bytes.pop();
                };
                print!("{}", String::from_utf8_lossy(&bytes));
            }
            THALT => self.running = false,
            0x26 => {
                let mut input = String::new();
                io::stdin().read_line(&mut input).unwrap_or(0);
                self.rw(0, Wrapping(input.trim().parse().unwrap_or(0)));
            }
            0x27 => println!("{}", self.r(0)),
            _ => self.crash(&format!("Invalid TRAP vector {:#02x}", t)),
        }
    }

    pub fn start(&mut self) {
        self.jmp(PC_START);
        self.running = true;
        while self.running {
            let i = self.ld(adr(self.pc())).0;
            self.inc_pc();
            self.exec(i);
        }
    }

    pub fn ld_img(&mut self, fname: &str) -> io::Result<()> {
        let mut file = fs::File::open(fname)?;
        let offset = file.read_u16::<byteorder::NetworkEndian>()? as usize;
        let nwords = file.metadata()?.len() as usize / 2 - 1;
        file.read_u16_into::<byteorder::NetworkEndian>(bytemuck::cast_slice_mut(
            &mut self.mem[offset..(offset + nwords)],
        ))
    }
}

fn main() -> io::Result<()> {
    let arg = env::args()
        .nth(1)
        .unwrap_or_else(|| panic!("Requires one argument: file to load"));
    let mut vm = VM::new();
    vm.ld_img(&arg)?;
    vm.start();
    Ok(())
}
