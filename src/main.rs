use bitmatch::bitmatch;
use byteorder::ReadBytesExt;
use std::io::{Read, Write};
use std::num::Wrapping;
use std::{env, fmt, fs, io};

#[cfg(test)]
use std::fmt::Write as FmtWrite;

mod assembler;
mod os;

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

#[inline]
fn ascii(val: Word) -> char {
    char::from_u32(val.0 as u32 & 0xff).unwrap_or('?')
}

type Reg = u16;
const R0: Reg = 0;
const R7: Reg = 7;
const RPC: Reg = 8;
const RCND: Reg = 9;
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

const KBSR: Addr = 0xfe00;
const KBDR: Addr = 0xfe02;
const DSR: Addr = 0xfe04;
const DDR: Addr = 0xfe06;
const MCR: Addr = 0xfffe;
const HIGH_BIT: Word = Wrapping(0x8000);

const MEM_SIZE: usize = u16::MAX as usize + 1;
const PC_START: Word = Wrapping(0x0200);

struct VM {
    mem: [Word; MEM_SIZE],
    reg: [Word; NREGS],
    #[cfg(not(test))]
    term: console::Term,
    #[cfg(test)]
    inbuf: String,
    #[cfg(test)]
    outbuf: String,
}

impl VM {
    fn new() -> Self {
        Self {
            mem: [Wrapping(0); MEM_SIZE],
            reg: [Wrapping(0); NREGS],
            #[cfg(not(test))]
            term: console::Term::stdout(),
            #[cfg(test)]
            inbuf: Default::default(),
            #[cfg(test)]
            outbuf: Default::default(),
        }
    }

    #[inline]
    fn ld(&mut self, addr: Addr) -> Word {
        match addr {
            KBSR => self.update_keyboard_registers(),
            _ => (),
        }
        self.mem[addr]
    }

    #[inline]
    fn st(&mut self, addr: Addr, val: Word) {
        match addr {
            KBSR | KBDR | DSR => panic!("Illegal memory write {:#04x}", addr),
            DDR => print!("{}", ascii(val)),
            _ => (),
        }
        self.mem[addr] = val;
    }

    #[inline] #[rustfmt::skip]    fn r(&self, r: Reg) -> Word { self.reg[r as usize] }
    #[inline] #[rustfmt::skip]    fn rw(&mut self, r: Reg, val: Word) { self.reg[r as usize] = val; }

    #[inline] #[rustfmt::skip]    fn pc(&self) -> Word { self.r(RPC) }
    #[inline] #[rustfmt::skip]    fn jmp(&mut self, pc: Word) { self.rw(RPC, pc); }
    #[inline] #[rustfmt::skip]    fn jrel(&mut self, offset: Word) { self.jmp(self.pc() + offset); }
    #[inline] #[rustfmt::skip]    fn inc_pc(&mut self) { self.jrel(Wrapping(1)); }
    #[inline] #[rustfmt::skip]    fn iaddr(&mut self, o: u16) -> Addr { adr(self.ld(adr(self.pc() + sext(o, 9)))) }

    #[inline]
    fn dst(&mut self, r: Reg, val: Word) {
        self.rw(r, val);
        self.rw(
            RCND,
            match val.0 {
                0 => FZ,               // val is zero
                1..=0x7fff => FP,      // val is a positive number
                0x8000..=0xffff => FN, // val is a negative number
            },
        );
    }

    fn halt(&mut self) {
        self.mem[MCR] ^= HIGH_BIT;
    }

    fn crash(&mut self, message: &str) {
        println!("{}", message);
        self.halt();
    }

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

    #[cfg(not(test))]
    fn getc(&mut self) -> char {
        let ch = self.term.read_char().unwrap_or('\0');
        let res = Wrapping(if ch.is_ascii() { ch as u16 & 0xff } else { 0 });
        self.mem[KBDR] = res;
        self.rw(R0, res);
        ch
    }

    #[cfg(test)]
    fn getc(&mut self) -> char {
        let mut chars = self.inbuf.chars();
        let ch = chars.next().unwrap_or('\0');
        self.inbuf = String::from_iter(chars);
        let res = Wrapping(if ch.is_ascii() { ch as u16 & 0xff } else { 0 });
        self.mem[KBDR] = res;
        self.rw(R0, res);
        ch
    }

    // Read::read_exact() reads without blocking, the buffer is just empty if no
    // key is pressed. Thanks to:
    // https://www.rodrigoaraujo.me/posts/lets-build-an-lc-3-virtual-machine/#memory-mapped-registers
    fn update_keyboard_registers(&mut self) {
        let mut buf = [0; 1];
        self.read_exact(&mut buf).unwrap_or(());
        if buf[0] != 0 {
            self.mem[KBSR] |= HIGH_BIT;
            self.mem[KBDR] = Wrapping(buf[0] as u16);
        } else {
            self.mem[KBSR] &= !HIGH_BIT;
        }
    }

    fn strz_words(&self) -> impl Iterator<Item = &Word> {
        self.mem[adr(self.r(R0))..].iter().take_while(|&v| v.0 != 0)
    }

    fn trap(&mut self, t: TrapVect) {
        match t {
            TGETC => {
                self.getc();
            }
            TOUT => write!(self, "{}", ascii(self.r(R0))).unwrap_or(()),
            TPUTS => {
                let string = self.strz_words().map(|&v| ascii(v)).collect::<String>();
                write!(self, "{}", string).unwrap_or(());
            }
            TIN => {
                write!(self, "> ").unwrap_or(());
                let ch = self.getc();
                write!(self, "{}", ch).unwrap_or(());
            }
            TPUTSP => {
                let mut bytes = self
                    .strz_words()
                    .flat_map(|&v| v.0.to_ne_bytes())
                    .collect::<Vec<_>>();
                if bytes[bytes.len() - 1] == 0 {
                    bytes.pop();
                };
                write!(self, "{}", String::from_utf8_lossy(&bytes)).unwrap_or(());
            }
            THALT => {
                self.halt();
                println!("Halted.");
            }
            _ => {
                self.rw(R7, self.pc());
                let trap_addr = self.mem[t as usize];
                if trap_addr == Wrapping(0) {
                    self.crash(&format!("Invalid TRAP vector {:#02x}", t));
                } else {
                    self.jmp(trap_addr);
                }
            }
        }
    }

    pub fn start(&mut self) -> anyhow::Result<()> {
        #[cfg(test)]
        self.outbuf.clear();
        self.jmp(PC_START);
        self.mem[MCR] |= HIGH_BIT;
        while self.mem[MCR] & HIGH_BIT != Wrapping(0) {
            self.mem[DSR] |= HIGH_BIT;
            let i = self.ld(adr(self.pc())).0;
            self.inc_pc();
            self.exec(i);
            self.flush()?;
        }
        Ok(())
    }

    pub fn ld_asm(&mut self, asm: &assembler::Program) {
        let mut addr = asm.origin();
        for inst in asm.bytecode() {
            self.mem[addr] = Wrapping(*inst);
            addr += 1;
        }
    }

    pub fn ld_img(&mut self, fname: &str) -> anyhow::Result<()> {
        let mut file = fs::File::open(fname)?;
        let offset = file.read_u16::<byteorder::NetworkEndian>()? as usize;
        let nwords = file.metadata()?.len() as usize / 2 - 1;
        Ok(
            file.read_u16_into::<byteorder::NetworkEndian>(bytemuck::cast_slice_mut(
                &mut self.mem[offset..(offset + nwords)],
            ))?,
        )
    }
}

#[cfg(not(test))]
impl Read for VM {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        io::stdin().read(buf)
    }
}

#[cfg(not(test))]
impl Write for VM {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        io::stdout().write(buf)
    }
    fn flush(&mut self) -> io::Result<()> {
        io::stdout().flush()
    }
    fn write_fmt(&mut self, args: fmt::Arguments<'_>) -> io::Result<()> {
        io::stdout().write_fmt(args)
    }
}

#[cfg(test)]
impl Read for VM {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.inbuf.as_bytes().read(buf)
    }
}

#[cfg(test)]
impl Write for VM {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        match self.outbuf.write_str(&String::from_utf8_lossy(buf)) {
            Err(e) => Err(io::Error::new(io::ErrorKind::Other, e)),
            _ => Ok(buf.len()),
        }
    }
    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
    fn write_fmt(&mut self, args: fmt::Arguments<'_>) -> io::Result<()> {
        self.outbuf
            .write_fmt(args)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e))
    }
}

fn main() -> anyhow::Result<()> {
    let arg = env::args()
        .nth(1)
        .unwrap_or_else(|| panic!("Requires one argument: file to load"));
    let mut vm = VM::new();
    os::load(&mut vm)?;
    vm.ld_img(&arg)?;
    vm.start()
}

#[cfg(test)]
mod tests;
