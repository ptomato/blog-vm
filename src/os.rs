use std::io;

use crate::{asm, VM};

pub(super) fn load(vm: &mut VM) -> io::Result<()> {
    vm.ld_asm(&asm! {
        ORIG 0;
        BLKW 0x26;
        FILL 0x0215;  // 0x26: digit input
        FILL 0x0239;  // 0x27: digit output
        END;
    })?;
    // OS trap routines
    vm.ld_asm(&asm! {
                                ORIG 0x0200;
        // --- Startup code area ---
        // (There is no startup code; jump directly to user code area and start
        // executing there.)
        /* 0x0200 */            LD R7, "user";
        /* 0x0201 */            JMP R7;         // jump directly to user code
        /* 0x0202 */ "user":    FILL 0x3000;    // address of user code

        // --- Data area for trap routines ---
        /* 0x0203 */ "regsave": BLKW 8;         // space for saving registers
        /* 0x020b */ "buf":     BLKW 6;         // five digits plus zero terminator
        /* 0x0211 */ "zero":    FILL 0x30;      // ASCII digit zero
        /* 0x0212 */ "negzero": FILL -0x30i16 as u16; // ASCII zero but already negated, for subtraction
        /* 0x0213 */ "minus":   FILL 0x2d;      // ASCII hyphen-minus
        /* 0x0214 */ "high":    FILL 0x8000;    // high bit constant

        // --- Digit input trap routine (0x26) ---
        // Registers used:
        //   R0 = character input; contains final result on exit
        //   R1 = max number of digits remaining to input; address of register save area
        //   R2 = interim total
        //   R3 = scratch
        //   R4 = negative ASCII '0', for subtraction
        //   R5 = digit
        // In pseudocode:
        //   total = 0;
        //   max = 5;
        //   while true:
        //     ch = input character;
        //     if ch == '\n' break;
        //     if max == 0 continue;
        //     if !isdigit(ch) continue;
        //     print ch;
        //     total *= 10;
        //     total += ch - '0';
        //     max--;
        //   print '\n';

        /* 0x0215 */            ST R1, "regsave"; // save registers
        /* 0x0216 */            LEA R1, "regsave";
        /* 0x0217 */            STR R2, R1, 1;
        /* 0x0218 */            STR R3, R1, 2;
        /* 0x0219 */            STR R4, R1, 3;
        /* 0x021a */            STR R5, R1, 4;
        /* 0x021b */            ZERO R2;         // clear R2, to be used as the interim total
        /* 0x021c */            ZERO R1;
        /* 0x021d */            ADD R1, R1, 5;   // R1 = max number of digits remaining
        /* 0x021e */            LD R4, "negzero";
        /* 0x021f */ "inloop":  GETC;            // store character in R0
        /* 0x0220 */            ADD R3, R0, -0x0a; // test for newline
        /* 0x0221 */            BRz "indone";    // we are done when a newline is entered
        /* 0x0222 */            MOV R1, R1;
        /* 0x0223 */            BRz "inloop";    // ignore further digits if we already have 5
        /* 0x0224 */            ADD R5, R0, R4;  // R5 = entered digit value
        /* 0x0225 */            BRn "inloop";    // next char if less than '0'
        /* 0x0226 */            ADD R3, R5, -9;
        /* 0x0227 */            BRp "inloop";    // next char if more than '9'
        /* 0x0228 */            OUT;             // echo digit in R0 back to console
        /* 0x0229 */            ADD R3, R2, R2;
        /* 0x022a */            ADD R2, R3, R3;
        /* 0x022b */            ADD R2, R2, R2;
        /* 0x022c */            ADD R2, R2, R3;  // R2 is now multiplied by 10
        /* 0x022d */            ADD R2, R2, R5;  // add latest digit
        /* 0x022e */            DEC R1;
        /* 0x022f */            BR "inloop";
        /* 0x0230 */ "indone":  OUT;             // print a final newline
        /* 0x0231 */            MOV R0, R2;      // R0 is returned to the caller
        /* 0x0232 */            LEA R1, "regsave"; // restore other registers
        /* 0x0233 */            LDR R5, R1, 4;
        /* 0x0234 */            LDR R4, R1, 3;
        /* 0x0235 */            LDR R3, R1, 2;
        /* 0x0236 */            LDR R2, R1, 1;
        /* 0x0237 */            LDR R1, R1, 0;
        /* 0x0238 */            RET;

        // --- Digit output trap routine (0x27) ---
        // Registers used:
        //   R0 = input to function; character output; buf pointer
        //   R1 = running total
        //   R2 = digit
        //   R3 = scratch
        //   R4 = ASCII '0' digit; address of register save area
        // In pseudocode:
        //   buf += 5;
        //   total = R0;
        //   if total == 0, print "0\n" and exit
        //   if total & 0x8000:
        //     print '-';
        //     total = -total;
        //   while total > 0:
        //     digit = total % 10;
        //     total /= 10;
        //     R0 = digit + '0';
        //     buf--;
        //     *buf = R0;
        //   puts buf;
        //   print '\n';

        /* 0x0239 */            ST R4, "regsave"; // save registers
        /* 0x023a */            LEA R4, "regsave";
        /* 0x023b */            STR R3, R4, 1;
        /* 0x023c */            STR R2, R4, 2;
        /* 0x023d */            STR R1, R4, 3;
        /* 0x023e */            STR R0, R4, 4;
        /* 0x023f */            MOV R1, R0;      // copy R0 to R1, this is our running total
        /* 0x0240 */            BRnp "nonzero";  // if total is already zero,...
        /* 0x0241 */            LD R0, "zero";
        /* 0x0242 */            OUT;             // ...output zero and skip everything else
        /* 0x0243 */            BR "newline";
        /* 0x0244 */ "nonzero": LD R3, "high";   // check if total is negative
        /* 0x0245 */            AND R3, R1, R3;  // by AND'ing it with the high bit
        /* 0x0246 */            BRz "pos";
        /* 0x0247 */            NOT R1, R1;      // if negative, negate it
        /* 0x0248 */            INC R1;          // (-x == ~x + 1)
        /* 0x0249 */            LD R0, "minus";  // and output a minus sign
        /* 0x024a */            OUT;
        /* 0x024b */ "pos":     LEA R0, "buf";   // load R0 with our string buffer pointer
        /* 0x024c */            ADD R0, R0, 5;   // the string grows backwards, so start one past the end
        /* 0x024d */            LD R4, "zero";   // R4 holds ASCII '0' digit, for convenience in adding
        /* 0x024e */ "outloop": MOV R2, R1;      // prepare to divide R1 by 10. R2 will hold the remainder
        /* 0x024f */            ZERO R1;         // R1 will hold the quotient
        /* 0x0250 */            ADD R3, R2, -10; // test remainder - divisor
        /* 0x0251 */            BRn "divdone";   // do:
        /* 0x0252 */ "divloop": MOV R2, R3;      //   remainder -= divisor
        /* 0x0253 */            INC R1;          //   quotient++
        /* 0x0254 */            ADD R3, R3, -10; // ... while remainder >= divisor
        /* 0x0255 */            BRzp "divloop";
        /* 0x0256 */ "divdone": DEC R0;          // move to the place in the buffer where this digit should go
        /* 0x0257 */            ADD R2, R2, R4;  // remainder is the value of the digit. add ASCII '0' to it
        /* 0x0258 */            STR R2, R0, 0;   // store in buffer
        /* 0x0259 */            MOV R1, R1;      // if the quotient is not 0, repeat for the next digit
        /* 0x025a */            BRnp "outloop";
        /* 0x025b */            PUTS;            // R0 now points to the beginning of the string to print
        /* 0x025c */ "newline": ZERO R0;
        /* 0x025d */            ADD R0, R0, 0x0a; // print 0x0a (ASCII newline)
        /* 0x025e */            OUT;
        /* 0x025f */            LEA R4, "regsave"; // restore registers
        /* 0x0260 */            LDR R0, R4, 4;
        /* 0x0261 */            LDR R1, R4, 3;
        /* 0x0262 */            LDR R2, R4, 2;
        /* 0x0263 */            LDR R3, R4, 1;
        /* 0x0264 */            LDR R4, R4, 0;
        /* 0x0265 */            RET;
                                END;
    })
}

#[test]
fn number_input() {
    let mut vm = VM::new();
    load(&mut vm).unwrap();
    vm.ld_asm(&asm! {
                  ORIG 0x3000;
                  TRAP 0x26;
                  HALT;
                  END;
    })
    .unwrap();

    fn test_number_input(vm: &mut VM, inp: &str, expected: u16) {
        vm.inbuf = String::from(inp);
        vm.start().unwrap();
        assert_eq!(vm.r(0).0, expected);
        assert_eq!(vm.inbuf, "");
        assert_eq!(vm.outbuf, inp);
    }
    test_number_input(&mut vm, "0\n", 0);
    test_number_input(&mut vm, "1\n", 1);
    test_number_input(&mut vm, "9\n", 9);
    test_number_input(&mut vm, "15\n", 15);
    test_number_input(&mut vm, "42\n", 42);
    test_number_input(&mut vm, "99\n", 99);
    test_number_input(&mut vm, "678\n", 678);
    test_number_input(&mut vm, "3303\n", 3303);
    test_number_input(&mut vm, "32767\n", 32767);
    test_number_input(&mut vm, "32768\n", 32768);
    test_number_input(&mut vm, "65535\n", 65535);
    test_number_input(&mut vm, "65536\n", 0);
    test_number_input(&mut vm, "99999\n", /* 99999 - 65536 = */ 34463);

    test_number_input(&mut vm, "\n", 0);

    vm.inbuf = String::from("-11111\n");
    vm.start().unwrap();
    assert_eq!(vm.r(0).0, 11111);
    assert_eq!(vm.outbuf, "11111\n");

    vm.inbuf = String::from("abc\n");
    vm.start().unwrap();
    assert_eq!(vm.r(0).0, 0);
    assert_eq!(vm.outbuf, "\n");
}

#[test]
fn number_output() {
    let mut vm = VM::new();
    load(&mut vm).unwrap();
    vm.ld_asm(&asm! {
                  ORIG 0x3000;
                  TRAP 0x27;
                  HALT;
                  END;
    })
    .unwrap();

    fn test_number_output(vm: &mut VM, inp: u16, expected: &str) {
        vm.rw(0, std::num::Wrapping(inp));
        vm.start().unwrap();
        assert_eq!(vm.outbuf, expected);
    }
    test_number_output(&mut vm, 0, "0\n");
    test_number_output(&mut vm, 1, "1\n");
    test_number_output(&mut vm, 9, "9\n");
    test_number_output(&mut vm, 15, "15\n");
    test_number_output(&mut vm, 42, "42\n");
    test_number_output(&mut vm, 99, "99\n");
    test_number_output(&mut vm, 678, "678\n");
    test_number_output(&mut vm, 3303, "3303\n");
    test_number_output(&mut vm, 32767, "32767\n");
    test_number_output(&mut vm, 32768, "-32768\n");
    test_number_output(&mut vm, 65535, "-1\n");
}

#[test]
fn register_save() {
    // Ensure that R1 through R6 are correctly preserved during the trap
    // routines.
    let mut vm = VM::new();
    load(&mut vm).unwrap();
    vm.ld_asm(&asm! {
        ORIG 0x3000;
        TRAP 0x26;
        TRAP 0x27;
        HALT;
        END;
    })
    .unwrap();
    vm.inbuf = String::from("1234\n");
    // Store values in registers before starting
    for val in 1..=6 {
        vm.rw(val, std::num::Wrapping(val));
    }
    vm.start().unwrap();
    for val in 1..=6 {
        assert_eq!(vm.r(val).0, val);
    }
}
