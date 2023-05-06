use std::num::Wrapping;

use super::{asm, VM};

// Example from the original LC3 textbook; multiply a number by six

#[test]
fn multiply_by_six() {
    let mut vm = VM::new();
    vm.ld_asm(
        &asm! {
                    .ORIG 0x3050;
                    LD R1, "six";
                    LD R2, "number";
                    ZERO R3;  // clear R3; will contain product
            :again  ADD R3, R3, R2;
                    DEC R1;
                    BRp "again";
                    HALT;
            :number BLKW 1;
            :six    FILL 6;
                    .END;
        }
        .unwrap(),
    );
    vm.start().unwrap();
    assert_eq!(vm.r(3).0, 0);
    vm.mem[0x3057] = Wrapping(1);
    vm.start().unwrap();
    assert_eq!(vm.r(3).0, 6);
    vm.mem[0x3057] = Wrapping(9);
    vm.start().unwrap();
    assert_eq!(vm.r(3).0, 54);
}

// Tests the farthest load offset that can be expressed in LC-3 machine code
#[test]
fn max_offset() {
    let mut vm = VM::new();
    vm.ld_asm(
        &asm! {
                    .ORIG 0x4000;
                    LD R1, "lbl";
                    HALT;
                    BLKW 254;
            :lbl    FILL 23;
                    .END;
        }
        .unwrap(),
    );
    vm.start().unwrap();
    assert_eq!(vm.r(1).0, 23);
}

// Some examples from the lab manual of a course:
// https://people.cs.georgetown.edu/~squier/Teaching/HardwareFundamentals/LC3-trunk/docs/LC3-AssemblyManualAndExamples.pdf

#[test]
fn arithmetic() {
    let mut vm = VM::new();
    vm.ld_asm(
        &asm! {
                    .ORIG 0x3000;

                    // Part 1
                    LEA R3, "data";
                    LDR R1, R3, 0;  // R1 = x
                    LDR R2, R3, 1;  // R2 = y

                    ADD R4, R1, R2;
                    STR R4, R3, 2;  // x + y

                    AND R4, R1, R2;
                    STR R4, R3, 3;  // x & y

                    NOT R5, R1;
                    STR R5, R3, 5;  // !x
                    NOT R6, R2;
                    STR R6, R3, 6;  // !y
                    AND R4, R5, R6;
                    NOT R4, R4;
                    STR R4, R3, 4;  // x | y = !(!x & !y)

                    ADD R4, R1, 3;
                    STR R4, R3, 7;  // x + 3

                    ADD R4, R2, -3;
                    STR R4, R3, 8;  // y - 3

                    AND R4, R1, 1;
                    STR R4, R3, 9;  // x % 2

                    // Part 2
                    LDI R1, "x";  // R1 = x
                    LDI R2, "y";  // R2 = y

                    NOT R4, R2;
                    INC R4;  // R4 = -y = !y + 1
                    ADD R5, R4, R1;
                    STI R5, "diff";  // x - y

                    MOV R4, R1;
                    BRzp "xpos";
                    NOT R4, R4;
                    INC R4;
            :xpos   STI R4, "absx";  // abs(x)

                    MOV R5, R2;
                    BRzp "ypos";
                    NOT R5, R5;
                    INC R5;
            :ypos   STI R5, "absy";  // abs(y)

                    ZERO R6;
                    NOT R5, R5;
                    INC R5;
                    ADD R4, R4, R5;  // R4 = abs(x) - abs(y)
                    BRz "eq";
                    BRn "neg";
                    INC R6;
                    BR "eq";
            :neg    ADD R6, R6, 2;
            :eq     STI R6, "comp";

                    HALT;
            :x      FILL 0x3120;
            :y      FILL 0x3121;
            :diff   FILL 0x3122;
            :absx   FILL 0x3123;
            :absy   FILL 0x3124;
            :comp   FILL 0x3125;
                    BLKW 0x100 - 53;
            :data   FILL 9;
                    FILL -13i16 as u16;
                    BLKW 0x1e;
                    FILL 9;
                    FILL -13i16 as u16;
                    .END;
        }
        .unwrap(),
    );
    vm.start().unwrap();
    assert_eq!(vm.r(1).0, 9);
    assert_eq!(vm.r(2).0 as i16, -13);
    assert_eq!(vm.r(3).0, 0x3100);

    assert_eq!(
        vm.mem[0x3100..=0x3109]
            .iter()
            .map(|w| w.0)
            .collect::<Vec<_>>(),
        [0x0009, 0xfff3, 0xfffc, 0x0001, 0xfffb, 0xfff6, 0x000c, 0x000c, 0xfff0, 0x0001]
    );
    assert_eq!(
        vm.mem[0x3120..=0x3125]
            .iter()
            .map(|w| w.0)
            .collect::<Vec<_>>(),
        [0x0009, 0xfff3, 0x0016, 0x0009, 0x000d, 0x0002]
    );
}

#[test]
fn mult() {
    let mut vm = VM::new();
    vm.ld_asm(
        &asm! {
                    .ORIG 0x3000;
                    LEA R3, "in";
                    LEA R4, "out";
                    ZERO R5;
                    ADD R5, R5, 5;
            :next   LDR R0, R3, 0;
                    LDR R1, R3, 1;
                    JSR "mult";
                    STR R2, R4, 0;
                    ADD R3, R3, 2;
                    INC R4;
                    DEC R5;
                    BRzp "next";
                    HALT;

            // 0x300d: multiplication subroutine
            // R0 - x
            // R1 - y
            // R2 - product x * y
            // R3 - sign of result
            :mult   ST R0, "r0";
                    ST R1, "r1";
                    ST R3, "r3";
                    ZERO R3;
                    INC R3;  // sign = 1
                    MOV R0, R0;
                    BRzp "xpos";
                    NOT R0, R0;
                    INC R0;
                    NOT R3, R3;
                    INC R3;
                    // now x is positive:
            :xpos   MOV R1, R1;
                    BRzp "ypos";
                    NOT R1, R1;
                    INC R1;
                    NOT R3, R3;
                    INC R3;
                    // now y is positive:
            :ypos   ZERO R2;
                    MOV R1, R1;
            :loop   BRz "skip";
                    ADD R2, R2, R0;
                    DEC R1;
                    BR "loop";
            :skip   MOV R3, R3;
                    BRp "done";
                    NOT R2, R2;
                    INC R2;
            :done   LD R0, "r0";
                    LD R1, "r1";
                    LD R3, "r3";
                    RET;
            :r0     BLKW 1;
            :r1     BLKW 1;
            :r3     BLKW 1;

            // 0x302f: input data
            :in     FILL 9;
                    FILL 3;
                    FILL 100;
                    FILL 17;
                    FILL 211;
                    FILL 4;
                    FILL 11;
                    FILL -15i16 as u16;
                    FILL 12;
                    FILL 0;
            // 0x3039: space for output
            :out    BLKW 5;

                    .END;
        }
        .unwrap(),
    );
    vm.start().unwrap();
    assert_eq!(vm.mem[0x3039].0, 27);
    assert_eq!(vm.mem[0x303a].0, 1700);
    assert_eq!(vm.mem[0x303b].0, 844);
    assert_eq!(vm.mem[0x303c].0 as i16, -165);
    assert_eq!(vm.mem[0x303d].0, 0);
}

#[test]
fn div() {
    let mut vm = VM::new();
    vm.ld_asm(
        &asm! {
                    .ORIG 0x3000;
                    LEA R3, "in";
                    LEA R4, "out";
                    ZERO R5;
                    ADD R5, R5, 5;
            :next   LDR R0, R3, 0;
                    LDR R1, R3, 1;
                    JSR "div";
                    STR R0, R4, 0;
                    STR R1, R4, 1;
                    STR R2, R4, 2;
                    ADD R3, R3, 2;
                    ADD R4, R4, 3;
                    DEC R5;
                    BRzp "next";
                    HALT;

            // 0x300f: division subroutine
            // R0 - x input / quotient
            // R1 - y input / running remainder
            // R2 - 0 if inputs were not valid
            // R3 - x
            // R4 - -y
            // R5 - scratch
            :div    ST R3, "r3";  // save registers
                    ST R4, "r4";
                    ST R5, "r5";
                    MOV R3, R0;  // copy inputs R0 and R1 to R3 and R4
                    MOV R4, R1;
                    ZERO R0;  // clear outputs
                    ZERO R1;
                    ZERO R2;
                    MOV R3, R3;
                    BRn "exit";
                    MOV R4, R4;
                    BRnz "exit";
                    INC R2;  // input is valid
                    MOV R1, R3;  // R1 now holds x
                    NOT R4, R4;
                    INC R4;  // R4 now holds -y
                    ADD R5, R1, R4; // test remainder - y
                    BRn "exit";
            :loop   MOV R1, R5;  // remainder -= y
                    INC R0;  // quotient++
                    ADD R5, R5, R4; // test remainder - y
                    BRzp "loop";
            :exit   LD R3, "r3";  // restore registers
                    LD R4, "r4";
                    LD R5, "r5";
                    RET;
            :r3     BLKW 1;
            :r4     BLKW 1;
            :r5     BLKW 1;

            // 0x302c: input data
            :in     FILL 9;
                    FILL 3;
                    FILL 100;
                    FILL 17;
                    FILL 211;
                    FILL 4;
                    FILL 11;
                    FILL -15i16 as u16;
                    FILL 12;
                    FILL 0;
            // 0x3036: space for output
            :out    BLKW 15;

                    .END;
        }
        .unwrap(),
    );
    vm.start().unwrap();
    assert_eq!(vm.mem[0x3036].0, 3);
    assert_eq!(vm.mem[0x3037].0, 0);
    assert_eq!(vm.mem[0x3038].0, 1);
    assert_eq!(vm.mem[0x3039].0, 5);
    assert_eq!(vm.mem[0x303a].0, 15);
    assert_eq!(vm.mem[0x303b].0, 1);
    assert_eq!(vm.mem[0x303c].0, 52);
    assert_eq!(vm.mem[0x303d].0, 3);
    assert_eq!(vm.mem[0x303e].0, 1);
    assert_eq!(vm.mem[0x303f].0, 0);
    assert_eq!(vm.mem[0x3040].0, 0);
    assert_eq!(vm.mem[0x3041].0, 0);
    assert_eq!(vm.mem[0x3042].0, 0);
    assert_eq!(vm.mem[0x3043].0, 0);
    assert_eq!(vm.mem[0x3044].0, 0);
}

#[test]
fn fast_mult() {
    let mut vm = VM::new();
    vm.ld_asm(
        &asm! {
                    .ORIG 0x3000;
                    LEA R3, "in";
                    LEA R4, "out";
                    ZERO R5;
                    ADD R5, R5, 5;
            :next   LDR R0, R3, 0;
                    LDR R1, R3, 1;
                    JSR "mult";
                    STR R2, R4, 0;
                    ADD R3, R3, 2;
                    INC R4;
                    DEC R5;
                    BRzp "next";
                    HALT;

            // 0x300d: multiplication subroutine
            // R0 - x
            // R1 - y (shifted)
            // R2 - product x * y
            // R3 - sign of result
            // R4 - bit to test
            // R5 - bit counter
            // R6 - scratch
            :mult   ST R1, "r1";
                    ST R3, "r3";
                    ST R4, "r4";
                    ST R5, "r5";
                    ST R6, "r6";
                    ZERO R3;
                    INC R3;  // sign = 1
                    MOV R0, R0;
                    BRzp "xpos";
                    NOT R0, R0;
                    INC R0;
                    NOT R3, R3;
                    INC R3;
            :xpos   MOV R1, R1;
                    BRzp "ypos";
                    NOT R1, R1;
                    INC R1;
                    NOT R3, R3;
                    INC R3;
            :ypos   ZERO R2;
                    ZERO R4;
                    INC R4;  // bit = 1
                    ADD R5, R2, 15;  // test 15 bits, excluding sign
            :loop   AND R6, R0, R4;
                    BRz "shl";
                    ADD R2, R2, R1;
            :shl    ADD R1, R1, R1;
                    ADD R4, R4, R4;
                    DEC R5;
                    BRp "loop";
                    MOV R3, R3;
                    BRp "done";
                    NOT R2, R2;
                    INC R2;
            :done   LD R1, "r1";
                    LD R3, "r3";
                    LD R4, "r4";
                    LD R5, "r5";
                    LD R6, "r6";
                    RET;
            :r1     BLKW 1;
            :r3     BLKW 1;
            :r4     BLKW 1;
            :r5     BLKW 1;
            :r6     BLKW 1;

            // 0x303a: input data
            :in     FILL 9;
                    FILL 3;
                    FILL 100;
                    FILL 17;
                    FILL -211i16 as u16;
                    FILL -4i16 as u16;
                    FILL 11;
                    FILL -15i16 as u16;
                    FILL 12;
                    FILL 0;
            // 0x3044: space for output
            :out    BLKW 5;

                    .END;
        }
        .unwrap(),
    );
    vm.start().unwrap();
    assert_eq!(vm.mem[0x3044].0, 27);
    assert_eq!(vm.mem[0x3045].0, 1700);
    assert_eq!(vm.mem[0x3046].0, 844);
    assert_eq!(vm.mem[0x3047].0 as i16, -165);
    assert_eq!(vm.mem[0x3048].0, 0);
}
