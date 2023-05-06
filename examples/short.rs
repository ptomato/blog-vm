#[path = "../src/assembler.rs"]
mod assembler;

// Short LC3 assembly examples

fn main() -> anyhow::Result<()> {
    asm! {
                .ORIG 0x3000;
                LEA R0, "hello";  // load address of string
                PUTS;             // output string to console
                HALT;
        :hello  STRINGZ "Hello World!\n";
                .END;
    }?
    .write_img("hello.obj")?;

    // Same thing but packed with two characters per word
    asm! {
                .ORIG 0x3000;
                LEA R0, "hello";
                PUTSP;
                HALT;
        :hello  FILL 0x6548;
                FILL 0x6c6c;
                FILL 0x206f;
                FILL 0x6f57;
                FILL 0x6c72;
                FILL 0x2164;
                FILL 0x000a;
                NOP;
                .END;
    }?
    .write_img("hello2.obj")?;

    asm! {
        .ORIG 0x3000;
        TRAP 0x26;       // read a u16 from stdin and put it in R0
        MOV R1, R0;
        TRAP 0x26;       // read a u16 from stdin and put it in R0
        ADD R0, R1, R0;  // add contents of R1 to R0
        TRAP 0x27;       // show the contents of R0 on stdout
        HALT;
        .END;
    }?
    .write_img("sum.obj")?;

    asm! {
        // Our goal is to take the ten numbers which are stored in memory
        // locations 0x300b through 0x3014, and add them together, leaving the
        // result in register 0 so we can print it out.
                             .ORIG 0x3000;
        /* 0x3000 */         ZERO R0;        // clear R0, to be used for the running sum
        /* 0x3001 */         ZERO R4;        // clear R4, to be used as a counter
        /* 0x3002 */         ADD R4, R4, 10; // load R4 with 10, the number of times to add
        /* 0x3003 */         LEA R2, "arr";  // load the starting address of the data
        /* 0x3004 */ :loop   LDR R3, R2, 0;  // load the next number to be added
        /* 0x3005 */         INC R2;         // increment the pointer
        /* 0x3006 */         ADD R0, R0, R3; // add the next number to the running sum
        /* 0x3007 */         DEC R4;         // decrement the counter
        /* 0x3008 */         BRp "loop";     // do it again if the counter is not yet zero
        /* 0x3009 */         TRAP 0x27;      // show the contents of R0 on stdout
        /* 0x300a */         HALT;
        // --- memory ---
        /* 0x300b */ :arr    FILL 0x0001; // 1
        /* 0x300c */         FILL 0x0002; // +2 = 3
        /* 0x300d */         FILL 0x0001; // +1 = 4
        /* 0x300e */         FILL 0x0002; // +2 = 6
        /* 0x300f */         FILL 0x0003; // +3 = 9
        /* 0x3010 */         FILL 0x0001; // +1 = 10
        /* 0x3011 */         FILL 0x0002; // +2 = 12
        /* 0x3012 */         FILL 0x0001; // +1 = 13
        /* 0x3013 */         FILL 0x0002; // +2 = 15
        /* 0x3014 */         FILL 0x0001; // +1 = 16
                             .END;
    }?
    .write_img("array.obj")?;

    Ok(())
}
