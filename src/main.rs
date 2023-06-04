// The CPU (Central Processing Unit) is one of the 4 main
// components of the NES's hardware, along with the PPU (Picture
// Processing Unit), the APU (Audio Processing Unit) and the
// RAM. The CPU is responsible for executing the game's code,
// and sends instructions to the PPU and APU to render it. The
// NES's CPU, the 2A03, is a modified version of the 6502 chip;
// it has access to two ressources, the first of which the
// memory map, a contiguous array of 1-byte cells adressed with
// 16-bit pointers and containing:
//  - [0x0000..0x2000]: 2 KiB of RAM memory;
//  - [0x2000..0x4020]: IO registers redirecting to the other
//        available NES modules (PPU, APU, and others);
//  - [0x4020..0x6000]: Expansion ROM, special space controlled
//        by cartridge circuitry, the mappers;
//  - [0x6000..0x8000]: SRAM (Save RAM), reserved space for
//        cartridges to store and retrieve game state;
//  - [0x8000..0xC000]: PRG-ROM (Program ROM), the game's code
//        on the cartridge.
// Since memory access is relatively slow, the CPU has a few
// internal memory slots, the registers, with significantly
// lower access delay (registers are accessed in 2 CPU cycles,
// while RAM is between 3 and 7 depending on the distance in the
// memory space).
pub struct CPU {
    // The NES CPU has 6 registers:
    //  1) Program Counter (PC): holds the address for the next
    //     machine language instruction to be executed.
    //  2) Stack Pointer (SP): holds the address of the top of
    //     the stack space, corresponding to [0x0100..0x1FF] in
    //     memory.
    //  3) Accumulator (A): stores the results of arithmetic,
    //     logic, and memory access operations.
    //  4) Index Register X/Y (X/Y): general purpose registers,
    //     commonly used to hold counters or offset for
    //     accessing memory.
    //  5) Processor Status (P): holds the current status of
    //     operations; each bit represents one of 7 flags that
    //     are set or cleared depending on the result of the
    //     last executed instruction. The flags are, from most
    //     to least significant bit:
    //      - Carry Flag (C): set if the last operation caused
    //          an overflow
    //      - Zero Flag (Z): set if the last operation was zero
    //      - Interrupt Disable (I): set if the program has
    //        executed a 'Set Interrupt Disable' (SEI)
    //        instruction. While set, the processor will not
    //        respond to interrupts from devices until it is
    //        cleared by a 'Clear Interrupt Disable' (CLI)
    //        instruction.
    //      - Decimal Mode (D): while set, the processor
    //        arithmetics will obey Binary Coded Decimal (BCD)
    //        rules, where each digit is represented by a fixed
    //        number of bits. This actually allows performing
    //        decimal arithmetic on numbers, instead of
    //        hexadecimal (for example, $99 + $01 returns $00
    //        with a carry, instead of $9A).
    //      - Break Command (B): set when a BRK instruction has
    //        been executed (forcing an interrupt request).
    //      - Overflow Flag (V): set during arithmetic
    //        operations if the result has yielded an invalid
    //        2's complement result.
    //      - Negative Flag (N): set if the result of the last
    //        operation had bit 7 set to 1 (i.e. the result was
    //        negative). 
    //     There is an unused bit between the B and V bits, not 
    //     corresponding to any flag.
    pub program_counter: u16,
    pub accumulator: u8,
    pub status: u8,
    pub register_x: u8,
}

impl CPU {
    pub fn new() -> Self {
        Self {
            program_counter: 0,
            accumulator: 0,
            status: 0,
            register_x: 0,
        }
    }

    pub fn interpret(&mut self, program: Vec<u8>) {
        self.program_counter = 0;

        loop {
            let opcode = program[self.program_counter as usize];
            self.program_counter += 1;

            match opcode {
                // BRK (Break): forces an interrupt
                // request
                0x00 => return,

                // LDA (Load Accumulator): loads a byte of
                // memory into the accumulator.
                0xA9 => {
                    // The command parameter is the next byte
                    // after the opcode itself
                    let param = program[self.program_counter as usize];
                    self.program_counter += 1;
                    
                    self.accumulator = param;
                    self.zero_negative(self.accumulator)
                }

                // TAX (Transfer Accumulator to X): copies the
                // current contents of the accumulator into the
                // X register.
                0xAA => {
                    self.register_x = self.accumulator;
                    self.zero_negative(self.register_x);
                }

                // INX (Increment X Register): add one to the X
                // register.
                0xE8 => {
                    self.register_x = self.register_x.wrapping_add(1);
                    self.zero_negative(self.register_x);
                }

                _ => todo!(),
            }
        }
    }

    /// Update the zero and negative flags of the status
    /// register depending on the contents of the given register
    fn zero_negative(&mut self, register: u8) {
        // If the register is 0, set the zero flag, otherwise
        // clear it
        if register == 0 {
            self.status |= 0b0000_0010;
        } else {
            self.status &= 0b1111_1101;
        }

        // Set the negative flag if the negative bit of the
        // register is set
        if register & 0b1000_0000 != 0 {
            self.status |= 0b1000_0000;
        } else {
            self.status &= 0b0111_1111;
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_0xa9_lda_immediate_load_data() {
        let mut cpu = CPU::new();
        cpu.interpret(vec![0xa9, 0x05, 0x00]);

        assert_eq!(cpu.accumulator, 0x05);
        assert!(cpu.status & 0b0000_0010 == 0b00);
        assert!(cpu.status & 0b1000_0000 == 0);
    }

    #[test]
    fn test_0xa9_lda_zero_flag() {
        let mut cpu = CPU::new();
        cpu.interpret(vec![0xa9, 0x00, 0x00]);
        assert!(cpu.status & 0b0000_0010 == 0b10);
    }

    #[test]
    fn test_0xaa_tax_move_a_to_x() {
        let mut cpu = CPU::new();
        cpu.accumulator = 10;
        cpu.interpret(vec![0xaa, 0x00]);

        assert_eq!(cpu.register_x, 10)
    }
    
    #[test]
    fn test_5_ops_working_together() {
        let mut cpu = CPU::new();
        cpu.interpret(vec![0xa9, 0xc0, 0xaa, 0xe8, 0x00]);

        assert_eq!(cpu.register_x, 0xc1)
    }

    #[test]
    fn test_inx_overflow() {
        let mut cpu = CPU::new();
        cpu.register_x = 0xff;
        cpu.interpret(vec![0xe8, 0x00]);

        assert_eq!(cpu.register_x, 0)
    }
}

fn main() {
    //
}
