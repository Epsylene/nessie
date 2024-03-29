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
    memory: [u8; 0xFFFF],
}

impl CPU {
    pub fn new() -> Self {
        Self {
            program_counter: 0,
            accumulator: 0,
            status: 0,
            register_x: 0,
            memory: [0; 0xFFFF],
        }
    }

    /// Read a byte from the CPU's memory
    fn read(&self, pos: u16) -> u8 {
        self.memory[pos as usize]
    }

    /// Write a byte to the CPU's memory
    fn write(&mut self, pos: u16, data: u8) {
        self.memory[pos as usize] = data;
    }

    fn read_u16(&self, pos: u16) -> u16 {
        // The NES CPU uses little-endian memory addressing, so
        // we need to split each word in two 8-bit chunks and
        // read them in the correct order
        let lo = self.read(pos) as u16;
        let hi = self.read(pos + 1) as u16;

        (hi << 8) | lo
    }

    fn write_u16(&mut self, pos: u16, data: u16) {
        // The NES CPU uses little-endian memory addressing, so
        // we need to split the 16-bit data into two 8-bit
        // chunks and write them in the correct order
        let hi = (data >> 8) as u8;
        let lo = (data & 0xff) as u8;
        
        self.write(pos, lo);
        self.write(pos + 1, hi);
    }

    pub fn load(&mut self, program: Vec<u8>) {
        // The program is loaded into the CPU's memory, starting
        // at 0x8000 (the beginning of the PRG-ROM space). Then
        // the adress of the first instruction is written at the
        // reset vector, 0xFFFC.
        self.memory[0x8000 .. (0x8000 + program.len())].copy_from_slice(&program[..]);
        self.write_u16(0xFFFC, 0x8000);
    }

    pub fn reset(&mut self) {
        // Resetting restores the state of all the registers,
        // and initializes the program counter with the value
        // stored at 0xFFFC (which tells the CPU where to start
        // the execution of the program)
        self.accumulator = 0;
        self.register_x = 0;
        self.status = 0;

        self.program_counter = self.read_u16(0xFFFC);
    }

    pub fn load_and_run(&mut self, program: Vec<u8>) {
        // The NES implements what is known as a von Neumann
        // architecture, where code and data are stored
        // alongside in the memory. To know where to start
        // execution of the program, the NES platform uses a
        // special mechanism, the reset interrupt: upon
        // inserting a new cartridge, the CPU receives a signal
        // that instructs it to reset its state and set the
        // program counter to the adress stored at 0xFFFC.
        self.load(program);
        self.reset();

        // The program can then be executed. It is a succession
        // of bytes, each either referecing an opcode
        // (instruction) or a parameter for the previous
        // command. For example, the program [0xa9, 0x05, 0x00]
        // loads into the acumulator (0xa9) the value 0x05 and
        // then breaks (0x00).
        loop {
            // To execute the program, we read it byte by byte,
            // retrieving the opcode...
            let opcode = self.read(self.program_counter);
            self.program_counter += 1;

            // ...and executing the corresponding instruction
            match opcode {
                // BRK (Break): forces an interrupt
                // request
                0x00 => return,

                // LDA (Load Accumulator): loads a byte of
                // memory into the accumulator.
                0xA9 => {
                    // The command parameter is the next byte
                    // after the opcode itself
                    let param = self.read(self.program_counter);
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
        cpu.load_and_run(vec![0xa9, 0x05, 0x00]);

        assert_eq!(cpu.accumulator, 0x05);
        assert!(cpu.status & 0b0000_0010 == 0b00);
        assert!(cpu.status & 0b1000_0000 == 0);
    }

    #[test]
    fn test_0xa9_lda_zero_flag() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x00, 0x00]);
        assert!(cpu.status & 0b0000_0010 == 0b10);
    }

    #[test]
    fn test_0xaa_tax_move_a_to_x() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x0a, 0xaa, 0x00]);

        assert_eq!(cpu.register_x, 10)
    }
    
    #[test]
    fn test_5_ops_working_together() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0xc0, 0xaa, 0xe8, 0x00]);

        assert_eq!(cpu.register_x, 0xc1)
    }

    #[test]
    fn test_inx_overflow() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0xff, 0xaa, 0xe8, 0x00]);

        assert_eq!(cpu.register_x, 0)
    }
}

fn main() {
    //
}
