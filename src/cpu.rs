use bitflags::bitflags;
use crate::opcodes::OP_MAP;

// The CPU (Central Processing Unit) is one of the 4 main
// components of the NES's hardware, along with the PPU
// (Picture Processing Unit), the APU (Audio Processing Unit)
// and the RAM. The CPU is responsible for executing the game's
// code, and sends instructions to the PPU and APU to render
// it. The NES CPU, the 2A03, is a modified version of the 6502
// chip; it has access to two ressources, the first of which
// the memory map, a contiguous array of 1-byte cells addressed
// with 16-bit pointers and containing:
//  - [0x0000..0x2000]: 2 KiB of RAM memory;
//  - [0x2000..0x4020]: IO registers redirecting to the other
//        available NES modules (PPU, APU, and others);
//  - [0x4020..0x6000]: Expansion ROM, special space controlled
//        by cartridge circuitry, the mappers;
//  - [0x6000..0x8000]: SRAM (Save RAM), reserved space for
//        cartridges to store and retrieve game state;
//  - [0x8000..0xC000]: PRG-ROM (Program ROM), the game's code
//        on the cartridge.
//
// Since memory access can be quite slow, the CPU has a few
// internal memory slots, the registers, with significantly
// lower access delay (registers are accessed in 2 CPU cycles,
// while RAM is between 3 and 7 depending on the distance in
// the memory space).
pub struct Cpu {
    // The NES CPU has 6 registers:
    //  1) Program Counter (PC): holds the address for the next
    //     machine language instruction to be executed.
    //  2) Stack Pointer (SP): holds the address of the top of
    //     the stack space, corresponding to [0x0100..0x1FF] in
    //     memory.
    //  3) Accumulator (A): stores the results of arithmetic,
    //     logic, and memory access operations.
    //  4-5) Index Register X/Y (X/Y): general purpose registers,
    //     commonly used to hold counters or offset for
    //     accessing memory.
    //  6) Processor Status (P): holds the current status of
    //     operations; each bit represents one of 7 flags that
    //     are set or cleared depending on the result of the
    //     last executed instruction.
    pub program_counter: u16,
    pub stack_pointer: u8,
    pub accumulator: u8,
    pub register_x: u8,
    pub register_y: u8,
    pub status: CpuFlags,
    memory: [u8; 0xFFFF],
}

bitflags! {
    #[derive(Clone)]
    pub struct CpuFlags: u8 {
        // - Carry Flag (C): set if the last operation caused
        //  an overflow
        // - Zero Flag (Z): set if the last operation was zero
        // - Interrupt Disable (I): set if the program has
        //  executed a 'Set Interrupt Disable' (SEI)
        //  instruction. While set, the processor will not
        //  respond to interrupts from devices until it is
        //  cleared by a 'Clear Interrupt Disable' (CLI)
        //  instruction.
        // - Decimal Mode (D): while set, the processor
        //  arithmetics will obey Binary Coded Decimal (BCD)
        //  rules, where each digit is represented by a fixed
        //  number of bits. This actually allows performing
        //  decimal arithmetic on numbers, instead of
        //  hexadecimal (for example, $99 + $01 returns $00
        //  with a carry, instead of $9A).
        // - Break Flag (B): set when a BRK instruction has
        //  been executed (forcing an interrupt request).
        // - None: unused bit between the B and V flags.
        // - Overflow Flag (V): set during arithmetic
        //  operations if the result has yielded an invalid 2's
        //  complement result (for example, in unsigned
        //  arithmetic 80+80 = 160, but in signed arithmetic
        //  80+80 = -96, because 160 is too big to fit in a
        //  byte as a signed number, setting the leftmost bit
        //  to 1 which interprets it as a negative number).
        // - Negative Flag (N): set if the result of the last
        //  operation had bit 7 set to 1 (i.e. the result was
        //  negative).
        const CARRY = 0b0000_0001;
        const ZERO = 0b0000_0010;
        const INTERRUPT_DISABLE = 0b0000_0100;
        const DECIMAL_MODE = 0b0000_1000;
        const BREAK = 0b0001_0000;
        const UNUSED = 0b0010_0000;
        const OVERFLOW = 0b0100_0000;
        const NEGATIVE = 0b1000_0000;
    }
}

impl Cpu {
    // The stack of the 6502 is hardcoded between the adresses
    // 0x0100 and 0x01FF (the second page of memory, after the
    // famous "zero page"). It is used to store the return
    // addresses of subroutines, the status of the processor
    // when an interrupt occurs, and for register preservation.
    // Upon reset, the CPU pushes the program counter and
    // processor status to the stack. The stack starts at
    // 0x1FF, and the stack pointer is decremented 3 times (2
    // bytes for PC, 1 for P) to 0x1FD, so the "reset offset"
    // with the base is 0xFD.
    pub const STACK_BASE: u16 = 0x0100;
    pub const STACK_RESET: u8 = 0xfd;

    pub fn new() -> Self {
        // At startup, the registers are all set to 0, except
        // the stack pointer which is at the stack reset
        // offset. The status register is set with two bits:
        // the unused flag, which is always set to 1 because
        // the bit it corresponds to is hardwired in the
        // internal logic circuitry of the CPU to a 'high'
        // signal line, and the interrupt disable flag, because
        // the hardware might be in a state that triggers an
        // interrupt before the system is ready to handle it.
        Self {
            program_counter: 0,
            accumulator: 0,
            stack_pointer: Cpu::STACK_RESET,
            register_x: 0,
            register_y: 0,
            status: CpuFlags::UNUSED | CpuFlags::INTERRUPT_DISABLE,
            memory: [0; 0xFFFF],
        }
    }

    /// Read a byte from the CPU's memory
    pub fn read_u8(&self, pos: u16) -> u8 {
        self.memory[pos as usize]
    }

    /// Write a byte to the CPU's memory
    pub fn write_u8(&mut self, pos: u16, data: u8) {
        self.memory[pos as usize] = data;
    }

    /// Read a word from the CPU's memory
    pub fn read_u16(&self, pos: u16) -> u16 {
        // The NES CPU uses little-endian memory addressing, so
        // we need to split each word in two 8-bit chunks and
        // read them in the correct order.
        let lo = self.read_u8(pos) as u16;
        let hi = self.read_u8(pos.wrapping_add(1)) as u16;

        (hi << 8) | lo
    }

    /// Write a word to the CPU's memory
    pub fn write_u16(&mut self, pos: u16, data: u16) {
        // Likewise, to write a 2-byte value we split it in two
        // parts and write each in the correct order.
        let hi = (data >> 8) as u8;
        let lo = (data & 0xff) as u8;
        
        self.write_u8(pos, lo);
        self.write_u8(pos + 1, hi);
    }

    /// Push a byte to the stack
    pub fn push_u8(&mut self, data: u8) {
        // To push a byte to the stack, we get its base address
        // (0x0100), offset by the stack pointer, and write at
        // that address.
        let pos = Cpu::STACK_BASE + self.stack_pointer as u16;
        self.write_u8(pos, data);

        // Then, the stack pointer is decremented by one, since
        // the stack grows downwards in memory.
        self.stack_pointer = self.stack_pointer.wrapping_sub(1);
    }

    /// Pop a byte from the stack
    pub fn pop_u8(&mut self) -> u8 {
        // Move the stack pointer one byte up the stack and
        // read the exposed value.
        self.stack_pointer = self.stack_pointer.wrapping_add(1);
        let pos = Cpu::STACK_BASE + self.stack_pointer as u16;
        self.read_u8(pos)
    }

    /// Push a word to the stack
    pub fn push_u16(&mut self, data: u16) {
        // Split into two bytes and push in the correct order
        // (little endian).
        let hi = (data >> 8) as u8;
        let lo = (data & 0xff) as u8;

        self.push_u8(hi);
        self.push_u8(lo);
    }

    /// Pop a word from the stack
    pub fn pop_u16(&mut self) -> u16 {
        // Pop two bytes from the stack and merge them into a
        // word.
        let lo = self.pop_u8() as u16;
        let hi = self.pop_u8() as u16;

        (hi << 8) | lo
    }

    pub fn load(&mut self, program: Vec<u8>) {
        // The program is loaded into the CPU's memory,
        // starting at 0x8000 (the start of of the PRG-ROM
        // space). Then this address is written at the reset
        // vector (0xFFFC), so that the reset interrupt can
        // then pick it up and start the execution of the
        // program.
        self.memory[0x600 .. (0x600 + program.len())].copy_from_slice(&program[..]);
        self.write_u16(0xfffc, 0x600);
    }

    pub fn reset(&mut self) {
        // Resetting restores the state of all the registers,
        // initializes the program counter with the value
        // stored at 0xFFFC (which tells the CPU where to start
        // the execution of the program), resets the stack
        // pointer, and disables interrupts.
        self.accumulator = 0;
        self.register_x = 0;
        self.register_y = 0;
        self.stack_pointer = Cpu::STACK_RESET;
        self.status = CpuFlags::UNUSED | CpuFlags::INTERRUPT_DISABLE;

        self.program_counter = self.read_u16(0xfffc);
    }

    pub fn load_and_run(&mut self, program: Vec<u8>) {
        // The NES implements what is known as a von Neumann
        // architecture, where code and data are stored
        // alongside in the memory. To know where to start
        // execution of the program, the NES platform uses a
        // special mechanism, the reset interrupt: upon
        // inserting a new cartridge, the CPU receives a signal
        // that instructs it to reset its state and set the
        // program counter to the address stored at 0xFFFC.
        self.load(program);
        self.reset();

        // Then the CPU is ready to execute the program.
        self.run();
    }

    pub fn run(&mut self) {
        self.run_with_callback(|_| {});
    }

    pub fn run_with_callback(&mut self, mut callback: impl FnMut(&mut Cpu)) {
        // The program is a succession of bytes, each either
        // referecing an opcode (instruction) or a parameter
        // for the previous command. For example, the program
        // [0xa9, 0x05, 0x00] loads into the acumulator (0xa9)
        // a value (0x05) and then breaks (0x00).
        loop {
            // To execute the program, we read it byte by byte,
            // retrieving the opcode...
            let code = self.read_u8(self.program_counter);
            let opcode = OP_MAP.get(&code).unwrap_or_else(|| panic!("Invalid opcode: 0x{:X}", code));
            
            self.program_counter += 1;
            let state = self.program_counter;
            
            // ...and executing the corresponding instruction
            // with the given addressing mode.
            let mode = &opcode.mode;
            match code {
                // BRK (Break): force an interrupt request
                0x00 => return,
                // ADC (Add with Carry)
                0x69 | 0x65 | 0x75 | 0x6d | 0x7d | 0x79 | 0x61 | 0x71  => {
                    self.adc(mode)
                },
                // SBC (Subtract with Carry)
                0xe9 | 0xe5 | 0xf5 | 0xed | 0xfd | 0xf9 | 0xe1 | 0xf1 => {
                    self.sbc(mode)
                }
                // AND (Logical AND)
                0x29 | 0x25 | 0x35 | 0x2d | 0x3d | 0x39 | 0x21 | 0x31 => {
                    self.and(mode)
                }
                // EOR (Exclusive OR)
                0x49 | 0x45 | 0x55 | 0x4d | 0x5d | 0x59 | 0x41 | 0x51 => {
                    self.eor(mode)
                }
                // ORA (Logical Inclusive OR)
                0x09 | 0x05 | 0x15 | 0x0d | 0x1d | 0x19 | 0x01 | 0x11 => {
                    self.ora(mode)
                }
                // ASL (Arithmetic Shift Left)
                0x0a => self.asl_a(),
                0x06 | 0x16 | 0x0e | 0x1e => {
                    self.asl(mode)
                }
                // LSR (Logical Shift Right)
                0x4a => self.lsr_a(),
                0x46 | 0x56 | 0x4e | 0x5e => {
                    self.lsr(mode)
                }
                // ROL (Rotate Left)
                0x2a => self.rol_a(),
                0x26 | 0x36 | 0x2e | 0x3e => {
                    self.rol(mode)
                }
                // ROR (Rotate Right)
                0x6a => self.ror_a(),
                0x66 | 0x76 | 0x6e | 0x7e => {
                    self.ror(mode)
                }
                // INC (Increment Memory)
                0xe6 | 0xf6 | 0xee | 0xfe => {
                    self.inc(mode)
                }
                // INX (Increment X Register)
                0xe8 => self.inx(),
                // INY (Increment Y Register)
                0xc8 => self.iny(),
                // DEC (Decrement Memory)
                0xc6 | 0xd6 | 0xce | 0xde => {
                    self.dec(mode)
                }
                // DEX (Decrement X Register)
                0xca => self.dex(),
                // DEY (Decrement Y Register)
                0x88 => self.dey(),
                // CMP (Compare Accumulator)
                0xc9 | 0xc5 | 0xd5 | 0xcd | 0xdd | 0xd9 | 0xc1 | 0xd1 => {
                    self.cmp(mode)
                }
                // CPX (Compare X Register)
                0xe0 | 0xe4 | 0xec => self.cpx(mode),
                // CPY (Compare Y Register)
                0xc0 | 0xc4 | 0xcc => self.cpy(mode),
                // JMP (Jump)
                0x4c => self.jmp_abs(),
                0x6c => self.jmp_ind(),
                // JSR (Jump to Subroutine)
                0x20 => self.jsr(),
                // RTS (Return from Subroutine)
                0x60 => self.rts(),
                // RTI (Return from Interrupt)
                0x40 => self.rti(),
                // BCC (Branch if Carry Clear)
                0x90 => self.bcc(),
                // BCS (Branch if Carry Set)
                0xb0 => self.bcs(),
                // BEQ (Branch if Equal)
                0xf0 => self.beq(),
                // BMI (Branch if Minus)
                0x30 => self.bmi(),
                // BNE (Branch if Not Equal)
                0xd0 => self.bne(),
                // BPL (Branch if Positive)
                0x10 => self.bpl(),
                // BVC (Branch if Overflow Clear)
                0x50 => self.bvc(),
                // BVS (Branch if Overflow Set)
                0x70 => self.bvs(),
                // BIT (Bit Test)
                0x24 | 0x2c => self.bit(mode),
                // LDA (Load Accumulator)
                0xa9 | 0xa5 | 0xb5 | 0xad | 0xbd | 0xb9 | 0xa1 | 0xb1 => {
                    self.lda(mode)
                }
                // LDX (Load X Register)
                0xa2 | 0xa6 | 0xb6 | 0xae | 0xbe => {
                    self.ldx(mode)
                }
                // LDY (Load Y Register)
                0xa0 | 0xa4 | 0xb4 | 0xac | 0xbc => {
                    self.ldy(mode)
                }
                // STA (Store Accumulator)
                0x85 | 0x95 | 0x8d | 0x9d | 0x99 | 0x81 | 0x91 => {
                    self.sta(mode)
                }
                // STX (Store X Register)
                0x86 | 0x96 | 0x8e => self.stx(mode),
                // STY (Store Y Register)
                0x84 | 0x94 | 0x8c => self.sty(mode),
                // CLD (Clear Decimal Mode)
                0xd8 => self.cld(),
                // CLI (Clear Interrupt Disable)
                0x58 => self.cli(),
                // CLV (Clear Overflow Flag)
                0xb8 => self.clv(),
                // CLC (Clear Carry Flag)
                0x18 => self.clc(),
                // SEC (Set Carry Flag)
                0x38 => self.sec(),
                // SEI (Set Interrupt Disable)
                0x78 => self.sei(),
                // SED (Set Decimal Flag)
                0xf8 => self.sed(),
                // TAX (Transfer Accumulator to X)
                0xaa => self.tax(),
                // TAY (Transfer Accumulator to Y)
                0xa8 => self.tay(),
                // TSX (Transfer Stack Pointer to X)
                0xba => self.tsx(),
                // TXA (Transfer X to Accumulator)
                0x8a => self.txa(),
                // TXS (Transfer X to Stack Pointer)
                0x9a => self.txs(),
                // TYA (Transfer Y to Accumulator)
                0x98 => self.tya(),
                // PHA (Push Accumulator)
                0x48 => self.pha(),
                // PLA (Pull Accumulator)
                0x68 => self.pla(),
                // PHP (Push Processor Status)
                0x08 => self.php(),
                // PLP (Pull Processor Status)
                0x28 => self.plp(),
                // NOP (No Operation)
                0xea => (),
                // Illegal opcodes
                _ => panic!("Illegal opcode"),
            }

            if self.program_counter == state {
                self.program_counter += (opcode.bytes - 1) as u16;
            }

            // A callback is executed after each instruction to
            // allow for a caller to intercept the execution
            // and read input, write output, or perform other
            // operations.
            callback(self);
        }
    }
}

#[cfg(test)]
mod test {
    use crate::cpu::Cpu;

    #[test]
    fn test_0xa9_lda_immediate_load_data() {
        let mut cpu = Cpu::new();
        cpu.load_and_run(vec![0xa9, 0x05, 0x00]);

        assert_eq!(cpu.accumulator, 0x05);
        assert!(cpu.status.bits() & 0b0000_0010 == 0b00);
        assert!(cpu.status.bits() & 0b1000_0000 == 0);
    }

    #[test]
    fn test_0xa9_lda_zero_flag() {
        let mut cpu = Cpu::new();
        cpu.load_and_run(vec![0xa9, 0x00, 0x00]);
        assert!(cpu.status.bits() & 0b0000_0010 == 0b10);
    }

    #[test]
    fn test_lda_from_memory() {
        let mut cpu = Cpu::new();
        cpu.write_u8(0x10, 0x55);

        cpu.load_and_run(vec![0xa5, 0x10, 0x00]);

        assert_eq!(cpu.accumulator, 0x55);
    }

    #[test]
    fn test_0xaa_tax_move_a_to_x() {
        let mut cpu = Cpu::new();
        cpu.load_and_run(vec![0xa9, 0x0a, 0xaa, 0x00]);

        assert_eq!(cpu.register_x, 10)
    }
    
    #[test]
    fn test_5_ops_working_together() {
        let mut cpu = Cpu::new();
        cpu.load_and_run(vec![0xa9, 0xc0, 0xaa, 0xe8, 0x00]);

        assert_eq!(cpu.register_x, 0xc1)
    }

    #[test]
    fn test_inx_overflow() {
        let mut cpu = Cpu::new();
        cpu.load_and_run(vec![0xa9, 0xff, 0xaa, 0xe8, 0x00]);

        assert_eq!(cpu.register_x, 0)
    }
}