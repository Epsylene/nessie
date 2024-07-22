use crate::addressing::AddressingMode;
use crate::cpu::{Cpu, CpuFlags};

use std::collections::HashMap;
use lazy_static::lazy_static;

// Instructions are executed by the CPU by reading Opcodes.
// Each Opcode corresponds to a byte in memory, to which we add
// a name (for debugging purposes), a number of bytes, a number
// of execution cycles, and an addressing mode.
pub struct Opcode {
    pub code: u8,
    pub name: &'static str,
    pub bytes: u16,
    pub cycles: u8,
    pub mode: AddressingMode,
}

impl Opcode {
    fn new(code: u8, name: &'static str, bytes: u16, cycles: u8, mode: AddressingMode) -> Self {
        Opcode {
            code,
            name,
            bytes,
            cycles,
            mode,
        }
    }
}

impl Cpu {
    /// Load a byte of memory into the accumulator
    pub fn lda(&mut self, mode: &AddressingMode) {
        // The address to load from is the operand of the
        // instruction, which is the next byte in memory
        let addr = self.get_op_address(mode);
        let value = self.read(addr);

        // Then the accumulator is loaded with the value, and
        // zero/negative value flags are checked.
        self.accumulator = value;
        self.update_zn_flags(self.accumulator);
    }

    /// Copy the accumulator to the X register
    pub fn tax(&mut self) {
        self.register_x = self.accumulator;
        self.update_zn_flags(self.register_x);
    }

    /// Increment the X register
    pub fn inx(&mut self) {
        self.register_x = self.register_x.wrapping_add(1);
        self.update_zn_flags(self.register_x);
    }

    /// Store the accumulator in memory
    pub fn sta(&mut self, mode: &AddressingMode) {
        let addr = self.get_op_address(mode);
        self.write(addr, self.accumulator);
    }

    fn add_with_carry(&mut self, data: u8) -> u8 {
        // Add value to accumulator, together with the carry bit
        let carry = self.status.contains(CpuFlags::CARRY) as u8;
        let result = (data as u16) + (self.accumulator as u16) + (carry as u16);

        // Set the carry flag if the result is larger than 255
        // (0xff), since that is the largest value for an
        // unsigned byte.
        if result > 0xff {
            self.status.insert(CpuFlags::CARRY);
        } else {
            self.status.remove(CpuFlags::CARRY);
        }

        // Then set the overflow flag. Overflow occurs when the
        // result of a signed operation does not fit into a
        // signed byte, which inverts the sign bit and makes
        // two positive inputs give a negative output or two
        // negative inputs give a positive output. This may
        // happen either if the two leftmost bits of the input
        // are 0 and there is a carry of 1 from the previous
        // bits, or if the two leftmost bits are 1 and there is
        // a carry of 0. In other words, there is overflow when
        // the two input bit 7 are the same and different from
        // the ouput bit 7: if the sign of both inputs is
        // different from the sign of the output, an overflow
        // has occured.
        let result = result as u8;
        if (result ^ self.accumulator) & (result ^ data) & 0x80 != 0 {
            self.status.insert(CpuFlags::OVERFLOW);
        } else {
            self.status.remove(CpuFlags::OVERFLOW);
        }

        result
    }

    /// Add with carry
    pub fn adc(&mut self, mode: &AddressingMode) {
        // Get value to be added
        let addr = self.get_op_address(mode);
        let value = self.read(addr);

        // Get the result of adding the value to the
        // accumulator with the carry bit
        let result = self.add_with_carry(value);

        // Set the result in the accumulator and update the Z/N
        // flags.
        self.accumulator = result;
        self.update_zn_flags(self.accumulator)
    }

    /// Subtract with carry
    pub fn sbc(&mut self, mode: &AddressingMode) {
        // Get value to be subtracted
        let addr = self.get_op_address(mode);
        let value = self.read(addr);

        // Get the result of subtracting the value from the
        // accumulator with the carry bit
        let result = self.add_with_carry((value as i8).wrapping_neg().wrapping_sub(1) as u8);

        // Finally, set the result in the accumulator and
        // update the Z/N flags. 
        self.accumulator = result;
        self.update_zn_flags(self.accumulator)
    } 

    /// Logical AND
    pub fn and(&mut self, mode: &AddressingMode) {
        let addr = self.get_op_address(mode);
        let value = self.read(addr);

        self.accumulator &= value;
        self.update_zn_flags(self.accumulator);
    }
}

lazy_static! {
    pub static ref OPCODES: Vec<Opcode> = vec![
        // Special
        Opcode::new(0x00, "BRK", 1, 7, AddressingMode::Implicit),
        Opcode::new(0xea, "NOP", 1, 2, AddressingMode::Implicit),

        // Arithmetic
        Opcode::new(0x69, "ADC", 2, 2, AddressingMode::Immediate),
        Opcode::new(0x65, "ADC", 2, 3, AddressingMode::ZeroPage),
        Opcode::new(0x75, "ADC", 2, 4, AddressingMode::ZeroPageX),
        Opcode::new(0x6d, "ADC", 3, 4, AddressingMode::Absolute),
        Opcode::new(0x7d, "ADC", 3, 4, AddressingMode::AbsoluteX),
        Opcode::new(0x79, "ADC", 3, 4, AddressingMode::AbsoluteY),
        Opcode::new(0x61, "ADC", 2, 6, AddressingMode::IndirectX),
        Opcode::new(0x71, "ADC", 2, 5, AddressingMode::IndirectY),

        Opcode::new(0xe9, "SBC", 2, 2, AddressingMode::Immediate),
        Opcode::new(0xe5, "SBC", 2, 3, AddressingMode::ZeroPage),
        Opcode::new(0xf5, "SBC", 2, 4, AddressingMode::ZeroPageX),
        Opcode::new(0xed, "SBC", 3, 4, AddressingMode::Absolute),
        Opcode::new(0xfd, "SBC", 3, 4, AddressingMode::AbsoluteX),
        Opcode::new(0xf9, "SBC", 3, 4, AddressingMode::AbsoluteY),
        Opcode::new(0xe1, "SBC", 2, 6, AddressingMode::IndirectX),
        Opcode::new(0xf1, "SBC", 2, 5, AddressingMode::IndirectY),

        Opcode::new(0x29, "AND", 2, 2, AddressingMode::Immediate),
        Opcode::new(0x25, "AND", 2, 3, AddressingMode::ZeroPage),
        Opcode::new(0x35, "AND", 2, 4, AddressingMode::ZeroPageX),
        Opcode::new(0x2d, "AND", 3, 4, AddressingMode::Absolute),
        Opcode::new(0x3d, "AND", 3, 4, AddressingMode::AbsoluteX),
        Opcode::new(0x39, "AND", 3, 4, AddressingMode::AbsoluteY),
        Opcode::new(0x21, "AND", 2, 6, AddressingMode::IndirectX),
        Opcode::new(0x31, "AND", 2, 5, AddressingMode::IndirectY),

        Opcode::new(0x49, "EOR", 2, 2, AddressingMode::Immediate),
        Opcode::new(0x45, "EOR", 2, 3, AddressingMode::ZeroPage),
        Opcode::new(0x55, "EOR", 2, 4, AddressingMode::ZeroPageX),
        Opcode::new(0x4d, "EOR", 3, 4, AddressingMode::Absolute),
        Opcode::new(0x5d, "EOR", 3, 4, AddressingMode::AbsoluteX),
        Opcode::new(0x59, "EOR", 3, 4, AddressingMode::AbsoluteY),
        Opcode::new(0x41, "EOR", 2, 6, AddressingMode::IndirectX),
        Opcode::new(0x51, "EOR", 2, 5, AddressingMode::IndirectY),

        Opcode::new(0x09, "ORA", 2, 2, AddressingMode::Immediate),
        Opcode::new(0x05, "ORA", 2, 3, AddressingMode::ZeroPage),
        Opcode::new(0x15, "ORA", 2, 4, AddressingMode::ZeroPageX),
        Opcode::new(0x0d, "ORA", 3, 4, AddressingMode::Absolute),
        Opcode::new(0x1d, "ORA", 3, 4, AddressingMode::AbsoluteX),
        Opcode::new(0x19, "ORA", 3, 4, AddressingMode::AbsoluteY),
        Opcode::new(0x01, "ORA", 2, 6, AddressingMode::IndirectX),
        Opcode::new(0x11, "ORA", 2, 5, AddressingMode::IndirectY),

        // Shifts and rotates
        Opcode::new(0x0a, "ASL", 1, 2, AddressingMode::Implicit),
        Opcode::new(0x06, "ASL", 2, 5, AddressingMode::ZeroPage),
        Opcode::new(0x16, "ASL", 2, 6, AddressingMode::ZeroPageX),
        Opcode::new(0x0e, "ASL", 3, 6, AddressingMode::Absolute),
        Opcode::new(0x1e, "ASL", 3, 7, AddressingMode::AbsoluteX),

        Opcode::new(0x4a, "LSR", 1, 2, AddressingMode::Implicit),
        Opcode::new(0x46, "LSR", 2, 5, AddressingMode::ZeroPage),
        Opcode::new(0x56, "LSR", 2, 6, AddressingMode::ZeroPageX),
        Opcode::new(0x4e, "LSR", 3, 6, AddressingMode::Absolute),
        Opcode::new(0x5e, "LSR", 3, 7, AddressingMode::AbsoluteX),

        Opcode::new(0x2a, "ROL", 1, 2, AddressingMode::Implicit),
        Opcode::new(0x26, "ROL", 2, 5, AddressingMode::ZeroPage),
        Opcode::new(0x36, "ROL", 2, 6, AddressingMode::ZeroPageX),
        Opcode::new(0x2e, "ROL", 3, 6, AddressingMode::Absolute),
        Opcode::new(0x3e, "ROL", 3, 7, AddressingMode::AbsoluteX),

        Opcode::new(0x6a, "ROR", 1, 2, AddressingMode::Implicit),
        Opcode::new(0x66, "ROR", 2, 5, AddressingMode::ZeroPage),
        Opcode::new(0x76, "ROR", 2, 6, AddressingMode::ZeroPageX),
        Opcode::new(0x6e, "ROR", 3, 6, AddressingMode::Absolute),
        Opcode::new(0x7e, "ROR", 3, 7, AddressingMode::AbsoluteX),

        Opcode::new(0xe6, "INC", 2, 5, AddressingMode::ZeroPage),
        Opcode::new(0xf6, "INC", 2, 6, AddressingMode::ZeroPageX),
        Opcode::new(0xee, "INC", 3, 6, AddressingMode::Absolute),
        Opcode::new(0xfe, "INC", 3, 7, AddressingMode::AbsoluteX),

        Opcode::new(0xe8, "INX", 1, 2, AddressingMode::Implicit),
        Opcode::new(0xc8, "INY", 1, 2, AddressingMode::Implicit),

        Opcode::new(0xc6, "DEC", 2, 5, AddressingMode::ZeroPage),
        Opcode::new(0xd6, "DEC", 2, 6, AddressingMode::ZeroPageX),
        Opcode::new(0xce, "DEC", 3, 6, AddressingMode::Absolute),
        Opcode::new(0xde, "DEC", 3, 7, AddressingMode::AbsoluteX),

        Opcode::new(0xca, "DEX", 1, 2, AddressingMode::Implicit),
        Opcode::new(0x88, "DEY", 1, 2, AddressingMode::Implicit),

        Opcode::new(0xc9, "CMP", 2, 2, AddressingMode::Immediate),
        Opcode::new(0xc5, "CMP", 2, 3, AddressingMode::ZeroPage),
        Opcode::new(0xd5, "CMP", 2, 4, AddressingMode::ZeroPageX),
        Opcode::new(0xcd, "CMP", 3, 4, AddressingMode::Absolute),
        Opcode::new(0xdd, "CMP", 3, 4, AddressingMode::AbsoluteX),
        Opcode::new(0xd9, "CMP", 3, 4, AddressingMode::AbsoluteY),
        Opcode::new(0xc1, "CMP", 2, 6, AddressingMode::IndirectX),
        Opcode::new(0xd1, "CMP", 2, 5, AddressingMode::IndirectY),

        Opcode::new(0xc0, "CPY", 2, 2, AddressingMode::Immediate),
        Opcode::new(0xc4, "CPY", 2, 3, AddressingMode::ZeroPage),
        Opcode::new(0xcc, "CPY", 3, 4, AddressingMode::Absolute),

        Opcode::new(0xe0, "CPX", 2, 2, AddressingMode::Immediate),
        Opcode::new(0xe4, "CPX", 2, 3, AddressingMode::ZeroPage),
        Opcode::new(0xec, "CPX", 3, 4, AddressingMode::Absolute),

        // Branching
        Opcode::new(0x4c, "JMP", 3, 3, AddressingMode::Absolute),
        Opcode::new(0x6c, "JMP", 3, 5, AddressingMode::Indirect),

        Opcode::new(0x20, "JSR", 3, 6, AddressingMode::Absolute),
        Opcode::new(0x60, "RTS", 1, 6, AddressingMode::Implicit),

        Opcode::new(0x40, "RTI", 1, 6, AddressingMode::Implicit),

        Opcode::new(0xd0, "BNE", 2, 2, AddressingMode::Implicit),
        Opcode::new(0x70, "BVS", 2, 2, AddressingMode::Implicit),
        Opcode::new(0x50, "BVC", 2, 2, AddressingMode::Implicit),
        Opcode::new(0x30, "BMI", 2, 2, AddressingMode::Implicit),
        Opcode::new(0xf0, "BEQ", 2, 2, AddressingMode::Implicit),
        Opcode::new(0xb0, "BCS", 2, 2, AddressingMode::Implicit),
        Opcode::new(0x90, "BCC", 2, 2, AddressingMode::Implicit),
        Opcode::new(0x10, "BPL", 2, 2, AddressingMode::Implicit),

        Opcode::new(0x24, "BIT", 2, 3, AddressingMode::ZeroPage),
        Opcode::new(0x2c, "BIT", 3, 4, AddressingMode::Absolute),

        // Stores and loads
        Opcode::new(0xa9, "LDA", 2, 2, AddressingMode::Immediate),
        Opcode::new(0xa5, "LDA", 2, 3, AddressingMode::ZeroPage),
        Opcode::new(0xb5, "LDA", 2, 4, AddressingMode::ZeroPageX),
        Opcode::new(0xad, "LDA", 3, 4, AddressingMode::Absolute),
        Opcode::new(0xbd, "LDA", 3, 4, AddressingMode::AbsoluteX),
        Opcode::new(0xb9, "LDA", 3, 4, AddressingMode::AbsoluteY),
        Opcode::new(0xa1, "LDA", 2, 6, AddressingMode::IndirectX),
        Opcode::new(0xb1, "LDA", 2, 5, AddressingMode::IndirectY),

        Opcode::new(0xa2, "LDX", 2, 2, AddressingMode::Immediate),
        Opcode::new(0xa6, "LDX", 2, 3, AddressingMode::ZeroPage),
        Opcode::new(0xb6, "LDX", 2, 4, AddressingMode::ZeroPageY),
        Opcode::new(0xae, "LDX", 3, 4, AddressingMode::Absolute),
        Opcode::new(0xbe, "LDX", 3, 4, AddressingMode::AbsoluteY),

        Opcode::new(0xa0, "LDY", 2, 2, AddressingMode::Immediate),
        Opcode::new(0xa4, "LDY", 2, 3, AddressingMode::ZeroPage),
        Opcode::new(0xb4, "LDY", 2, 4, AddressingMode::ZeroPageX),
        Opcode::new(0xac, "LDY", 3, 4, AddressingMode::Absolute),
        Opcode::new(0xbc, "LDY", 3, 4, AddressingMode::AbsoluteX),

        Opcode::new(0x85, "STA", 2, 3, AddressingMode::ZeroPage),
        Opcode::new(0x95, "STA", 2, 4, AddressingMode::ZeroPageX),
        Opcode::new(0x8d, "STA", 3, 4, AddressingMode::Absolute),
        Opcode::new(0x9d, "STA", 3, 5, AddressingMode::AbsoluteX),
        Opcode::new(0x99, "STA", 3, 5, AddressingMode::AbsoluteY),
        Opcode::new(0x81, "STA", 2, 6, AddressingMode::IndirectX),
        Opcode::new(0x91, "STA", 2, 6, AddressingMode::IndirectY),

        Opcode::new(0x86, "STX", 2, 3, AddressingMode::ZeroPage),
        Opcode::new(0x96, "STX", 2, 4, AddressingMode::ZeroPageY),
        Opcode::new(0x8e, "STX", 3, 4, AddressingMode::Absolute),

        Opcode::new(0x84, "STY", 2, 3, AddressingMode::ZeroPage),
        Opcode::new(0x94, "STY", 2, 4, AddressingMode::ZeroPageX),
        Opcode::new(0x8c, "STY", 3, 4, AddressingMode::Absolute),

        // Clear flags
        Opcode::new(0xD8, "CLD", 1, 2, AddressingMode::Implicit),
        Opcode::new(0x58, "CLI", 1, 2, AddressingMode::Implicit),
        Opcode::new(0xb8, "CLV", 1, 2, AddressingMode::Implicit),
        Opcode::new(0x18, "CLC", 1, 2, AddressingMode::Implicit),
        Opcode::new(0x38, "SEC", 1, 2, AddressingMode::Implicit),
        Opcode::new(0x78, "SEI", 1, 2, AddressingMode::Implicit),
        Opcode::new(0xf8, "SED", 1, 2, AddressingMode::Implicit),

        Opcode::new(0xaa, "TAX", 1, 2, AddressingMode::Implicit),
        Opcode::new(0xa8, "TAY", 1, 2, AddressingMode::Implicit),
        Opcode::new(0xba, "TSX", 1, 2, AddressingMode::Implicit),
        Opcode::new(0x8a, "TXA", 1, 2, AddressingMode::Implicit),
        Opcode::new(0x9a, "TXS", 1, 2, AddressingMode::Implicit),
        Opcode::new(0x98, "TYA", 1, 2, AddressingMode::Implicit),

        // Stack
        Opcode::new(0x48, "PHA", 1, 3, AddressingMode::Implicit),
        Opcode::new(0x68, "PLA", 1, 4, AddressingMode::Implicit),
        Opcode::new(0x08, "PHP", 1, 3, AddressingMode::Implicit),
        Opcode::new(0x28, "PLP", 1, 4, AddressingMode::Implicit),
    ];

    pub static ref OP_MAP: HashMap<u8, &'static Opcode> = {
        OPCODES.iter().map(|opcode| (opcode.code, opcode)).collect()
    };
}