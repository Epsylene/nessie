use crate::addressing::AddressingMode;
use crate::cpu::Cpu;

use std::collections::HashMap;
use lazy_static::lazy_static;

// Instructions are executed by the CPU by reading opcodes.
// Each opcode corresponds to a byte in memory, to which we add
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
        self.zero_negative(self.accumulator);
    }

    /// Copy the accumulator to the X register
    pub fn tax(&mut self) {
        self.register_x = self.accumulator;
        self.zero_negative(self.register_x);
    }

    /// Increment the X register
    pub fn inx(&mut self) {
        self.register_x = self.register_x.wrapping_add(1);
        self.zero_negative(self.register_x);
    }

    /// Store the accumulator in memory
    pub fn sta(&mut self, mode: &AddressingMode) {
        let addr = self.get_op_address(mode);
        self.write(addr, self.accumulator);
    }
}

lazy_static! {
    pub static ref OPCODES: Vec<Opcode> = vec![
        Opcode::new(0x00, "BRK", 1, 7, AddressingMode::Implicit),
        Opcode::new(0xaa, "TAX", 1, 2, AddressingMode::Implicit),

        Opcode::new(0xa9, "LDA", 2, 2, AddressingMode::Immediate),
        Opcode::new(0xa5, "LDA", 2, 3, AddressingMode::ZeroPage),
        Opcode::new(0xad, "LDA", 2, 4, AddressingMode::ZeroPageX),
        Opcode::new(0xb5, "LDA", 3, 4, AddressingMode::Absolute),
        Opcode::new(0xbd, "LDA", 3, 4, AddressingMode::AbsoluteX),
        Opcode::new(0xb9, "LDA", 3, 4, AddressingMode::AbsoluteY),
        Opcode::new(0xa1, "LDA", 2, 6, AddressingMode::IndirectX),
        Opcode::new(0xb1, "LDA", 2, 5, AddressingMode::IndirectY),

        Opcode::new(0xe8, "INX", 1, 2, AddressingMode::Implicit),
    ];

    pub static ref OP_MAP: HashMap<u8, &'static Opcode> = {
        let mut map = HashMap::new();
        for opcode in OPCODES.iter() {
            map.insert(opcode.code, opcode);
        }
        map
    };
}