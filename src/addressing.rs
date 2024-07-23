
use crate::cpu::Cpu;

// The 6502 uses a 16-bit address bus, meaning that there are
// 65536 bytes of memory available. However, memory can be
// addressed in a number of different ways, called "addressing
// modes", which vary depending on the instruction (with many
// instructions having several addressing modes):
//  - Absolute ($c0000): the full memory location, in the range
//    0x0000 to 0xffff;
//  - Zero page ($c0): addressing on the first page (the "zero
//    page"), that is, the first 256 bytes of memory. This is
//    faster, as only one byte needs to be looked up, and takes
//    up less space in the assembled code as well;
//  - Zero page X/Y ($c0, X/Y): a zero page address is given,
//    and then the value of the X/Y register is added at the
//    address. The Y variant is only used for the LDX and STX
//    commands, since they are already using the X register;
//  - Absolute X/Y ($c000, X/Y): same as above, but the X/Y
//    registers are added to an absolute address;
//  - Immediate (#$c0): the address value is given directly to
//    the instruction (for example, LDX #$c0 loads the value
//    0xc0 into the register, not the value stored at the
//    address 0xc0);
//  - Implicit: instructions that don't deal with memory
//    locations are said to have implicit addressing -- the
//    argument is implied by the instruction;
//  - Indirect (($c000)): only used by JMP. The byte stored at
//    the argument address and the byte after that are used to
//    form the new address. For example, if 0x0120 contains the
//    value 0xfc and 0x0121 contains 0x02, then "JMP ($0120)"
//    will dereference to "JMP $02fc";
//  - Indirect X (($c0, X)): also "indexed indirect". Just like
//    indirect addressing, but the value of the X register is
//    added to the argument before dereferencing. For example,
//    "LDA ($c0, X)" with X = 0x04 becomes "LDA ($c4)", where
//    ($c4) dereferences to the word stored at 0xc4 and 0xc5;
//  - Indirect Y (($c0), Y): also "indirect indexed". The
//    argument is dereferenced first, and then the value of the
//    Y register is added to the new address.
pub enum AddressingMode {
    Absolute,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    AbsoluteX,
    AbsoluteY,
    Immediate,
    Implicit,
    Indirect,
    IndirectX,
    IndirectY,
}

impl Cpu {
    /// Get the address of the operand of the current
    /// instruction, depending on the addressing mode
    pub fn get_op_address(&self, mode: &AddressingMode) -> u16 {
        match mode {
            AddressingMode::Absolute => self.read_u16(self.program_counter),            
            AddressingMode::ZeroPage => self.read_u8(self.program_counter) as u16,

            AddressingMode::ZeroPageX => self.read_u8(self.program_counter).wrapping_add(self.register_x) as u16,
            AddressingMode::ZeroPageY => self.read_u8(self.program_counter).wrapping_add(self.register_y) as u16,
            
            AddressingMode::AbsoluteX => self.read_u16(self.program_counter).wrapping_add(self.register_x as u16),
            AddressingMode::AbsoluteY => self.read_u16(self.program_counter).wrapping_add(self.register_y as u16),
            
            AddressingMode::Immediate => self.program_counter,

            AddressingMode::Indirect => {
                let ptr = self.read_u16(self.program_counter);
                self.read_u16(ptr)
            },
            AddressingMode::IndirectX => {
                let ptr = self.read_u8(self.program_counter).wrapping_add(self.register_x);
                self.read_u16(ptr as u16)
            }
            AddressingMode::IndirectY => {
                let ptr = self.read_u8(self.program_counter);
                self.read_u16(ptr as u16).wrapping_add(self.register_y as u16)
            }

            AddressingMode::Implicit => panic!("Invalid addressing mode"),
        }
    }
}