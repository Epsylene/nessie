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
}

impl CPU {
    
}

fn main() {
    //
}
