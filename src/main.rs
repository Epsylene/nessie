mod cpu;
mod addressing;
mod opcodes;

use cpu::Cpu;

fn main() {
    let game = vec![0x69, 0x05, 0x00, 0x00];

    let mut cpu = Cpu::new();
    cpu.load_and_run(game);
}