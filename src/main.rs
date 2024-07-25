mod cpu;
mod addressing;
mod opcodes;

use cpu::Cpu;
use sdl2::{
    event::Event,
    keyboard::Keycode,
    pixels::{Color, PixelFormatEnum},
    EventPump,
};
use rand::Rng;

fn handle_user_input(event_pump: &mut EventPump) -> Option<u8> {
    for event in event_pump.poll_iter() {
        match event {
            Event::Quit { .. } | Event::KeyDown { keycode: Some(Keycode::Escape), .. } => {
                std::process::exit(0);
            }
            Event::KeyDown { keycode: Some(Keycode::Up), .. } => return Some(0x77),
            Event::KeyDown { keycode: Some(Keycode::Down), .. } => return Some(0x73),
            Event::KeyDown { keycode: Some(Keycode::Left), .. } => return Some(0x61),
            Event::KeyDown { keycode: Some(Keycode::Right), .. } => return Some(0x64),
            _ => {}
        }
    }
    None
}

fn color_from_u8(value: u8) -> Color {
    match value {
        0 => Color::BLACK,
        1 => Color::WHITE,
        _ => Color::RED,
    }
}

fn read_screen_state(cpu: &Cpu, screen: &mut [u8; 32*32*3]) -> bool {
    let mut changed = false;

    // For each index in the game's "screen memory"...
    for mem_idx in 0x200..0x600 {
        // ...there are 3 bytes in the screen buffer, one per
        // color component.
        let screen_idx = (mem_idx - 0x200) * 3_usize;
        
        // The color is mapped from the value at the memory index...
        let ucolor = cpu.read_u8(mem_idx as u16);
        let (b1, b2, b3) = color_from_u8(ucolor).rgb();

        // ...and if it has changed, the screen buffer is
        // updated.
        if screen[screen_idx] != b1 || screen[screen_idx + 1] != b2 || screen[screen_idx + 2] != b3 {
            screen[screen_idx] = b1;
            screen[screen_idx + 1] = b2;
            screen[screen_idx + 2] = b3;
            changed = true;
        }
    }

    // Return whether the screen has changed, to avoid
    // unnecessarily updating the texture.
    changed
}

fn main() {
    // Init SDL and create a window
    let sdl = sdl2::init().unwrap();
    let video = sdl.video().unwrap();
    let scale = 10.0;
    let window = video
        .window("NES Emulator", 32*scale as u32, 32*scale as u32)
        .position_centered()
        .build()
        .unwrap();

    // Create a canvas to draw on
    let mut canvas = window.into_canvas().build().unwrap();
    let mut event_handler = sdl.event_pump().unwrap();
    canvas.set_scale(scale, scale).unwrap();

    // Create texture to render to
    let tex_creator = canvas.texture_creator();
    let mut texture = tex_creator
        .create_texture_target(PixelFormatEnum::RGB24, 32, 32)
        .unwrap();

    let game = vec![
        0x20, 0x06, 0x06, 0x20, 0x38, 0x06, 0x20, 0x0d, 0x06, 0x20, 0x2a, 0x06, 0x60, 0xa9, 0x02, 0x85,
        0x02, 0xa9, 0x04, 0x85, 0x03, 0xa9, 0x11, 0x85, 0x10, 0xa9, 0x10, 0x85, 0x12, 0xa9, 0x0f, 0x85,
        0x14, 0xa9, 0x04, 0x85, 0x11, 0x85, 0x13, 0x85, 0x15, 0x60, 0xa5, 0xfe, 0x85, 0x00, 0xa5, 0xfe,
        0x29, 0x03, 0x18, 0x69, 0x02, 0x85, 0x01, 0x60, 0x20, 0x4d, 0x06, 0x20, 0x8d, 0x06, 0x20, 0xc3,
        0x06, 0x20, 0x19, 0x07, 0x20, 0x20, 0x07, 0x20, 0x2d, 0x07, 0x4c, 0x38, 0x06, 0xa5, 0xff, 0xc9,
        0x77, 0xf0, 0x0d, 0xc9, 0x64, 0xf0, 0x14, 0xc9, 0x73, 0xf0, 0x1b, 0xc9, 0x61, 0xf0, 0x22, 0x60,
        0xa9, 0x04, 0x24, 0x02, 0xd0, 0x26, 0xa9, 0x01, 0x85, 0x02, 0x60, 0xa9, 0x08, 0x24, 0x02, 0xd0,
        0x1b, 0xa9, 0x02, 0x85, 0x02, 0x60, 0xa9, 0x01, 0x24, 0x02, 0xd0, 0x10, 0xa9, 0x04, 0x85, 0x02,
        0x60, 0xa9, 0x02, 0x24, 0x02, 0xd0, 0x05, 0xa9, 0x08, 0x85, 0x02, 0x60, 0x60, 0x20, 0x94, 0x06,
        0x20, 0xa8, 0x06, 0x60, 0xa5, 0x00, 0xc5, 0x10, 0xd0, 0x0d, 0xa5, 0x01, 0xc5, 0x11, 0xd0, 0x07,
        0xe6, 0x03, 0xe6, 0x03, 0x20, 0x2a, 0x06, 0x60, 0xa2, 0x02, 0xb5, 0x10, 0xc5, 0x10, 0xd0, 0x06,
        0xb5, 0x11, 0xc5, 0x11, 0xf0, 0x09, 0xe8, 0xe8, 0xe4, 0x03, 0xf0, 0x06, 0x4c, 0xaa, 0x06, 0x4c,
        0x35, 0x07, 0x60, 0xa6, 0x03, 0xca, 0x8a, 0xb5, 0x10, 0x95, 0x12, 0xca, 0x10, 0xf9, 0xa5, 0x02,
        0x4a, 0xb0, 0x09, 0x4a, 0xb0, 0x19, 0x4a, 0xb0, 0x1f, 0x4a, 0xb0, 0x2f, 0xa5, 0x10, 0x38, 0xe9,
        0x20, 0x85, 0x10, 0x90, 0x01, 0x60, 0xc6, 0x11, 0xa9, 0x01, 0xc5, 0x11, 0xf0, 0x28, 0x60, 0xe6,
        0x10, 0xa9, 0x1f, 0x24, 0x10, 0xf0, 0x1f, 0x60, 0xa5, 0x10, 0x18, 0x69, 0x20, 0x85, 0x10, 0xb0,
        0x01, 0x60, 0xe6, 0x11, 0xa9, 0x06, 0xc5, 0x11, 0xf0, 0x0c, 0x60, 0xc6, 0x10, 0xa5, 0x10, 0x29,
        0x1f, 0xc9, 0x1f, 0xf0, 0x01, 0x60, 0x4c, 0x35, 0x07, 0xa0, 0x00, 0xa5, 0xfe, 0x91, 0x00, 0x60,
        0xa6, 0x03, 0xa9, 0x00, 0x81, 0x10, 0xa2, 0x00, 0xa9, 0x01, 0x81, 0x10, 0x60, 0xa2, 0x00, 0xea,
        0xea, 0xca, 0xd0, 0xfb, 0x60
    ];

    let mut cpu = Cpu::new();
    cpu.load(game);
    cpu.reset();

    let mut rng = rand::thread_rng();
    let mut screen_buf = [0; 32*32*3];

    cpu.run_with_callback(|cpu| {
        // Get the input and write it to the controller
        if let Some(keycode) = handle_user_input(&mut event_handler) {
            cpu.write_u8(0xff, keycode);
        }

        cpu.write_u8(0xfe, rng.gen_range(0..255));

        if read_screen_state(cpu, &mut screen_buf) {
            texture.update(None, &screen_buf, 32*3).unwrap();
            canvas.copy(&texture, None, None).unwrap();
            canvas.present();
        }

        // Sleep for a bit to avoid running too fast
        std::thread::sleep(std::time::Duration::from_micros(70));
    });
}