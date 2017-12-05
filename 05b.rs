use std::io;
use std::io::BufRead;

fn main() {
    let mut offsets: Vec<i32> = Vec::new();

    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        offsets.push(line.unwrap().parse().unwrap());
    }

    let mut pointer: i32 = 0;
    let mut counter: u64 = 0;
    let length = offsets.len();

    while pointer >= 0 && pointer < (length as i32) {
        counter += 1;

        let offset = offsets[pointer as usize];

        offsets[pointer as usize] = if offset >= 3 { offset - 1 } else { offset + 1 };
        pointer += offset;
    }

    println!("{}", counter);
}
