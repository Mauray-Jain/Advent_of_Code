use std::fs;

fn part1(inp: &String) {
    let mut ans: u32 = 0;
    for line in inp.lines() {
        let mut first = 0;
        let mut idx = 0;
        let iter = line[0..line.len() - 1].bytes().enumerate();
        for (i, v) in iter {
            if v > first {
                first = v;
                idx = i;
            }
            if v == b'9' {
                break;
            }
        }
        let second = line[idx + 1..].bytes().max().unwrap();
        let joltage = (first - b'0') * 10 + second - b'0';
        ans += joltage as u32;
    }
    println!("{ans}");
}

fn part2(inp: &String) {
    let mut ans: u64 = 0;
    for line in inp.lines() {
        let mut joltage: u64 = 0;
        let mut next_idx = 0;

        for i in 0..12 {
            let iter = line[next_idx..line.len() - (11-i)].bytes().enumerate();
            let mut max = 0;
            let mut max_idx = 0;
            for (j,v) in iter {
                if v > max {
                    max = v;
                    max_idx = j + 1;
                }
                if v == 9 {
                    break;
                }
            }

            next_idx += max_idx;
            joltage = joltage * 10;
            joltage += (max - b'0') as u64;
        }
        ans += joltage;
    }
    println!("{ans}");
}

fn main() {
    let inp = fs::read_to_string("inp.txt").unwrap();
    // part1(&inp);
    part2(&inp);
}
