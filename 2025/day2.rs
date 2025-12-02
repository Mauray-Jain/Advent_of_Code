use std::fs;

fn is_invalid1(num: u64) -> bool {
    let str_num = num.to_string();
    let len = str_num.len();
    len % 2 == 0 && str_num[0..len/2] == str_num[len/2..]
}

fn part1(inp: &str) {
    let mut ans = 0;

    for range in inp.split(',') {
        let minus = range.find('-').unwrap();
        let lower: u64 = range[0..minus].parse().unwrap();
        let upper: u64 = range[minus + 1..].parse().unwrap();
        for i in lower..upper + 1 {
            if is_invalid1(i) {
                ans += i;
            }
        }
    }

    println!("{ans}");
}

fn is_invalid2(num: u64) -> bool {
    let mut vec_num = Vec::new();
    let mut k = num;
    while k > 0 {
        vec_num.push(k % 10);
        k /= 10;
    }
    vec_num.reverse();

    let len = vec_num.len();

    for i in (1..=len/2).rev() {
        let mut chonks = vec_num.chunks(i);
        let first = chonks.next().unwrap();
        let invalidity = chonks.all(|x| x == first);
        if invalidity {
            return true;
        }
    }

    false
}

fn part2(inp: &str) {
    let mut ans = 0;

    for range in inp.split(',') {
        let minus = range.find('-').unwrap();
        let lower: u64 = range[0..minus].parse().unwrap();
        let upper: u64 = range[minus + 1..].parse().unwrap();
        for i in lower..upper + 1 {
            if is_invalid2(i) {
                ans += i;
            }
        }
    }

    println!("{ans}");
}
fn main() {
    let inp = fs::read_to_string("inp.txt").unwrap();
    let inp = &inp[..inp.len() - 1]; // stripping off newline at end

    // part1(inp);
    part2(inp);
}
