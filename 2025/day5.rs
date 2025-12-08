use std::{cmp::max, fs, ops::Range};

fn part1(range: &str, id: &str) {
    let mut ranges = Vec::new();
    for rng in range.lines() {
        let (lower, upper) = rng.split_once('-').unwrap();
        let lower = lower.parse::<u64>().unwrap();
        let upper = upper.parse::<u64>().unwrap();
        ranges.push(Range {start: lower, end: upper + 1});
    }

    let mut ans = 0;
    for i in id.lines() {
        let num = i.parse::<u64>().unwrap();
        for j in ranges.iter() {
            if j.contains(&num) {
                ans += 1;
                break;
            }
        }
    }
    println!("{ans}");
}

fn part2(range: &str) {
    let mut ranges = Vec::new();
    for rng in range.lines() {
        let (lower, upper) = rng.split_once('-').unwrap();
        let lower = lower.parse::<u64>().unwrap();
        let upper = upper.parse::<u64>().unwrap();
        ranges.push((lower, upper));
    }
    ranges.sort_unstable();

    let mut merged = Vec::new();
    let mut curr = ranges[0];
    for (s,e) in ranges.iter().copied() {
        if s <= curr.1 && e >= curr.0 {
            curr.1 = max(e, curr.1);
        } else {
            merged.push(curr);
            curr = (s,e);
        }
    }
    merged.push(curr);

    let ans = merged.iter().fold(0, |acc, (s,e)| {
        acc + (e - s) + 1
    });
    println!("{ans}");
}

fn main() {
    let inp = fs::read_to_string("inp.txt").unwrap();
    let (range, id) = inp.split_once("\n\n").unwrap();
    // part1(range, id);
    part2(range);
}
