use std::fs;

fn part1(inp: &String) {
    let mut ans = 0;
    let mut dial = 50;
    for line in inp.lines() {
        let multiplier = if line.starts_with('L') { -1 } else { 1 };
        let num = multiplier * line[1..].parse::<i32>().unwrap();
        dial = (dial + num).rem_euclid(100);
        if dial == 0 {
            ans += 1;
        }
    }
    println!("{ans}");
}

fn part2(inp: &String) {
    let mut ans = 0;
    let mut dial = 50;
    for line in inp.lines() {
        let multiplier = if line.starts_with('L') { -1 } else { 1 };
        let num = multiplier * line[1..].parse::<i32>().unwrap();
        let mut temp = dial + num;

        // = is important here
        if temp <= 0 {
            temp = temp.abs() + 100;
        }

        ans += temp / 100;

        if dial == 0 && num < 0 {
            // if it came from 0 ends up double counting it
            ans -= 1;
        }

        dial = (dial + num).rem_euclid(100);
    }
    println!("{ans}");
}

fn main() {
    let inp = fs::read_to_string("inp.txt").unwrap();

    part1(&inp);
    part2(&inp);
}

