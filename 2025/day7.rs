use std::{
    cmp::{min, max},
    collections::HashSet,
    fs
};

fn part1(inp: &String) {
    let mut ans = 0;
    let mut tachyon_rays = HashSet::new();
    let mut lines = inp.lines();
    let first = lines.next().unwrap();
    let source = first.find('S').unwrap();
    tachyon_rays.insert(source);

    for i in lines {
        let mut new = HashSet::new();
        for idx in tachyon_rays.drain() {
            if i.bytes().nth(idx).unwrap() == b'^' {
                new.insert(max(0, idx - 1));
                new.insert(min(i.len() - 1, idx + 1));
                ans += 1;
            } else {
                new.insert(idx);
            }
        }
        tachyon_rays = new;
    }

    println!("{ans}");
}

fn part2(inp: &String) {
    let mut lines = inp.lines();
    let first = lines.next().unwrap();
    let source = first.find('S').unwrap();
    let mut tachyon_rays = vec![0; first.len()];
    tachyon_rays[source] = 1;

    for i in lines {
        for (j,v) in i.bytes().enumerate() {
            if tachyon_rays[j] > 0 && v == b'^' {
                // https://www.reddit.com/r/adventofcode/comments/1pgxv5w/year_2025_day_7_no_memoization_still_runs_in_10_%C2%B5s/
                // kind of like pascal triangle
                // everytime we reach a ^ we add the number of timelines coming
                // at ^ to timelines already at j - 1 and j + 1
                tachyon_rays[j - 1] += tachyon_rays[j];
                tachyon_rays[j + 1] += tachyon_rays[j];
                tachyon_rays[j] = 0; // no ray below a splitter
            }
        }
    }

    println!("{}", tachyon_rays.iter().sum::<u64>());
}

fn main() {
    let inp = fs::read_to_string("inp.txt").unwrap();
    // part1(&inp);
    part2(&inp);
}
