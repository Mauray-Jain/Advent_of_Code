use std::fs;

const NEIGHBOURS: [(isize, isize); 8] = [
    (1, 0),
    (1, 1),
    (0, 1),
    (-1, 1),
    (-1, 0),
    (-1, -1),
    (0, -1),
    (1, -1),
];

#[inline]
fn in_bounds(x: isize, y: isize, xlen: usize, ylen: usize) -> bool {
    0 <= x && (x as usize) < xlen && 0 <= y && (y as usize) < ylen
}

fn neighbours(line: isize, idx: isize, inp: &[&[u8]]) -> i32 {
    NEIGHBOURS
        .iter()
        .fold(0, |around, (x,y)| {
            let x = x + line;
            let y = y + idx;
            if in_bounds(x, y, inp.len(), inp[0].len())
               && inp[x as usize][y as usize] == b'@'
            {
                return around + 1;
            }
            around
        })
}

fn part1(inp: &Vec<&[u8]>) {
    let mut ans = 0;
    for (i, line) in inp.iter().enumerate() {
        for (j, item) in line.iter().enumerate() {
            if *item == b'@' {
                if neighbours(i as isize, j as isize, &inp[..]) < 4 {
                    ans += 1;
                }
            }
        }
    }
    println!("{ans}");
}

// Good ideas:
// https://github.com/michel-kraemer/adventofcode-rust/blob/main/2025/day04/src/main.rs
fn part2(inp: &Vec<&[u8]>) {
    let mut ans = 0;
    let mut q = Vec::new();
    let xlen = inp.len();
    let ylen = inp[0].len();
    let mut neighbourcnt = vec![vec![0; ylen]; xlen];

    for (i, line) in inp.iter().enumerate() {
        for (j, item) in line.iter().enumerate() {
            if *item == b'@' {
                for (x,y) in NEIGHBOURS {
                    let x = x + i as isize;
                    let y = y + j as isize;
                    if in_bounds(x, y, xlen, ylen) && inp[x as usize][y as usize] == b'@' {
                        neighbourcnt[i][j] += 1;
                    }
                }
                if neighbourcnt[i][j] < 4 {
                    q.push((i as isize, j as isize));
                }
            }
        }
    }

    while let Some((i, j)) = q.pop() {
        ans += 1;
        for (x,y) in NEIGHBOURS {
            let x = x + i;
            let y = y + j;
            if in_bounds(x, y, xlen, ylen) {
                if neighbourcnt[x as usize][y as usize] == 4 {
                    q.push((x,y));
                }
                neighbourcnt[x as usize][y as usize] -= 1;
            }
        }
    }

    println!("{ans}");
}

fn main() {
    let inp = fs::read_to_string("inp.txt").unwrap();
    let inp = inp.lines().collect::<Vec<_>>();
    let inp = inp.iter().map(|x| x.as_bytes()).collect::<Vec<_>>();
    // part1(&inp);
    part2(&inp);
}
