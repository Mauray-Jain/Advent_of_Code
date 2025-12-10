use std::{cmp, fs};

fn part1(inp: &Vec<(i64, i64)>) {
    let max = inp.iter()
        .enumerate()
        .fold(0, |acc, (i, (x1,y1))| {
            inp.iter()
                .skip(i + 1)
                .fold(acc, |inner_acc, (x2,y2)| {
                    let x_diff = (x2 - x1).abs() + 1;
                    let y_diff = (y2 - y1).abs() + 1;
                    cmp::max(inner_acc, x_diff * y_diff)
                })
        });
    println!("{max}");
}

fn part2(inp: &Vec<(i64, i64)>) {
    let pairs = (0..inp.len())
        .flat_map(|i| {
            (i + 1..inp.len())
                .map(|j| (i, j))
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    let mut points = inp.clone();
    points.push(inp[0]);
    let mut max = 0;

    for &(i, j) in pairs.iter() {
        let min_x = cmp::min(inp[i].0, inp[j].0);
        let max_x = cmp::max(inp[i].0, inp[j].0);
        let min_y = cmp::min(inp[i].1, inp[j].1);
        let max_y = cmp::max(inp[i].1, inp[j].1);

        // https://www.reddit.com/r/adventofcode/comments/1phywvn/comment/nt2hps9/?context=3
        // doesnt work for all inputs
        // for example wont work if max area is completely outside
        let mut is_valid = true;
        let mut iter = points.windows(2);
        while let Some(edges) = iter.next() {
            let x1 = edges[0].0;
            let y1 = edges[0].1;
            let x2 = edges[1].0;
            let y2 = edges[1].1;
            if !(cmp::max(x1,x2) <= min_x || cmp::min(x1,x2) >= max_x ||
                 cmp::max(y1,y2) <= min_y || cmp::min(y1,y2) >= max_y)
            {
                is_valid = false;
                break;
            }
        }
        if is_valid {
            let area = (max_x - min_x + 1) * (max_y - min_y + 1);
            max = cmp::max(max, area);
        }
    }

    println!("{max}");
}

fn main() {
    let inp = fs::read_to_string("inp.txt").unwrap();
    let inp = inp.lines()
        .map(|pt| {
            let mut pt = pt.split(',');
            let x: i64 = pt.next().unwrap().parse().unwrap();
            let y: i64 = pt.next().unwrap().parse().unwrap();
            (x,y)
        })
        .collect::<Vec<_>>();
    // part1(&inp);
    part2(&inp);
}
