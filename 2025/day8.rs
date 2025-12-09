use std::fs;

fn find_parent(dsu: &Vec<i64>, u: usize) -> usize {
    let mut parent_val = dsu[u];
    let mut idx = u;
    while parent_val >= 0 {
        idx = parent_val as usize;
        parent_val = dsu[idx];
    }
    return idx;
}

fn union(dsu: &mut Vec<i64>, u: usize, v: usize) {
    dsu[u] += dsu[v];
    dsu[v] = u as i64;
}

fn part1(points: &Vec<(i64, i64, i64)>, dist: &Vec<(i64, usize, usize)>) {
    let mut dsu = vec![-1; points.len()];
    let mut i = 0;
    while i < 1000 {
        let uparent = find_parent(&dsu, dist[i].1);
        let vparent = find_parent(&dsu, dist[i].2);
        if uparent != vparent {
            union(&mut dsu, uparent, vparent);
        }
        i += 1;
    }
    dsu.sort_unstable();
    println!("{}", -dsu[0] * dsu[1] * dsu[2]);
}

fn part2(points: &Vec<(i64, i64, i64)>, dist: &Vec<(i64, usize, usize)>) {
    let mut dsu = vec![-1; points.len()];
    let mut i = 0;
    let mut max = 1;
    loop {
        let uparent = find_parent(&dsu, dist[i].1);
        let vparent = find_parent(&dsu, dist[i].2);
        if uparent != vparent {
            union(&mut dsu, uparent, vparent);
            max = std::cmp::max(max, -dsu[uparent] as usize);
        }
        if max == points.len() {
            break;
        }
        i += 1;
    }
    let record = dist[i];
    println!("{}", points[record.1].0 * points[record.2].0);
}

fn main() {
    let inp = fs::read_to_string("inp.txt").unwrap();
    let inp = inp.lines()
        .map(|point| {
            let mut point = point.split(',');
            let x: i64 = point.next().unwrap().parse().unwrap();
            let y: i64 = point.next().unwrap().parse().unwrap();
            let z: i64 = point.next().unwrap().parse().unwrap();
            (x, y, z)
        })
        .collect::<Vec<_>>();

    let mut dist = inp.iter()
        .enumerate()
        .flat_map(|(i, &u)| {
            inp.iter()
                .enumerate()
                .skip(i + 1)
                .map(|(j, &v)| {
                    (
                        (u.0 - v.0) * (u.0 - v.0) +
                        (u.1 - v.1) * (u.1 - v.1) +
                        (u.2 - v.2) * (u.2 - v.2),
                        i,
                        j
                    )
                })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    dist.sort_unstable();

    // part1(&inp, &dist);
    part2(&inp, &dist);
}
