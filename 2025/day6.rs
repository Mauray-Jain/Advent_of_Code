use std::fs;

fn part1(inp: &String) {
    let mut nums: Vec<Vec<u64>> = Vec::new();
    let mut ops = Vec::new();
    for line in inp.lines() {
        let line = line.split_ascii_whitespace().collect::<Vec<_>>();
        if line[0] != "+" && line[0] != "*" {
            let num = line
                .iter()
                .map(|x| x.parse::<u64>().unwrap())
                .collect::<Vec<_>>();
            for (i,&v) in num.iter().enumerate() {
                if i < nums.len() {
                    nums[i].push(v);
                } else {
                    nums.push(Vec::from([v]));
                }
            }
        } else {
            ops = line;
        }
    }

    let mut ans: u64 = 0;
    for (i,&v) in ops.iter().enumerate() {
        if v == "+" {
            ans += nums[i].iter().sum::<u64>();
        } else {
            ans += nums[i].iter().product::<u64>();
        }
    }
    println!("{ans}");
}

fn part2(inp: &String) {
    let lines = inp.lines().collect::<Vec<_>>();
    let ops = lines[lines.len() - 1].split_ascii_whitespace().collect::<Vec<_>>();
    let lines = &lines[..lines.len() - 1];
    let mut num_lines: Vec<String> = Vec::new();

    for i in 0..lines.len() {
        for (j,v) in lines[i].chars().enumerate() {
            if j < num_lines.len() {
                num_lines[j].push(v);
            } else {
                num_lines.push(String::from(v));
            }
        }
    }

    let mut nums: Vec<Vec<u64>> = Vec::new();
    let mut curr = Vec::new();
    for i in num_lines.iter() {
        let s = i.trim();
        if s != "" {
            let num = s.parse::<u64>().unwrap();
            curr.push(num);
        } else {
            nums.push(curr.clone());
            curr.clear();
        }
    }
    nums.push(curr);

    let mut ans: u64 = 0;
    for (i,&v) in ops.iter().enumerate() {
        if v == "+" {
            ans += nums[i].iter().sum::<u64>();
        } else {
            ans += nums[i].iter().product::<u64>();
        }
    }
    println!("{ans}");
}

fn main() {
    let inp = fs::read_to_string("inp.txt").unwrap();
    // part1(&inp);
    part2(&inp);
}
