use std::path::PathBuf;

fn build_tree(input: &Vec<i64>) -> Vec<Vec<i64>> {
    let mut results = vec![input.clone()];
    loop {
        let r = results
            .last()
            .unwrap()
            .windows(2)
            .map(|x| x[1] - x[0])
            .collect::<Vec<i64>>();
        if r.iter().filter(|&&x| x == 0).count() == r.len() {
            results.push(r);
            break;
        }
        results.push(r);
    }
    results
}

fn parse(input_file: PathBuf) -> Vec<Vec<i64>> {
    std::fs::read_to_string(input_file)
        .unwrap()
        .lines()
        .map(|x| {
            x.split_whitespace()
                .map(|x| x.parse::<i64>().unwrap())
                .collect()
        })
        .collect::<Vec<Vec<i64>>>()
}

pub fn first(input_file: PathBuf) {
    let lines = parse(input_file);
    let res = lines
        .iter()
        .map(build_tree)
        .map(|x| x.iter().fold(0, |acc, i| acc + i.last().unwrap()))
        .sum::<i64>();
    dbg!(res);
}

pub fn second(input_file: PathBuf) {
    let lines = parse(input_file);
    let res = lines
        .iter()
        .map(build_tree)
        .map(|x| x.iter().fold(0, |acc, i| i.first().unwrap() - acc))
        .sum::<i64>();
    dbg!(res);
}
