use std::path::PathBuf;

fn parse_time_distance(lines: Vec<String>) -> (Vec<i64>, Vec<i64>) {
    let time = lines[0]
        .replace("Time: ", "")
        .trim()
        .split_whitespace()
        .map(|x| x.parse::<i64>().unwrap())
        .collect::<Vec<_>>();
    let distance = lines[1]
        .replace("Distance: ", "")
        .trim()
        .split_whitespace()
        .map(|x| x.parse::<i64>().unwrap())
        .collect::<Vec<_>>();
    (time, distance)
}

fn solve_quadratic_eq(a: i64, b: i64, c: i64) -> Option<(f64, f64)> {
    let disrminant = b.pow(2) - 4 * a * c;
    if disrminant < 0 {
        return None;
    }
    let x = (disrminant as f64).powf(0.5);
    return Some((
        (-b as f64 + x) / (2.0 * a as f64),
        (-b as f64 - x) / (2.0 * a as f64),
    ));
}

pub fn first(input_file: PathBuf) {
    let lines = std::fs::read_to_string(input_file)
        .unwrap()
        .lines()
        .map(String::from)
        .collect::<Vec<String>>();
    let mut result = 1;
    let (time, distance) = parse_time_distance(lines);
    for (t, d) in time.iter().zip(distance.iter()) {
        solve_quadratic_eq(-1, *t, -d).map(|(a, b)| {
            let mut start = a.ceil() as i64;
            let mut end = b.ceil() as i64;
            if a % 1.0 == 0.0 {
                start = a as i64;
            }
            if b % 1.0 == 0.0 {
                end = b as i64 - 1;
            }
            let diff = end.abs_diff(start);
            result *= diff;
        });
    }
    dbg!(result);
}

pub fn second(input_file: PathBuf) {
    let lines = std::fs::read_to_string(input_file)
        .unwrap()
        .lines()
        .map(String::from)
        .collect::<Vec<String>>();
    let t = lines[0]
        .replace("Time: ", "")
        .trim()
        .replace(" ", "")
        .parse::<i64>()
        .unwrap();
    let d = lines[1]
        .replace("Distance: ", "")
        .trim()
        .replace(" ", "")
        .parse::<i64>()
        .unwrap();
    solve_quadratic_eq(-1, t, -d).map(|(a, b)| {
        let mut start = a.ceil() as i64;
        let mut end = b.ceil() as i64;
        if a % 1.0 == 0.0 {
            start = a as i64;
        }
        if b % 1.0 == 0.0 {
            end = b as i64 - 1;
        }
        let diff = end.abs_diff(start);
        dbg!(diff);
    });
}
