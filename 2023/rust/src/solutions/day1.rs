use std::path::PathBuf;

pub fn first(input_file: PathBuf) {
    let mut result = 0;
    for line in std::fs::read_to_string(input_file).unwrap().lines() {
        let digit1 = line
            .chars()
            .find(|x| x.is_digit(10))
            .map(|x| x as usize - '0' as usize)
            .unwrap_or(0);
        let digit2 = line
            .chars()
            .rev()
            .find(|x| x.is_digit(10))
            .map(|x| x as usize - '0' as usize)
            .unwrap_or(0);
        result += digit1 * 10 + digit2;
    }
    println!("{:?}", result);
}

pub fn second(input_file: PathBuf) {
    let mut result = 0;
    let digits = [
        "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
    ];
    for line in std::fs::read_to_string(input_file).unwrap().lines() {
        let mut left = line.to_string();
        let mut right = line.to_string();
        let mut min_index = usize::MAX;
        let mut min_digit = None;
        let mut max_digit = None;
        let mut max_index = usize::MIN;
        for digit in digits.iter() {
            let indices = line
                .match_indices(digit)
                .map(|x| (x.0, x.1.to_string()))
                .collect::<Vec<(usize, String)>>();
            indices.first().map(|(index, digit)| {
                if min_index > *index {
                    min_index = *index;
                    min_digit = Some(digit.clone());
                }
            });
            indices.last().map(|(index, digit)| {
                if max_index < *index {
                    max_index = *index;
                    max_digit = Some(digit.clone());
                }
            });
        }
        if let Some(min_digit) = min_digit {
            let x = digits.iter().position(|&x| x == min_digit).unwrap_or(0) + 1;
            left = left.replace(&min_digit, &x.to_string());
        }
        if let Some(max_digit) = max_digit {
            let x = digits.iter().position(|&x| x == max_digit).unwrap_or(0) + 1;
            right = right.replace(&max_digit, &x.to_string());
        }
        let digit1 = left
            .chars()
            .find(|x| x.is_digit(10))
            .map(|x| x as usize - '0' as usize)
            .unwrap_or(0);
        let digit2 = right
            .chars()
            .rev()
            .find(|x| x.is_digit(10))
            .map(|x| x as usize - '0' as usize)
            .unwrap_or(0);
        let num = digit1 * 10 + digit2;
        dbg!(num);
        result += num;
    }
    println!("{:?}", result);
}
