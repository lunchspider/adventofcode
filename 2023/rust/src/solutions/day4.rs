use std::{collections::HashSet, path::PathBuf};

pub fn first(input_file: PathBuf) {
    let mut result = 0;
    let lines = std::fs::read_to_string(input_file)
        .unwrap()
        .lines()
        .map(String::from)
        .collect::<Vec<String>>();
    for line in lines.iter() {
        let mut point = 0;
        let mut game = line.split(": ").nth(1).unwrap().split(" | ");
        let winning_numbers = game
            .next()
            .unwrap()
            .split(' ')
            .map(str::trim)
            .filter(|&x| x.len() != 0)
            .map(|x| x.parse::<u32>().unwrap())
            .collect::<HashSet<u32>>();
        let what_we_have = game
            .next()
            .unwrap()
            .split(" ")
            .map(str::trim)
            .filter(|&x| x.len() != 0)
            .map(|x| x.parse::<u32>().unwrap())
            .collect::<Vec<u32>>();
        for i in what_we_have {
            if winning_numbers.contains(&i) {
                if point == 0 {
                    point = 1;
                } else {
                    point *= 2;
                }
            }
        }
        result += point;
    }
    dbg!(result);
}

pub fn second(input_file: PathBuf) {
    let lines = std::fs::read_to_string(input_file)
        .unwrap()
        .lines()
        .map(String::from)
        .collect::<Vec<String>>();

    let mut count = vec![1; lines.len()];
    for (card_number, line) in lines.iter().enumerate() {
        let mut number_of_copies = 0;
        let mut game = line.split(": ").nth(1).unwrap().split(" | ");
        let winning_numbers = game
            .next()
            .unwrap()
            .split(' ')
            .map(str::trim)
            .filter(|&x| x.len() != 0)
            .map(|x| x.parse::<u32>().unwrap())
            .collect::<HashSet<u32>>();
        let what_we_have = game
            .next()
            .unwrap()
            .split(" ")
            .map(str::trim)
            .filter(|&x| x.len() != 0)
            .map(|x| x.parse::<u32>().unwrap())
            .collect::<Vec<u32>>();
        for i in what_we_have {
            if winning_numbers.contains(&i) {
                number_of_copies += 1;
            }
        }
        let card_count = count[card_number];
        while number_of_copies != 0 {
            count[number_of_copies + card_number] += card_count;
            number_of_copies -= 1;
        }
    }
    let result = count.into_iter().sum::<usize>();
    dbg!(result);
}
