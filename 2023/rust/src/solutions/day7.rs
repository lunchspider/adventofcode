use std::{cmp::Ordering, collections::HashMap, path::PathBuf};

#[derive(PartialEq, Eq, Debug)]
enum Rank {
    Five = 7,
    Four = 6,
    Full = 5,
    Three = 4,
    Two = 3,
    One = 2,
    High = 1,
}
const CARDSRANK1: [char; 13] = [
    'A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2',
];

const CARDSRANK2: [char; 13] = [
    'A', 'K', 'Q', 'T', '9', '8', '7', '6', '5', '4', '3', '2', 'J',
];

fn parse(input_file: PathBuf) -> Vec<(String, i64)> {
    let lines = std::fs::read_to_string(input_file)
        .unwrap()
        .lines()
        .map(String::from)
        .collect::<Vec<String>>();
    lines
        .iter()
        .map(|s| {
            let hand = s.split_whitespace().nth(0).unwrap().to_string();
            let bid = s.split_whitespace().nth(1).unwrap().parse::<i64>().unwrap();
            (hand, bid)
        })
        .collect::<Vec<(String, i64)>>()
}

fn calc_position_1(ch: char) -> usize {
    CARDSRANK1.iter().position(|&x| x == ch).unwrap_or(0)
}

fn calc_position_2(ch: char) -> usize {
    CARDSRANK2.iter().position(|&x| x == ch).unwrap_or(0)
}

fn calc_rank_1(hand: &str) -> Rank {
    let mut freq = HashMap::new();
    for i in hand.chars() {
        *freq.entry(i).or_insert(0) += 1;
    }
    let mut label_counts = freq.values().collect::<Vec<_>>();
    label_counts.sort();

    match label_counts.as_slice() {
        [5] => Rank::Five,
        [1, 4] => Rank::Four,
        [2, 3] => Rank::Full,
        [1, 1, 3] => Rank::Three,
        [1, 2, 2] => Rank::Two,
        [1, 1, 1, 2] => Rank::One,
        _ => Rank::High,
    }
}

pub fn first(input_file: PathBuf) {
    let mut info = parse(input_file);
    info.sort_by(|a, b| {
        let a_rank = calc_rank_1(&a.0);
        let b_rank = calc_rank_1(&b.0);
        if a_rank == b_rank {
            for index in 0..5 {
                let a_index = calc_position_1(a.0.chars().nth(index).unwrap());
                let b_index = calc_position_1(b.0.chars().nth(index).unwrap());
                match a_index.cmp(&b_index) {
                    std::cmp::Ordering::Equal => {}
                    Ordering::Less => return Ordering::Greater,
                    Ordering::Greater => return Ordering::Less,
                }
            }
            return Ordering::Equal;
        } else {
            return (a_rank as usize).cmp(&(b_rank as usize));
        }
    });
    let result = info
        .iter()
        .enumerate()
        .fold(0, |acc, (index, x)| acc + x.1 as usize * (index + 1));
    dbg!(result);
}

fn calc_rank_2(hand: &str) -> Rank {
    let mut freq = HashMap::new();

    let mut max_char = '0';
    let mut max_count = 0;

    freq.insert('J', 0);
    hand.chars().for_each(|c| {
        let count = freq.entry(c).or_insert(0);
        *count += 1;
        if (*count > max_count || *count == max_count) && (c != 'J') {
            max_char = c;
            max_count = *count;
        }
    });

    if max_char != '0' {
        let add = freq.get(&'J').unwrap().clone();
        let count = freq.entry(max_char).or_insert(0);
        *count += add;
        freq.remove(&'J');
    }

    let mut label_counts = freq.values().collect::<Vec<_>>();
    label_counts.sort();

    match label_counts.as_slice() {
        [5] => Rank::Five,
        [1, 4] => Rank::Four,
        [2, 3] => Rank::Full,
        [1, 1, 3] => Rank::Three,
        [1, 2, 2] => Rank::Two,
        [1, 1, 1, 2] => Rank::One,
        _ => Rank::High,
    }
}

pub fn second(input_file: PathBuf) {
    let mut info = parse(input_file);
    info.sort_by(|a, b| {
        let a_rank = calc_rank_2(&a.0);
        let b_rank = calc_rank_2(&b.0);
        if a_rank == b_rank {
            for index in 0..5 {
                let a_index = calc_position_2(a.0.chars().nth(index).unwrap());
                let b_index = calc_position_2(b.0.chars().nth(index).unwrap());
                match a_index.cmp(&b_index) {
                    std::cmp::Ordering::Equal => {}
                    Ordering::Less => return Ordering::Greater,
                    Ordering::Greater => return Ordering::Less,
                }
            }
            return Ordering::Equal;
        } else {
            return (a_rank as usize).cmp(&(b_rank as usize));
        }
    });
    let result = info
        .iter()
        .enumerate()
        .fold(0, |acc, (index, x)| acc + x.1 as usize * (index + 1));
    dbg!(result);
}
