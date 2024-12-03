use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
};

#[derive(Debug, Clone, Copy)]
struct NumberInfo {
    num: u32,
    row: usize,
    col: usize,
    len: usize,
}

// this works only for ascii characters do not try this at home
fn split_whitespace_indices(s: &str) -> impl Iterator<Item = (usize, &str)> {
    s.split_whitespace()
        .map(|text| {
            // since we can have strings like 6*17*
            let mut result = Vec::new();
            let mut last = 0;
            for (index, matched) in
                text.match_indices(|c: char| !(c.is_alphanumeric() || c == '\''))
            {
                if last != index {
                    result.push(&text[last..index]);
                }
                result.push(matched);
                last = index + matched.len();
            }
            if last < text.len() {
                result.push(&text[last..]);
            }
            return result.into_iter();
        })
        .flatten()
        .map(move |sub| (sub.as_ptr() as usize - s.as_ptr() as usize, sub))
}

pub fn first(input_file: PathBuf) {
    let mut result = 0;
    let lines = std::fs::read_to_string(input_file)
        .unwrap()
        .lines()
        .map(String::from)
        .collect::<Vec<String>>();
    let mut symbol_info: HashSet<(usize, usize)> = HashSet::new();
    let mut numbers = Vec::new();
    for (row_number, rows) in lines.iter().enumerate() {
        let x = rows.as_str().replace(".", " ");
        split_whitespace_indices(&x).for_each(|x| {
            if let Ok(num) = x.1.parse::<u32>() {
                numbers.push(NumberInfo {
                    num,
                    row: row_number,
                    col: x.0,
                    len: x.1.len(),
                });
            } else {
                // we got a symbol
                symbol_info.insert((row_number, x.0));
            }
        });
    }
    for i in numbers {
        let mut found = false;
        // top
        if i.row != 0 {
            let col = if i.col == 0 { 0 } else { i.col - 1 };
            let row = i.row - 1;
            for k in col..=col + i.len + 1 {
                if symbol_info.contains(&(row, k)) {
                    found = true;
                }
            }
        }
        // bottom
        let col = if i.col == 0 { 0 } else { i.col - 1 };
        let row = i.row + 1;
        for k in col..=col + i.len + 1 {
            if symbol_info.contains(&(row, k)) {
                found = true;
            }
        }
        //right
        if symbol_info.contains(&(i.row, i.col + i.len)) {
            found = true;
        }
        //left
        if i.col != 0 {
            if symbol_info.contains(&(i.row, i.col - 1)) {
                found = true;
            }
        }
        if found {
            result += i.num;
        }
    }
    dbg!(result);
}

#[derive(Debug, Clone)]
struct NumberStr {
    num: u32,
    id: usize,
}

pub fn second(input_file: PathBuf) {
    let mut result = 0;
    let lines = std::fs::read_to_string(input_file)
        .unwrap()
        .lines()
        .map(String::from)
        .collect::<Vec<String>>();
    let mut symbol_info: Vec<(usize, usize)> = Vec::new();
    let mut numbers: HashMap<(usize, usize), NumberStr> = HashMap::new();
    let mut id = 0;
    for (row_number, rows) in lines.iter().enumerate() {
        let x = rows.as_str().replace(".", " ");
        split_whitespace_indices(&x).for_each(|x| {
            if let Ok(num) = x.1.parse::<u32>() {
                numbers.insert((row_number, x.0), NumberStr { num, id });
                numbers.insert((row_number, x.0 + x.1.len() - 1), NumberStr { num, id });
                id += 1;
            } else {
                // we got a symbol
                if x.1 == "*" {
                    symbol_info.push((row_number, x.0));
                }
            }
        });
    }
    for symbol_loc in symbol_info {
        let mut nums = HashMap::new();
        //top
        if symbol_loc.0 != 0 {
            let row = symbol_loc.0 - 1;
            let col = if symbol_loc.1 == 0 {
                0
            } else {
                symbol_loc.1 - 1
            };
            for k in col..=col + 2 {
                if let Some(num) = numbers.get(&(row, k)) {
                    nums.insert(num.id, num);
                }
            }
        }
        // bottom
        let col = if symbol_loc.1 == 0 {
            0
        } else {
            symbol_loc.1 - 1
        };
        let row = symbol_loc.0 + 1;
        dbg!(row, col);
        for k in col..=col + 2 {
            if let Some(num) = numbers.get(&(row, k)) {
                dbg!(num);
                nums.insert(num.id, num);
            }
        }

        //left
        let row = if symbol_loc.1 == 0 {
            0
        } else {
            symbol_loc.1 - 1
        };
        if let Some(num) = numbers.get(&(symbol_loc.0, row)) {
            nums.insert(num.id, num);
        }

        //right
        if let Some(num) = numbers.get(&(symbol_loc.0, symbol_loc.1 + 1)) {
            nums.insert(num.id, num);
        }
        dbg!(&nums);
        if nums.len() == 2 {
            result += nums.iter().fold(1, |acc, x| acc * x.1.num);
        }
    }
    dbg!(result);
}
