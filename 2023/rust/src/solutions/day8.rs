use std::{collections::HashMap, path::PathBuf};

#[derive(Debug)]
pub struct Tree {
    adjaceny_list: HashMap<String, Vec<String>>,
}

impl Tree {
    pub fn new() -> Self {
        Tree {
            adjaceny_list: HashMap::new(),
        }
    }
}

// Find GCD
fn gcd(mut a: usize, mut b: usize) -> usize {
    if a == b {
        return a;
    }
    if b > a {
        let temp = a;
        a = b;
        b = temp;
    }
    while b > 0 {
        let temp = a;
        a = b;
        b = temp % b;
    }
    return a;
}

fn lcm(a: usize, b: usize) -> usize {
    // LCM = a*b / gcd
    return a * (b / gcd(a, b));
}

fn parse(input_file: PathBuf) -> (String, Tree) {
    let lines = std::fs::read_to_string(input_file)
        .unwrap()
        .lines()
        .map(String::from)
        .collect::<Vec<String>>();
    let instructions = lines[0].trim();
    let mut tree = Tree::new();

    for i in lines.iter().skip(2) {
        let x = i.split(" = ").collect::<Vec<_>>();
        let root = x[0].to_string();
        let children = x[1];
        let children = children
            .replace("(", "")
            .replace(")", "")
            .split(", ")
            .map(String::from)
            .collect::<Vec<String>>();
        tree.adjaceny_list.insert(root, children);
    }
    (instructions.to_string(), tree)
}

pub fn first(input_file: PathBuf) {
    let (instructions, tree) = parse(input_file);
    let instruction_len = instructions.len();
    let mut curr_instruction = 0;
    let mut curr_node = "AAA";
    while curr_node != "ZZZ" {
        match instructions.chars().nth(curr_instruction % instruction_len) {
            Some('R') => {
                curr_node = &tree.adjaceny_list.get(curr_node).unwrap()[1];
            }
            Some('L') => {
                curr_node = &tree.adjaceny_list.get(curr_node).unwrap()[0];
            }
            _ => unreachable!(),
        }
        curr_instruction += 1;
    }
    dbg!(curr_instruction);
}
pub fn second(input_file: PathBuf) {
    let (instructions, tree) = parse(input_file);
    let starting_nodes = tree
        .adjaceny_list
        .keys()
        .filter(|x| x.chars().last() == Some('A'))
        .collect::<Vec<_>>();

    let instruction_len = instructions.len();
    dbg!(&starting_nodes);
    let mut result = Vec::new();
    for mut i in starting_nodes {
        let mut steps = 0;
        while i.chars().last() != Some('Z') {
            match instructions.chars().nth(steps % instruction_len) {
                Some('R') => {
                    i = &tree.adjaceny_list.get(i).unwrap()[1];
                }
                Some('L') => {
                    i = &tree.adjaceny_list.get(i).unwrap()[0];
                }
                _ => unreachable!(),
            }
            steps += 1;
        }
        result.push(steps);
    }
    let r = result.iter().fold(1, |acc, x| lcm(acc, *x));
    dbg!(r);
}
