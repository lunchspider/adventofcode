mod solutions;

use std::path::PathBuf;

use clap::Parser;

#[derive(Parser, Debug)]
struct Args {
    #[arg(short, long)]
    day: u8,

    #[arg(short, long)]
    input: String,

    #[arg(short, long, default_value_t = 1)]
    part: u8,
}

fn main() {
    let args = Args::parse();
    let day = args.day;
    let input_file = PathBuf::from(args.input);
    if !input_file.exists() {
        panic!("{:?} doesn't exist.", input_file)
    }

    match day {
        1 => match args.part {
            1 => solutions::day1::first(input_file),
            2 => solutions::day1::second(input_file),
            _ => unreachable!(),
        },
        2 => match args.part {
            1 => solutions::day2::first(input_file),
            2 => solutions::day2::second(input_file),
            _ => unreachable!(),
        },
        3 => match args.part {
            1 => solutions::day3::first(input_file),
            2 => solutions::day3::second(input_file),
            _ => unreachable!(),
        },
        4 => match args.part {
            1 => solutions::day4::first(input_file),
            2 => solutions::day4::second(input_file),
            _ => unreachable!(),
        },
        5 => match args.part {
            1 => solutions::day5::first(input_file),
            2 => solutions::day5::second(input_file),
            _ => unreachable!(),
        },
        6 => match args.part {
            1 => solutions::day6::first(input_file),
            2 => solutions::day6::second(input_file),
            _ => unreachable!(),
        },
        7 => match args.part {
            1 => solutions::day7::first(input_file),
            2 => solutions::day7::second(input_file),
            _ => unreachable!(),
        },
        8 => match args.part {
            1 => solutions::day8::first(input_file),
            2 => solutions::day8::second(input_file),
            _ => unreachable!(),
        },
        9 => match args.part {
            1 => solutions::day9::first(input_file),
            2 => solutions::day9::second(input_file),
            _ => unreachable!(),
        },
        _ => panic!("day count wrong"),
    }
}
