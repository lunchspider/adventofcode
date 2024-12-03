use std::path::PathBuf;

#[derive(Default, Debug)]
struct Game {
    red: usize,
    blue: usize,
    green: usize,
}

impl Game {
    fn pow(&self) -> usize {
        self.red * self.blue * self.green
    }
}

fn parse_round(rounds: &str) -> Game {
    let mut game = Game::default();
    rounds.split(",").map(str::trim).for_each(|r| {
        let num = r.split(" ").nth(0).unwrap().parse::<usize>().unwrap();
        let color = r.split(" ").nth(1).unwrap().trim();
        match color {
            "red" => game.red = num,
            "blue" => game.blue = num,
            "green" => game.green = num,
            _ => unreachable!(),
        }
    });
    game
}

fn parse_game(game: &str) -> Vec<Game> {
    game.split(";").map(str::trim).map(parse_round).collect()
}

pub fn first(input_file: PathBuf) {
    let requirement = Game {
        red: 12,
        green: 13,
        blue: 14,
    };
    let mut result = 0;
    for (id, line) in std::fs::read_to_string(input_file)
        .unwrap()
        .lines()
        .enumerate()
    {
        let game = line.split(":").map(str::trim).nth(1).unwrap();
        let game = parse_game(game);
        let mut is_ok = true;
        for round in game {
            if round.red > requirement.red
                || round.blue > requirement.blue
                || round.green > requirement.green
            {
                is_ok = false;
            }
        }
        if is_ok {
            result += id + 1;
        }
    }
    dbg!(result);
}

pub fn second(input_file: PathBuf) {
    let mut result = 0;
    for line in std::fs::read_to_string(input_file).unwrap().lines() {
        let game = line.split(":").map(str::trim).nth(1).unwrap();
        result += parse_game(game)
            .iter()
            .fold(Game::default(), |mut acc, x| {
                acc.red = acc.red.max(x.red);
                acc.blue = acc.blue.max(x.blue);
                acc.green = acc.green.max(x.green);
                return acc;
            })
            .pow();
    }
    dbg!(result);
}
