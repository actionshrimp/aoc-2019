#[aoc(day1, part1)]
pub fn part1(input: &str) -> i32 {
    input.lines().map(|x| {
        let parsed = x.parse::<i32>().unwrap();
        (parsed / 3) - 2
    }).sum()
}

fn fuel_cost_rec(x : i32) -> i32 {
    let cost = x / 3 - 2;
    if cost < 0 {
        0
    } else {
        cost + fuel_cost_rec(cost)
    }
}

#[aoc(day1, part2, recursive)]
pub fn part2_rec(input: &str) -> i32 {
    input.lines().map(|x| {
        let parsed = x.parse::<i32>().unwrap();
        fuel_cost_rec(parsed)
    }).sum()
}

fn fuel_cost_mut(x : i32) -> i32 {
    let mut total = 0;
    let mut rem = x / 3 - 2;
    while rem > 0 {
        total += rem;
        rem = rem / 3 - 2;
    }
    total
}

#[aoc(day1, part2, mutation)]
pub fn part2_mut(input: &str) -> i32 {
    input.lines().map(|x| {
        let parsed = x.parse::<i32>().unwrap();
        fuel_cost_mut(parsed)
    }).sum()
}
