import gleam/result

// pub fn start_worker(init: fn() -> s, loop: fn(s) -> s) {
//     loop(init())
// }

// fn example() {
//     start_worker(fn() { "HEllo"}, fn(x: Int) { x + 1})
// }

pub type Process(m) {
    Pid
}

pub type Message(m) {
    Down
    Inner(m)
}

pub external fn start(init: fn(fn() -> Message(m)) -> Nil) -> Result(Process(m), Nil)
  = "process_native" "start"


pub fn send(pid: Process(m), message: m) -> Result(Nil, Nil) {
    Ok(Nil)
}
