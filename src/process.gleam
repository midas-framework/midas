import gleam/result

pub type Process(m) {
    Pid
}

// NOTE references can be picked up without any reference to what type of process they correspond to, e.g. DOWN messages
pub type Monitor() {
    Reference
}

pub type Protocol(m) {
    Down(Monitor)
    Message(m)
}

pub external fn start_link(fn(fn() -> Protocol(m)) -> Nil) -> Process(m)
  = "process_native" "start_link"

pub external fn monitor(Process(m)) -> Monitor()
    = "process_native" "monitor"

pub external fn send(pid: Process(m), message: m) -> Result(Nil, Nil)
    = "erlang" "send"
