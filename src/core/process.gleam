import core/core

// function call
pub type Process(m) {
    Pid
}

// Send should return the same as ending a process as normally the last thing
pub external fn send(Process(m), m) -> Nil
    = "erlang" "send"

pub external fn monitor(Process(m)) -> core.Ref()
    = "core_process_native" "monitor"
