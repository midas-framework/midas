import core/core

// function call
pub type Process(m) {
    Pid
}

pub type Call(a, b) {
    CallRef(core.Ref)
}

// Send should return the same as ending a process as normally the last thing
pub external fn send(Process(m), m) -> Nil
    = "erlang" "send"

pub external fn monitor(Process(m)) -> core.Ref()
    = "core_process_native" "monitor"

pub external fn receive_reply(Call(a, b)) -> b
    = "core_process_native" "receive_reply"
