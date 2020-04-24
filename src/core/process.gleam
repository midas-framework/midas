import core/core

// function call
pub type Pid(m) {
    Internal(Pid(m))
}
// Is a Bare pid a pid of type Dynamic

// Send should return the same as ending a process as normally the last thing
pub external fn send(Pid(m), m) -> Nil
    = "erlang" "send"

pub external fn monitor(Pid(m)) -> core.Ref()
    = "core_process_native" "monitor"
