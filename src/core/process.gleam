import core/core

// function call
pub type Pid(m) {
    Internal(Pid(m))
}
// Is a Bare pid a pid of type Dynamic

// Could Pid be parameterised supervisor or worker then receive could take self as an argument.
// That would allow receive to check the pid wasn't send
// Is there a performace improvement for not having an anonymous function around receive

// Send should return the same as ending a process as normally the last thing
pub external fn send(Pid(m), m) -> Nil
    = "erlang" "send"

pub external fn monitor(Pid(m)) -> core.Ref()
    = "core_process_native" "monitor"
