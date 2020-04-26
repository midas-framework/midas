import core/core

// type Never {
//     Never(Never)
// }
//
// type ChildPid(a) {
//     Temporary(Pid(a)) // Recoverable
//     Transient(Pid(a)) // Unrecoverable
//     Permanent(Pid(a))
// }
// Pids can have their behavior, Permanent(a) etc bit like child spec.
// Permanent can have an unrepresentable return


// function call
pub type Pid(m) {
    Internal(Pid(m))
}

// Could call this a Pid and a Pid with a type an address
pub type BarePid {
    Internal2(BarePid)
}
// Is a Bare pid a pid of type Dynamic

// Could Pid be parameterised supervisor or worker then receive could take self as an argument.
// That would allow receive to check the pid wasn't send
// Is there a performace improvement for not having an anonymous function around receive

pub type Caller(r) {
    From(core.Ref, Pid(tuple(core.Ref, r)))
}

pub type CallError {
    Timeout
    Down
}

// Send should return the same as ending a process as normally the last thing
external fn erlang_send(Pid(m), m) -> m
    = "erlang" "send"

pub external fn monitor(Pid(m)) -> core.Ref()
    = "core_process_native" "monitor"

external fn self() -> Pid(a)
    = "erlang" "self"

pub external fn bare(Pid(a)) -> BarePid
    = "erlang" "todo"

// Can check pid is self
// Need error because supervisor could have died
// needs to be separate receive fn because we want to ignore exitsand motiors from other pids
pub external fn receive_reply(Caller(r), core.Wait) -> Result(r, CallError)
    = "core_process_native" "receive_reply"

pub fn send(pid, message) -> Nil {
    erlang_send(pid, message)
    Nil
}

pub fn call(pid: Pid(m), message_fn: fn(Caller(r)) -> m, wait: core.Wait) -> Result(r, CallError) {
    let reference = monitor(pid)
    let from = From(reference, self())
    let Nil = send(pid, message_fn(from))
    receive_reply(from, wait)
}

pub fn reply(from: Caller(r), message: r) {
    let From(reference, pid) = from
    send(pid, tuple(reference, message))
}
