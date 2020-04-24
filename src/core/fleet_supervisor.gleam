import gleam/result
import gleam/dynamic
import core/core
import core/process
import core/supervisor
import midas_utils

// Other Possible name: Clone, Copy, Replica, Homogenious

pub type Protocol(c) {
    // The return is a process that accepts messages about new processes that accept messages of the child
    // StartChild(core.Ref, process.Pid(tuple(core.Ref, process.Pid(c))))
    StartChild(process.Caller(process.Pid(c)))
}

// Children will be a list of child pids
fn loop(receive, children, task_fn) {
    let message = receive()
    case message {
        // TODO Needs to handle exit of parent properly
        supervisor.Exit(_) -> {
            midas_utils.display("EXIT")
            let child = task_fn()
            loop(receive, children, task_fn)
        }
        supervisor.Message(m) -> {
            midas_utils.display("supervisor")
            midas_utils.display(m)

            let StartChild(from) = m
            let child = task_fn()
            process.reply(from, child)
            loop(receive, children, task_fn)
        }
        // Can monitor only be callable if self is a receive that accepts DOWNS?
        // This allows for a general Call Fn
        // Message(Call(from, StartChild)) ->
        unexpected -> {
            loop(receive, children, task_fn)
        }
    }
}
//
fn init(receive, task_fn) {
    let children = []
    loop(receive, children, task_fn)
}
// other names for child, function/runner/main/task. I think child refers more to pid, subroutine, run work?

pub fn spawn_link(child_fn) -> process.Pid(Protocol(c)) {
    supervisor.spawn_link(init(_, child_fn))
}

pub fn start_child(supervisor: process.Pid(Protocol(c))) -> process.Pid(c) {
    midas_utils.display("pool")
    let Ok(pid) = process.call(supervisor, StartChild(_from))
    pid

}

// From could include fn for promise that resolves to answer, probably more complicated to pass around anonymous fn
