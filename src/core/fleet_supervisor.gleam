import gleam/result
import gleam/dynamic
import core/core
import core/process
import core/supervisor
import midas_utils

// Other Possible name: Clone, Copy, Replica, Homogenious

pub type Caller(r) {
    From(core.Ref, process.Pid(tuple(core.Ref, r)))
}

pub type Call(m, r) {
    Ask(Caller(r), m)
}

pub type Protocol(c) {
    // The return is a process that accepts messages about new processes that accept messages of the child
    // StartChild(core.Ref, process.Pid(tuple(core.Ref, process.Pid(c))))
    StartChild(Caller(process.Pid(c)))
}

fn reply(from: Caller(r), message: r) {
    let From(reference, pid) = from
    process.send(pid, tuple(reference, message))
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
            reply(from, child)
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

external fn self() -> process.Pid(a) = "erlang" "self"

// Can check pid is self
// Need error because supervisor could have died
// needs to be separate receive fn because we want to ignore exitsand motiors from other pids
pub external fn receive_reply(Caller(r)) -> Result(r, Nil)
    = "core_process_native" "receive_reply"


pub fn call(pid, message_fn) {
    let reference = process.monitor(pid)
    let from = From(reference, self())
    let Nil = process.send(pid, message_fn(from))
    receive_reply(from)
}

pub fn start_child(supervisor: process.Pid(Protocol(c))) -> process.Pid(c) {
    midas_utils.display("pool")
    let Ok(pid) = call(supervisor, StartChild(_from))
    pid

}

// From could include fn for promise that resolves to answer, probably more complicated to pass around anonymous fn

import core/task
fn debug() {
    let child_fn = fn() {
        task.spawn_link(fn(receive) {
            let task.Message(5) = receive()
            Nil
        })
    }
    let sup = spawn_link(child_fn)
    let task = start_child(sup)
    process.send(task, 8)
}
