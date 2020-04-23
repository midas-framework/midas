import gleam/result
import core/core
import core/process
import core/supervisor
import midas_utils

// // Clone Copy, Replica, Homogenious replica

pub type M(c) {
    // The return is a process that accepts messages about new processes that accept messages of the child
    StartChild(core.Ref, process.Process(tuple(core.Ref, process.Process(c))))
}
// // Children is a list of child pids
fn loop(receive, children, task_fn) {
    let message = receive()
//     midas_utils.display("--------------------")
//     midas_utils.display(message)
    case message {
        // Needs to handle exit of parent
        supervisor.Exit -> {
            midas_utils.display("EXIT")
            let child = task_fn()
            loop(receive, children, task_fn)
        }
//         // Need to make temporary mailbox
//         // resolve fn for promise
        supervisor.Message(StartChild(reference, caller)) -> {
            midas_utils.display("supervisor")
            let child = task_fn()
            process.send(caller, tuple(reference, child))
            loop(receive, children, task_fn)
        }
        unexpected -> {
//             midas_utils.display(unexpected)
            loop(receive, children, task_fn)
//
        }
    }
}
//
fn init(receive, task_fn) {
    let children = []
    loop(receive, children, task_fn)
}
// // function/runner/main/task
// // task makes sense because main task is a thing

pub fn spawn_link(child_fn) -> process.Process(M(c)) {
    supervisor.spawn_link(init(_, child_fn))
}

// ref to a process that accepts a process
// TODO remove
external fn get_caller() -> process.Process(tuple(core.Ref, process.Process(c))) = "erlang" "self"

pub fn start_child(supervisor: process.Process(M(c))) -> process.Process(c) {
    let reference = process.monitor(supervisor)
    let Nil = process.send(supervisor, StartChild(reference, get_caller()))
    // Need error because supervisor could have died
    let Ok(pid) = process.receive_reply(process.CallRef(reference))
    pid
    // TODO link between call and reply. This code works regardless of the type of the supervisor
    // process.send(pid, "")
    // process.Pid
}


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
