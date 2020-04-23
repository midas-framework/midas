// TODO rename worker?
import process
import midas_utils

// Core.gleam call reply
// Supervisor for restart strategies

// rename this Supervisor
pub type Supervisor(c) {
    Pid
}

pub type Protocol(c) {
    // For now just start one more
    Exit
    StartChild(process.Process(process.Process(c)))
    Down()
}
// All in one protocol allows us to send Exit messages

// is external always pub
// Have one erlang spawn with options
// c or m for type, child messages
pub external fn spawn(fn(fn() -> Protocol(c)) -> Nil) -> Supervisor(c)
    = "fleet_supervisor_native" "start_link"

pub external fn send(pid: Supervisor(m), message: Protocol(m)) -> Protocol(m)
    = "erlang" "send"

pub type Flag() {
    TrapExit
}
external fn process_flag(Flag, Bool) -> Nil = "erlang" "process_flag"

// ref to a process that accepts a process
external fn get_caller() -> process.Process(process.Process(c)) = "erlang" "self"

// Children is a list of child pids
fn loop(receive, children, task_fn: fn(fn() -> process.Protocol(m)) -> Nil) {
    let message = receive()
    midas_utils.display("--------------------")
    midas_utils.display(message)
    case message {
        Exit -> {
            midas_utils.display("EXIT")
            loop(receive, children, task_fn)
        }
        // Need to make temporary mailbox
        // resolve fn for promise
        StartChild(caller) -> {
            let child = process.start_link(task_fn)
            midas_utils.display("supervisor")
            process.send(caller, child)
            loop(receive, children, task_fn)
        }
        unexpected -> {
            midas_utils.display(unexpected)
            loop(receive, children, task_fn)

        }
    }
}

fn init(receive, task_fn: fn(fn() -> process.Protocol(m)) -> Nil) {
    process_flag(TrapExit, True)
    let children = []
    loop(receive, children, task_fn)
}
// function/runner/main/task
// task makes sense because main task is a thing
pub fn start_link(task_fn: fn(fn() -> process.Protocol(m)) -> Nil) -> Supervisor(m) {
    spawn(init(_, task_fn))
}

pub external fn receive() -> process.Protocol(m)
    = "process_native" "do_receive"

//  instead of Int
pub fn start_child(supervisor: Supervisor(m)) -> process.Process(m) {
    // let ref = monitor(supervisor)
    let StartChild(_) = send(supervisor, StartChild(get_caller()))
    // Need error because supervisor could have died
    // receive can accept a call
    // TODO right call fn
    // case receive() {
    //     process.Message(pid) -> pid
    //
    // }
    let process.Message(pid) = receive()
    // midas_utils.display("pid")
    // midas_utils.display(pid)
    // let b = receive()
    // midas_utils.display("b")
    // midas_utils.display(b)
    pid
}
