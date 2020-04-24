import core/process
import core/task
import core/supervisor

import midas_utils
import gleam/expect

pub external fn unsafe_receive() -> supervisor.Protocol(x)
    = "core_supervisor_native" "do_receive"

pub external fn unsafe_self() -> process.Pid(m)
    = "erlang" "self"

fn echo(receive) {
    let m = receive()
    let task.Message(tuple(test, number)) = m
    process.send(test, number)
}

pub fn sending_messages_to_task_test() {
    let pid = task.spawn_link(echo)
    process.send(pid, tuple(unsafe_self(), 500))

    let supervisor.Message(reply) = unsafe_receive()
    expect.equal(reply, 500)
}

pub fn monitor_test() {
    let pid = task.spawn_link(fn(_) { Nil })

    let monitor_reference = process.monitor(pid)
    let supervisor.Down(down_reference) = unsafe_receive()
    expect.equal(monitor_reference, down_reference)
}
