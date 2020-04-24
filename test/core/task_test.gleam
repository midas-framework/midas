import core/process
import core/task
import core/supervisor

import midas_utils
import gleam/expect

pub external fn unsafe_receive() -> supervisor.Protocol(x)
    = "core_supervisor_native" "do_receive"

type Protocol {
    Ping(process.Caller(Int), Int)
};

fn echo(receive) {
    let task.Message(Ping(from, number)) = receive()
    process.reply(from, number)
}

pub fn sending_messages_to_task_test() {
    let pid = task.spawn_link(echo)
    let reply = process.call(pid, Ping(_, 500))
    expect.equal(reply, Ok(500))
    // TODO this shouldn't be here
    let supervisor.Down(down_reference) = unsafe_receive()
    Nil
}

pub fn monitor_test() {
    let pid = task.spawn_link(fn(_) { Nil })

    let monitor_reference = process.monitor(pid)
    let supervisor.Down(down_reference) = unsafe_receive()
    expect.equal(monitor_reference, down_reference)
}
