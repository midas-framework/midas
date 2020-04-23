import core/process
import core/task
import core/supervisor

import midas_utils
import gleam/expect

pub external fn unsafe_receive() -> supervisor.Protocol(x)
    = "core_supervisor_native" "do_receive"

pub external fn unsafe_self() -> process.Process(m)
    = "erlang" "self"

fn echo(receive) {
    let m = receive()
    let supervisor.Message(tuple(test, number)) = m
    process.send(test, number)
}

pub fn sending_messages_to_supervisor_test() {
    let pid = supervisor.spawn_link(echo)
    process.send(pid, tuple(unsafe_self(), 500))

    let supervisor.Message(reply) = unsafe_receive()
    expect.equal(reply, 500)
}

pub fn catch_exits_test() {
    let test = unsafe_self()
    let pid = supervisor.spawn_link(fn(receive) {
        task.spawn_link(fn(_) {
            1/0
            Nil
        })

        process.send(test, receive())
    })
    let supervisor.Exit = unsafe_receive()
    Nil
}
