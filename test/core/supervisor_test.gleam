import core/core.{Milliseconds}
import core/process
import core/task
import core/supervisor

import midas_utils
import gleam/expect

pub external fn unsafe_receive() -> supervisor.Protocol(x)
    = "core_supervisor_native" "do_receive"

pub external fn unsafe_self() -> process.Pid(m)
    = "erlang" "self"

type Protocol {
    Ping(process.Caller(Int), Int)
};

fn echo(receive) {
    let supervisor.Message(Ping(from, number)) = receive()
    process.reply(from, number)
}

pub fn sending_messages_to_task_test() {
    let pid = supervisor.spawn_link(echo)
    let reply = process.call(pid, Ping(_, 500), Milliseconds(1000))
    expect.equal(reply, Ok(500))
    // TODO this shouldn't be here
    let supervisor.Down(_down_reference) = unsafe_receive()
    Nil
}

pub fn catch_exits_test() {
    let test = unsafe_self()
    let _ = supervisor.spawn_link(fn(receive) {
        let child = task.spawn_link(fn(_) {
            1/0
            Nil
        })
        process.send(test, tuple(child, receive()))
    })
    let supervisor.Message(tuple(pid1, supervisor.Exit(pid2))) = unsafe_receive()
    expect.equal(pid1, pid2)
}
