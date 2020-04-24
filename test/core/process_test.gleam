import core/core.{Milliseconds, Infinity}
import core/process
import core/task

import midas_utils
import gleam/expect

pub external fn unsafe_receive(core.Wait) -> task.Protocol(x)
    = "core_task_native" "do_receive"

pub external fn unsafe_self() -> process.Pid(m)
    = "erlang" "self"

type Protocol {
    Ping(process.Caller(Int), Int)
};

pub fn call_task_test() {
    let pid = task.spawn_link(fn(receive) {
        let task.Message(from) = receive(Infinity)
        process.reply(from, Nil)
    })
    let reply = process.call(pid, fn(f) { f }, Milliseconds(100))
    expect.equal(reply, Ok(Nil))
    // Assert there is no message left in mailbox
    let task.Timeout = unsafe_receive(Milliseconds(1000))
    Nil
}

pub fn call_timeout_for_slow_process_test() {
    let pid = task.spawn_link(fn(receive) {
        let _ = receive(Infinity)
        // Receive twice because call sends message for first one
        let _ = receive(Milliseconds(500))
        Nil
    })
    let reply = process.call(pid, Ping(_, 500), Milliseconds(100))

    expect.equal(reply, Error(process.Timeout))
    // Assert there is no message left in mailbox
    let task.Timeout = unsafe_receive(Milliseconds(1000))
    Nil
}

pub fn call_error_for_down_process_test() {
    let pid = task.spawn_link(fn(_) { Nil})
    let reply = process.call(pid, Ping(_, 500), Infinity)

    expect.equal(reply, Error(process.Down))
}
