import process
import process.{Protocol}
import gleam/expect

pub external fn receive() -> process.Protocol(m)
    = "process_native" "do_receive"

pub external fn self() -> process.Process(m)
    = "erlang" "self"

// How to reference types in other files
// pub type MyProtocol = Protocol(String)
// pub type MyProtocol = process.Protocol(String)

pub fn echo_test() {
    let pid = process.start_link(fn(receive) {
        let m = receive()
        let process.Message(tuple(test, number)) = m
        process.send(test, number)
        Nil
    })
    process.send(pid, tuple(self(), 500))

    let process.Message(500) = receive()
    Nil
}

pub fn stop_test() {
    let pid = process.start_link(fn(_) { Nil })

    let monitor_reference = process.monitor(pid)
    let process.Down(down_reference) = receive()
    expect.equal(monitor_reference, down_reference)
}
