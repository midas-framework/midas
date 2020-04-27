import core/core.{Infinity}
import core/process
import core/task
import core/supervisor
import core/supervisor/rest_for_one

import midas_utils
import gleam/expect



pub external fn unsafe_receive() -> supervisor.Protocol(x)
    = "core_supervisor_native" "do_receive"

pub external fn unsafe_self() -> process.Pid(m)
    = "erlang" "self"

type Protocol(a) {
    Pid(process.Pid(a))
}

pub fn start_up_order_test() {
    let test = unsafe_self()
    let sup = rest_for_one.spawn_link(fn(){
        rest_for_one.Two(
            fn() {
                task.spawn_link(fn(receive) {
                    process.send(test, unsafe_self())
                    let _ = receive(Infinity)
                    Nil
                })
            },
            fn(first) {
                task.spawn_link(fn(receive) {
                    process.send(test, first)
                    process.send(test, unsafe_self())
                    let _ = receive(Infinity)
                    Nil
                })
            },
        )
    })

    let supervisor.Message(child1_from_self) = unsafe_receive()
    let supervisor.Message(child1_from_child2) = unsafe_receive()
    expect.equal(child1_from_self, child1_from_child2)
    let supervisor.Message(child2_from_self) = unsafe_receive()
    expect.not_equal(child1_from_self, child2_from_self)

    // Everything is permanent so a Process exiting normally is restarted
    process.send(child2_from_self, Nil)
    let supervisor.Message(child1_from_child2_again) = unsafe_receive()
    let supervisor.Message(child2_from_self_again) = unsafe_receive()
    expect.equal(child1_from_self, child1_from_child2_again)
    expect.not_equal(child2_from_self, child2_from_self_again)

    // Restarts from kill as well as normal exit
    process.kill(child1_from_self)
    let supervisor.Message(child1_from_self_again) = unsafe_receive()
    let supervisor.Message(child1_from_child2_again) = unsafe_receive()
    expect.equal(child1_from_self_again, child1_from_child2_again)
    let supervisor.Message(child2_from_self_again) = unsafe_receive()
    expect.not_equal(child1_from_self_again, child2_from_self_again)

    expect.not_equal(child1_from_self, child1_from_self_again)
    expect.not_equal(child2_from_self, child2_from_self_again)
    // process.shutdown(sup)
    // Leave the supervisor running. Stopping it if cancels further tests
    // Related process exited with reason: xxx  (related process being the supervisor)
}
