import process/process
import process/process.{MonitorType, Ref, Flush, From, Pid, BarePid, Process, ExitReason, Normal, Kill, TrapExit, Wait, Infinity, Milliseconds, Timeout}
import process/supervisor/rest_for_one

import midas_utils
import gleam/expect

pub external fn unsafe_receive(Wait) -> m
    = "process_native" "do_receive"

pub fn start_up_order_test() {
    let test = process.unsafe_self()
    let sup = rest_for_one.spawn_link(fn(){
        rest_for_one.Two(
            fn() {
                process.spawn_link(fn(receive) {
                    process.send(test, process.self(receive))
                    let _ = receive(Infinity)
                    Normal
                })
            },
            fn(first) {
                process.spawn_link(fn(receive) {
                    process.send(test, first)
                    process.send(test, process.self(receive))
                    let _ = receive(Infinity)
                    Normal
                })
            },
        )
    })

    let Ok(child1_from_self) = unsafe_receive(Milliseconds(100))
    let Ok(child1_from_child2) = unsafe_receive(Milliseconds(100))
    expect.equal(child1_from_self, child1_from_child2)
    let Ok(child2_from_self) = unsafe_receive(Milliseconds(100))
    expect.not_equal(child1_from_self, child2_from_self)

    // Everything is permanent so a Process exiting normally is restarted
    process.send(child2_from_self, Nil)
    let Ok(child1_from_child2_again) = unsafe_receive(Milliseconds(100))
    let Ok(child2_from_self_again) = unsafe_receive(Milliseconds(100))
    expect.equal(child1_from_self, child1_from_child2_again)
    expect.not_equal(child2_from_self, child2_from_self_again)

    // Restarts from kill as well as normal exit
    process.exit(child1_from_self, Kill)
    let Ok(child1_from_self_again) = unsafe_receive(Milliseconds(100))
    let Ok(child1_from_child2_again) = unsafe_receive(Milliseconds(100))
    expect.equal(child1_from_self_again, child1_from_child2_again)
    let Ok(child2_from_self_again) = unsafe_receive(Milliseconds(100))
    expect.not_equal(child1_from_self_again, child2_from_self_again)

    expect.not_equal(child1_from_self, child1_from_self_again)
    expect.not_equal(child2_from_self, child2_from_self_again)
    // process.shutdown(sup)
    // Leave the supervisor running. Stopping it if cancels further tests
    // Related process exited with reason: xxx  (related process being the supervisor)
}
