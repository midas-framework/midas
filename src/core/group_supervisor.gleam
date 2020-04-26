

// type Supervisor(spec) {
//     Foo
// };

import core/core.{Infinity}
import core/process
import core/task

// Just make a TwoSupervisor

fn init() {
    tuple(fn() {
        task.spawn_link(fn(receive) {
            let task.Message(5) = receive(Infinity)
            Nil
        })
    },
    fn(previous) {
        task.spawn_link(fn(receive) {
            let task.Message(5) = receive(Infinity)
            process.send(previous, 5)
            Nil
        })
    })
}

fn start() {
0
}

fn which_children(s) {
    let tuple(fn1, fn2) = init()
    let p1 = fn1()
    let p2 = fn2(p1)
    tuple(p1, p2)
}

// type Box(t) {
//     Item(t)
// }
//
// fn foo(b) {
//     let Item(x) = b
//     x + 5
// }
// fn foo(b) {
//     let Item(x) = b
//     x +. 0.0
// }
