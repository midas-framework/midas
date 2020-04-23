// https://erlang.org/doc/man/supervisor.html
// The start function must create and link to
// Gove

import process

pub type Supervisor(c) {
    Pid
}

// parent in OTP started
pub fn example_test() {
    let start = fn() {
        let pid = process.start_link(fn(_receive) {
            Nil
        })
        Ok(pid)

        // let p1 = Child(start_link, 5)
        // let p2 = Child2(start_link, resolve(p1), "foo")
        Nil
    }

    [
        // Different pid types returned
        // No destructuring in function head for list
        // fn(_) { worker1.start_link(value) },
        // fn([pid1]) { worker2.start_link(pid1) }
        // fn([pid1, _]) { worker3.start_link(pid1) }
        // Apply1(start_link, 3),
    ]
    // Needs to be tuples for message type
    // Children3(
    //     fn() { worker1.start_link(value) },
    //     fn(pid1) { worker2.start_link(pid1) },
    //     fn(pid1, _) { worker3.start_link(pid1) }
    // )
}
