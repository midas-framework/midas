// https://erlang.org/doc/man/supervisor.html
// The start function must create and link to
// Gove

// When terminating children supervisor unlinks and monitors, to make sure a child that has unlinked cannot cause the supervisor to hang
// https://github.com/erlang/otp/blob/master/lib/stdlib/src/supervisor.erl#L896

// The above leads to a race condition when killing supervisors, they must be given infinity time to kill their own children
// After a timeout will always kill,
// THis supervisor works on assumption that kill is ok

// Team/Group/Individual/myriad/motly

// fn init(state) {
//     state
//     |> add_one()
//
//     [
//         Individal(fn() {})
//         Three()
//     ]
//     // Clearly nothing is missed in erlang by not having full control over a supervision heirachy
//
// }

import core/core.{Infinity}
import core/process
import core/task

// named tuple
// tuple(foo: Pid, blah: String)


// type Children(a, b) {
//     One(a)
//     Two(b, b)
// }

type Spec(a, b) {
    One(
        fn() -> process.Pid(a)
    )
    Two(
        fn() -> process.Pid(a),
        fn(process.Pid(a)) -> process.Pid(b)
    )
}

type Children(a, b) {
    ChilrenOne(process.Pid(a))
    ChilrenTwo(process.Pid(a), process.Pid(b))
}


fn do_start(children: Spec(a, b)) {
    case children {
        One(first) -> {
            let p1 = first()
            let p2 = task.spawn_link(fn(_) { Nil })
            ChilrenTwo(p1, p2)
        }
        Two(first, second) -> {
            let p1 = first()
            let p2 = second(p1)
            ChilrenTwo(p1, p2)

        }
    }
}

// Null Pid

// Will always return optional for pid when we know it is not their or should be there

fn debug() {
    let spec1 = One(fn() {
        task.spawn_link(fn(receive) {
            let task.Message(5) = receive(Infinity)
            Nil
        })
    })
    // Can I wrap an make sure supervisor has type of nil nil
    let spec2 = Two(fn() {
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
    let spec3 = Two(fn() {
        task.spawn_link(fn(receive) {
            let task.Message(5) = receive(Infinity)
            Nil
        })
    },
    fn(previous) {
        task.spawn_link(fn(receive) {
            let task.Message(Nil) = receive(Infinity)
            process.send(previous, 5)
            Nil
        })
    })
    spec1 == spec2
    // spec1 == spec3

    0
}


type Child(a, m) {
    Child(fn(a) -> process.Pid(m), a )
}
//
// import process
//
// pub type Supervisor(c) {
//     Pid
// }
//
// // parent in OTP started
// pub fn example_test() {
//     let start = fn() {
//         let pid = process.start_link(fn(_receive) {
//             Nil
//         })
//         Ok(pid)
//
//         // let p1 = Child(start_link, 5)
//         // let p2 = Child2(start_link, resolve(p1), "foo")
//         Nil
//     }
//
//     [
//         // Different pid types returned
//         // No destructuring in function head for list
//         // fn(_) { worker1.start_link(value) },
//         // fn([pid1]) { worker2.start_link(pid1) }
//         // fn([pid1, _]) { worker3.start_link(pid1) }
//         // Apply1(start_link, 3),
//     ]
//     // Needs to be tuples for message type
//     // Children3(
//     //     fn() { worker1.start_link(value) },
//     //     fn(pid1) { worker2.start_link(pid1) },
//     //     fn(pid1, _) { worker3.start_link(pid1) }
//     // )
// }


// Pair(
//     Spec(start: server.start_link)
//     Pair(
//         Spec()
//         Spec
//     )
// )

// Spec Top level == 1

// let (Ok(p), (Ok(p), (Ok(p) , Error(Nil))))
//
//
// type MySupervisor = Spec(Int, Spec(Float, Spec(String, End)))
//
// let Child(p1, Child(p2, Child(p3, E))) = which_children(supervisor)
//
// Spec(Int, Spec(Float, Spec(Sting, Nil)))
//
// type Spec(a, b) {
//     Child(a, Spec(b))
//     // This could specify restart mechanism
//     Last
// }
