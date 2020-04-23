// import core/supervisor
// // TODO rename worker?
// import process
// import midas_utils
//
// // Core.gleam call reply
// // Supervisor for restart strategies
//
// // All in one protocol allows us to send Exit messages
//
// // is external always pub
// // Have one erlang spawn with options
// // c or m for type, child messages
// type M(c) {
//     StartChild(process.Process(process.Process(c)))
// }
//
// // ref to a process that accepts a process
// external fn get_caller() -> process.Process(process.Process(c)) = "erlang" "self"
//
// // Children is a list of child pids
// fn loop(receive, children, task_fn: fn(fn() -> process.Protocol(m)) -> Nil) {
//     let message = receive()
//     midas_utils.display("--------------------")
//     midas_utils.display(message)
//     case message {
//         supervisor.Exit -> {
//             midas_utils.display("EXIT")
//             loop(receive, children, task_fn)
//         }
//         // Need to make temporary mailbox
//         // resolve fn for promise
//         supervisor.Message(StartChild(caller)) -> {
//             let child = process.start_link(task_fn)
//             midas_utils.display("supervisor")
//             process.send(caller, child)
//             loop(receive, children, task_fn)
//         }
//         unexpected -> {
//             midas_utils.display(unexpected)
//             loop(receive, children, task_fn)
//
//         }
//     }
// }
//
// fn init(receive, task_fn: fn(fn() -> process.Protocol(m)) -> Nil) {
//     let children = []
//     loop(receive, children, task_fn)
// }
// // function/runner/main/task
// // task makes sense because main task is a thing
// pub fn start_link(task_fn: fn(fn() -> process.Protocol(m)) -> Nil) -> process.Process(M(c)) {
//     supervisor.spawn_link(init(_, task_fn))
// }
//
// pub external fn receive() -> process.Protocol(m)
//     = "process_native" "do_receive"
//
// //  instead of Int
// pub fn start_child(supervisor: Supervisor(m)) -> process.Process(m) {
//     // let ref = monitor(supervisor)
//     let StartChild(_) = send(supervisor, StartChild(get_caller()))
//     // Need error because supervisor could have died
//     // receive can accept a call
//     // TODO right call fn
//     // case receive() {
//     //     process.Message(pid) -> pid
//     //
//     // }
//     let process.Message(pid) = receive()
//     // midas_utils.display("pid")
//     // midas_utils.display(pid)
//     // let b = receive()
//     // midas_utils.display("b")
//     // midas_utils.display(b)
//     pid
// }
