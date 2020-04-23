// import process
// import midas/governor
//
// pub type Process(m) {
//     Pid
// }
//
// pub type PidBare {
//     OtherPid
// }
//
// pub type Protocol {
//     // Down(Monitor)
//     Exit(PidBare, String)
// }
//
//
// fn loop(pids, receive) {
//     case receive() {
//         Exit(p, "ok") -> {
//             // Look up in pid map and restart
//             loop(pids, receive)
//         }
//     }
// }
//
// pub external fn do_start_link(fn(fn() -> Protocol) -> Nil) -> Process(m)
//     = "process_native" "start_link"
//
// pub fn start_link(start: fn() -> process.Process(String), count: Int) {
//
//
//     do_start_link(fn(receive) {
//         let pids = 5
//         loop(pids, receive)
//     })
// }
