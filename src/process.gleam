// import gleam/result
//
// // Links for pmap and nothing else
// // One for all, can you just restart the supervisor
// // Is the a single child supervisor that makes sn
//
// pub type Process(m) {
//     Pid
// }
//
// // NOTE references can be picked up without any reference to what type of process they correspond to, e.g. DOWN messages
// pub type Monitor() {
//     Reference
// }
//
//
// pub type Protocol(m) {
//     Down(Monitor)
//     Message(m)
// }
// //
// // pub type MyProtocol = Protocol(String)
// //
// // fn accept(x: MyProtocol) {
// //     let Message(m) = x
// //     m + 1
// // }
//
// pub external fn start_link(fn(fn() -> Protocol(m)) -> Nil) -> Process(m)
//   = "process_native" "start_link"
//
// pub external fn monitor(Process(m)) -> Monitor()
//     = "process_native" "monitor"
//
// pub external fn send(pid: Process(m), message: m) -> Result(Nil, Nil)
//     = "erlang" "send"
