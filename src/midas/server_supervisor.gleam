import core/fleet_supervisor
import midas/server

pub fn spawn_link(handler, listen_socket) {
    let sup = fleet_supervisor.spawn_link(fn() {
        server.spawn_link(handler, listen_socket)
    })
    sup
}


// import midas/server
// // Needs a start child function
//
// pub fn start_link(listen_socket, handler) {
//     // Supervisor 1 takes one arg
//     let start = fn() {
//         server.start_link(listen_socket, handler)
//     }
//
//     0
// }
//
// // Fleet supervisor
// // Clone Copy, Replica, Homogenious replica
// pub fn start_server(s) {
//     s
// }
//
// // Protocol is start child
