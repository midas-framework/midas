import core/fleet_supervisor
import midas/server

pub fn spawn_link(handler, listen_socket) {
    let sup = fleet_supervisor.spawn_link(fn() {
        server.spawn_link(handler, listen_socket)
    })
    sup
}
