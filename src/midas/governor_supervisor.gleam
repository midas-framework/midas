import core/fleet_supervisor
import midas/governor

pub fn spawn_link(server_supervisor) {
    let sup = fleet_supervisor.spawn_link(fn() {
        governor.spawn_link(server_supervisor)
    })
    let _ = fleet_supervisor.start_child(sup)
    sup
}
