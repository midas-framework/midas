import core/process
import core/supervisor
import midas_tcp
import midas_utils
import midas/server_supervisor
import midas/governor_supervisor
// TODO remove
import core/fleet_supervisor

fn loop(receive, handler, listen_socket) {
    let server_sup = server_supervisor.spawn_link(handler, listen_socket)
    // Pass in acceptor count here
    let governor_sup = governor_supervisor.spawn_link(server_sup)
    // These types aren't the same
    // Potentially both are designated by the same protocol

    // But should have diff return types
    // Can't do this a only types.
    // let process.Pid(fleet_supervisor.M(Int)) = governor_sup
    server_sup == governor_sup
    let pids = [server_sup, governor_sup]
    case receive() {
        supervisor.Exit(_) -> {
            // Kill both and restart
            Nil
        }
        supervisor.Message(Nil) ->
            Nil
            // TODO supervisor can't exit normally no message. so have unrealisable type at the end state.
    }
}

fn init(receive, handler, port) {
    let Ok(listen_socket) = midas_tcp.listen(port)
    loop(receive, handler, listen_socket)
}

pub fn spawn_link(handler, port: Int) -> process.Pid(Nil) {
    midas_utils.display("Getting started")
    supervisor.spawn_link(init(_, handler, port))
}


// app ssupervisor wraps
