import core/process
import core/supervisor
import midas_tcp
import midas/server_supervisor
import midas/governor_supervisor

fn loop(receive, handler, listen_socket) {
    let server_sup = server_supervisor.spawn_link(handler, listen_socket)
    // Pass in acceptor count here
    let governor_sup = governor_supervisor.spawn_link(server_sup)
    case receive() {
        supervisor.Exit -> {
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

pub fn spawn_link(handler, port: Int) -> process.Process(Nil) {
    supervisor.spawn_link(init(_, handler, port))
}


// app ssupervisor wraps
