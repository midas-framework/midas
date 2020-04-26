import core/process
import core/supervisor/rest_for_one
import midas_tcp
import midas_utils
import midas/server_supervisor
import midas/governor_supervisor

fn init(handler, port) {
    let Ok(listen_socket) = midas_tcp.listen(port)

    rest_for_one.Two(
        fn() { server_supervisor.spawn_link(handler, listen_socket) },
        fn(server_sup) { governor_supervisor.spawn_link(server_sup) }
    )
}

pub fn spawn_link(handler, port: Int) -> process.Pid(Nil) {
    rest_for_one.spawn_link(fn() { init(handler, port) })
}
