import process/process
import process/process.{From, Pid, BarePid, ExitReason, Infinity, Milliseconds, TrapExit}
import process/supervisor/rest_for_one
import midas/net/http
import midas/lean/server_supervisor
import midas/lean/governor_supervisor

fn init(handler, port) {
  let Ok(listen_socket) = http.listen(port)

  rest_for_one.Two(
    fn() { server_supervisor.spawn_link(handler, listen_socket) },
    fn(server_sup) { governor_supervisor.spawn_link(server_sup) },
  )
}

pub fn spawn_link(handler, port: Int) -> Pid(rest_for_one.Messages(a)) {
  rest_for_one.spawn_link(fn() { init(handler, port) })
}
