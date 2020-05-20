import process/process
import process/process.{From, Pid, BarePid, ExitReason, Infinity, Milliseconds, TrapExit}
import process/supervisor/set_supervisor
import magpie/server

pub fn spawn_link(handler, listen_socket) {
  let sup = set_supervisor.spawn_link(
    fn() { server.spawn_link(handler, listen_socket) },
  )
  sup
}
