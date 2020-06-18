import gleam/io
import process/process
import process/process.{Ref, From, Pid, BarePid, ExitReason, Infinity, Milliseconds, TrapExit}
import process/supervisor/set_supervisor
import midas/lean/server

fn loop(receive, server_supervisor) {
  assert Ok(pid) = set_supervisor.start_child(server_supervisor, Nil)
  case process.call(pid, server.Accept(_), Infinity) {
    Ok(r) -> {
      io.debug(r)
      Nil
    }
    Error(process.Gone) -> process.sleep(100)
  }
  loop(receive, server_supervisor)
}

fn init(receive, server_supervisor) {
  loop(receive, server_supervisor)
}

pub fn spawn_link(server_supervisor) {
  process.spawn_link(init(_, server_supervisor))
}
