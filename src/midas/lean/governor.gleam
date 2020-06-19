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
    // Need a backoff?
    // If server dies before accepting just die too
    // Can only get gone if exit normal, otherwise linking will kill it.
    // We aalways reply so gone only in non normal exit.
    // Exit normal but have permanent as reason on set supervisor
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
