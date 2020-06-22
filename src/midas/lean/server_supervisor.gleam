import gleam/list
import process/process
import process/process.{From, Pid, BarePid, ExitReason, Infinity, Milliseconds, TrapExit}
import process/supervisor/set_supervisor
import midas/lean/server
import midas/net/http

// connection limit
// connections/concurrency
// server count
pub fn spawn_link(handler, listen_socket, count) {
  let supervisor = set_supervisor.spawn_link(
    fn(_: Nil) { server.spawn_link(handler, listen_socket) },
    set_supervisor.Permanent,
  )
  assert Ok(
    _,
  ) = list.range(0, count)
    |> list.try_map(fn(_i) { set_supervisor.start_child(supervisor, Nil) })
  supervisor
}

pub fn start_link(
  handler,
  port: Int,
  count: Int,
) -> Result(Pid(set_supervisor.Messages(Nil, Nil)), Nil) {
  try listen_socket = http.listen(port)
  Ok(spawn_link(handler, listen_socket, count))
}
