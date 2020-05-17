import process/process
import process/process.{Ref, From, Pid, BarePid, ExitReason, Kill, Infinity, Milliseconds, TrapExit}
import process/supervisor/set_supervisor
import magpie/server

// pub type Messages {
//     DOWN(Ref, MonitorType, BarePid, ExitReason)
//     Accepted()
// }
fn loop(receive, server_supervisor) {
  // let tuple(server_supervisor, server, monitor)= server_supervisor
  // case receive(Infinity) {
  //     Ok(DOWN(ref, _, pid, _)) if ref == monitor -> {
  //         loop(receive, server_supervisor)
  //
  //     }
  // }
  // let monitor = process.monitor_process(server)
  // Would be great to call reply in the server fn but can give the ref because monitor called after process creation.
  // Alternative is a receive block, pass argument to start child and wait for down or accepted.
  let Ok(pid) = set_supervisor.start_child(server_supervisor)
  // TODO Link then unlink
  let Ok(Nil) = server.accept(pid)
  loop(receive, server_supervisor)
}

fn init(receive, server_supervisor) {
  loop(receive, server_supervisor)
}

pub fn spawn_link(server_supervisor) {
  process.spawn_link(init(_, server_supervisor))
}
