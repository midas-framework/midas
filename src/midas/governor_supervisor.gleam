import process/process
import process/process.{From, Pid, BarePid, ExitReason, Normal, Kill, Infinity, Milliseconds, TrapExit}
import process/supervisor/set_supervisor
import midas/governor

pub fn spawn_link(server_supervisor) {
  let sup = set_supervisor.spawn_link(
    fn() { governor.spawn_link(server_supervisor) },
  )
  let _ = set_supervisor.start_child(sup)
  sup
}
