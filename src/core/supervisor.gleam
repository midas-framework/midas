import core/core
import core/process.{Pid, BarePid}

pub type Protocol(m) {
    Exit(BarePid)
    Down(core.Ref)
    Message(m)
}

// TODO add timeout
type Receive(m) = fn() -> Protocol(m)
// Waiting for release that fixes defining alias on import
// type Supervisor(m) = fn(Receive(m)) -> Pid(m)

pub external fn spawn_link(fn(Receive(m)) -> Nil) -> Pid(m)
    = "core_supervisor_native" "spawn_link"
