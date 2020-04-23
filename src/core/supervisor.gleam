import core/core
import core/process.{Process}

pub type Protocol(m) {
    Exit
    Down(core.Ref)
    Message(m)
}

// TODO add timeout
type Receive(m) = fn() -> Protocol(m)
// Waiting for release that fixes defining alias on import
// type Supervisor(m) = fn(Receive(m)) -> Process(m)

pub external fn spawn_link(fn(Receive(m)) -> Nil) -> Process(m)
    = "core_supervisor_native" "spawn_link"
