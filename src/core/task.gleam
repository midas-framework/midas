// Probably a worker if term task confusing
import core/process.{Pid}

pub type Protocol(m) {
    // Potentinally get rid of down and have special receive
    Down
    Message(m)
}

// TODO add timeout
type Receive(m) = fn() -> Protocol(m)
// Waiting for release that fixes defining alias on import
// type Supervisor(m) = fn(Receive(m)) -> Pid(m)

pub external fn spawn_link(fn(Receive(m)) -> Nil) -> Pid(m)
    = "core_task_native" "spawn_link"
