// Probably a worker if term task confusing
import core/core.{Wait}
import core/process.{Pid}

// Mailbox is better name somehow the protocol might not in include down, or in might include timeout
pub type Protocol(m) {
    // Potentinally get rid of down and have special receive
    Timeout
    Down
    Message(m)
}

// TODO add timeout
// Can define because Wait external
// type Receive(m) = fn(Wait) -> Protocol(m)
// Waiting for release that fixes defining alias on import
// type Supervisor(m) = fn(Receive(m)) -> Pid(m)

pub external fn spawn_link(fn(fn(Wait) -> Protocol(m)) -> Nil) -> Pid(m)
    = "core_task_native" "spawn_link"
