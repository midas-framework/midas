import core/core
import core/process.{Pid, BarePid}

pub type Protocol(m) {
    Exit(BarePid)
    Down(core.Ref)
    Message(m)
}

// Using a receive it is possible to not wrap all custom messages in a Message block,
// Doing so does allow one to forget to implement Down and Exit messages
// It also allows owners of the address to send in the Down and exit messages
// It does mean there is no problem with a
// Much nicer to do tests with

// Proc lib defines init_ack, and funtions to wait for that, init_ack returns the pid in most cases
// spawn_link does nothing but get parents
// init_p simply sets parents in new process
// start_link uses spawn link, awaits a reply from init_ack and then demonitors if appropriate

// Gen module defines call, but also naming that I don't yet want to handle

// To keep a protocol private you will need send and receive external functions defined in module.

// TODO add timeout
type Receive(m) = fn() -> Protocol(m)
// Waiting for release that fixes defining alias on import
// type Supervisor(m) = fn(Receive(m)) -> Pid(m)

pub external fn spawn_link(fn(Receive(m)) -> Nil) -> Pid(m)
    = "core_supervisor_native" "spawn_link"
