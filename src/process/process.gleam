//// Working with typed processes in Gleam.
////
//// This module allows you to:
//// - spawn processes
//// - send and receive messages
//// - call processes, send a message and expect a reply.
//// - monitor processes
//// - link to processes
//// - supervise processes with the `set_supervisor` and `rest_for_one`
////
//// *This module is being developed concurrently with the Midas web framework.
//// It will be extracted before Midas reaches 1.0.
//// Help with this module is greatly appreciated.*
////
//// You will need these imports for the examples to work
////
////    import process/process
////    import process/process.{Infinity, From}
////
//// ## Starting and stopping processes
////
//// A process is started using `process.spawn_link/1`.
//// The `spawn_link` function executed the given function in a new process.
////
//// This is an example of the simplest process that immediately exits.
////
////    let pid = process.spawn_link(fn(_receive) {
////        Nil
////    })
////
//// ## Sending and receiving messages
////
//// Messages are sent to a Processes reference `Pid(m)` using `process.send`.
//// A procees receives these messages using the receive function that was passed as an argument to the run function went it was spawned.
////
//// Example a simple process that sums all the numbers sent to it.
////
////    fn loop(receive, state) {
////      let value = receive(Infinity)
////      loop(receive, state + value)
////    }
////
////    let pid = process.spawn_link(loop(_, 0))
////
////    process.send(pid, 1)
////    process.send(pid, 2)
////
//// By recursively calling loop this function runs forever.
//// The type of the receive function is parameters by the type of messages it accepts.
////
//// Because gleam infers the type of value to be an Int it is known that only integers can be sent to this pid.
//// The following call will not be allowed by the type checker.
////
////    process.send(pid, "0")
////
//// Normally a process will accept more message types than an Int,
//// This is handled by defining a single type with a constructor for each expected message type.
////
//// ## Calling a process
////
//// Send a message to a process an await a reply.
////
////    type Messages {
////      Reverse(From(String), String)
////    }
////
////    fn loop(receive) {
////      let Reverse(from, str) = receive(Infinity)
////      process.reply(from, string.reverse(str))
////      loop(receive)
////    }
////
////    let pid = process.spawn_link(loop(_))
////
////    let Ok(reversed) = process.call(pid, Reverse(_, "hello"))
////
//// The `Reverse` message type includes a from reference that says callers accept a `String` type as a response.
//// Receiving this message works the same way as before, but now we have a from value that we can send a reply to.
//// Sending a call message uses `process.call/2`.
//// The first argument is a Pid.
//// The second argument is a function that takes a from reference and creates a message.
////
//// Having a constructor for the sent message allows the process library to create a from value.
//// Doing this allows the calling process to receive replies without having to specify them as part of their message contract.
//// Again Gleam ensures we can't send a message that isn't understood and we can't reply with a type that isn't expected.


import gleam/atom.{Atom}
import gleam/result.{Option}

/// A value in milliseconds or infinity.
pub type Wait {
  Infinity
  Milliseconds(Int)
}

// WORKING WITH PROCESSES

/// Identifier for a process
/// This type is parameterised by the message type acceptable to that process.
pub external type Pid(m)

/// Reason for a process to terminate.
///
/// In most cases `Normal` is the correct value to return from a process's run function.
pub type ExitReason {
  Normal
  // TODO Add a reference to stacktrace
  Crashed
  Killed
}

// https://erlang.org/doc/reference_manual/errors.html#exit_reasons
// Other types e.g. BadArith(Stack) can all be enumerated here.
// receive can only fail due to timeout so option is an acceptable response type
type Receive(m) =
  fn(Wait) -> Option(m)

type Run(m, r) =
  fn(Receive(m)) -> r

/// Start a new process to execute the given function.
///
/// The run function, passed to `spawn_link`, will be called with a function that is used to pop messages from the mailbox.
/// Messages can be sent to the mailbox for this process using `process.send` and the Pid reference returned by `spawn_link`.
///
/// When the run function completes the process will stop with a normal exit reason.
///
pub external fn spawn_link(run: Run(m, r)) -> Pid(m) =
  "process_native" "spawn_link"

external fn do_send(Pid(m), m) -> m =
  "erlang" "send"

/// Send a message to a process.
///
/// Note the message is sent asynchronously and this call returns immediately.
/// This call **will not** error if the process is not alive or is unresponse.
/// Use `process.call` to await on a reply from the receiving process.
pub fn send(pid, message) {
  do_send(pid, message)
  Nil
}

/// Get a reference to the Pid of the calling process.
///
/// The message types accepted by this process is inferred from it's usage.
pub external fn unsafe_self() -> Pid(m) =
  "erlang" "self"

/// Get a typed reference to the Pid of the calling process.
///
/// This function takes the process's receive function as an argument to correctly infer the message type of the Pid.
pub fn self(_: Receive(m)) -> Pid(m) {
  unsafe_self()
}

// This can be typed because an untyped pid is only the result of a DOWN or EXIT Message
external fn exit(Pid(m), Atom) -> Bool =
  "erlang" "exit"

/// Kill a process
///
/// Stop the exection of a process.
/// The process unconditionally exits with the reason `killed`.
pub fn kill(pid: Pid(m)) -> Bool {
    exit(pid, atom.create_from_string("kill"))
}

// WORKING WITH UNTYPED PROCESSES

/// A Pid that is not parameterised by the messages it can receive.
/// Bare Pids are useful when you receive a Pid from a DOWN or EXIT message.
/// They can also be used to check equality between two pids.
pub external type BarePid

/// Create a `BarePid` from a `Pid(m)`
pub external fn bare(Pid(a)) -> BarePid =
  "process_native" "identity"

// WORKING WITH MONITORS AND REFERENCES

/// A unique reference
///
pub external type Ref

/// The type of a monitor.
///
/// Monitors can be set on processes or ports.
/// This module only allows you to monitor processes,
/// but this type is needed for deconstructing Down message.
/// If exit/monitor mapping is implemented then this type will no longer be needed
///
/// https://github.com/midas-framework/midas/issues/20
pub type MonitorType {
  Process
}

external fn erlang_monitor(MonitorType, Pid(m)) -> Ref =
  "erlang" "monitor"

/// Start a monitor on a process
///
/// When the process terminates, a monitor message will be sent to the calling process.
/// See `demonitor` to cancel running monitors
pub fn monitor(pid) {
  erlang_monitor(Process, pid)
}

/// Options when demonitoring a process
pub type DemonitorOptions {
  Flush
}

/// Stop monitoring a process
///
/// Use the reference from monitoring a process to stop monitoring the process.
/// Use the Flush option to remove any messages from this monitor from the processes mailbox
pub external fn demonitor(Ref, List(DemonitorOptions)) -> Bool =
  "erlang" "demonitor"

// PROCESS FLAGS, WARNING:

/// Options that can be set using `process_flag`
///
/// TrapExit can change the messages expected in a receive function
pub type ProcessFlag {
  TrapExit(Bool)
}

/// Set a process flag
///
/// See the erlang documentation for details.
/// https://erlang.org/doc/man/erlang.html#process_flag-2
pub external fn process_flag(ProcessFlag) -> ProcessFlag =
  "process_native" "process_flag"

// CALL PROCESS

/// Reference to a calling process
///
/// See `call` for how to send a message to a process and await a reply.
pub type From(r) {
  From(Ref, Pid(tuple(Ref, r)))
}

/// Reasons for a call to fail
///
/// A call may fail due to the process being dead, `Gone` or a response not being received in time.

// We should always have {ref, message} on a separate channel for late replies from calls.
pub type CallError {
  Timeout
  Gone
}

// Gone(ExitReason) To not conflict with Down types
external fn receive_reply(Ref, Wait) -> Result(r, CallError) =
  "process_native" "receive_reply"

/// Make a call to a process
///
/// A call is a message sent to a process which the client expect a reply.
/// This funtion will not return until the response to the call is received or an error is detected.
///
pub fn call(
  pid: Pid(m),
  constructor: fn(From(r)) -> m,
  wait: Wait,
) -> Result(r, CallError) {
  let reference = monitor(pid)
  let from = From(reference, unsafe_self())
  let _message = send(pid, constructor(from))
  receive_reply(reference, wait)
}

/// Reply to a call.
///
/// Send a message in reply to a call from another process.
/// *See `process.call` for starting a call.
pub fn reply(from: From(r), message: r) {
  let From(reference, pid) = from
  send(pid, tuple(reference, message))
}

external fn do_sleep(Int) -> Atom =
  "timer" "sleep"

/// Suspend a process.
///
/// The process calling this function is suspended for the number of milliseconds given.
///
pub fn sleep(time: Int) -> Nil {
    do_sleep(time)
    Nil
}
