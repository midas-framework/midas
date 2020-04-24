// import process
// import core/fleet_supervisor
// import midas_utils
//
// pub external fn self() -> process.Pid(m)
//     = "erlang" "self"
//
// pub external fn receive() -> process.Protocol(m)
//     = "process_native" "do_receive"
//
// pub fn echo(receive) {
//     midas_utils.display("started_task")
//
//     let m = receive()
//     midas_utils.display("got call")
//     let process.Message(tuple(test, number)) = m
//     midas_utils.display(number)
//     process.send(test, number)
//     Nil
// }
//
// pub fn start_test() {
//     midas_utils.display("foo")
//     let supervisor = fleet_supervisor.start_link(echo)
//     midas_utils.display(supervisor)
//     midas_utils.display("supervisor")
//     let child1 = fleet_supervisor.start_child(supervisor)
//     midas_utils.display(child1)
//     // It doesn't down until message is sent
//     // let r = process.monitor(child1)
//     midas_utils.display("111111111")
//     midas_utils.display("222")
//     // can't leave around
//     // midas_utils.display(r)
//     midas_utils.display(child1)
//     process.send(child1, tuple(self(), 500))
//     // Down message comes before reply
//     let x = receive()
//     midas_utils.display("111111111")
//     let child2 = fleet_supervisor.start_child(supervisor)
//     midas_utils.display("1222")
//     midas_utils.display(child2)
//     process.send(child1, tuple(self(), 530))
//     let x = receive()
//     midas_utils.display(x)
//
// }
