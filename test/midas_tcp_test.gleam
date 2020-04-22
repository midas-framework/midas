import midas_tcp
import gleam/expect

// 
// pub fn parse_segments_test() {
//     let Ok(listen_socket) = midas_tcp.listen(8080)
//     let Ok(socket) = midas_tcp.accept(listen_socket)
//     let Ok(data) = midas_tcp.pull(socket, 5000)
//     display(data)
//     let Ok(Nil) = midas_tcp.send(socket, data)
//     0
// }
