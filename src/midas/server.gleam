import midas_tcp

external fn display(a) -> Nil = "erlang" "display"

pub fn start_link() {
    display("data")
    let Ok(listen_socket) = midas_tcp.listen(8080)
    let Ok(socket) = midas_tcp.accept(listen_socket)
    let Ok(data) = midas_tcp.pull(socket, 5000)
    display(data)
    let Ok(Nil) = midas_tcp.send(socket, data)
    0
}
