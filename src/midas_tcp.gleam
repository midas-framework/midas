
pub type ListenSocket {
    ListenSocket()
}
pub type Socket {
    Socket()
}

pub external fn listen(Int) -> Result(ListenSocket, Nil)
    = "midas_tcp_native" "listen"

pub external fn accept(ListenSocket) -> Result(Socket, Nil)
    = "gen_tcp" "accept"

pub external fn send(Socket, String) -> Result(Nil, Nil)
    = "midas_tcp_native" "send"

pub external fn read_line(Socket, Int) -> Result(String, Nil)
    = "midas_tcp_native" "read_line"

pub external fn read_blob(Socket, Int, Int) -> Result(String, Nil)
    = "midas_tcp_native" "read_blob"

    // Port to list and port info
