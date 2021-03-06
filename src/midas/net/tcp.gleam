import gleam/atom.{Atom}

external type Charlist

external fn binary_to_list(String) -> Charlist =
  "erlang" "binary_to_list"

pub external type ListenSocket

pub external type Socket

type InetOpts {
  Binary
  Active(Bool)
}

external fn do_connect(Charlist, Int, List(InetOpts)) -> Result(Socket, Nil) =
  "gen_tcp" "connect"

pub fn connect(address, port) -> Result(Socket, Nil) {
  do_connect(binary_to_list(address), port, [Binary, Active(False)])
}

pub external fn listen(Int) -> Result(ListenSocket, Nil) =
  "net_http_native" "listen"

pub external fn accept(ListenSocket) -> Result(Socket, Nil) =
  "gen_tcp" "accept"

pub external fn send(Socket, String) -> Result(Nil, Nil) =
  "net_tcp_native" "send"

pub external fn read_line(Socket, Int) -> Result(String, Nil) =
  "net_tcp_native" "read_line"

pub external fn read_blob(Socket, Int, Int) -> Result(String, Nil) =
  "net_tcp_native" "read_blob"

// Port to list and port info
external fn erl_close(Socket) -> Atom =
  "gen_tcp" "close"

pub fn close(socket: Socket) -> Nil {
  erl_close(socket)
  Nil
}
