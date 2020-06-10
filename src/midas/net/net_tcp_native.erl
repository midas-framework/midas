-module(net_tcp_native).
-export([listen/1, send/2, read_line/2, read_blob/3]).

listen(Port) ->
  TcpOptions = [{mode, binary}, {packet, line}, {active, false}, {reuseaddr, true}],
  gen_tcp:listen(Port, TcpOptions).

send(Socket, String) ->
  case gen_tcp:send(Socket, String) of
    ok -> {ok, nil};
    {error, reason} -> {error, reason}
  end.

read_line(Socket, Timeout) ->
  inet:setopts(Socket, [{packet, line}]),
  gen_tcp:recv(Socket, 0, Timeout).

read_blob(Socket, Length, Timeout) ->
  inet:setopts(Socket, [{packet, raw}]),
  gen_tcp:recv(Socket, Length, Timeout).
