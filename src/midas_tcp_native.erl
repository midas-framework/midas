-module(midas_tcp_native).
-export([listen/1, send/2, pull/2]).

listen(Port) ->
  TcpOptions = [{mode, binary}, {packet, raw}, {active, false}, {reuseaddr, true}],
  gen_tcp:listen(Port, TcpOptions).

send(Socket, String) ->
  case gen_tcp:send(Socket, String) of
    ok -> {ok, nil};
    {error, reason} -> {error, reason}
  end.

pull(Socket, Timeout) ->
  gen_tcp:recv(Socket, 0, Timeout).
