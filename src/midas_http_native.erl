-module(midas_http_native).
-export([parse/1]).

parse(String) ->
  {ok, StartLine, Rest} = erlang:decode_packet(http_bin, String, []),
  {http_request, HttpMethod, HttpUri, HttpVersion} = StartLine,
  {abs_path, HttpPath} = HttpUri,
  {ok, HttpHeaders} = parse_headers(Rest, []),
  {ok, {HttpMethod, HttpPath, HttpHeaders}}.

parse_headers(String, Acc) ->
  case erlang:decode_packet(httph_bin, String, []) of
    {ok, http_eoh, Rest} ->
      {ok, lists:reverse(Acc)};
    {ok, HttpHeader, Rest} ->
      {http_header, _, Key, _, Value} = HttpHeader,
      parse_headers(Rest, [{Key, Value} | Acc])
  end.
