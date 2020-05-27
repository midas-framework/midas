-module(midas_utils_native).
-export([split_on/2]).

% TODO make a net/utils module
split_on(Subject, Pattern) ->
  case binary:split(Subject, Pattern) of
    [Part, Rest] -> {Part, {some, Rest}};
    [Part] -> {Part, none}
  end.
