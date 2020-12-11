-module(common).

-export([to_res_id/1]).

to_res_id(<<S/binary>>) ->
    S;
to_res_id(S) ->
    S1 = io_lib:format("~tw", [S]),
    erlang:iolist_to_binary(S1).
