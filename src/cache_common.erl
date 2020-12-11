-module(cache_common).

-export([to_res_id/1, datetime/2, action/2]).

to_res_id(<<S/binary>>) ->
    S;
to_res_id(S) ->
    S1 = io_lib:format("~tw", [S]),
    erlang:iolist_to_binary(S1).

%% constraints
datetime(forward, Val) ->
    try
        iso8601:parse(Val)
    catch
        _:_ -> {error, {not_a_date, Val}}
    end;
datetime(reverse, Val) ->
    iso8601:format(Val);
datetime(format_error, {error, {not_a_date, Val}}) ->
    io_lib:format("Expecting datetime(yyyy-mm-ddThh:mm:ssZ), but got ~s", [Val]).

action(forward, Val) when Val =:= <<"lookup">>; Val =:= <<"lookup_by_date">> ->
    {ok, Val};
action(forvard, Val) ->
    {error, {invalid_action, Val}};
action(format_error, E) ->
    io_lib:format("~p", [E]).
