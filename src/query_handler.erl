-module(query_handler).

-export([
    init/2,
    allowed_methods/2,
    content_types_provided/2,
    malformed_request/2,
    resource_exists/2,
    to_json/2
]).

init(Req, _) ->
    {cowboy_rest, Req, #{
        action => cowboy_req:binding(action, Req),
        table  => cowboy_req:binding(tab, Req)
    }}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, to_json}
    ], Req, State}.

malformed_request(Req, #{table := Tab} = State) ->
    {not cache:table_exists(Tab), Req, State}.

resource_exists(Req, #{action := <<"lookup">>} = State) ->
    M = cowboy_req:match_qs([{key, nonempty}], Req),
    {true, Req, maps:merge(State, M)};
resource_exists(Req, #{action := <<"lookup_by_date">>} = State) ->
    M = cowboy_req:match_qs(
        [{date_from, fun cache_common:datetime/2}, {date_to, fun cache_common:datetime/2}], Req),
    {true, Req, maps:merge(State, M)}.

to_json(Req, #{key := Key, table := TabName} = State) ->
    Res = cache:lookup(TabName, Key),
    Body = jsone:encode(#{result => from_res(Res)}),
    {[Body, $\n], Req, State};
to_json(Req, #{table := TabName, date_from := From, date_to := To} = State) ->
    {ok, Values} = cache:lookup_by_date(TabName, From, To),
    Body = jsone:encode(#{
        result => Values
    }),
    {[Body, $\n], Req, State}.

from_res({ok, X}) -> X;
from_res(X) -> X.
