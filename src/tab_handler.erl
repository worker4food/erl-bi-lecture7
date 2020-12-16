-module(tab_handler).

-export([
    init/2,
    allowed_methods/2,
    content_types_accepted/2,
    resource_exists/2,
    from_json/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[
        {<<"application/json">>, from_json}
    ], Req, State}.

resource_exists(Req, State) ->
    {false, Req, State}.

from_json(Req, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    #{<<"name">> := RawName} = jsone:decode(Body),
    Name = cache_common:to_res_id(RawName),
    {Code, Res} = case cache:new(Name) of
        {ok, _} -> {201, #{result => ok}};
        {error, {already_started, _}} -> {409, #{result => error}}
    end,
    Resp = cowboy_req:reply(Code,
        #{<<"location">> => [cowboy_req:uri(Req), $/, Name]},
        [jsone:encode(Res), $\n],
        Req1
    ),
    {stop, Resp, State}.
