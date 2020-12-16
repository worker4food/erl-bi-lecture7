-module(obj_handler).

-export([
    init/2,
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2,
    malformed_request/2,
    resource_exists/2,
    from_json/2,
    to_json/2
]).

init(Req, _) ->
    {cowboy_rest, Req, #{}}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"HEAD">>, <<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, from_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}], Req, State}.

malformed_request(Req, State) ->
    Tab = cowboy_req:binding(tab, Req),
    {not cache:table_exists(Tab), Req, State}.

resource_exists(#{method := <<"POST">>} = Req, State) ->
    {false, Req, State};
resource_exists(Req, State) ->
    #{tab := TabName, key := Key} = cowboy_req:bindings(Req),
    Res = cache:lookup(TabName, Key),
    {Res =/= undefined, Req, State#{val => Res}}.

from_json(Req, State) ->
    TabName = cowboy_req:binding(tab, Req),
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    #{<<"key">> := RawKey, <<"value">> := Value} = jsone:decode(Body),
    Key = cache_common:to_res_id(RawKey),
    ok = cache:insert(TabName, Key, Value),
    Uri = [cowboy_req:uri(Req1), $/, Key],
    Res = jsone:encode(#{result => ok}),
    Resp = cowboy_req:set_resp_body([Res, $\n], Req1),
    {{true, Uri}, Resp, State}.

to_json(Req, #{val := {ok, Value}} = State) ->
    Body = jsone:encode(#{result => Value}),
    {[Body, $\n], Req, State}.
