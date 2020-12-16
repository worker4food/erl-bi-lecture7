-module(cache_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
    all/0, groups/0,
    init_per_group/2, end_per_group/2
]).

-export([
    create/1,
    insert/1,
    lookup/1,
    lookup_restish/1,
    lookup_restish_404/1,
    lookup_by_date/1
]).

-export([
    no_table_insert/1,
    no_table_lookup/1,
    no_table_lookup_restish/1,
    no_table_lookup_by_date/1
]).

-define(Table, <<"test">>).
-define(BaseUrl, <<"http://localhost:8080/api/">>).

all() -> [
    {group, normal},
    {group, no_table}
].

groups() -> [
    {no_table, [shuffle], [
        no_table_insert,
        no_table_lookup,
        no_table_lookup_restish,
        no_table_lookup_by_date
    ]},
    {normal, [sequence], [
        create,
        insert,
        lookup,
        lookup_restish,
        lookup_restish_404,
        lookup_by_date
    ]}
].

init_per_group(_, Cfg) ->
    {ok, _} = application:ensure_all_started(cache),
    {ok, _} = application:ensure_all_started(hackney),
    Cfg.

end_per_group(_, _) ->
    ok = application:stop(hackney),
    ok = application:stop(cache).

%% Bad requests (no table exists)
no_table_insert(_) ->
    Url = hackney_url:make_url(?BaseUrl, ["tables", ?Table], []),
    {ok, Code, _, _} = hackney:post(
        Url,
        [{<<"content-type">>, <<"application/json">>}],
        jsone:encode(#{key => one, value => 1})),
    400 = Code.

no_table_lookup(_) ->
    Url = hackney_url:make_url(
        ?BaseUrl,
        [<<"query">>, ?Table, <<"lookup">>],
        [{key, one}]
    ),
    {ok, Code, _, _} = hackney:get(Url),
    400 = Code.

no_table_lookup_restish(_) ->
    Url = hackney_url:make_url(
        ?BaseUrl,
        [<<"tables">>, ?Table, <<"one">>],
        []
    ),
    {ok, Code, _, _} = hackney:get(Url),
    400 = Code.

no_table_lookup_by_date(_) ->
    Dt = iso8601:format(erlang:universaltime()),
    Url = hackney_url:make_url(
        ?BaseUrl,
        [<<"query">>, ?Table, <<"lookup_by_date">>],
        [{date_from, Dt}, {date_to, Dt}]
    ),
    {ok, Code, _, _} = hackney:get(Url),
    400 = Code.

%% Normal
create(_) ->
    Input = jsone:encode(#{name => ?Table}),
    {ok, Code, _, CliRef} = hackney:post(
        hackney_url:make_url(?BaseUrl, <<"tables">>, []),
        [{<<"content-type">>, <<"application/json">>}],
        Input
    ),
    201 = Code,
    {ok, Body} = hackney:body(CliRef),
    R = jsone:decode(Body),
    R =  #{<<"result">> => <<"ok">>}.

insert(_) ->
    Url = hackney_url:make_url(?BaseUrl, [<<"tables">>, ?Table], []),
    Input = lists:zip([one, two, three], [1, 2, 3]),
    [begin
        ReqBody = jsone:encode(#{key => K, value => V}),
        {ok, Code, _, CliRef} = hackney:post(
            Url,
            [{<<"content-type">>, <<"application/json">>}],
            ReqBody
        ),
        201 = Code,
        {ok, Resp} = hackney:body(CliRef),
        R = jsone:decode(Resp),
        R = #{<<"result">> => <<"ok">>}
    end || {K, V} <- Input],
    ok.

lookup(_) ->
    Input = lists:zip([one, two, three], [1, 2, 3]),
    [begin
        Url = hackney_url:make_url(
            ?BaseUrl,
            [<<"query">>, ?Table, <<"lookup">>],
            [{key, Key}]),
        ct:comment(Url),
        {ok, Code, _, CliRef} = hackney:get(Url),
        200 = Code,
        {ok, Resp} = hackney:body(CliRef),
        R = jsone:decode(Resp),
        R = #{<<"result">> => Val}
    end || {Key, Val} <- Input],
    ok.

lookup_restish(_) ->
    Input = lists:zip([one, two, three], [1, 2, 3]),
    [begin
        Key = cache_common:to_res_id(RawKey),
        Url = hackney_url:make_url(
            ?BaseUrl,
            [<<"tables">>, ?Table, Key],
            []),
        ct:comment(Url),
        {ok, Code, _, CliRef} = hackney:get(Url),
        200 = Code,
        {ok, Resp} = hackney:body(CliRef),
        R = jsone:decode(Resp),
        R = #{<<"result">> => Val}
    end || {RawKey, Val} <- Input],
    ok.

lookup_restish_404(_) ->
    Url = hackney_url:make_url(
        ?BaseUrl,
        [<<"tables">>, ?Table, <<"key_not_exists">>],
        []
    ),
    {ok, Code, _, _} = hackney:get(Url),
    404 = Code.

lookup_by_date(_) ->
    Now = calendar:universal_time(),
    DateFrom = iso8601:format(iso8601:add_days(Now, -1)),
    DateTo = iso8601:format(Now),
    Url = hackney_url:make_url(
        ?BaseUrl,
        [<<"query">>, ?Table, <<"lookup_by_date">>],
        [{date_from, DateFrom}, {date_to, DateTo}]),
    {ok, Code, _, CliRef} = hackney:get(Url),
    200 = Code,
    {ok, Body} = hackney:body(CliRef),
    #{<<"result">> := Res} = jsone:decode(Body),
    #{<<"one">> := 1, <<"two">> := 2, <<"three">> := 3} = Res.
