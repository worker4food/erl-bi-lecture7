-module(cache_crud_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
    insert/1,
    lookup/1,
    lookup_expired/1,
    lookup_by_date/1,
    lookup_by_date_expired/1,
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2
]).

-define(Sz, 1000).

all() -> [
    insert,
    lookup,
    lookup_expired,
    lookup_by_date,
    lookup_by_date_expired
].

init_per_suite(Cfg) ->
    [{keys, lists:seq(1, ?Sz)} | Cfg].

end_per_suite(_) ->
    ok.

init_per_testcase(_, Cfg) ->
    [{table, cache_crud:new()} | Cfg].

%% TESTS
insert(Cfg) ->
    T = ?config(table, Cfg),
    [ok = cache_crud:insert(T, X, X, X) || X <- ?config(keys, Cfg)].

lookup(Cfg) ->
    T = ?config(table, Cfg),
    Keys = ?config(keys, Cfg),
    [cache_crud:insert(T, X, X, 10000) || X <- Keys],
    [{ok, X} = cache_crud:lookup(T, X) || X <- Keys].

lookup_expired(Cfg) ->
    T = ?config(table, Cfg),
    Keys = ?config(keys, Cfg),
    [cache_crud:insert(T, X, X, -1) || X <- Keys],
    [undefined = cache_crud:lookup(T, X) || X <- Keys].

lookup_by_date(Cfg) ->
    T = ?config(table, Cfg),
    Keys = ?config(keys, Cfg),
    Start = calendar:universal_time(),
    [cache_crud:insert(T, X, X, 10000) || X <- Keys],
    End = calendar:universal_time(),
    {ok, Vals} = cache_crud:lookup_by_date(T, Start, End),
    Keys = lists:sort(Vals).

lookup_by_date_expired(Cfg) ->
    T = ?config(table, Cfg),
    Keys = ?config(keys, Cfg),
    Start = calendar:universal_time(),
    [cache_crud:insert(T, X, X, -1) || X <- Keys],
    End = calendar:universal_time(),
    {ok, []} = cache_crud:lookup_by_date(T, Start, End).
