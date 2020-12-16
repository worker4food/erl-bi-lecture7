-module(cache).

-export([
    new/1, get_table/1, table_exists/1,
    insert/3, lookup/2, lookup_by_date/3
]).

new(TabName) ->
    cache_sup:start_child(TabName).

get_table(TabName) ->
    case syn:whereis({table, TabName}, with_meta) of
        {_, T} -> {ok, T};
        Other -> Other
    end.

table_exists(TabName) ->
    syn:whereis({table, TabName}) =/= undefined.

insert(TabName, Key, Val) ->
    {ok, T} = get_table(TabName),
    cache_crud:insert(T, Key, Val).

lookup(TabName, Key) ->
    {ok, T} = get_table(TabName),
    cache_crud:lookup(T, Key).

lookup_by_date(TabName, DateFrom, DateTo) ->
    {ok, T} = get_table(TabName),
    cache_crud:lookup_by_date(T, DateFrom, DateTo).
