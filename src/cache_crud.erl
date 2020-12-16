-module(cache_crud).

-include("cache_tables.hrl").

-define(ETS_DEFAULT, [
    public, {write_concurrency, true}, {read_concurrency, true}]).

-define(TTL_DEFAULT, 600).

-export([
    new/0, new/1,
    insert/4, insert/3,
    lookup/2, lookup_by_date/3,
    delete_obsolete/1]).

-define(MIN_V, 0).
-define(MID_V, stub).
-define(MAX_V, {}).

new() ->
    new(?ETS_DEFAULT).

new(Opts) ->
    #tables{
        data       = ets:new(data, [set | Opts]),
        ix_ttl     = ets:new(ix_ttl, [ordered_set | Opts]),
        ix_created = ets:new(ix_created, [ordered_set | Opts])
    }.

insert(T, Key, Val) ->
    insert(T, Key, Val, ?TTL_DEFAULT).

insert(#tables{data = D, ix_ttl = IxTtl, ix_created = IxCreated}, Key, Val, TtlSec) ->
    CreatedAt = now_dt(),
    ExpiredAt = add_seconds(CreatedAt, TtlSec),
    Row = {Key, Val, CreatedAt, ExpiredAt},
    case ets:insert_new(D, Row) of
        false ->
            [{_, _, OldCreatedAt, OldExpiredAt}] = ets:lookup(D, Key),
            ets:delete(IxTtl, {OldExpiredAt, ?MID_V, Key}),
            ets:delete(IxCreated, {OldCreatedAt, ?MID_V, Key}),
            ets:insert(D, Row);
        true -> true
    end,
    ets:insert(IxTtl, {{ExpiredAt, ?MID_V, Key}}),
    ets:insert(IxCreated, {{CreatedAt, ?MID_V, Key}}),
    ok.

lookup(T, Key) ->
    lookup(T, Key, now_dt()).

lookup(T, Key, Now) ->
    case lookup_kv(T, Key, Now) of
        {_, Val} -> {ok, Val};
        Other -> Other
    end.

lookup_kv(#tables{data = Data}, Key, Now) ->
    case ets:lookup(Data, Key) of
        [{_, Val, _, ExpiredAt}] when Now =< ExpiredAt -> {Key, Val};
        _ -> undefined
    end.

lookup_by_date(#tables{ix_created = Ix} = Tabs, DateFrom, DateTo) ->
    Now = now_dt(),
    Fn = fun ({_, _, Key}, Acc) -> [lookup_kv(Tabs, Key, Now) | Acc] end,
    BeforeFisrt = {DateFrom, ?MIN_V, undefined},
    AfterLast = {DateTo, ?MAX_V, undefined},
    StartKey = ets:next(Ix, BeforeFisrt),
    Rows = [R || {_, _} = R <- fold_ix_tab(Fn, Ix, StartKey, AfterLast, [])],
    {ok, Rows}.

delete_obsolete(Tabs) ->
    delete_obsolete(Tabs, now_dt()).

delete_obsolete(#tables{data = D, ix_ttl = IxTtl, ix_created = IxCreated}, Eol) ->
    MaxKey = {Eol, ?MIN_V, undefined}, % do not touch CreatedAt == Eol
    Fn = fun ({ExpiredAt, _, Key}, Acc) ->
        CreatedAt = ets:lookup_element(D, Key, 3),
        ets:delete(D, Key),
        ets:delete(IxTtl, {ExpiredAt, ?MID_V, Key}),
        ets:delete(IxCreated, {CreatedAt, ?MID_V, Key}),
        Acc
    end,
    MinKey = ets:next(IxTtl, 0), % number < tuple
    fold_ix_tab(Fn, IxTtl, MinKey, MaxKey, ok).

%% private functions
now_dt() ->
    calendar:universal_time().

add_seconds(Date, OffsetSec) ->
    DateSec = calendar:datetime_to_gregorian_seconds(Date),
    calendar:gregorian_seconds_to_datetime(DateSec + OffsetSec).

fold_ix_tab(Fn, IxTab, {_, _, _} = Key, ToKey, Acc) when Key =< ToKey ->
    NewAcc = Fn(Key, Acc),
    NextKey = ets:next(IxTab, Key),
    fold_ix_tab(Fn, IxTab, NextKey, ToKey, NewAcc);
fold_ix_tab(_, _, _, _, Acc) ->
    Acc.
