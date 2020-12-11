-module(cache_table_sup).

-behaviour(supervisor).

-export([start_link/1, init/1]).

-include("cache_names.hrl").

start_link(Name) ->
    supervisor:start_link(?GEN_NAME(Name), ?MODULE, [Name]).

% start_link(Name) ->
    % supervisor:start_link(?MODULE, [Name]).

init([Name]) ->
    Tables = cache_crud:new(),
    SrvId = ?GEN_NAME(cache_table_srv, Name),
    TabSrv = #{
        id       => cache_table_srv,
        start    => {cache_table_srv, start_link, [SrvId, Tables]},
        restart  => permanent,
        type     => worker,
        shutdown => brutal_kill
    },
    TabGC = #{
        id       => cache_table_gc,
        start    => {cache_table_gc, start_link, [Tables]},
        restart  => permanent,
        type     => worker,
        shutdown => brutal_kill
    },
    {ok, {{rest_for_one, 10, 30}, [TabSrv, TabGC]}}.
