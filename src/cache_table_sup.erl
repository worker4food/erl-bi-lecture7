-module(cache_table_sup).

-behaviour(supervisor).

-export([start_link/1, init/1]).

-define(FQN(X), {table, X}).

start_link(Name) ->
    supervisor:start_link({via, syn, ?FQN(Name)}, ?MODULE, [Name]).

init([Name]) ->
    Tables = cache_crud:new(),
    %% attach tables via meta
    ok = syn:unregister_and_register(?FQN(Name), self(), Tables),
    Spec = #{
        id       => cache_table_gc,
        start    => {cache_table_gc, start_link, [Tables]},
        restart  => permanent,
        type     => worker,
        shutdown => brutal_kill
    },
    {ok, {{rest_for_one, 10, 30}, [Spec]}}.
