-module(cache_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/1, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Name) ->
    supervisor:start_child(?MODULE, [Name]).

init(_) ->
    Spec = #{
        id       => cache_table_sup,
        start    => {cache_table_sup, start_link, []},
        restart  => transient,
        shutdown => infinity,
        type     => supervisor
    },
    {ok, {{simple_one_for_one, 10, 60}, [Spec]}}.
