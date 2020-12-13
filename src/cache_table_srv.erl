-module(cache_table_srv).

-behaviour(gen_server).

%% API
-export([start/2, start_link/2, stop/1, get_tables/1, table_exists/1]).
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include("cache_names.hrl").

start(Name, Tables) ->
    gen_server:start(Name, ?MODULE, Tables, []).

start_link(Name, Tables) ->
    gen_server:start_link(Name, ?MODULE, Tables, []).

stop(Name) ->
    gen_server:call(Name, stop).

init(Tabs) ->
    {ok, Tabs}.

handle_call(get_tables, _From, Tables) ->
    {reply, {ok, Tables}, Tables};

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% client functions

get_tables(Name) ->
    gen_server:call(?GEN_NAME(Name), get_tables).

table_exists(Name) ->
    cache_ns:whereis_name({?MODULE, Name}) =/= undefined.
