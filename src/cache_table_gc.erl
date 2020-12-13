-module(cache_table_gc).

-behaviour(gen_server).

%% API
-export([start/1, start_link/1]).
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

start(Tables) ->
    gen_server:start(?MODULE, Tables, []).

start_link(Tables) ->
    gen_server:start_link(?MODULE, Tables, []).

init(Tables) ->
    Interval = 10000,
    timer:send_interval(Interval, gc),
    {ok, Tables}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(gc, Tables) ->
    cache_crud:delete_obsolete(Tables),
    {noreply, Tables};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
