%% (Table)Name Server
-module(cache_ns).

-behaviour(gen_server).

%% API
-export([start/0, start/1, stop/1, start_link/0]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

%% 'client' functions
-export([register_name/2, unregister_name/1, whereis_name/1, send/2]).

%% API
start() ->
    start(#{}).

start(_) ->
    gen_server:start({local, ?MODULE}, ?MODULE, #{}, []).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, #{}, []).

stop(_) ->
    gen_server:call(?MODULE, stop).

init(Args) when is_map(Args) ->
    {ok, Args};
init(Args) ->
    {stop, {expected_map, Args}}.

%% gen_server
handle_call({register_name, Name, Pid}, _, State) ->
    case maps:is_key(Name, State) of
        true -> {reply, no, State};
        false ->
            monitor(process, Pid),
            {reply, yes, State#{Name => Pid}}
    end;

handle_call({whereis_name, Name}, _, State)->
    {reply, maps:get(Name, State, undefined), State};

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({unregister_name, Name}, State) ->
    {noreply, maps:remove(Name, State)};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _, process, Pid, _}, State) ->
    NewState = maps:filter(fun (_, V) -> V =/= Pid end, State),
    {noreply, NewState};

handle_info(_, State) ->
    {noreply, State}.

%% 'client' functions
register_name(Name, Pid) ->
    gen_server:call(?MODULE, {register_name, Name, Pid}).

unregister_name(Name) ->
    gen_server:cast(?MODULE, {unregister_name, Name}),
    ok.

whereis_name(Name) ->
    gen_server:call(?MODULE, {whereis_name, Name}).

send(Name, Msg) ->
    case whereis_name(Name) of
        undefined ->
            {badarg, {Name, Msg}};
        Pid ->
            Pid ! Msg,
            Pid
    end.
