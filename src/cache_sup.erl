-module(cache_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    NS = #{
        id       => cache_ns,
        start    => {cache_ns, start_link, []},
        restart  => permanent,
        type     => worker,
        shutdown => brutal_kill
    },
    Mgr = #{
        id       => cache_mgr_sup,
        start    => {cache_mgr_sup, start_link, []},
        restart  => permanent,
        type     => supervisor,
        shutdown => infinity
    },
    {ok, {{one_for_all, 10, 10}, [NS, Mgr]}}.

%% internal functions
