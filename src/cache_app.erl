%%%-------------------------------------------------------------------
%% @doc cache public API
%% @end
%%%-------------------------------------------------------------------

-module(cache_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Root = <<"/api/tables">>,
    Routes = cowboy_router:compile([
        {'_', [
            {Root, tab_handler, []},
            {[Root, <<"/:tab">>], obj_handler, []},
            {[Root, <<"/:tab/:key">>], obj_handler, []},
            {<<"/api/query/:tab/:action">>, [{action, fun cache_common:action/2}], query_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
        env => #{dispatch => Routes}
    }),
    cache_sup:start_link().

stop(_State) ->
    cowboy:stop_listener(http),
    ok.
