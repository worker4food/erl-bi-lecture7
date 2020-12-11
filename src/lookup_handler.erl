-module(lookup_handler).

-export([init/2]).

init(Req, _) ->
    {cowboy_rest, Req, #{}}.
