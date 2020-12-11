-module(query_handler).

init(Req, _) ->
    {cowboy_rest, Req, #{}}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {"application/json", to_json}
    ]}.

to_json(Req, State) ->
    todo.

datetime(forward, Val) -> todo;
datetime(reverse, Val) -> todo;
datetime(format_error, Val) -> todo.
