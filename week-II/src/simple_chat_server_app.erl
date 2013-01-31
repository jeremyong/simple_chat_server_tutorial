-module(simple_chat_server_app).

-behaviour(application).

-export([
         start/2,
         stop/1
        ]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile(
                 [
                  {'_', [
                         {"/", ws_handler, []}
                        ]}
                 ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 7777}],
                                [{env, [{dispatch, Dispatch}]}]),
    chat_room:start_link(main, "This is the main chat room").

stop(_State) ->
    ok.
