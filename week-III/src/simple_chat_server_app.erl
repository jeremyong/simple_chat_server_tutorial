-module(simple_chat_server_app).

-behaviour(application).

-export([
         start/0,
         start/2,
         stop/1
        ]).

start() ->
    application:start(crypto),
    application:start(ranch),
    application:start(cowboy),
    application:start(simple_chat_server).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile(
                 [
                  {'_', [
                         {"/", ws_handler, []}
                        ]}
                 ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 7777}],
                                [{env, [{dispatch, Dispatch}]}]),
    chat_room_sup:start_link().

stop(_State) ->
    ok.
