-module(ws_handler).

-behaviour(cowboy_websocket_handler).

-export([
         init/3,
         websocket_init/3,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3
        ]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    chat_room:join(main, self()),
    {ok, Req, undefined}.

websocket_handle({text, Msg}, Req, State) ->
    chat_room:recv(main, self(), Msg),
    {ok, Req, State};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info([{room, Room},
                {sender, Sender},
                {msg, Msg}],
               Req, State) ->
    {reply, {text, "Received message:" ++ Msg}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    chat_room:leave(main, self()),
    ok.
