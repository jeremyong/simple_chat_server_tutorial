-module(ws_handler).

-behaviour(cowboy_websocket_handler).

-export([
         init/3,
         websocket_init/3,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3
        ]).

-record(state, {username="unregistered_user"}).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    chat_room:join(main, self()),
    {ok, Req, #state{}}.

websocket_handle({text, Msg}, Req, State) ->
    case lists:sublist(binary_to_list(Msg), 5) of
        "user:" ->
            {"user:", Username} = lists:split(5, binary_to_list(Msg)),
            chat_room:recv(main, [], State#state.username ++
                           " is now known as " ++ Username
                          ),
            {ok, Req, State#state{username = Username}};
        _ ->
            chat_room:recv(main, State#state.username, Msg),
            {ok, Req, State}
    end;
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info([{room, Room},
                {sender, Sender},
                {msg, Msg}],
               Req, State) ->
    {reply, {text, Sender ++ " says: " ++ Msg}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, State) ->
    chat_room:recv(main, State#state.username, "Goodbye!"),
    chat_room:leave(main, self()),
    ok.
