-module(chat_user).

-export([
         init/1,
         terminate/0,
         send/2,
         recv/3,
         join/1,
         leave/1
        ]).

init([User]) ->
    ok.

terminate() ->
    ok.

send(Room, Msg) ->
    ok.

recv(Room, From, Msg) ->
    ok.

join(Room) ->
    ok.

leave(Room) ->
    ok.
