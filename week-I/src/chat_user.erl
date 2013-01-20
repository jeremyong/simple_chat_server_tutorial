-module(chat_user).
-include_lib("eunit/include/eunit.hrl").

-export([
         init/1,
         terminate/0,
         send/2,
         recv/3,
         join/1,
         leave/1
        ]).

init([User]) ->
    spawn_link(?MODULE, run, [User]).

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

run(User) ->
    receive
        _ ->
            io:format("Message received"),
            run(User)
    after
        1000*60*60 ->
            io:format("Disconnecting after 1 hour of inactivity"),
            terminate()
    end.
