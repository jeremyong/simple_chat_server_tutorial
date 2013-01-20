-module(chat_room).

-export([
         init/1,
         emit/2,
         recv/2,
         join/1,
         leave/1,
         roster/0
        ]).

init([Name, Topic]) ->
    ok.

emit(User, Msg) ->
    ok.

recv(User, Msg) ->
    ok.

join(User) ->
    ok.

leave(User) ->
    ok.

roster() ->
    ok.
