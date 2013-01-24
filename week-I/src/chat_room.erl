-module(chat_room).
-include_lib("eunit/include/eunit.hrl").

-export([
         init/1,
         run/3,
         terminate/1,
         change_topic/2,
         emit/3,
         recv/3,
         join/2,
         leave/2,
         roster/1
        ]).

init([Name, Topic]) ->
    Pid = spawn(?MODULE, run, [Name, Topic, []]),
    {ok, Pid}.

run(Name, Topic, Users) ->
    receive
        {roster, From} ->
            From ! {roster, Users},
            run(Name, Topic, Users);
        {join, User} ->
            io:format("~p joined room ~p", [User, Name]),
            run(Name, Topic, [User|Users]);
        _ ->
            io:format("Message received"),
            run(Name, Topic, Users)
    after
        3600000 ->
            io:format("Disconnecting after 1 hour of inactivity"),
            terminate(self())
    end.

terminate(Room) ->
    exit(Room, "User terminated room"),
    ok.

change_topic(Room, Topic) ->
    ok.

emit(Room, User, Msg) ->
    ok.

recv(Room, User, Msg) ->
    ok.

join(Room, User) ->
    Room ! {join, User}.

leave(Room, User) ->
    ok.

roster(Room) ->
    Room ! {roster, self()},
    receive
        {roster, Roster} ->
            {ok, Roster}
    end.

%% Unit tests

% I can start a room
start_room_test() ->
    {ok, RoomPid} = chat_room:init(["Room", "Topic"]),
    ?assert(is_process_alive(RoomPid)).

% Terminating test
terminate_test() ->
    {ok, RoomPid} = chat_room:init(["Room", "Topic"]),
    chat_room:terminate(RoomPid),
    ?assertNot(is_process_alive(RoomPid)).

% Joining a room should return the room topic
join_returns_room_topic_test() ->
    {ok, RoomPid} = chat_room:init(["Room", "Topic"]),
    Return = chat_room:join(RoomPid, "testuser"),
    ?assert(false).

% Joining a room should return the room roster
join_returns_room_roster_test() ->
    {ok, RoomPid} = chat_room:init(["Room", "Topic"]),
    Return = chat_room:join(RoomPid, "testuser"),
    ?assert(false).

% Leaving a room removes a user from the roster
leave_removes_user_from_roster_test() ->
    ?assert(false).

% Emitting a message sends the message to users in the room
emit_test() ->
    ?assert(false).

% Sending a message from a user causes the message to be emitted
send_triggers_emit_test() ->
    ?assert(false).

% Requesting the roster returns it
roster_test() ->
    {ok, RoomPid} = chat_room:init(["Room", "Topic"]),

    ?assert(false).
