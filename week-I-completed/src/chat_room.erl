-module(chat_room).
-include_lib("eunit/include/eunit.hrl").

-export([
         init/1,
         run/3,
         terminate/1,
         change_topic/2,
         read_topic/1,
         emit/4,
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
        {change_topic, Topic1} ->
            run(Name, Topic1, Users);
        {read_topic, User} ->
            User ! {topic, Topic},
            run(Name, Topic, Users);
        {roster, From} ->
            From ! {roster, Users},
            run(Name, Topic, Users);
        {join, User} ->
            io:format("~p joined room ~p", [User, Name]),
            run(Name, Topic, [User|Users]);
        {leave, User} ->
            Users1 = lists:delete(User, Users),
            run(Name, Topic, Users1);
        {recv, User, Msg} ->
            lists:map(fun(U) ->
                              emit(self(), U, User, Msg)
                      end, Users),
            run(Name, Topic, Users);
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
    Room ! {change_topic, Topic},
    ok.

read_topic(Room) ->
    Room ! {read_topic, self()},
    receive
        {topic, Topic} ->
            {ok, Topic}
    end.

emit(Room, User, Sender, Msg) ->
    User ! [{room, Room}, {sender, Sender}, {msg, Msg}],
    ok.

recv(Room, User, Msg) ->
    Room ! {recv, User, Msg},
    ok.

join(Room, User) ->
    Room ! {join, User},
    ok.

leave(Room, User) ->
    Room ! {leave, User},
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
    {ok, RoomPid} = init(["Room", "Topic"]),
    ?assert(is_process_alive(RoomPid)).

% Can read the room topic
read_topic_test() ->
    {ok, RoomPid} = init(["Room", "Topic"]),
    {ok, Topic} = read_topic(RoomPid),
    ?assertEqual(Topic, "Topic").

% Can change the room topic
change_topic_test() ->
    {ok, RoomPid} = init(["Room", "Topic"]),
    change_topic(RoomPid, "Topic1"),
    {ok, Topic} = read_topic(RoomPid),
    ?assertEqual(Topic, "Topic1").

% Terminating test
terminate_test() ->
    {ok, RoomPid} = init(["Room", "Topic"]),
    chat_room:terminate(RoomPid),
    ?assertNot(is_process_alive(RoomPid)).

% Joining a room should return the room topic and roster
join_returns_room_topic_test() ->
    {ok, RoomPid} = init(["Room", "Topic"]),
    chat_room:join(RoomPid, self()),
    {ok, Roster} = roster(RoomPid),
    ?assertEqual(Roster, [self()]).

% Leaving a room removes a user from the roster
leave_removes_user_from_roster_test() ->
    {ok, RoomPid} = init(["Room", "Topic"]),
    join(RoomPid, self()),
    leave(RoomPid, self()),
    {ok, Roster} = roster(RoomPid),
    ?assertEqual(Roster, []).

% Emitting a message sends the message to a specified user in the room
emit_test() ->
    {ok, RoomPid} = init(["Room", "Topic"]),
    join(RoomPid, self()),
    emit(RoomPid, self(), self(), "Hi"),
    Msg = receive
              M -> M
          end,
    ?assertEqual(Msg, [{room, RoomPid}, {sender, self()}, {msg, "Hi"}]).

% Sending a message from a user causes the message to be emitted
send_triggers_emit_test() ->
    {ok, RoomPid} = init(["Room", "Topic"]),
    join(RoomPid, self()),
    recv(RoomPid, self(), "Hi"),
    Msg = receive
              M -> M
          end,
    ?assertEqual(Msg, [{room, RoomPid}, {sender, self()}, {msg, "Hi"}]).

% Requesting the roster returns it
roster_test() ->
    {ok, RoomPid} = chat_room:init(["Room", "Topic"]),
    {ok, Roster} = roster(RoomPid),
    ?assertEqual(Roster, []).
