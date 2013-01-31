%%%-------------------------------------------------------------------
%%% File    : chat_room.erl
%%% Author  :  <jeremy@galois>
%%% Description : Chat server room
%%%
%%% Created : 30 Jan 2013 by  <jeremy@galois>
%%%-------------------------------------------------------------------
-module(chat_room).

-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
-export([
         start_link/2,
         read_topic/1,
         change_topic/2,
         roster/1,
         join/2,
         leave/2,
         emit/4,
         recv/3
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {name, topic, users = []}).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/2
%% Description: Starts the chat room with a room name and topic
%%--------------------------------------------------------------------
start_link(Name, Topic) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name, Topic], []).

change_topic(Room, Topic) ->
    gen_server:cast(Room, {change_topic, Topic}).

read_topic(Room) ->
    gen_server:call(Room, read_topic).

emit(Room, User, Sender, Msg) ->
    gen_server:cast(Room, {emit, User, Sender, Msg}).

recv(Room, User, Msg) ->
    gen_server:cast(Room, {recv, User, Msg}).

join(Room, User) ->
    gen_server:cast(Room, {join, User}).

leave(Room, User) ->
    gen_server:cast(Room, {leave, User}).

roster(Room) ->
    gen_server:call(Room, roster).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init([Name, Topic]) ->
    {ok, #state{name = Name, topic = Topic}}.

%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call(roster, _From, State) ->
    {reply, {ok, State#state.users}, State};
handle_call(read_topic, _From, State) ->
    {reply, {ok, State#state.topic}, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast({join, User}, State = #state{users = Users}) ->
    {noreply, State#state{users = [User|Users]}};

handle_cast({leave, User}, State = #state{users = Users}) ->
    Users1 = lists:delete(User, Users),
    {noreply, State#state{users = Users1}};

handle_cast({change_topic, Topic}, State) ->
    {noreply, State#state{topic = Topic}};

handle_cast({emit, User, Sender, Msg}, State) ->
    User ! [{room, State#state.name}, {sender, Sender}, {msg, Msg}],
    {noreply, State};

handle_cast({recv, Sender, Msg}, State) ->
    lists:map(fun(User) ->
                      User ! [{room, State#state.name},
                              {sender, Sender},
                              {msg, Msg}]
              end, State#state.users),
    {noreply, State};
handle_cast(Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------


%%--------------------------------------------------------------------
%%% Unit tests
%%--------------------------------------------------------------------

chat_room_test_() ->
    {inparallel,
     {setup, fun setup/0, fun cleanup/1,
      [
       fun test_read_topic/0,
       fun test_change_topic/0,
       fun test_join_adds_user_to_roster/0,
       fun test_leave_removes_user_from_roster/0,
       fun test_emit/0,
       fun test_recv_triggers_emit/0,
       fun test_roster/0
      ]
     }
    }.

setup() ->
    {ok, Pid} = start_link(room_name, "room topic"),
    Pid.

cleanup(Pid) ->
    exit(Pid, "unit test cleanup").

%% Can read the room topic
test_read_topic() ->
    {ok, Topic} = read_topic(room_name),
    ?assertMatch(Topic, "room topic").

%% Can change the room topic
test_change_topic() ->
    change_topic(room_name, "Topic1"),
    {ok, Topic} = read_topic(room_name),
    ?assertEqual(Topic, "Topic1").

%% Joining a room should return the room topic and roster
test_join_adds_user_to_roster() ->
    join(room_name, self()),
    {ok, Roster} = roster(room_name),
    Result = lists:any(fun(Elem) ->
                               Elem == self()
                       end, Roster),
    ?assert(Result).

%% Leaving a room removes a user from the roster
test_leave_removes_user_from_roster() ->
    join(room_name, self()),
    leave(room_name, self()),
    {ok, Roster} = roster(room_name),
    Result = lists:any(fun(Elem) ->
                               Elem == self()
                       end, Roster),
    ?assertNot(Result).

%% Emitting a message sends the message to a specified user in the room
test_emit() ->
    join(room_name, self()),
    emit(room_name, self(), self(), "Hi"),
    Msg = receive
              M -> M
          end,
    ?assertEqual(Msg, [{room, room_name}, {sender, self()}, {msg, "Hi"}]).

%% Sending a message from a user causes the message to be emitted
test_recv_triggers_emit() ->
    join(room_name, self()),
    recv(room_name, self(), "Hi there"),
    Msg = receive
              M -> M
          end,
    ?assertEqual(Msg,
                 [{room, room_name}, {sender, self()}, {msg, "Hi there"}]).

%% Requesting the roster returns it
test_roster() ->
    {ok, Roster} = roster(room_name).
