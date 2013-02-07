%%%-------------------------------------------------------------------
%%% File    : number_game_bot.erl
%%% Author  :  <jeremy@galois>
%%% Description : Plays the number game in the chat channel
%%%
%%% Created :  7 Feb 2013 by  <jeremy@galois>
%%%-------------------------------------------------------------------
-module(number_game_bot).

-behaviour(gen_fsm).
-export([start_link/0]).

%% gen_fsm callbacks
-export([init/1, disconnected/2, connected/2,
         handle_info/3, terminate/3, code_change/4]).

-record(state, {number}).

-define(room, main).
-define(name, "number game bot").

start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    gen_fsm:send_event(?MODULE, attempt_connection),
    {ok, disconnected, #state{}}.

disconnected(attempt_connection, StateData) ->
    case whereis(?room) of
        undefined ->
            timer:sleep(1000),
            {next_state, disconnected, StateData};
        Pid ->
            chat_room:join(main, self()),
            Number = start_game(),
            {next_state, connected, StateData#state{number = Number}}
    end.

connected({guess, Sender, Guess},
             StateData = #state{number = Number}
            ) when Guess < Number ->
    chat_room:recv(main, ?name, "Sorry, " ++ Sender ++
                   " but " ++ integer_to_list(Guess) ++
                   " is too low."
                  ),
    {next_state, connected, StateData};
connected({guess, Sender, Guess},
             StateData = #state{number = Number}
            ) when Guess > Number ->
    chat_room:recv(main, ?name, "Sorry, " ++ Sender ++
                   " but " ++ integer_to_list(Guess) ++
                   " is too high."
                  ),
    {next_state, connected, StateData};
connected({guess, Sender, Guess},
             StateData = #state{number = Number}
            ) when Guess == Number ->
    chat_room:recv(main, ?name, "Congrats " ++ Sender ++
                   "! " ++ integer_to_list(Guess) ++
                   " was the right answer!"
                  ),
    NewNumber = start_game(),
    {next_state, connected, StateData#state{number = NewNumber}}.

handle_info([{room, _}, {sender, Sender}, {msg, Msg}],
            connected, StateData) ->
    case re:run(Msg, "^[0-9]*\$") of
        nomatch ->
            {next_state, connected, StateData};
        _ ->
            Guess = list_to_integer(binary_to_list(Msg)),
            gen_fsm:send_event(?MODULE, {guess, Sender, Guess}),
            {next_state, connected, StateData}
    end.

terminate(Reason, StateName, StatData) ->
    ok.

code_change(OldVsn, StateName, StateData, Extra) ->
    {ok, StateName, StateData}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

start_game() ->
    RandomNumber = random:uniform(100),
    chat_room:recv(main, ?name, "I'm thinking of a number from 1 to 100"),
    RandomNumber.
