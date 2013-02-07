%%%-------------------------------------------------------------------
%%% File    : chat_room_sup.erl
%%% Author  :  <jeremy@galois>
%%% Description : Chat room supervisor
%%%
%%% Created :  6 Feb 2013 by  <jeremy@galois>
%%%-------------------------------------------------------------------
-module(chat_room_sup).

-behaviour(supervisor).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
         start_link/0
        ]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([
         init/1
        ]).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Server functions
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%%--------------------------------------------------------------------
init([]) ->
    MainRoom = room_definition(main, "Main room"),
    {ok,{{one_for_one,10,60}, [MainRoom]}}.

%%====================================================================
%% Internal functions
%%====================================================================

room_definition(Name, Topic) ->
    {Name,{chat_room,start_link,[Name, Topic]},
     permanent,2000,worker,[chat_room]}.
