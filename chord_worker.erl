%%%-------------------------------------------------------------------
%%% @author lynn
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Oct 2022 4:06 PM
%%%-------------------------------------------------------------------
-module(chord_worker).
%-mode(compile).
-author("Ruilin Zhu, Zihan Wei").
-behaviour(gen_server).


%% API
-export([start_link/2]).
-export([init/1,handle_call/3, handle_cast/2,handle_info/2, code_change/3,terminate/2]).
-export([join/2,stabilize/1,notify/2,fix_fingers/3,setPre/2,setSuc/2,getHops/1,getSuccessorPre/1,find_successor/2,closest_preceding_node/4]).

start_link(Index,Power) ->
  gen_server:start_link({global, string:concat("chord", integer_to_list(Index))}, ?MODULE, [{Index,Power,-1, Index,[],0}], []).

init([{SelfIndex,Power,Predecessor,Successor,FingerTable,Count}]) ->
  %io:format("chord worker number ~w starts.... ~n", [SelfIndex]),
  {ok, {SelfIndex,Power,Predecessor,Successor,FingerTable,Count}}.

join(MyIndex,TargetIndex) ->
  %io:format("~p joining ~p ~n",[MyIndex,TargetIndex]),
  gen_server:call({global, string:concat("chord", integer_to_list(MyIndex))}, {join,TargetIndex}).


stabilize(SelfIndex) ->
  gen_server:call({global, string:concat("chord", integer_to_list(SelfIndex))}, {stabilize}).

notify(NewSuccessor,NewPredecessor) ->
  %io:format("~p is notified by ~p ~n",[NewSuccessor,NewPredecessor]),
  gen_server:call({global, string:concat("chord", integer_to_list(NewSuccessor))}, {notify,NewPredecessor}).

fix_fingers(MyIndex,FT,0)->
  %io:format("~p update FT: ~n",[MyIndex]),
  gen_server:call({global, string:concat("chord", integer_to_list(MyIndex))}, {fix_fingers,FT});
fix_fingers(MyIndex,FT,Power)->
  %io:format("Node: ~p fix_finger ~n",[MyIndex]),
  Target = MyIndex + round(math:pow(2,Power-1)),
  %TargetIndex = binary:decode_unsigned(crypto:hash(sha, Target)),
  %io:format("Target: ~p ~n",[Target]),
  Finger = find_successor(MyIndex,Target),
  %io:format("Finger: ~p~n",[Finger]),
  fix_fingers(MyIndex,[Finger]++FT,Power-1).

getHops(MyIndex)->
  gen_server:call({global, string:concat("chord", integer_to_list(MyIndex))}, {getHops}).

handle_call({find_successor,ID}, _From, {SelfIndex,Power,Predecessor,Successor,FingerTable,Count}) ->
  % Initial State, only 1 node in the chord
  %io:format("~p finding ~p~n",[SelfIndex,ID]),
  %io:format("Node :~p, Pre:~p, Succ: ~p ~n",[SelfIndex,Predecessor,Successor]),
  if
    % Chord Initializing state
    SelfIndex == Successor ->
      %io:format("Successor equal to self, Result: ~p~n",[SelfIndex]),
      Result = SelfIndex,
      {reply, Result, {SelfIndex,Power,Predecessor,Successor,FingerTable,Count+1}};
    % Normal operation
    SelfIndex < ID andalso ID < Successor ->
      %io:format("Target ID between~n"),
      Result = Successor,
      {reply, Result, {SelfIndex,Power,Predecessor,Successor,FingerTable,Count+1}};
    % Last node in the chord
    Successor < SelfIndex andalso SelfIndex < ID  ->
      %io:format("Last node ~n"),
      Result = Successor,
      {reply, Result, {SelfIndex,Power,Predecessor,Successor,FingerTable,Count+1}};
    ID < Successor andalso Successor < SelfIndex ->
      Result = Successor,
      {reply, Result, {SelfIndex,Power,Predecessor,Successor,FingerTable,Count+1}};
    true ->
      % Lookup the finger table
      %io:format("Finget Table ~n"),
      Len = length(FingerTable),
      if
        % if finger table doesn't exits, ask it successor to query
        Len == 0 ->
          %io:format("~p iterate to ~p~n",[SelfIndex,Successor]),
          Result = find_successor(Successor,ID),
          {reply, Result, {SelfIndex,Power,Predecessor,Successor,FingerTable,Count}};
        % else query the finger table
        true ->
          %io:format("~p use finger talbe ~n",[SelfIndex]),
          TargetNode = closest_preceding_node(FingerTable,SelfIndex,ID,Power),
          if
            TargetNode == SelfIndex ->
              Result = SelfIndex,
              {reply, Result, {SelfIndex,Power,Predecessor,Successor,FingerTable,Count}};
            true ->
              Result = find_successor(TargetNode,ID),
              {reply, Result, {SelfIndex,Power,Predecessor,Successor,FingerTable,Count}}
          end
      end
  end;

handle_call({join,TargetIndex},_From, {SelfIndex,Power,Predecessor,_Successor,FingerTable,Count}) ->
  %io:format("joining ~n"),
  SuccessorIndex = find_successor(TargetIndex,SelfIndex),
  %io:format("~p joined ~p ~n",[SelfIndex,SuccessorIndex]),
  setPre(SuccessorIndex,SelfIndex),
  %io:format("finishing join ~n"),

  {reply, ok,{SelfIndex,Power,Predecessor,SuccessorIndex,FingerTable,Count}};

handle_call({getSuccessorPre}, _From, {SelfIndex,Power,Predecessor,Successor,FingerTable,Count}) ->
  {reply, Predecessor, {SelfIndex,Power,Predecessor,Successor,FingerTable,Count}};

handle_call({fix_fingers,FT}, _From, {SelfIndex,Power,Predecessor,Successor,_FingerTable,Count}) ->
  {reply, ok, {SelfIndex,Power,Predecessor,Successor,FT,Count}};


handle_call({stabilize},_From, {SelfIndex,Power,Predecessor,Successor,FingerTable,Count}) ->
 % io:format("~p stabilizing ~n",[SelfIndex]),
  if
    Successor == SelfIndex ->
      {reply,ok, {SelfIndex,Power,Predecessor,Successor,FingerTable,Count}};
    true ->
      X = getSuccessorPre(Successor),
      if
        SelfIndex < X andalso X < Successor ->
          notify(X,SelfIndex),
          %io:format("Notified ~n"),
          {reply, ok, {SelfIndex,Power,Predecessor,X,FingerTable,Count}};
        true ->
          %io:format("Nothing to Notify ~n"),
          {reply, ok,{SelfIndex,Power,Predecessor,Successor,FingerTable,Count}}
      end
    end;

handle_call({notify,NewPredecessor}, _From,{SelfIndex,Power,Predecessor,Successor,FingerTable,Count}) ->
  if
    Predecessor == -1 ->
      {reply,ok, {SelfIndex,Power,NewPredecessor,Successor,FingerTable,Count}};
    Predecessor < NewPredecessor andalso NewPredecessor < SelfIndex ->
      {reply,ok, {SelfIndex,Power,NewPredecessor,Successor,FingerTable,Count}};
    true ->
      {reply,ok, {SelfIndex,Power,Predecessor,Successor,FingerTable,Count}}
  end;

handle_call({getHops},_From, {SelfIndex,Power,Predecessor,Successor,FingerTable,Count}) ->
  %io:format("~p has ~p hops ~n",[SelfIndex,Count]),
  {reply,Count,{SelfIndex,Power,Predecessor,Successor,FingerTable,Count}}.

handle_cast({setPre,PredecessorIndex}, {SelfIndex,Power,_Predecessor,Successor,FingerTable,Count}) ->
  %io:format("setting ~p's Pre to be ~p~n",[SelfIndex,PredecessorIndex]),
  {noreply, {SelfIndex,Power,PredecessorIndex,Successor,FingerTable,Count}};

handle_cast({setSuc,SuccessorIndex}, {SelfIndex,Power,Predecessor,_Successor,FingerTable,Count}) ->
  %io:format("setting ~p's Succ to be ~p",[SelfIndex,SuccessorIndex]),
  {noreply, {SelfIndex,Power,Predecessor,SuccessorIndex,FingerTable,Count}}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
find_successor(TargetIndex,ID) ->
  gen_server:call({global, string:concat("chord", integer_to_list(TargetIndex))}, {find_successor,ID}).

closest_preceding_node(_FingerTable,SelfIndex,_ID,0)->
  SelfIndex;
closest_preceding_node(FingerTable,SelfIndex,ID,Power)->
  Finger = lists:nth(Power, FingerTable),
  if
    SelfIndex < Finger andalso Finger < ID ->
      Finger;
    true ->
      closest_preceding_node(FingerTable,SelfIndex,ID,Power-1)
  end.

getSuccessorPre(SuccessorIndex)->
  gen_server:call({global, string:concat("chord", integer_to_list(SuccessorIndex))}, {getSuccessorPre}).

setPre(TargetIndex,PredIndex)->
  %io:format("Setting Predecesor~n"),
  gen_server:cast({global, string:concat("chord", integer_to_list(TargetIndex))}, {setPre,PredIndex}).

setSuc(TargetIndex,SuccIndex)->
 %io:format("Setting Successor~n"),
  gen_server:cast({global, string:concat("chord", integer_to_list(TargetIndex))}, {setSuc,SuccIndex}).