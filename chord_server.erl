%%%-------------------------------------------------------------------
%%  Author:  Ruilin Zhu and  Zinan Wei at Unversity of Florida
%%%-------------------------------------------------------------------

-module(chord_server).
-mode(compile).
-author("Ruilin Zhu, Zinan Wei").
-behaviour(gen_server).
%% API
-export([start_link/2]).
-export([init/1,handle_call/3, handle_cast/2, handle_info/2, code_change/3,terminate/2]).
-export([get_hash/1,generateChordNodes/2,generateList/3,start_chord/3,form_chord/4]).


start_link(NumNodes,NumRequests) ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, [{NumNodes,NumRequests,[],0}], []).

interval_milliseconds()->
  1000.

init([{NumNodes,NumRequests,List,Count}]) ->
  Power = trunc(math:log2(NumNodes))+1,
  ChordNodeList = generateChordNodes(NumNodes,Power),

  start_chord(NumNodes,Power,ChordNodeList),

  form_chord(Power,ChordNodeList,NumNodes,1),
  % send request every 1s
  timer:send_interval(interval_milliseconds(), interval),
  {ok, {NumNodes,NumRequests,List,Count}}.

%%  APIs
setList(NewList) ->
  gen_server:cast({global, ?MODULE}, {setList,NewList}).

% 1->2, 2->3, 3->4, 4->5, 5->1.....
form_chord(Power,ChordNodeList,1,ACC)->
  % close the ring
  First = lists:nth(1,ChordNodeList),
  Last = lists:nth(ACC,ChordNodeList),
  chord_worker:setPre(First,Last),
  chord_worker:setSuc(Last,First),

  chord_worker:stabilize(First),
  chord_worker:stabilize(Last),
  chord_worker:fix_fingers(First,[],Power),
  chord_worker:fix_fingers(Last,[],Power);

form_chord(Power,ChordNodeList,NumNodes,ACC)->
  Me = lists:nth(ACC,ChordNodeList),
  Target = lists:nth(ACC+1,ChordNodeList),
  chord_worker:join(Me,Target),

  chord_worker:stabilize(Me),
  chord_worker:stabilize(Target),
  chord_worker:fix_fingers(Me,[],Power),
  chord_worker:fix_fingers(Target,[],Power),

  form_chord(Power,ChordNodeList,NumNodes-1,ACC+1).


start_chord(0,_Power,_ChordNodeList) ->
  ok;
start_chord(N,Power,ChordNodeList) ->
  chord_worker:start_link(lists:nth(N,ChordNodeList),Power),
  start_chord(N-1,Power,ChordNodeList).

generateChordNodes(NumNodes,Power)->
  Maximum = math:pow(2,Power),
  Gap = round(Maximum/NumNodes),
  List = lists:sort(generateList([],Gap,NumNodes)),
  setList(List),
  List.

generateList(ResultList,_Gap,1) ->
  [get_hash(integer_to_list(1))]++ResultList;
generateList(ResultList,Gap,NumNodes) ->
  SHA1Value = get_hash(integer_to_list(NumNodes+Gap)),
  generateList([SHA1Value]++ResultList,Gap,NumNodes-1) .

get_hash(InputString) ->
  binary:decode_unsigned(crypto:hash(sha, InputString)).

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({setList,NewList}, {NumNodes,NumRequests,_List,Count}) ->
  {noreply, {NumNodes,NumRequests,NewList,Count}}.

handle_info(interval, {NumNodes,NumRequests,List,Count}) ->

  if
    Count == NumRequests->
      get_hops(NumNodes,0,NumRequests,List);
    true ->
      sendRequest(NumNodes,List),
      {noreply, {NumNodes,NumRequests,List,Count+1}}
  end.

sendRequest(0,_List)->
  ok;
sendRequest(NumNodes,List)->
  RandNum = get_hash(integer_to_list(rand:uniform(NumNodes))),
  chord_worker:find_successor(lists:nth(NumNodes,List),RandNum),
  sendRequest(NumNodes-1,List).

get_hops(0,Hops,NumRequests,_List)->

  AverageHop = round(Hops/NumRequests),
  io:format("The average number of hops is: ~p~n",[AverageHop]),
  erlang:halt();
get_hops(NumNodes,Hops,NumRequests,List)->

  Count = chord_worker:getHops(lists:nth(NumNodes,List)),
  get_hops(NumNodes-1,Hops+Count,NumRequests,List).


terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.