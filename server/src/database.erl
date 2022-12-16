%%%-------------------------------------------------------------------
%%% @author lynn
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(database).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-export([register/2, check_user/1,get_followers/1, add_follower/2 ,get_followings/1, get_user_tweets/1
  , add_following/2, delete_follower/2, get_all_users/0, get_users_table/0]).

% tweets database
-export([update_tweet/2]).
% tags database
-export([update_tag/2, get_tags_from_tweet/1, get_tag_tweets/1]).

% mentions database
-export([get_mentions_from_tweet/1, get_mention_tweets/1, update_mention/2]).

% login in / out
-export([login_in/1, login_in/2,login_out/1, get_status/1,get_tweets_table_format/0, get_all_online_followers/1, get_followings_tweets/1]).

-export([set_user_frequency/3 ,get_user_frequency/1,get_password/1]).

-define(SERVER, ?MODULE).


-record(database_state, {}).

-record(user, {user_name::string(), password::string(), on_line::boolean(), followers::list(), following::list()}).
-record(tweet, {user_name::string(), content::list() }).
-record(tag, {tag::string(), tweet::list()}).
-record(mention, {mention::string(), tweet::list()}).
-record(frequency, {user_name::string(), followers:: integer(), frequency ::float()}).

start_link() ->
  gen_server:start_link({global, ?SERVER}, database, [],[]).

init([]) ->
  ets:new(users, [named_table, public, set, {keypos, #user.user_name}, {write_concurrency, true}, {read_concurrency, true}]),
  ets:new(tweets, [named_table, public, set, {keypos, #tweet.user_name}, {write_concurrency, true}, {read_concurrency, true}]),
  ets:new(tags, [named_table, public, set, {keypos, #tag.tag}, {write_concurrency, true}, {read_concurrency, true}]),
  ets:new(mentions, [named_table, public, set, {keypos, #mention.mention}, {write_concurrency, true}, {read_concurrency, true}]),
  ets:new(frequencies, [named_table, public, set, {keypos, #frequency.user_name}, {write_concurrency, true}, {read_concurrency, true}]),
  {ok, []}.




%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================




handle_call(_Request, _From, State = #database_state{}) ->
  {reply, ok, State}.

handle_cast(_Request, State = #database_state{}) ->
  {noreply, State}.

handle_info(_Info, State = #database_state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #database_state{}) ->
  ok.

code_change(_OldVsn, State = #database_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

register(User_name, Password) ->
  ets:insert_new(users, #user{user_name = User_name, password = Password, on_line = true, followers = [], following = []}),
  ets:insert_new(tweets, #tweet{user_name = User_name, content = []}),
  ets:insert_new(frequencies, #frequency{user_name = User_name, followers= 0, frequency = 0.0}).

check_user(User_name) ->
  ets:member(users, User_name).

get_all_users() ->
  Users = ets:match(users, {user,'$1' , '_', '_', '_', '_'}),
  List = lists:foldl(fun(X, List) -> [Iner] = X, List ++ [Iner]  end , [], Users),
  List.

get_followers(User_name) ->
  [[Followers]] = ets:match(users, {user, User_name, '_', '_', '$1', '_'}),
  Followers.

get_all_online_followers(User_name) ->
  All_Followers =  get_followers(User_name),
  Online_followers = lists:foldl(fun(X, Acc) ->
    Res = get_status(X),
    if Res == true ->
      Acc ++ [X];
      true->
        Acc
    end end,[], All_Followers),
  Online_followers.


get_status(User_name) ->
  [[Status]] = ets:match(users, {user, User_name, '_', '$1','_' , '_'}),
  Status.

get_followings(User_name) ->
  [[Followings]] = ets:match(users, {user, User_name, '_', '_', '_', '$1'}),
  Followings.


get_followings_tweets(User_name) ->
  All_following = database:get_followings(User_name),

  lists:foldl(fun(X, Map) ->
    User_tweets = database:get_user_tweets(X),
    maps:put(X, User_tweets, Map)
              end, #{}, All_following).



%%  add/delete only one follower each time
add_follower(User_name, New_follower) ->
  Followers = get_followers(User_name),
  New_followers = Followers ++ [New_follower],
  ets:update_element(users, User_name, [{#user.followers, New_followers}]).

add_following(User_name, New_following) ->
  Followings = get_followings(User_name),
  New_followings = Followings ++ [New_following],
  ets:update_element(users, User_name, [{#user.following, New_followings}]).


delete_follower(User_name, Delete_follower) ->
  Followers = get_followers(User_name),
  New_followers = Followers -- [Delete_follower],
  ets:update_element(users, User_name, [{#user.followers, New_followers}]).


login_in(User_name) ->
  ets:update_element(users, User_name, [{#user.on_line, true}]).

get_password(User_name) ->
  [[Password]] = ets:match(users, {user, User_name, '$1', '_','_' , '_'}),
  Password.

login_in(User_name, Password) ->
  True_Password = get_password(User_name),
  Result = True_Password =:= Password,

  Result.

login_out(User_name) ->
  ets:update_element(users, User_name, [{#user.on_line, false}]).




update_tweet(User_name, New_Tweet) ->
  Tweets = get_user_tweets(User_name),
  New_Tweets = Tweets ++ [New_Tweet],
  ets:update_element(tweets, User_name, [{#tweet.content, New_Tweets}]),
  TagsList = get_tags_from_tweet(New_Tweet),
  lists:foreach(fun(Tag) -> update_tag(Tag, New_Tweet)  end, TagsList),
  MentionsList = get_mentions_from_tweet(New_Tweet),
  lists:foreach(fun(Mention) -> update_mention(Mention, New_Tweet) end, MentionsList).

%% get the tweets from tweets database
get_user_tweets(User_name) ->
  [[Tweets]] = ets:match(tweets, {tweet, User_name, '$1'}),
  Tweets.

%% ————————————————————————————
%% Add tweets under each tag
%% ————————————————————————————

%% get the tweets with Tag from tags database
get_tag_tweets(Tag) ->
  A = ets:match(tags, {tag, Tag, '$1'}),
  if A =:= [] ->
    ets:insert_new(tags, #tag{tag = Tag, tweet = []}),
    A;
    true ->
      [[Tweets]] = A,
      Tweets
  end.


update_tag(Tag, New_Tweet) ->

  Tweets = get_tag_tweets(Tag),
  New_Tweets = Tweets ++ [New_Tweet],
  ets:update_element(tags, Tag, [{#tag.tweet, New_Tweets}]).

get_tags_from_tweet(Tweet) ->
  Words  = string:lexemes(Tweet, " "),
  TagsList = lists:foldl(fun(X, List) ->
    Pre = string:sub_string(X,1,1),
    if Pre =:= "#" ->
      List ++ [string:sub_string(X,2)];
      true ->
        List ++ ""
    end
                         end, [], Words).


%% ————————————————————————————
%% Add tweets under each mentions
%% ————————————————————————————

%% get the tweets from mentions database
get_mention_tweets(Mention) ->
  A = ets:match(mentions, {mention, Mention, '$1'}),
  if A =:= [] ->
    ets:insert_new(mentions, #mention{mention = Mention, tweet = []}),
    A;
    true ->
      [[Tweets]] = A,
      Tweets
  end.


update_mention(Mention, New_Tweet) ->

  Tweets = get_mention_tweets(Mention),
  New_Tweets = Tweets ++ [New_Tweet],
  ets:update_element(mentions, Mention, [{#mention.tweet, New_Tweets}]).

get_mentions_from_tweet(Tweet) ->
  Words  = string:lexemes(Tweet, " "),
  MentionsList = lists:foldl(fun(X, List) ->
    Pre = string:sub_string(X,1,1),
    if Pre =:= "@" ->
      List ++ [string:sub_string(X,2)];
      true ->
        List ++ ""
    end
                             end, [], Words).


get_users_table() ->
  ets:tab2list(users).

get_tweets_table_format()->
  Lists = ets:tab2list(tweets),
  lists:foldl(fun(X, Acc)  ->
    {tweet, User, List} = X,
    Acc ++ user_and_tweets(User, List)
              end, [], Lists).

user_and_tweets(User, List) ->
  lists:foldl(fun(X, Acc) ->
    Tweet =  X ++" &#13;&#10;" ,
    Acc ++ [Tweet]
              end, [], List).


%% ————————————————————————————
%% Freq
%% ————————————————————————————


set_user_frequency(User_name, Num_followers, Frequency) ->
  ets:update_element(frequencies, User_name, [{#frequency.followers, Num_followers}, {#frequency.frequency, Frequency}]).

get_user_frequency(User_name) ->
  ets:match(frequencies, {frequency, User_name, '_', '$1'}).

