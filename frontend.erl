%% -*- mode: nitrogen -*-
-module(index).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

main() -> #template{file = "./site/templates/bare.html"}.

title() -> "Welcome to Nitrogen".

body() ->
    #container_12{
        body = [
            #image{
                image =
                    "https://global-uploads.webflow.com/5e157548d6f7910beea4e2d6/6304a2578abd315b18c8f6e9_twitter-logo.png"
            },
            #grid_12{alpha = true, prefix = 2, suffix = 2, omega = true, body = inner_body()}
        ]
    }.

inner_body() ->
    [
        #h1{id = banner, text = "Welcome to Tweeter clone!"},
        #panel{
            id = wrapper,
            body = [
                #button{id = register, text = "register", postback = register},
                #h1{text = "  "},
                #button{id = login, text = "login", postback = login}
            ]
        }
    ].

event(register) ->
    wf:replace(register, #panel{
        body = [
            #label{text = "User Name:"},
            #textbox{id = username, size = 20, placeholder = "Enter your user name here"},
            #label{text = "Password:"},
            #password{id = psw, size = 20},
            #button{id = confirm, text = "confrim register", postback = confirm},
            #label{id = flag, text = ""}
        ],
        actions = #effect{effect = highlight}
    });
event(confirm) ->
    Username = wf:q(username),
    Password = wf:q(psw),
    A = register1(Username, Password),
    wf:update(flag, A);
event(login) ->
    wf:replace(login, #panel{
        body = [
            #label{text = "User Name:"},
            #textbox{id = username1, size = 20, placeholder = "Enter your user name here"},
            #label{text = "Password:"},
            #password{id = psw1, size = 20},
            #button{id = log_in, text = "log me in", postback = log_in}
        ],
        actions = #effect{effect = highlight}
    });
event(log_in) ->
    Username = wf:q(username1),
    Password = wf:q(psw1),
    A1 = login_in(Username, Password),
    A = string:equal(A1, "true"),
    case A of
        true ->
            wf:state(usn, Username),
            wf:update(banner, Username ++ ", welcome to Tweeter clone!"),
            wf:replace(wrapper, #panel{
                id = new_wrapper,
                style = "margin: 50px;",
                body = [
                    #panel{
                        style = "margin: 20px;",
                        body = [
                            #label{text = "See what people are posting"},
                            #textarea{
                                id = all_tweets, text = "", style = "width: 20rem; height:8rem"
                            },
                            #button{
                                id = get_all_tweet, text = "refresh", postback = get_all_tweet
                            },
                            "<br>"
                        ]
                    },
                    #panel{
                        style = "margin: 20px;",
                        body = [
                            #label{text = "Found some tweets interesting & want to retweet?"},
                            #textarea{
                                id = target_tweet,
                                placeholder = "Copy&paste the tweet you want to retweet",
                                style = "width: 20rem; height:4rem"
                            },
                            #textbox{id = comment, placeholder = "Enter your comment here"},
                            #button{
                                id = retweet, text = "retweet", postback = retweet
                            },
                            "<br>"
                        ]
                    },
                    #panel{
                        style = "margin: 20px;",
                        body = [
                            #label{text = "New here? We recommend those people to you:"},
                            #textarea{id = people, text = "", style = "width: 20rem; height:4rem"},
                            #button{
                                id = get_people,
                                text = "see our recommendation",
                                postback = get_people
                            },
                            "<br>"
                        ]
                    },

                    #panel{
                        style = "margin: 20px;",
                        body = [
                            #label{
                                text = "See what your subscribers have posted & tweets that @you"
                            },
                            #textarea{
                                id = tweets,
                                text = "",
                                trap_tabs = true,
                                style = "width: 20rem; height:6rem"
                            },
                            #button{id = check_tweets, text = "refresh", postback = check_tweets},
                            "<br>"
                        ]
                    },
                    #panel{
                        style = "margin: 20px;",
                        body = [
                            #label{text = "Interested in some topics? Search #hashtags here: "},
                            #textbox{id = hashtag, placeholder = "Enter #topic here"},
                            #button{id = search_tag, text = "search", postback = search_tag},
                            #label{id = ht1, text = ""},
                            #textarea{
                                 style = "width: 20rem; height:6rem",
                                id = hashtagtweets,
                                text = "",
                                trap_tabs = true
                            },
                            "<br>"
                        ]
                    },
                    #panel{
                        style = "margin: 20px;",
                        body = [
                            #label{text = "write your tweet here and send to people: "},
                            #textarea{
                                id = write,
                                text = "",
                                trap_tabs = true,
                                style = "width: 20rem; height:4rem"
                            },
                            #button{id = send_tweet, text = "send tweet", postback = send_tweet},
                            "<br>"
                        ]
                    },
                    #panel{
                        style = "margin: 20px;",
                        body = [
                            #label{text = "Your tweets:"},
                            #textarea{
                                id = my_tweets,
                                text = "",
                                trap_tabs = true,
                                style = "width: 20rem; height:4rem"
                            },
                            #button{id = get_my_tweet, text = "refresh", postback = get_my_tweet},
                            "<br>"
                        ]
                    },
                    #panel{
                        style = "margin: 20px;",
                        body = [
                            #label{text = "Want to subscribe? Enter his/her ID here:"},
                            #textbox{id = follow, text = "", placeholder = "Do not include '@'"},
                            #button{id = subscribe, text = "subscribe", postback = subscribe},
                            #label{id = after_subscribe, text = ""},
                            "<br>"
                        ]
                    },
                    #panel{
                        style = "margin: 20px;",
                        body = [
                            #label{text = "People who followed you:"},
                            #textarea{
                                id = follower,
                                text = "",
                                trap_tabs = true,
                                style = "width: 20rem; height:3rem"
                            },
                            #button{
                                id = get_follower,
                                text = "see my followers",
                                postback = get_follower
                            },
                            "<br>"
                        ]
                    },
                    #panel{
                        style = "margin: 20px;",
                        body = [
                            #label{text = "Your subscribers:"},
                            #textarea{
                                id = subscribers,
                                text = "",
                                trap_tabs = true,
                                style = "width: 20rem; height:3rem"
                            },
                            #button{
                                id = get_subscriber,
                                text = "see my subscribers",
                                postback = get_subscriber
                            },
                            "<br>"
                        ]
                    },
                    #panel{
                        style = "margin: 20px;",
                        body = [
                            #button{id = logout, text = "logout", postback = logout}
                        ]
                    }
                ],
                actions = #effect{effect = highlight}
            });
        false ->
            wf:replace(wrapper, #label{
                text = "login failed, please check you username & password and try again!"
            })
    end;
event(get_all_tweet) ->
    AllTweets = get_all_tweets(),
    wf:update(all_tweets, AllTweets);
event(get_my_tweet) ->
    Username = wf:state(usn),
    Mytweets = get_all_user_tweets(Username),
    Result = lists:foldl(fun(X, Acc) -> X ++ "&#13;&#10;" ++ Acc end, [], Mytweets),
    wf:update(my_tweets, Result);
event(send_tweet) ->
    Tweet = wf:q(write),
    Username = wf:state(usn),
    Mytweet = send_tweet(Username, Username ++ ":" ++ Tweet),
    Result = lists:foldl(fun(X, Acc) -> X ++ "&#13;&#10;" ++ Acc end, [], Mytweet),
    wf:update(my_tweets, Result);
event(retweet) ->
    Username = wf:state(usn),
    TargetTweet = wf:q(target_tweet),
    Comment = wf:q(comment),
    Mytweet = send_tweet(Username, Username ++ ":" ++ Comment ++ "//" ++ TargetTweet),
    Result = lists:foldl(fun(X, Acc) -> X ++ "&#13;&#10;" ++ Acc end, [], Mytweet),
    wf:update(my_tweets, Result);
event(check_tweets) ->
    Username = wf:state(usn),
    SubscribedTweet = get_all_subscribee_tweets(Username),
    MentionedTweet = get_mention_tweet(Username),
    Mytweets = SubscribedTweet ++ MentionedTweet,
    Result = lists:foldl(fun(X, Acc) -> X ++ "&#13;&#10;" ++ Acc end, [], Mytweets),
    wf:update(tweets, Result);
event(search_tag) ->
    Hashtag = wf:q(hashtag),
    A = Hashtag--"#",
    HashtagTweets = get_tag_tweets(A),
    Result = lists:foldl(fun(X, Acc) -> X ++ "&#13;&#10;" ++ Acc end, [], HashtagTweets),
    wf:update(hashtagtweets, Result);
event(subscribe) ->
    Username = wf:state(usn),
    New_subscribe = wf:q(follow),
    ReplyString = subscribe(Username, New_subscribe),
    wf:update(after_subscribe, ReplyString);
event(get_subscriber) ->
    Username = wf:state(usn),
    Following = get_all_following(Username),
    Result = lists:foldl(fun(X, Acc) -> "@" ++ X ++ "&#13;&#10;" ++ Acc end, [], Following),
    wf:update(subscribers, Result);
event(get_follower) ->
    Username = wf:state(usn),
    Follower = get_all_followers(Username),
    Result = lists:foldl(fun(X, Acc) -> "@" ++ X ++ "&#13;&#10;" ++ Acc end, [], Follower),
    wf:update(follower, Result);
event(get_people) ->
    Username = wf:state(usn),
    Unfollowing = get_all_unfollowing(Username),
    Result = lists:foldl(
        fun(X, Acc) -> "@" ++ X ++ "&#13;&#10;" ++ Acc end, [], Unfollowing -- [Username]
    ),
    wf:update(people, Result);
event(logout) ->
    wf:state(usn, " "),
    wf:replace(new_wrapper, #panel{
        id = wrapper,
        body = [
            #button{id = register, text = "register", postback = register},
            #h1{text = "  "},
            #button{id = login, text = "login", postback = login}
        ]
    }).

get_all_following(Username) ->
    % to make it runnable on one machine
    SomeHostInNet = "localhost",
    {ok, Sock} = gen_tcp:connect(
        SomeHostInNet,
        3456,
        [binary, {packet, 4}, {active, false}]
    ),
    Msg = getJson_get_all_following_users(Username),
    ok = gen_tcp:send(Sock, Msg),
    gen_tcp:close(Sock),
    receive_from_server(SomeHostInNet).

getJson_get_all_following_users(Username) ->
    UsernameBin = list_to_binary(Username),
    Json = [{"Action", <<"get_all_following_users">>}, {"Username", UsernameBin}],
    iolist_to_binary(mochijson2:encode(Json)).

get_all_unfollowing(Username) ->
    % to make it runnable on one machine
    SomeHostInNet = "localhost",
    {ok, Sock} = gen_tcp:connect(
        SomeHostInNet,
        3456,
        [binary, {packet, 4}, {active, false}]
    ),
    Msg = getJson_get_all_not_following_users(Username),
    ok = gen_tcp:send(Sock, Msg),
    gen_tcp:close(Sock),
    receive_from_server(SomeHostInNet).

getJson_get_all_not_following_users(Username) ->
    UsernameBin = list_to_binary(Username),
    Json = [{"Action", <<"get_all_not_following_users">>}, {"Username", UsernameBin}],
    iolist_to_binary(mochijson2:encode(Json)).

get_all_subscribee_tweets(Username) ->
    % to make it runnable on one machine
    SomeHostInNet = "localhost",
    {ok, Sock} = gen_tcp:connect(
        SomeHostInNet,
        3456,
        [binary, {packet, 4}, {active, false}]
    ),
    Msg = getJson_getAllFollowingTweets(Username),
    ok = gen_tcp:send(Sock, Msg),
    gen_tcp:close(Sock),
    receive_from_server(SomeHostInNet).

getJson_getAllFollowingTweets(Username) ->
    UsernameBin = list_to_binary(Username),
    Json = [{"Action", <<"get_all_subscribe_tweets">>}, {"Username", UsernameBin}],
    iolist_to_binary(mochijson2:encode(Json)).

get_tag_tweets(Tag) ->
    % to make it runnable on one machine
    SomeHostInNet = "localhost",
    {ok, Sock} = gen_tcp:connect(
        SomeHostInNet,
        3456,
        [binary, {packet, 4}, {active, false}]
    ),
    Msg = getJson_get_tag_tweet(Tag),
    ok = gen_tcp:send(Sock, Msg),
    gen_tcp:close(Sock),
    receive_from_server(SomeHostInNet).

getJson_get_tag_tweet(Tag) ->
    TagBin = list_to_binary(Tag),

    Json = [{"Action", <<"get_tag_tweet">>}, {"Username", <<"get_tag_tweet">>}, {"Tag", TagBin}],
    iolist_to_binary(mochijson2:encode(Json)).

get_mention_tweet(Mention) ->
    % to make it runnable on one machine
    SomeHostInNet = "localhost",
    {ok, Sock} = gen_tcp:connect(
        SomeHostInNet,
        3456,
        [binary, {packet, 4}, {active, false}]
    ),
    Msg = getJson_get_mention_tweet(Mention),
    ok = gen_tcp:send(Sock, Msg),
    gen_tcp:close(Sock),
    receive_from_server(SomeHostInNet).

getJson_get_mention_tweet(Mention) ->
    MentionBin = list_to_binary(Mention),

    Json = [
        {"Action", <<"get_mention_tweet">>},
        {"Username", <<"get_mention_tweet">>},
        {"Mention", MentionBin}
    ],
    iolist_to_binary(mochijson2:encode(Json)).

register1(Username, Password) ->
    % to make it runnable on one machine
    SomeHostInNet = "localhost",
    {ok, Sock} = gen_tcp:connect(
        SomeHostInNet,
        3456,
        [binary, {packet, 4}, {active, false}]
    ),
    Msg = getJson_reg(Username, Password),
    ok = gen_tcp:send(Sock, Msg),
    gen_tcp:close(Sock),
    receive_from_server_string(SomeHostInNet).

getJson_reg(Username, Password) ->
    UsernameBin = list_to_binary(Username),
    PasswordBin = list_to_binary(Password),
    Json = [{"Action", <<"register">>}, {"Username", UsernameBin}, {"Password", PasswordBin}],
    iolist_to_binary(mochijson2:encode(Json)).

receive_from_server_string(SomeHostInNet) ->
    {ok, SockB} = gen_tcp:connect(SomeHostInNet, 3456, [binary, {packet, 4}, {active, false}]),
    {ok, B} = gen_tcp:recv(SockB, 0),
    io:format("From server ~s ~n", [B]),
    ok = gen_tcp:close(SockB),
    B.

send_tweet(Username, Tweet) ->
    % to make it runnable on one machine
    SomeHostInNet = "localhost",
    {ok, Sock} = gen_tcp:connect(
        SomeHostInNet,
        3456,
        [binary, {packet, 4}, {active, false}]
    ),
    Msg = getJson_send_tweet(Username, Tweet),
    ok = gen_tcp:send(Sock, Msg),
    gen_tcp:close(Sock),
    receive_from_server(SomeHostInNet).

getJson_send_tweet(Username, Tweet) ->
    UsernameBin = list_to_binary(Username),
    New_Tweet = list_to_binary(Tweet),
    Json = [{"Action", <<"send_tweet">>}, {"Username", UsernameBin}, {"Send_tweet", New_Tweet}],
    iolist_to_binary(mochijson2:encode(Json)).

receive_from_server(SomeHostInNet) ->
    {ok, SockB} = gen_tcp:connect(SomeHostInNet, 3456, [binary, {packet, 4}, {active, false}]),
    {ok, B} = gen_tcp:recv(SockB, 0),
    Dec = decode_api_list(B),
    ok = gen_tcp:close(SockB),
    Dec.

decode_api_list(Bin) ->
    mochijson2:decode(Bin).

login_in(Username, Password) ->
    % to make it runnable on one machine
    SomeHostInNet = "localhost",
    {ok, Sock} = gen_tcp:connect(
        SomeHostInNet,
        3456,
        [binary, {packet, 4}, {active, false}]
    ),
    Msg = getJson_login_in(Username, Password),
    ok = gen_tcp:send(Sock, Msg),
    gen_tcp:close(Sock),
    receive_from_server_string(SomeHostInNet).

getJson_login_in(Username, Password) ->
    UsernameBin = list_to_binary(Username),
    PasswordBin = list_to_binary(Password),
    Json = [{"Action", <<"login_in">>}, {"Username", UsernameBin}, {"Password", PasswordBin}],
    iolist_to_binary(mochijson2:encode(Json)).

subscribe(Username, New_subscribe) ->
    % to make it runnable on one machine
    SomeHostInNet = "localhost",
    {ok, Sock} = gen_tcp:connect(
        SomeHostInNet,
        3456,
        [binary, {packet, 4}, {active, false}]
    ),
    Msg = getJson_subscribe(Username, New_subscribe),
    ok = gen_tcp:send(Sock, Msg),
    gen_tcp:close(Sock),
    receive_from_server_string(SomeHostInNet).

getJson_subscribe(Username, New_subscribe) ->
    UsernameBin = list_to_binary(Username),
    New_subscribeBin = list_to_binary(New_subscribe),
    Json = [
        {"Action", <<"subscribe">>}, {"Username", UsernameBin}, {"New_subscribe", New_subscribeBin}
    ],
    iolist_to_binary(mochijson2:encode(Json)).

get_all_tweets() ->
    % to make it runnable on one machine
    SomeHostInNet = "localhost",
    {ok, Sock} = gen_tcp:connect(
        SomeHostInNet,
        3456,
        [binary, {packet, 4}, {active, false}]
    ),
    Msg = getJson_get_all_tweets(),
    ok = gen_tcp:send(Sock, Msg),
    gen_tcp:close(Sock),
    receive_from_server(SomeHostInNet).

getJson_get_all_tweets() ->
    Json = [{"Action", <<"get_all_tweets">>}, {"Username", <<"get_all_tweets">>}],
    iolist_to_binary(mochijson2:encode(Json)).

get_all_followers(Username) ->
    % to make it runnable on one machine
    SomeHostInNet = "localhost",
    {ok, Sock} = gen_tcp:connect(
        SomeHostInNet,
        3456,
        [binary, {packet, 4}, {active, false}]
    ),
    Msg = getJson_get_all_followers(Username),
    ok = gen_tcp:send(Sock, Msg),
    gen_tcp:close(Sock),
    receive_from_server(SomeHostInNet).

getJson_get_all_followers(Username) ->
    UsernameBin = list_to_binary(Username),
    Json = [
        {"Action", <<"get_all_followers">>},
        {"Username", UsernameBin}
    ],
    iolist_to_binary(mochijson2:encode(Json)).

get_all_user_tweets(Username) ->
    % to make it runnable on one machine
    SomeHostInNet = "localhost",
    {ok, Sock} = gen_tcp:connect(
        SomeHostInNet,
        3456,
        [binary, {packet, 4}, {active, false}]
    ),
    Msg = getJson_get_all_user_tweets(Username),
    ok = gen_tcp:send(Sock, Msg),
    gen_tcp:close(Sock),
    Map = receive_from_server(SomeHostInNet),
    Map.

getJson_get_all_user_tweets(Username) ->
    UsernameBin = list_to_binary(Username),
    Json = [{"Action", <<"get_all_user_tweets">>}, {"Username", UsernameBin}],
    iolist_to_binary(mochijson2:encode(Json)).