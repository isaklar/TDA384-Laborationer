-module(channel).
-export([initial_state/1, handle/2]).

-record(channel_st, {
clients,
channel
}).


initial_state(Channel) ->
  #channel_st{
  clients=[],
  channel = Channel
  }.

%connect user to channel
handle(St, {join, Pid}) ->
  case lists:member(Pid, St#channel_st.clients) of
    false ->
      NewState = St#channel_st{clients = [ Pid | St#channel_st.clients]},
      {reply, ok, NewState};
    true ->
      {reply, {error, user_already_joined}, St}
  end;

% disconnect user from channel
handle(St, {leave, Pid}) ->
  case lists:member(Pid, St#channel_st.clients) of
    true ->
      DeleteClient = lists:delete(Pid, St#channel_st.clients),
      NewState  = St#channel_st{clients = DeleteClient},
      {reply, ok, NewState};
    false ->
      {reply,{error, user_not_joined}, St}
  end;

% Send "Msg" to all channel users except urself
handle(St, {msg_from_client, Pid, Nick, Msg}) ->
    Receivers = lists:delete(Pid, St#channel_st.clients),
    send(Receivers, St#channel_st.channel, Nick, Msg),
    {reply, ok, St}.



% Send "Msg" to all users in list.
send([],_,_,_) ->
  {reply, ok};
send([H|T], Channel, Nick, Msg) ->
  spawn(fun() -> genserver:request(H, {message_receive, Channel, Nick, Msg})end),
  send(T, Channel, Nick, Msg).
