-module(channel).
-export([initial_state/3, handle/2]).

-record(channel_st, {
clients,
channel
}).


initial_state(Channel, Nick, Pid) ->
  #channel_st{
  clients=[{Nick, Pid}],
  channel = Channel
  }.

%connect user to channel
handle(St, {connect, Nick, Pid}) ->
  case lists:keymember(Nick, 1, St#channel_st.clients) of
    false ->
      NewState = St#channel_st{clients = [ Pid | St#channel_st.clients ]},
      {reply, ok, NewState};
    true ->
      {{error, user_already_joined}, St}
  end;


handle(St, {leave, Nick, Pid}) ->
  case lists:keymember(Nick, 1, St#channel_st.clients) of
    true ->
      NewState  = St#channel_st{clients = lists:delete(Pid, St#channel_st.clients)},
      {reply, ok, NewState};
    false ->
      {{error, user_not_joined}, St}
  end;

% Send "Msg" to all channel users except urself
handle(St, {msg_from_client, Pid, Nick, Msg}) ->
    Receivers = lists:delete(Pid, St#channel_st.clients),
    send_to_cl(Receivers, St#channel_st.channel, Nick, Msg),
    {reply, ok, St}.

% Send "Msg" to all users in list.
send_to_cl([],_,_,_) ->
  {reply, ok};
send_to_cl([H|T], Channel, Nick, Msg) ->
  spawn(fun() -> genserver:request(H, {message_receive, Channel, Nick, Msg})end),
  send_to_cl(T, Channel, Nick, Msg).
