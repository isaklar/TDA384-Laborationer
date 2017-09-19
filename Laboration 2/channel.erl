-module(channel).
-export([initial_state/1, handle/2]).

-record(channel_st, {
clients=[], channel
}).

initial_state(Channel) ->
  #channel_st{clients=[], channel = Channel}.

% connect user to channel
handle(St, {connect, Pid}) ->
  case lists:member(Pid, St#channel_st.clients) of
    false ->
      NewState = St#channel_st{clients = [ Pid | St#channel_st.clients ]},
      {ok, NewState};
    true ->
      {{error, user_already_joined, "User already joined the channel."}, St}
  end;

handle(St, {leave, Pid}) ->
  case lists:member(Pid, St#channel_st.clients) of
    true ->
      NewState  = St#channel_st{clients = lists:delete(Pid, St#channel_st.clients)},
      {reply, ok, NewState};
    false ->
      {{error, user_not_joined, "User haven't joined a channel."}, St}
  end.
