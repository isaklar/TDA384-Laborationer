-module(server).
-export([start/1, handle/2, stop/1]).


-record(server_st, {
channels
}).


initial_state() ->
  #server_st{
  channels=[]
  }.

% Connect user to channel
handle(St, {join, Channel, Pid}) ->
  ChannelExist = lists:member(Channel, St#server_st.channels),
  if
    ChannelExist == false ->
      NewChannel = St#server_st.channels ++ [Channel | St#server_st.channels],
      NewState = St#server_st{channels=NewChannel},
      genserver:start(list_to_atom(Channel), channel:initial_state(Channel), fun channel:handle/2),
      genserver:request(list_to_atom(Channel), {join, Pid}),
      {reply, ok, NewState};
    true ->
      genserver:request(list_to_atom(Channel), {join, Pid}),
      {reply,ok, St}
    end;

% disconnect user to channel
handle(St, {leave, Channel, Pid}) ->
  ChannelExist = lists:member(Channel, St#server_st.channels),
  if
    ChannelExist == true ->
      genserver:request(list_to_atom(Channel), {leave, Pid}),
      {reply, ok, St};
    true ->
      {reply, {error, user_not_joined, "Sorry you are not connected to channel"}, St}
    end.



% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
  genserver:start(ServerAtom, initial_state(), fun handle/2). %returns "Pid"

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    %not_implemented.
    genserver:stop(ServerAtom). % returns "ok."
