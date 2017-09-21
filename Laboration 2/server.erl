-module(server).
-export([start/1, handle/2]).


-record(server_st, {
channels
}).


initial_state() ->
  #server_st{
  channels=[]
  }.

% Connect user to channel
handle(St, {join, Channel, Nick, Pid}) ->
  ChannelExist = lists:member(Channel, St#server_st.channels),
  if
    ChannelExist == false ->
      NewChannel = St#server_st.channels ++ [Channel],
      NewState = St#server_st{channels=NewChannel},
      genserver:start(list_to_atom(Channel), channel:initial_state(Channel, Nick, Pid), fun channel:handle/2),
      {reply, ok, NewState};
    true ->
      genserver:request(list_to_atom(Channel), {connect, Nick, Pid}),
      {reply,ok, St}
    end;

% disconnect user to channel
handle(St, {leave, Channel, Nick, Pid}) ->
  ChannelExist = lists:member(Channel, St#server_st.channels),
  if
    ChannelExist == true ->
      NewState  = St#server_st{channels = lists:delete(Channel, St#server_st.channels)},
      genserver:start(list_to_atom(Channel), channel:initial_state(Channel, Nick, Pid), fun channel:handle/2),
      {reply, ok, NewState}
    end.

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
  genserver:start(ServerAtom, initial_state(), fun handle/2). %returns "Pid"

% Stop the server process registered to the given name,
% together with any other associated processes
%stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    %not_implemented.
    %genserver:stop(ServerAtom). % returns "ok."
