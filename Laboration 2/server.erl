-module(server).
-export([start/1, handle/2]).


-record(server_st, {
server,
channels
}).

%main(State) ->
    %receive
      %  {request, From, Ref, Request} ->
      %      {Response, NextState} = handle(State, Request),
      %      From ! {result, Ref, Response},
      %      main(NextState)
  %  end.


initial_state(ServerAtom) ->
  #server_st{
  server = ServerAtom,
  channels=[]
  }.


% Connect user to channel
handle(St, {join, Channel, Pid}) ->
  ChannelExist = lists:member(Channel, St#server_st.channels),
  if
    ChannelExist == false ->
      NewChannel = St#server_st.channels ++ [Channel],
      NewState = St#server_st{channels=NewChannel},
      genserver:start(list_to_atom(Channel), channel:initial_state(Channel), fun channel:handle/2),
      genserver:request(list_to_atom(Channel), {connect, Pid}),
      {reply, ok, NewState};
    true ->
      genserver:request(list_to_atom(Channel), {connect, Pid}),
      {reply, ok, St}
    end.

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
  genserver:start(ServerAtom, initial_state(ServerAtom), fun handle/2). %returns "Pid"

% Stop the server process registered to the given name,
% together with any other associated processes
%stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    %not_implemented.
    %genserver:stop(ServerAtom). % returns "ok."
