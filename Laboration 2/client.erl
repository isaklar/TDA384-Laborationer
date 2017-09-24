-module(client).
-export([initial_state/3, handle/2]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server, % atom of the chat server
    channels
}).

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(Nick, GUIAtom, ServerAtom) ->
    #client_st{
        gui = GUIAtom,
        nick = Nick,
        server = ServerAtom,
        channels =[]
    }.

% handle/2 handles each kind of request from GUI
% Parameters:
%   - the current state of the client (St)
%   - request data from GUI
% Must return a tuple {reply, Data, NewState}, where:
%   - Data is what is sent to GUI, either the atom `ok` or a tuple {error, Atom, "Error message"}
%   - NewState is the updated state of the client

% Join channel
handle(St, {join, Channel}) ->
  ChannelExist = lists:member(Channel, St#client_st.channels),
  if
    ChannelExist == false ->
      case catch(genserver:request(St#client_st.server, {join, Channel, self()})) of
        {'EXIT', Reason} ->
          {reply, {error, server_not_reached, Reason}, St};
        ok ->
          NewChannel = St#client_st.channels ++ [Channel],
    			NewState = St#client_st{channels=NewChannel},
    			{reply, ok, NewState}
      end;
    true ->
      {reply, {error, user_already_joined, "You are already in the channel"}, St}
    end;

%disconnect
handle(St, {leave, Channel}) ->
  ChannelExist = lists:member(Channel, St#client_st.channels),
  if
    ChannelExist == false ->
      {reply, {error, user_not_joined, "You haven't joined a channel"}, St};
    true ->
      case catch(genserver:request(St#client_st.server, {leave, Channel, self()})) of
        {'EXIT', Reason} ->
          {reply,{error, server_not_reached, Reason}, St};
        ok ->
          NewChannel = lists:delete(Channel, St#client_st.channels),
    			NewState = St#client_st{channels=NewChannel},
    			{reply, ok, NewState}
        end
    end;

% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
  ChannelExist = lists:member(Channel, St#client_st.channels),
  if
    ChannelExist == false ->
      {reply,{error, user_not_joined, "Sorry you are not connected to channel"}, St};
    true ->
      genserver:request(list_to_atom(Channel), {msg_from_client, self(), St#client_st.nick, Msg}),
      {reply, ok, St}
    end;

% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St};

% Change nick (no check, local only)
handle(St, {nick, NewNick}) ->
    {reply, ok, St#client_st{nick = NewNick}};

% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, St};

% Quit client via GUI
handle(St, quit) ->
    % Any cleanup should happen here, but this is optional
    {reply, ok, St};

% Catch-all for any unhandled requests
handle(St, _) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St}.
