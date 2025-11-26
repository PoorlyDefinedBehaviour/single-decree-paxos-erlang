-module(acceptor).

-behaviour(gen_server).

-export([init/1, handle_call/3]).

-record(acceptor,
        {maxBallot :: integer(), acceptedBallot :: integer(), acceptedValue :: string()}).

init(_) ->
  {ok,
   #acceptor{maxBallot = 0,
             acceptedBallot = nil,
             acceptedValue = nil}}.

handle_call({prepare, Ballot}, _From, State) ->
  io:format("acceptor got prepare ~p\n", [Ballot]),
  if Ballot =< State#acceptor.maxBallot ->
       {reply, {ballot_too_low, Ballot, State#acceptor.maxBallot}, State};
     true ->
       {reply,
        {prepare, ok, Ballot, State#acceptor.acceptedBallot, State#acceptor.acceptedValue},
        State#acceptor{maxBallot = Ballot}}
  end;
handle_call({accept, Ballot, Value}, _From, State) ->
  if Ballot < State#acceptor.maxBallot ->
       {reply, {ballot_too_low, Ballot}, State};
     true ->
       {reply,
        {accept, ok, Ballot},
        State#acceptor{maxBallot = Ballot,
                       acceptedBallot = Ballot,
                       acceptedValue = Value}}
  end.
