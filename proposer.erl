-module(proposer).

-behaviour(gen_server).

-export([init/1, handle_call/3]).

-record(proposer, {acceptors :: [pid()], nextBallot :: integer()}).

init(Acceptors) ->
  {ok, #proposer{acceptors = Acceptors, nextBallot = 1}}.

handle_call({propose, Value}, _From, State) ->
  Ballot = State#proposer.nextBallot,
  NewState = State#proposer{nextBallot = Ballot + 1},
  Majority = majority(length(State#proposer.acceptors)),
  broadcast(prepare, State#proposer.acceptors, {prepare, Ballot}),
  {Status, Responses} = wait_for_quorum(prepare, Ballot, Majority),
  if Status =:= failure ->
       {reply, {error, Responses}, NewState};
     Status =:= success ->
       {_, AcceptedValue} =
         lists:foldl(fun({prepare, ok, _Ballot, AcceptedBallot, AcceptedValue},
                         {HighestBallot, HighestAcceptedValue}) ->
                        if AcceptedBallot > HighestBallot -> {AcceptedBallot, AcceptedValue};
                           true -> {HighestBallot, HighestAcceptedValue}
                        end
                     end,
                     {0, nil},
                     Responses),
       ValueToPropose =
         if AcceptedValue =:= nil ->
              Value;
            true ->
              AcceptedValue
         end,
       broadcast(accept, State#proposer.acceptors, {accept, Ballot, ValueToPropose}),
       {Status2, Responses2} = wait_for_quorum(accept, Ballot, Majority),
       if Status2 =:= success ->
            {reply, {ok, ValueToPropose}, NewState};
          Status2 =:= failure ->
            {reply, {error, Responses2}, NewState}
       end
  end.

broadcast(Phase, Acceptors, Req) ->
  ProposerPID = self(),
  lists:foreach(fun(AcceptorPID) ->
                   spawn(fun() ->
                            Res = gen_server:call(AcceptorPID, Req),
                            ProposerPID ! Res
                         end)
                end,
                Acceptors).

wait_for_quorum(Phase, Ballot, Majority) ->
  wait_for_quorum(Phase, Ballot, Majority, [], []).

wait_for_quorum(Phase, Ballot, Majority, Success, Failure) ->
  if length(Success) >= Majority ->
       {success, Success};
     length(Failure) >= Majority ->
       {failure, Failure};
     true ->
       receive
         {Phase, ballot_too_low, Ballot, _MaxBallot} = Msg ->
           wait_for_quorum(Phase, Ballot, Majority, Success, [Msg | Failure]);
         {Phase, ok, Ballot, _AcceptedBallot, _AcceptedValue} = Msg ->
           wait_for_quorum(Phase, Ballot, Majority, [Msg | Success], Failure);
         {Phase, ballot_too_low, Ballot} = Msg ->
           wait_for_quorum(Phase, Ballot, Majority, Success, [Msg | Failure]);
         {Phase, ok, Ballot} = Msg ->
           wait_for_quorum(Phase, Ballot, Majority, [Msg | Success], Failure);
         Other ->
           _ = logger:warning("wait_for_quorum: ignoring message ~p",
                              [#{phase => Phase, message => Other}]),
           wait_for_quorum(Phase, Ballot, Majority, Success, Failure)
       end
  end.

majority(N) when is_integer(N) ->
  N div 2 + 1.
