### Getting started

Start the shell

```terminal
erl
```

Start the processes

```erlang
make:all().
{ok, A1} = gen_server:start_link(acceptor, [], []).
{ok, A2} = gen_server:start_link(acceptor, [], []).
{ok, A3} = gen_server:start_link(acceptor, [], []).
{ok, P1} = gen_server:start_link(proposer, [A1,A2,A3], []).
```

Propose a value

```erlang
gen_server:call(P1, {propose, "v1"}).
```
