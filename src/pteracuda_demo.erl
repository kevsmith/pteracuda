-module(pteracuda_demo).

-compile([export_all,
          native]).

start(N) ->
    {T1, T2, T3} = erlang:now(),
    random:seed(T1, T2, T3),
    io:format("Generating test data: ~p~n", [N]),
    D = [random:uniform(N) || _ <- lists:seq(1, N)],
    io:format("Measuring performance "),
    {Time1, _} = timer:tc(lists, sort, [D]),
    io:format("."),
    {ok, C} = pteracuda_context:new(),
    {ok, B} = pteracuda_buffer:new(integer),
    pteracuda_buffer:write(B, D),
    {Time2, _} = timer:tc(pteracuda_demo, pteracuda_sort, [C, B, D]),
    io:format(".~n"),
    io:format("Erlang: ~pms, CUDA: ~pms~n", [Time1 / 1000, Time2 / 1000]).

pteracuda_sort(C, B, D) ->
    pteracuda_buffer:write(B, D),
    pteracuda_buffer:sort(C, B),
    pteracuda_buffer:read(B).
