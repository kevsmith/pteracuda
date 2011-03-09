-module(pteracuda_bench).

-export([run/0]).

run() ->
    {T1, T2, T3} = erlang:now(),
    random:seed(T1, T2, T3),
    F = fun(_, _) -> random:uniform(100) > 50 end,
    N = lists:sort(F, lists:seq(1, 50000)),
    Erlang = bench_pure_erlang(N, 10, []),
    {ok, B} = pteracuda_nifs:new_buffer(),
    Cuda = bench_cuda(B, N, 10, []),
    pteracuda_nifs:destroy_buffer(B),
    {lists:sum(Erlang) / length(Erlang), lists:sum(Cuda) / length(Cuda)}.

bench_pure_erlang(_N, 0, Accum) ->
    Accum1 = lists:delete(lists:max(Accum), Accum),
    lists:delete(lists:min(Accum1), Accum1);
bench_pure_erlang(N, Count, Accum) ->
    {Time, _} = timer:tc(lists, sort, [N]),
    bench_pure_erlang(N, Count - 1, [Time|Accum]).

bench_cuda(_Buf, _N, 0, Accum) ->
    Accum1 = lists:delete(lists:max(Accum), Accum),
    lists:delete(lists:min(Accum1), Accum1);
bench_cuda(Buf, N, Count, Accum) ->
    pteracuda_nifs:write_buffer(Buf, N),
    {Time, _} = timer:tc(pteracuda_nifs, sort_buffer, [Buf]),
    pteracuda_nifs:clear_buffer(Buf),
    bench_cuda(Buf, N, Count - 1, [Time|Accum]).
