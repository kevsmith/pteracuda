-module(pteracuda_stress).

-export([run/0]).

run() ->
    {T1, T2, T3} = erlang:now(),
    random:seed(T1, T2, T3),
    F = fun(_, _) -> random:uniform(100) > 50 end,
    Data = lists:sort(F, lists:seq(1, 1000000)),
    io:format("Pid: ~p~n", [os:getpid()]),
    io:get_chars("Press any key when ready...", 1),
    stress(Data, 1000000).

stress(_Data, 0) ->
    ok;
stress(Data, Count) ->
    {ok, B} = pteracuda_nifs:new_buffer(),
    pteracuda_nifs:write_buffer(B, Data),
    pteracuda_nifs:sort_buffer(B),
    %{ok, SD} = pteracuda_nifs:read_buffer(B),
    pteracuda_nifs:destroy_buffer(B),
    io:format("~p~n", [1000000 - Count]),
    %% case length(SD) of
    %%     1000000 ->
    %%         io:format("~p...ok~n", [1000000 - Count]);
    %%     _ ->
    %%         io:format("~p...bad~n", [1000000 - Count])
    %% end,
    stress(Data, Count - 1).
