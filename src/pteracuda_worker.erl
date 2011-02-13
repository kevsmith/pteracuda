-module(pteracuda_worker).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([new/0,
         destroy/1]).

new() ->
    pteracuda_nifs:new_worker().

destroy(Worker) ->
    pteracuda_nifs:destroy_worker(Worker).

-ifdef(TEST).
create_destroy_test() ->
    {ok, Worker} = pteracuda_worker:new(),
    pteracuda_worker:destroy(Worker).

-endif.
