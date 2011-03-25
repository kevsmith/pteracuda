-module(pteracuda_context).

-include("pteracuda_internals.hrl").

-export([new/0,
         new/1,
         destroy/1]).

new() ->
    {ok, Ctx} = pteracuda_nifs:new_context(),
    {ok, #pc_context{ref=Ctx}}.

new(Device) when is_integer(Device) ->
    {ok, Ctx} = pteracuda_nifs:new_context(Device),
    {ok, #pc_context{ref=Ctx}}.

destroy(#pc_context{ref=Ctx}) ->
    pteracuda_nifs:destroy_context(Ctx).
