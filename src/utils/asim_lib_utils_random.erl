%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu <madalingrigoreenescu@gmail.com>
%%% @doc Contains helper functions for random numbers generation
%%% @end
%%%-------------------------------------------------------------------

-module(asim_lib_utils_random).
-author("menescu").

%% API
-export([integer_interval/2]).
-export([true_or_false/0]).

%% Return a random integer in the specified interval
integer_interval(Min, Max) when
  erlang:is_integer(Min),
  erlang:is_integer(Max),
  Min =< Max ->

  Min + erlang:trunc(rand:uniform() * ((Max - Min) + 1)).

%% Returns a random true or false boolean
true_or_false() -> (rand:uniform(16) > 8).