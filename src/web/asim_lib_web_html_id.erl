%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu <madalingrigoreenescu@gmail.com>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(asim_lib_web_html_id).
-author("madalin").

%% API
-export([]).

%% API
-export([new/0]).
-export([is_valid/1]).
-export([is_valid_or_new/1]).

new() ->

	Counter = asim_lib_utils_type_conv:to_binary(asim_lib_db_ets:get_node_counter()),
	<<$i, Counter/binary>>.

is_valid(<<$i, _/binary>>) -> true;
is_valid(_Id) -> false.

is_valid_or_new(Id) ->
	case is_valid(Id) of
		true -> Id;
		_ -> new()
	end.