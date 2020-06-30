%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu <madalingrigoreenescu@gmail.com>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(asim_lib_db_ets).
-author("madalin").

-include("../include/asim.hrl").

%% API
-export([init/0]).

-export([get_node_counter/0]).
-export([get_node_counter_uint32/0]).
-export([get_node_counter_uint64/0]).
-export([get_node_counter_len/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% init
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Creates all necessary ETS tables (init the ETS database)
init() ->

	[ets:new(TableName, TableSpecs) || {TableName, TableSpecs} <- ?ASIM_TABLES_ETS],
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% counter
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Update the specified counter
counter_update(Key) ->
	try
		ets:update_counter(?ASIM_TABLE_COUNTERS, Key, 1)
	catch
		_:_ ->
			ets:insert(?ASIM_TABLE_COUNTERS, {Key, 1}),
			1
	end.

%% @doc Reset the specified counter
counter_reset(Key) -> ets:insert(?ASIM_TABLE_COUNTERS, {Key, 0}).

%% @spec get_node_unique_counter() -> integer()
%% @doc Returns unique counter, guaranted to be unique only on the current node this function is called from.
get_node_counter() -> counter_update(uint).

%% @spec get_node_counter_max_len7() -> integer()
%% @doc Returns unique unsigned integer counter, guaranted always to increase or reset on the current node this function is called from.

%% Optimize some common length usage
get_node_counter_len(8) ->
	Value = counter_update(len8),
	case Value > 99999999 of
		true ->
			counter_reset(len8),
			get_node_counter_len(8);
		_ -> Value
	end;
get_node_counter_len(7) ->
	Value = counter_update(len7),
	case Value > 9999999 of
		true ->
			counter_reset(len7),
			get_node_counter_len(7);
		_ -> Value
	end;
get_node_counter_len(6) ->
	Value = counter_update(len6),
	case Value > 999999 of
		true ->
			counter_reset(len6),
			get_node_counter_len(6);
		_ -> Value
	end;
get_node_counter_len(5) ->
	Value = counter_update(len5),
	case Value > 99999 of
		true ->
			counter_reset(len5),
			get_node_counter_len(5);
		_ -> Value
	end;
get_node_counter_len(4) ->
	Value = counter_update(len4),
	case Value > 9999 of
		true ->
			counter_reset(len4),
			get_node_counter_len(4);
		_ -> Value
	end;

%% Handle any other arbitrary length (not recommended to use/slower - best define your own length handler if you need a specific length)
get_node_counter_len(Length) when erlang:is_integer(Length) ->

	BinLength = erlang:integer_to_binary(Length),
	BinKey    = <<"len", BinLength/binary>>,
	Value     = counter_update(BinKey),
	MaxValue  = erlang:list_to_integer(lists:flatten(lists:duplicate(Length,"9"))),
	case Value > MaxValue of
		true ->
			counter_reset(BinKey),
			get_node_counter_len(Length);
		_ -> Value
	end.

%% @spec get_node_uint32_counter() -> integer()
%% @doc Returns unique 32 bit unsigned integer counter, guaranted always to increase or reset on the current node this function is called from.
get_node_counter_uint32() ->

	Value = counter_update(uint32),
	case Value > 4294967295 of
		true ->
			counter_reset(uint32),
			get_node_counter_uint32();
		_ -> Value
	end.

%% @spec get_node_counter_uint64() -> integer()
%% @doc Returns unique 64 bit unsigned integer counter, guaranted always to increase or reset on the current node this function is called from.
get_node_counter_uint64() ->

	Value = counter_update(uint64),
	case Value > 18446744073709551615 of
		true ->
			counter_reset(uint64),
			get_node_counter_uint64();
		_ -> Value
	end.