%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu <madalingrigoreenescu@gmail.com>
%% @doc Contains functions for loading, updating and saving various configuration options.
%%% @end
%%%-------------------------------------------------------------------

-module(asim_lib_utils_config).

-include("../include/asim.hrl").

-export([get_option/1]).
-export([get_option/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% get
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @spec get_option(Name :: binary()) -> term()
%% @doc Return a config option value. Raise an exception if the specified configuration option does not exist.
get_option(Name) ->

	Key = build_option_key(Name),
	case application:get_env(asim, Key) of
		{ok, Value} -> Value;
        _ -> throw({config_option_not_found, Name})
    end.

%% @spec get_option(Name :: binary(), Default :: term()) -> term() | Default
%% @doc Return a config option value. If the specified configuration option does not exist returns Default parameter
get_option(Name,Default) ->

	Key = build_option_key(Name),
	case application:get_env(asim, Key) of
		{ok, Value} -> Value;
		_ -> Default
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @spec build_option_key(Name) -> binary()
%% @doc Build option key (convert key to proper atom type)
build_option_key(Name) when is_atom(Name) -> Name;
build_option_key(Name) when is_list(Name) -> build_option_key(Name,0);
build_option_key(Name) when is_binary(Name) -> binary_to_atom(Name,latin1).

build_option_key([], Accumulator) when Accumulator =/= 0 -> build_option_key(Accumulator);
build_option_key([H|T],0) when is_atom(H) -> build_option_key(T,atom_to_binary(H,latin1));
build_option_key([H|T],0) -> build_option_key(T,H);
build_option_key([H|T],Accumulator) when is_atom(H) -> build_option_key(T,iolist_to_binary([Accumulator,<<"_">>,atom_to_binary(H,latin1)]));
build_option_key([H|T],Accumulator) -> build_option_key(T,iolist_to_binary([Accumulator,<<"_">>,H])).
