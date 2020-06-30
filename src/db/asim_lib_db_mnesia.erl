%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu <madalingrigoreenescu@gmail.com>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(asim_lib_db_mnesia).
-author("madalin").

-include("../include/asim.hrl").

%% API
-export([init/0]).

%% @doc Creates all necessary MNESIA tables (init the MNESIA database, only if necessary and no database already exists)
init() ->

	application:stop(mnesia),

	%% Create schema (and delete the old ones if necessary)
	case asim_lib_utils_config:get_option(reset_database, false) of
		true -> mnesia:delete_schema([erlang:node()]);
		_ -> ignore
	end,
	mnesia:create_schema([erlang:node()]),

	%% Start mnesia database
	mnesia:start(),

	%% Create tables
	Result = [mnesia:create_table(TableName, TableSpecs) || {TableName, TableSpecs} <- ?ASIM_TABLES_MNESIA],
	asim_lib_utils_log:debug(asim, create_mnesia_table_result, Result),

	ok.



