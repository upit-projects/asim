%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu <madalingrigoreenescu@gmail.com>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------

-module(asim_lib_utils).
-author("madalin").

%% API
-export([local_path_application/0, local_path_application/1]).
-export([local_path_tmp/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% local_path_
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @spec server_local_path(Components) -> string()
%% @doc Return an application relative directory from components.
local_path_application(Components) ->
	filename:join([local_path_application() | Components]).

%% @spec local_path_server() -> string()
%% @doc Return the base directory for all our applications.
%%
%% The function assumes this module is in a standard OTP layout application in the ebin or src directory
%% and the application is found in the server directory containing all subdirectories.
local_path_application() ->

	{file, Here} = code:is_loaded(?MODULE),
	filename:dirname(filename:dirname(filename:dirname(filename:dirname(Here)))).

%% @spec local_path_tmp() -> string()
%% @doc Return the server temporary directory
local_path_tmp() -> filename:join([local_path_application() | ["data", "tmp"]]).