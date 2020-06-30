%%%-------------------------------------------------------------------
%%% @author madalin
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Aug 2019 1:35 PM
%%%-------------------------------------------------------------------
-module(asim_sup).
-author("Madalin Grigore-Enescu").

-behaviour(supervisor).

-include("../include/asim.hrl").

%% supervisor exports
-export([start_link/0]).
-export([init/1]).
-export([upgrade/0]).

%% @doc Creates a supervisor process as part of a supervision tree.
start_link() ->

	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Returns supervisor flags and child specifications.
init(Args) ->

	%% Add grouping child to the supervisor children specifications
	ChildSpecs   = [
		{asim_srv_sim_manager,
		{asim_srv_sim_manager, start_link, []},
		permanent,
		5000,
		worker,
		[asim_srv_sim_manager]}],

	%% Returns supervisor flags and child specifications
	{ok,{{one_for_one, ?ASIM_DEFAULT_SUP_RESTART_INTENSITY, ?ASIM_DEFAULT_SUP_RESTART_PERIOD}, ChildSpecs}}.

%% @spec upgrade() -> ok
%% @doc Handle the upgrade process.
upgrade() -> ok.
