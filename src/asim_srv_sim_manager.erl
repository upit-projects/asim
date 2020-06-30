%%%-------------------------------------------------------------------
%%% @author madalin
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Aug 2019 1:50 PM
%%%-------------------------------------------------------------------
-module(asim_srv_sim_manager).
-author("madalin").

-behaviour(gen_server).

-include("../include/asim.hrl").

%% API
-export([start_link/0]).
-export([simulation_create/1]).
-export([simulation_delete/1]).
-export([simulation_status/0]).
-export([simulation_report/1]).
-export([simulation_clear_status/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Hold simulation manager server state
-record(asim_srv_sim_manager_state, {
	simulations = [], %% Holds a list of running simulations (processes id's)
	status = [] %% Holds cached information about simulations for the web service to display
}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Create simulation server call
simulation_create(Simulation) -> gen_server:call(?MODULE, {create, Simulation}, infinity).

%% @doc Delete simulation server call
simulation_delete(Result) -> gen_server:call(?MODULE, {delete, self(), Result}, infinity).

%% @doc Returns information about current running simulations
simulation_status() -> gen_server:call(?MODULE, status, infinity).

%% @doc Clear all ended simulations
simulation_clear_status() -> gen_server:call(?MODULE, clear_status, infinity).

%% @doc Report the simulation status to the simulation manager server
simulation_report(NewStatus) -> gen_server:cast(?MODULE, {report, self(), NewStatus}).

-spec start_link() -> {'ok', pid()} | 'ignore' | {'error', term()}.
start_link() ->

	gen_server:start_link({local, asim_srv_sim_manager}, ?MODULE, [], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INIT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_Args) ->

	%% Build initial state
	State = #asim_srv_sim_manager_state{
		simulations = [],
		status = []
	},

	%% Create a new pool
	pooler:start(),
	PoolConfig = [{name, asim_sim_pool},
		{group, asim_sim_pool},
		{max_count, 5000},
		{init_count, 100},
		{start_mfa,	{asim_srv_sim, start_link, []}}],
	pooler:new_pool(PoolConfig),

	{ok, State, infinity}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_call
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Handle create simulation call
handle_call({create, #asim_simulation_specs{
	how_many    = HowMany,
	sim_id = SimId,
	sim_template_id = SimTemplate
}},
	{_CallerPid, _},
	State = #asim_srv_sim_manager_state{}) ->

	%% Create al requested simulations one by one
	NewState = handle_call_create(HowMany, SimId, SimTemplate, State),

	%% Reply to caller
	{reply, ok, NewState};

%% Handle delete simulation call
handle_call({delete, SimPid, Result},
	{_CallerPid, _},
	State = #asim_srv_sim_manager_state{}) ->

	%% Remove simulation
	NewState = handle_call_delete(SimPid, State),

	%% Ok
	{reply, ok, NewState};

%% Handle status call (returning information about current running simulations)
handle_call(status,
	{_CallerPid, _},
	State = #asim_srv_sim_manager_state{
		status = Status
	}) ->

	%% Ok - reply as fast as possible with current status
	%% Status of the workers is not actually interrogated,
	%% we use the other way approach - workers reports their status
	%% when their simulation loop advance
	{reply, {ok, Status}, State};

%% Clear all finished simulation status to release memory
handle_call(clear_status,
	{_CallerPid, _},
	State = #asim_srv_sim_manager_state{
		status = Status
	}) ->

	%% Delete finished simulations
	NewStatus = handle_call_clear(Status),

	%% Ok
	{reply, ok, State#asim_srv_sim_manager_state{
		status = NewStatus
	}};

%% Handle any other call
handle_call(Msg, From, State) ->

	%% Log the unknown call
	asim_lib_utils_log:error(asim, srv_sim_manager_unknown_call, [{msg, Msg}, {from, From}, {state, State}]),

	{reply, {error, unknown_call}, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_cast
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Handle report cast (this is triggered by the workers reporting their status)
handle_cast({report, SimPid, NewStatus = #asim_simulation{time_ended = TimeEnded}},
	State = #asim_srv_sim_manager_state{
		simulations = Simulations,
		status = Status
	}) ->

	%% Replace old status with the new one
	UpdatedStatus = [{SimPid, NewStatus} | proplists:delete(SimPid, Status)],

	%% Check to see if ended
	case TimeEnded > 0 of
		true ->

			%% Release the worker back to pooler
			pooler:return_member(asim_sim_pool, SimPid, ok),

			NewSimulations = lists:delete(SimPid, Simulations),

			%% Ok - no reply - as fast as possible to allow us processing other requests
			{noreply, State#asim_srv_sim_manager_state{simulations = NewSimulations, status = UpdatedStatus}};

		_ ->

			%% Ok - no reply - as fast as possible to allow us processing other requests
			{noreply, State#asim_srv_sim_manager_state{status = UpdatedStatus}}

	end;

%% Handle any other cast
handle_cast(_Msg, State) -> {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Handle workers or caller exit
handle_info({'DOWN', MonitorRef, process, _, _},
	State = #asim_srv_sim_manager_state{simulations = Monitors}) ->

	{noreply, State};

handle_info(_Info, State) -> {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% terminate/code_change
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Creates a new simulation
handle_call_create(HowMany, SimId, SimTemplate, State = #asim_srv_sim_manager_state{
	simulations = Simulations
}) when HowMany > 0 ->

	%% Create/get a free worker from the workers pool
	SimWorker = pooler:take_member(asim_sim_pool),

	%% Give worker simulation parameters
	case gen_server:call(SimWorker, {start, SimId, SimTemplate}, infinity) of
		ok ->

			%% Add simulation worker to simulations list
			NewSimulations = [SimWorker | Simulations],

			%% Modify our state and keep the simulation worker
			NewState = State#asim_srv_sim_manager_state{simulations = NewSimulations},

			%% Create the next simulation
			handle_call_create(HowMany-1, SimId, SimTemplate, NewState);

		_ ->

			%% Ignore failure (we should implement some failure messages to
			%% handle this situation)
			handle_call_create(HowMany-1, SimId, SimTemplate, State)

	end;
handle_call_create(_HowMany, _SimId, _SimTemplate, State) -> State.


%% @doc Delete the specified simulation because the simulation ended
handle_call_delete(SimPid, State = #asim_srv_sim_manager_state{
	simulations = Simulations
}) ->

	NewSimulations = lists:delete(SimPid, Simulations),

	%% Modify our state
	State#asim_srv_sim_manager_state{simulations = NewSimulations}.

%% @doc Clear finished simulations information to release RAM
%% This can be triggered from the web service manually.
handle_call_clear([]) -> [];
handle_call_clear(Status) -> handle_call_clear(Status, []).
handle_call_clear([Status = #asim_simulation{time_ended = 0}|T], Acum) ->
	handle_call_clear(T, [Status | Acum]);
handle_call_clear([#asim_simulation{}|T], Acum) ->
	handle_call_clear(T, Acum);
handle_call_clear(_, Acum) -> Acum.