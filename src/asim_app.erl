%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu <madalingrigoreenescu@gmail.com>
%%% @doc 
%%% Callbacks for the application.
%%% Contains application start and stop functions.
%%% @end
%%%-------------------------------------------------------------------

-module(asim_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% @spec start(Type, Args) -> ServerRet
%% @doc application start callback.
start(Type, Args) ->

	%% Log start arguments
	error_logger:info_msg("[~p]: Start type: ~p Start Args = ~p~n", [?MODULE, Type, Args]),

    %% Start required applications
    {ok, _} = application:ensure_all_started(cowboy),

	%% Init application libraries
	ok = asim_lib_db_ets:init(),
	ok = asim_lib_db_mnesia:init(),
	ok = asim_lib_web_imt:init(),

	%% Setup our routes handlers
	ok = start_https_listener([{'_', [
		{"/assets/[...]", asim_lib_web_router_assets, #{}},
		{"/[...]", asim_lib_web_router, #{}}]
	}]),

	%% Start application main supervisor
    case asim_sup:start_link() of
		{ok, Pid} ->
	    	{ok, Pid};
		Error -> Error
    end.


%% @spec stop(State) -> ServerRet
%% @doc application stop callback.
stop(_State) ->	ok.


start_https_listener([]) -> no_routes;
start_https_listener(Routes) ->

	%% Build transport options
	TransportOpts = #{

		num_acceptors => asim_lib_utils_config:get_option(https_acceptors, 20),
		socket_opts => [

			{ip, asim_lib_utils_inet:guard_binary_to_ip_address(asim_lib_utils_config:get_option(node_public_ip))},
			{port, asim_lib_utils_type_conv:to_integer(asim_lib_utils_config:get_option(http_port, 5012))}

		]

	},

	%% Compile routes
	Dispatch = cowboy_router:compile(Routes),

	%% Start HTTPS server
	case cowboy:start_clear(asimsrvhttp, TransportOpts, #{env => #{dispatch => Dispatch}}) of
		{ok, Result} ->

			asim_lib_utils_log:info(<<"http">>, <<"cowboy_start">>, [{cowboy_start_https_result, Result}, {transport_opts, TransportOpts}, {dispatch, Dispatch}]),
			ok;
		Error ->

			asim_lib_utils_log:error(<<"http">>, <<"cowboy_start_https_error">>, [{cowboy_start_https_result, Error}]),
			error

	end.