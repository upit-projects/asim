%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu <madalingrigoreenescu@gmail.com>
%%% @doc Error logging module
%%% @end
%%%-------------------------------------------------------------------

-module(asim_lib_utils_log).
-author("madalin").

-export([throw/5]).

-export([debug/2, debug/3]).
-export([info/2, info/3]).
-export([warning/2, warning/3]).
-export([error/2, error/3]).
-export([panic/2, panic/3]).

%% Resolve name clash with erlang:error()
-compile({no_auto_import, [error/2, error/3, error/4, error/5]}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% throw
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

throw(CallerCategory, Exception, Module, File, Line) ->

	error(CallerCategory, <<"exception">>, [
		{exception, Exception},
		{exception_module, Module},
		{exception_file, File},
		{exception_line, Line}
	]),

	throw(Exception).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% debug/info/warning/error/panic
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%=============================================
%% debug
%%=============================================

%% @spec debug(Event,Data) -> ok
%% @doc Log a debug event
%% Use debug for logging a large amount of data only for debugging purposes.
debug(Category, Event) -> debug(Category, Event, []).
debug(Category, Event, Data) ->

	NewData = add_error_logger_params(Category, Event, Data),
	error_logger:info_report(NewData).

%%=============================================
%% info
%%=============================================

%% @spec info(Category, Event, Data) -> ok
%% @doc Log a info event
%% Use info to inform server administrators of various server states.
info(Category, Event) -> info(Category, Event, []).
info(Category, Event, Data) ->

	NewData = add_error_logger_params(Category, Event, Data),
	error_logger:info_report(NewData).

%%=============================================
%% warning
%%=============================================

%% @spec warning(Event,Data,Module,Line) -> ok
%% @doc Log a warning event
%% Use warning for common errors that may occure many times.
warning(Category, Event) -> warning(Category, Event, []).
warning(Category, Event, Data) ->

	NewData = add_error_logger_params(Category, Event, Data),
	error_logger:warning_report(NewData).

%%=============================================
%% error
%%=============================================

%% @spec error(Category, Event) -> ok
%% @doc Log an error event
%% Use error rarely only for errors that must be reported at all levels.
error(Category, Event) -> error(Category, Event, []).
error(Category, Event, Data) ->

	NewData = add_error_logger_params(Category, Event, Data),
	error_logger:error_report(NewData).

%%=============================================
%% panic
%%=============================================

%% @spec panic(Event,Data) -> ok
%% @doc Log the specified panic event and stops the execution of the calling process with the proper exit reason.
%% Use panic rarely only for errors that must be reported at all levels.
panic(Category, Event) -> panic(Category, Event, []).
panic(Category, Event, Data) ->

	NewData = add_error_logger_params(Category, Event, Data),
	error_logger:error_report(NewData),
	erlang:exit(panic).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% backtrace
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Get backtrace information using erlang stack trace
backtrace() -> try throw(xbacktrace) catch xbacktrace -> erlang:get_stacktrace() end.

%% @doc Returns backtrace string
backtrace_binary() -> erlang:iolist_to_binary(io_lib:format("~p", [backtrace()])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% params
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @spec add_error_logger_params(Event,Data) -> {Format,Data}
%% @doc Prepare standard error event data to send to the error logger.
add_error_logger_params(Category, Event, Data) ->

	Backtrace = backtrace_binary(),
	lists:append([Data, [
		{category, Category},
		{msg, Event},
		{trace, Backtrace}
	]
	]).
