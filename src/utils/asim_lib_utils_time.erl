%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu <madalingrigoreenescu@gmail.com>
%%% @doc Contains time related helper functions.
%%% @end
%%%-------------------------------------------------------------------

-module(asim_lib_utils_time).
-author("madalin").

%% Gregorian seconds corresponding to the Unix Epoch on January 1st, 1970 at UTC
%% This value must match the one returned by calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}})
-define(ASIM_TIME_UNIX_EPOCH_GREGORIAN_SECONDS, 62167219200).

%% API
-export([erlang_timestamp/0]).
-export([erlang_timestamp_format/1]).

-export([microseconds/0]).
-export([milliseconds/0]).
-export([seconds/0]).
-export([datetime/0]).
-export([full/0]).

-export([convert_seconds_to_datetime/1]).
-export([convert_milliseconds_to_datetime/1]).
-export([convert_microseconds_to_datetime/1]).
-export([convert_datetime_to_fulltime/1]).

-export([format_datetime/1]).

%% @doc Returns current OS system time in the same format as erlang:timestamp/0.
%%
%% The tuple can be used together with the function calendar:now_to_universal_time/1 or calendar:now_to_local_time/1 to get calendar time.
%% Using the calendar time together with the MicroSecs part of the return tuple from this function allows you to log timestamps
%% in high resolution and consistent with the time in the rest of the operating system.
%%
%% Time since Epoch. Epoch is defined to be 00:00:00 UTC, 1970-01-01. A day in POSIX time is defined to be exactly 86400 seconds long.
%% Strangely enough Epoch is defined to be a time in UTC, and UTC has another definition of how long a day is.
%% Quoting the Open Group "POSIX time is therefore not necessarily UTC, despite its appearance".
%% The effect of this is that when an UTC leap second is inserted, POSIX time either stops for a second, or repeats the last second.
%% If an UTC leap second would be deleted (which has not happened yet), POSIX time would make a one second leap forward.
%%
%% This may or may not be an accurate view of POSIX time. This time may typically be adjusted both backwards and forwards without limitation.
%% That is, time warps may be observed.
erlang_timestamp() -> os:timestamp().

%% @doc Returns erlang timestamp formatted according to our format
erlang_timestamp_format(TS = {_,_,Micro}) ->

    {{Year,Month,Day},{Hour,Minute,Second}} = calendar:now_to_universal_time(TS),
    Mstr = element(Month,{"Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"}),
    io_lib:format("~w ~s ~4w ~2w:~2..0w:~2..0w.~6..0w", [Day,Mstr,Year,Hour,Minute,Second,Micro]).

%% @doc Returns the milliseconds according to Universal Time Coordinated (UTC)
microseconds() ->
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega*1000000 + Sec)*1000000 + Micro.

%% @doc Returns the milliseconds according to Universal Time Coordinated (UTC)
milliseconds() ->
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega*1000000 + Sec)*1000 + round(Micro/1000).

%% @doc Returns the seconds according to Universal Time Coordinated (UTC)
seconds() ->
    {Mega, Sec, _Micro} = os:timestamp(),
    (Mega*1000000) + Sec.

%% @doc Returns the current date and time according to Universal Time Coordinated (UTC), also called GMT, in the form {{Year, Month, Day}, {Hour, Minute, Second}}
%% if supported by the underlying operating system
datetime() ->

    {Mega, Sec, _Micro} = os:timestamp(),
    Seconds             = Mega * 1000000 + Sec,

    GregorianSeconds    = Seconds + ?ASIM_TIME_UNIX_EPOCH_GREGORIAN_SECONDS,
    calendar:gregorian_seconds_to_datetime(GregorianSeconds).

%% @doc Returns full time in the form {Seconds, Milliseconds, Microseconds, {{Year, Month, Day}, {Hour, Minute, Second}}}
full() ->

    {Mega, Sec, Micro}  = os:timestamp(),
    Seconds             = Mega * 1000000 + Sec,

    Microseconds        = Seconds * 1000000 + Micro,
    Milliseconds        = Seconds * 1000 + round(Micro/1000),

    GregorianSeconds    = Seconds + ?ASIM_TIME_UNIX_EPOCH_GREGORIAN_SECONDS,
    DateTime            = calendar:gregorian_seconds_to_datetime(GregorianSeconds),

    {Seconds, Milliseconds, Microseconds, DateTime}.

%% @doc Convert seconds to datetime
convert_seconds_to_datetime(Seconds) ->

    GregorianSeconds    = Seconds + ?ASIM_TIME_UNIX_EPOCH_GREGORIAN_SECONDS,
    calendar:gregorian_seconds_to_datetime(GregorianSeconds).

%% @doc Convert milliseconds to datetime
convert_milliseconds_to_datetime(Milliseconds) ->

    GregorianSeconds    = round(Milliseconds/1000) + ?ASIM_TIME_UNIX_EPOCH_GREGORIAN_SECONDS,
    calendar:gregorian_seconds_to_datetime(GregorianSeconds).

%% @doc Convert microseconds to datetime
convert_microseconds_to_datetime(Microseconds) ->

    GregorianSeconds    = round(Microseconds/1000000) + ?ASIM_TIME_UNIX_EPOCH_GREGORIAN_SECONDS,
    calendar:gregorian_seconds_to_datetime(GregorianSeconds).

%% @doc Convert datetime to fulltime
convert_datetime_to_fulltime(DateTime) ->

    GregorianSeconds    = calendar:datetime_to_gregorian_seconds(DateTime),
    UnixSeconds         = GregorianSeconds - ?ASIM_TIME_UNIX_EPOCH_GREGORIAN_SECONDS,
    {UnixSeconds, UnixSeconds*1000, UnixSeconds*1000000, DateTime}.

%% @doc Returns erlang timestamp formatted according to our format
format_datetime({{Year, Month, Day}, {Hour, Minute, Second}}) ->

    Mstr = element(Month,{"Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"}),
    lists:flatten(io_lib:format("~w ~s ~4w ~2w:~2..0w:~2..0w", [Day,Mstr,Year,Hour,Minute,Second])).