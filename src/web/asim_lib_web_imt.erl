%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu <madalingrigoreenescu@gmail.com>
%%% @doc Internet media types (IMT) library.
%%%
%%% An Internet media type is a standard identifier used on the Internet to indicate the type of data that a file contains. Common uses include the following:
%%% - email clients use them to identify attachment files,
%%% - web browsers use them to determine how to display or output files that are not in HTML format,
%%% - search engines use them to classify data files on the web.
%%%
%%% The identifiers were originally defined in RFC 2046, and were called MIME types because they referred to the non-ASCII
%%% parts of email messages that were composed using the MIME (Multipurpose Internet Mail Extensions) specification.
%%% They are also sometimes referred to as Content-types.
%%% @end
%%%-------------------------------------------------------------------

-module(asim_lib_web_imt).

-include("../include/asim.hrl").

-export([init/0]).

-export([get_imt/2]).
-export([get_file_imt/2]).
-export([get_extensions/2]).

-export([load_files/0]).
-export([save_files/0]).

%% Path to IMT files
-define(FILE_IMT_EXTENSIONS, 	"/http/imt_extensions.erl").
-define(FILE_IMT_TYPES,      	"/http/imt_types.erl").

%% @spec init() -> ok
%% @doc This must be called when your application is started in order to create IMT ets tables and load the IMT tables data from files.
init() -> load_files().

%% @spec get_imt(Extensions,Default) -> string() | Default
%% @doc Return the IMT for the specified extension. Returns Default parameter if the specified extension does not exist in our tables.
get_imt(Extension,Default) ->
	Result = ets:lookup(?ASIM_TABLE_IMT_EXTENSIONS, iolist_to_binary(Extension)),
	case Result of
		[{Name, Value}] ->
			Value;
		_ ->
			Default
	end.

%% @spec get_file_imt(File,Default) -> string() | Default
%% @doc Return the IMT for the specified filename. Returns Default parameter if the IMT could not be determined.
get_file_imt(File, Default) ->

	FileExtension = filename:extension(erlang:iolist_to_binary(File)),
	case FileExtension of
		<<".", Extension/binary>> ->
			get_imt(Extension,Default);
		_ -> Default
	end.

%% @spec get_extensions(Imt,Default) -> list() | Default
%% @doc Return the extension for the specified IMT. Returns Default parameter if the specified IMT does not exist in our tables.
get_extensions(Imt,Default) ->
	Result = ets:lookup(?ASIM_TABLE_IMT_TYPES, iolist_to_binary(Imt)),
	case Result of
		[{Name, Value}] ->
			Value;
		_ ->
			Default
	end.

%% @spec load_files() -> ok
%% @doc Load IMT files.
load_files() ->

	%%asim_lib_utils_log:debug(asim, imt_extensions_path, [asim_lib_utils:local_path_application(["assets", "files"])]),
	{ok, [Extensions]} = asim_lib_utils_assets:file_consult(?FILE_IMT_EXTENSIONS),
	ets:match_delete(?ASIM_TABLE_IMT_EXTENSIONS, '_'),
	[ets:insert(?ASIM_TABLE_IMT_EXTENSIONS, Tuple) || Tuple <- Extensions],

	{ok, [Types]} = asim_lib_utils_assets:file_consult(?FILE_IMT_TYPES),
	ets:match_delete(?ASIM_TABLE_IMT_TYPES, '_'),
	[ets:insert(?ASIM_TABLE_IMT_TYPES, Tuple) || Tuple <- Types],

	ok.

%% @spec save_files() -> ok | {error, Reason}
%% @doc Save IMT to files. The files are created if it does not exist.
%% If it exists, the previous contents are overwritten. Returns ok, or {error, Reason}.
save_files() ->

    Extensions 		= ets:tab2list(?ASIM_TABLE_IMT_EXTENSIONS),
	ExtensionsData	= io_lib:fwrite("~p.\n",[Extensions]),
	Result = file:write_file(?FILE_IMT_EXTENSIONS,ExtensionsData),

	case Result of
		ok ->
		    Types = ets:tab2list(?ASIM_TABLE_IMT_TYPES),
			TypesData = io_lib:fwrite("~p.\n",[Types]),
			file:write_file(?FILE_IMT_TYPES,TypesData);
		Other ->
			Other
	end.

