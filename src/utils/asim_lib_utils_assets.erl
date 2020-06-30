%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu <madalingrigoreenescu@gmail.com>
%%% @doc Handle all our server assets (resource files, html templates).
%%%
%%% Uses Sgte library you cand find in deps/sgte.
%%%
%%% - Call init() to load assets into proper memory ETS tables for fast access later.
%%% - Call reload() function to reload assets
%%% - Call template_get() to get the specified compiled template
%%% - Call template_render() to render the specified template
%%% @end
%%%-------------------------------------------------------------------

-module(asim_lib_utils_assets).

-include("../include/asim.hrl").

-export([reload/0]).

-export([file_get/1]).
-export([file_consult/1]).

-export([template_get/1]).
-export([template_exist/1]).
-export([template_render/2]).
-export([check_path_injection/1]).

-define(ASIM_ASSETS_PATH_FILES, asim_lib_utils:local_path_application(["assets", "files"])).
-define(ASIM_ASSETS_PATH_TEMPLATES, asim_lib_utils:local_path_application(["assets", "templates"])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% init/reload
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @spec reload() -> true | false
%% @doc Delete all ETS cached assets forcing reloading them from files.
reload() ->
	ets:delete_all_objects(?ASIM_TABLE_ASSETS_FILES),
	ets:delete_all_objects(?ASIM_TABLE_ASSETS_TEMPLATES).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% files
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @spec file_get(Path) -> binary() | false
%% @doc Load the specified asset file into memory ETS table (if necessary) and returns the asset file content.
file_get(Path) ->


    Record = ets:lookup(?ASIM_TABLE_ASSETS_FILES, Path),
	case Record of
		[{Path,{File}}] -> File;
		_ ->

            %% Verify StringPath for injection
            case check_path_injection(Path) of
	            {ok, StringPath} ->

			        FullPath 	= string:concat(?ASIM_ASSETS_PATH_FILES,StringPath),
			        Result   	= file:read_file(FullPath),

                    case Result of
                        {ok, Binary} ->

	                        case asim_lib_utils_config:get_option(cache_assets, false) of
		                        true -> ets:insert(?ASIM_TABLE_ASSETS_FILES, {Path,{Binary}});
		                        _ -> ignored
	                        end,
                            Binary;
                        _ ->
	                        asim_lib_utils_log:error(<<"assets">>, <<"Error reading asset file.">>, [FullPath]),
                            false
                    end;

                _ -> false

            end
	end.

%% @spec file_consult(Path) -> {ok, Terms} | {error, Reason}
file_consult(Path) ->

    %% Verify StringPath for injection
    case check_path_injection(Path) of
	    {ok, StringPath} ->

            FullPath 	= string:concat(?ASIM_ASSETS_PATH_FILES,StringPath),
            Result      = file:consult(FullPath),
            case Result of
                {ok, _} ->
                    Result;
                _ ->
	                asim_lib_utils_log:error(<<"assets">>, <<"Error reading asset file.">>, [FullPath]),
                    Result
            end;

        _ -> {error, insecure_path}

    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% templates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @spec template_get(Name) -> binary() | false
%% @doc Load the specified template asset file, compile it and save it into memory ETS table (if necessary) and returns the compiled template.
%% Templates are compiled and loaded only once in memory when they are first requested. If the template file is already loaded in ETS memory tables,
%% we simply return the template.
template_get(Name) ->

	Record = ets:lookup(?ASIM_TABLE_ASSETS_TEMPLATES, Name),
	case Record of

        [{Name,{Compiled}}] -> Compiled;
		_ ->

            %% Verify StringPath for injection
            case check_path_injection(Name) of
	            {ok, StringPath} ->

                    FullPath 	= string:concat(?ASIM_ASSETS_PATH_TEMPLATES,StringPath),
                    Result   	= file:read_file(FullPath),
                    case Result of

                        {ok, Binary} ->

                            {ok, CompiledTemplate}  = sgte:compile(Binary),
	                        case asim_lib_utils_config:get_option(cache_assets, false) of
								true -> ets:insert(?ASIM_TABLE_ASSETS_TEMPLATES, {Name,{CompiledTemplate}});
								_ -> ignored
	                        end,
                            CompiledTemplate;

                        _ ->

                            %% Insert into table so we don't attempt to load this template again
	                        case asim_lib_utils_config:get_option(cache_assets, false) of
		                        true -> ets:insert(?ASIM_TABLE_ASSETS_TEMPLATES, {Name,{false}});
		                        _ -> ignored
	                        end,

                            %% Log this situation
	                        %%asim_lib_utils_log:error(<<"assets">>, <<"Error reading asset template file.">>, [FullPath]),

                            %% Return false
                            false

                    end;

                _ -> false

            end

	end.

%% @spec template_exist(Name) -> true | false
%% @doc Load the specified template asset file, compile it and save it into memory ETS table (if necessary) and returns if the compiled template exist.
%% Templates are compiled and loaded only once in memory when they are first requested. If the template file is already loaded in ETS memory tables,
%% we simply check if template exist in ETS table.
template_exist(Name) ->

    Compiled = template_get(Name),
    case Compiled of
        false -> false;
        _ -> true
    end.

%% @spec template_render(Name, Data) -> binary() | false
%% @doc Render the specified "template" file replacing the specified variables with the proper values
%% Name is the path and name of the template file.
template_render(Name,Data) ->
	Compiled = template_get(Name),
	case Compiled of
		false -> false;
		_ -> erlang:iolist_to_binary(sgte:render_bin(Compiled, Data))
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Check the specified path for injection
%% (PURPOSE: Path should NOT contain /../; ../; /.. //)
%% Path should only contain /patt/path/; path/path/; path/path.jpg
%% where path is ALPHA-NUMERIC, -, _ and only ONE dot (.)
check_path_injection(Path) when erlang:is_binary(Path) -> check_path_injection(erlang:binary_to_list(Path));
check_path_injection([]) -> invalid;
check_path_injection(Path) when erlang:is_list(Path) ->
	case check_path_injection(Path, undefined) of
		ok -> {ok, Path};
		_ -> invalid
	end;
check_path_injection(_Path) -> invalid.

check_path_injection([$.|_], $.) -> invalid;
check_path_injection([$.|T], _Before) -> check_path_injection(T, $.);
check_path_injection([$/|_], $/) -> invalid;
check_path_injection([$/|T], _Before) -> check_path_injection(T, $/);
check_path_injection([H|T], _Before) when
H >= $a, H =< $z;
H >= $A, H =< $Z;
H >= $0, H =< $9;
H == $_;
H == $- -> check_path_injection(T, H);
check_path_injection([_|_], _Before) -> invalid;
check_path_injection([], _Before) -> ok.

