%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu <madalingrigoreenescu@gmail.com>
%%% @doc Implements various useful functions to deal with a HTTP multipart post.
%%% @end
%%%-------------------------------------------------------------------

-module(asim_lib_http_request_multipart).
-author("madalin").

-export([parse_post/1]).

%% @doc Return the request multipart bodies as a list of tuples.
parse_post(Req) -> parse_post(Req, []).
parse_post(Req, Acum) ->

	case cowboy_req:read_part(Req) of

		{ok, Headers, Req1} ->

			{LoopAcum, LoopReq} = case cow_multipart:form_data(Headers) of

				                      {data, DataFieldName} ->

					                      {ok, DataBody, Req2} = cowboy_req:read_part_body(Req1),
					                      DataAcum = lists:append(Acum, [{DataFieldName, DataBody}]),
					                      {DataAcum, Req2};

				                      {file, FileFieldName, Filename, CType} ->

					                      {TemporaryFilename, FileReq} = parse_file(Req1),
					                      FileAcum = lists:append(Acum, [{FileFieldName, [{file_name, Filename}, {mime_type, CType}, {file, TemporaryFilename}]}]),
					                      {FileAcum, FileReq};

				                      _ ->

					                      {Acum, Req1}

			                      end,

			parse_post(LoopAcum, LoopReq);

		{done, DoneReq} -> {Acum, DoneReq};

		_ ->

			throw({http_error_bad_request, <<"Error parsing post">>})

	end.

parse_file(Req) ->

	FileName = tmp_file_name(),
	case file:open(FileName, [write, binary]) of
		{ok, IoDevice} ->
			FinalReq = parse_file(Req, IoDevice),
			file:close(IoDevice),
			{FileName, FinalReq}
	end.

parse_file(Req, IoDevice) ->

	case cowboy_req:read_part_body(Req) of
		{ok, LastBodyChunk, ReqOk} ->

			case file:write(IoDevice, LastBodyChunk) of
				ok -> ReqOk
			end;

		{more, BodyChunk, ReqMore} ->

			case file:write(IoDevice, BodyChunk) of
				ok -> parse_file(ReqMore, IoDevice)
			end

	end.


tmp_file_name() ->

	{_, Name} = asim_lib_utils_timeuuid:now(),
	FullFileName = filename:append(asim_lib_utils_config:get_option(temporary_directory, <<"/tmp">>), <<Name/binary, ".xerltmp">>),
	case file:read_file_info(FullFileName) of

		%% File already exist, generate another one
		{ok, _FileInfo} -> tmp_file_name();

		%% File doesn't exist
		{error, _Reason} -> FullFileName

	end.
