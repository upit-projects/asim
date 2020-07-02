%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu <madalingrigoreenescu@gmail.com>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(asim_lib_web_table_nav_renderer).
-author("madalin").

-include("../include/asim.hrl").

%% API
-export([to_html/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% to_html
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Render the specified row column to binary containing html code

%%=============================================
%% always convert key into binary
%%=============================================

%% Convert any key atom to binary
to_html(Key, Value, Row) when erlang:is_atom(Key) ->

    to_html(asim_lib_utils_type_conv:to_binary(Key), Value, Row);

%%=============================================
%% handle null value
%%=============================================

%% null
to_html(_Key, null, _Row) -> <<"<div class=\"null-value\">null</div>">>;

%%=============================================
%% handle binary value
%%=============================================

%% Binary
to_html(Key, Value, Row) when erlang:is_binary(Value) ->

    binary_value_to_html(Key, Value, Row);

%%=============================================
%% handle list
%%=============================================

%% Population list
to_html(<<"population">>, Value, Row) when erlang:is_list(Value) ->

	BinaryValue = asim_lib_utils_type_conv:to_binary(io_lib:format("~tp",[Value])),
	erlang:iolist_to_binary([<<"<div>">>,
		asim_lib_web_htmlentities:encode(asim_lib_utils_binaries:part_from_begining(BinaryValue, 128)), <<"...</div>">>]);

%% List
to_html(Key, Value, Row) when erlang:is_list(Value) ->

    case Value of
        [] -> <<"">>;
        [T|_] ->
            case erlang:is_integer(T) of
                true -> to_html(Key, erlang:list_to_binary(Value), Row);
                _ -> asim_lib_web_htmlentities:encode(io_lib:format("~tp",[Value]))
            end
    end;

%%=============================================
%% handle integer
%%=============================================

%% Integer
to_html(Key, Value, _Row) when erlang:is_integer(Value) ->

    case Key of
        <<"time_", Rest/binary>> -> to_html_utils_column_type_time_seconds(Value);
        _ ->
            asim_lib_web_htmlentities:encode(io_lib:format("~B",[Value]))
    end;

%%=============================================
%% handle anything else
%%=============================================

%% Anything else try to render some compatible way with any term
to_html(_Key, Value, _Row) ->

    asim_lib_web_htmlentities:encode(io_lib:format("~tp",[Value])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% binary_value_to_html
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

binary_value_to_html(<<"link_id">>, Value, _Row) ->

    Url = asim_lib_web_url:get(<<"link_id">>, <<"update">>, <<"default">>, [{<<"link_id">>, Value}]),
    erlang:iolist_to_binary([<<"<a href=\"">>, Url, <<"\">">>,
	    asim_lib_web_htmlentities:encode(asim_lib_utils_binaries:part_from_begining(Value, 16)), <<"...</a>">>]);

binary_value_to_html(<<"matrix">>, Value, Row) ->

	Width  = proplists:get_value(width, Row, undefined),
	Matrix = proplists:get_value(matrix, Row, undefined),
	case {erlang:is_list(Matrix), erlang:is_integer(Width)} of
		{true, true} ->
			erlang:iolist_to_binary([asim_ext_maps_create:convert_matrix_to_string(Matrix, Width, "</br>")]);
		_ -> <<>>
	end;

binary_value_to_html(<<"ip">>, Value, _Row) ->

    EncodedValue                  = asim_lib_web_htmlentities:encode(Value),
    erlang:iolist_to_binary([
                                EncodedValue,
                                <<"<div class=\"links-blank\">">>,
                                <<"<a href=\"http://whois.domaintools.com/">>, EncodedValue, <<"\" target=\"_blank\">domaintools.com</a>">>,
                                <<"<a href=\"https://who.is/tools/">>, EncodedValue, <<"\" target=\"_blank\">who.is</a>">>,
                                <<"</div>">>
                            ]);

binary_value_to_html(Key, Value, _Row) ->
    asim_lib_web_htmlentities:encode(Value).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% to_html_utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

to_html_utils_column_type_time_seconds(Value) ->

    BaseDate      = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    Seconds       = BaseDate + Value,
    {{Year, Month, Day}, {Hour, Minute, Second}}  = calendar:gregorian_seconds_to_datetime(Seconds),

    Mstr = element(Month,{"Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"}),
    asim_lib_web_htmlentities:encode(io_lib:format("~w ~s ~4w ~2w:~2..0w:~2..0w", [Day,Mstr,Year,Hour,Minute,Second])).




