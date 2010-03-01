%% @author Juan Jose Comellas <jcomellas@novamens.com>
%% @author Mahesh Paolini-Subramanya <mahesh@aptela.com>
%% @copyright 2009 Novamens S.A.
%% @doc Module that generates and validates UUIDs. 
-module(uuid).
-author('Juan Jose Comellas <jcomellas@novamens.com>').
-author('Mahesh Paolini-Subramanya <mahesh@aptela.com>').

-import(bstr, [integer_to_hex_char/2]).

-export([new/0, as_string/0, as_bstr/0, to_string/1, to_bstr/1, is_valid/1]).

%%--------------------------------------------------------------------
%% @spec new() -> binary()
%% @doc  Generate a binary representing a v4 UUID (see RFC 4122).
%%--------------------------------------------------------------------
new() ->
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    new(random:uniform(1 bsl 48) - 1, random:uniform(1 bsl 12) - 1, 
        random:uniform(1 bsl 32) - 1, random:uniform(1 bsl 30) - 1).
new(R1, R2, R3, R4) ->
    <<R1:48, 4:4, R2:12, 2:2, R3:32, R4:30>>.


%%--------------------------------------------------------------------
%% @spec as_string() -> string()
%% @doc  Generate a v4 UUID as a string.
%%--------------------------------------------------------------------
as_string() ->
    to_string(new()).


%%--------------------------------------------------------------------
%% @spec as_bstr() -> bstr()
%% @doc  Generate a v4 UUID as a string encoded in a binary.
%%--------------------------------------------------------------------
as_bstr() ->
    to_bstr(new()).


%%--------------------------------------------------------------------
%% @spec to_string(Uuid :: binary()) -> string()
%% @doc  Convert a UUID to a string.
%%--------------------------------------------------------------------
to_string(<<TL:4/binary, TM:2/binary, THV:2/binary, CS:2/binary, N:6/binary>>) ->
    to_string([TL, TM, THV, CS, N], []).

to_string([Head | [_ | _] = Tail], Acc) ->
    to_string(Tail, [$- | hexencode(Head, Acc)]);
to_string([Head | Tail], Acc) ->
    to_string(Tail, hexencode(Head, Acc));
to_string([], Acc) ->
    lists:reverse(Acc).

hexencode(<<Hi:4, Lo:4, Tail/binary>>, Acc) ->
    hexencode(Tail, [integer_to_hex_char(Lo, lower), integer_to_hex_char(Hi, lower) | Acc]);
hexencode(<<>>, Acc) ->
    Acc.


%%--------------------------------------------------------------------
%% @spec to_bstr(Uuid :: binary()) -> bstr()
%% @doc  Convert a UUID to a string encoded in a binary.
%%--------------------------------------------------------------------
to_bstr(Uuid) when is_binary(Uuid) ->
    list_to_binary(to_string(Uuid)).


%%--------------------------------------------------------------------
%% @spec is_valid(Uuid) -> 
%%         Uuid = string() | binary()
%% @doc  Determine if a string is a valid UUID. A UUID is formed by a sequence
%%       of hexadecimal characters (usually in lower case) separated by dashes.
%%       e.g. 6e5902ba-c09f-11dd-b9ab-001d7dd022f8
%%--------------------------------------------------------------------
is_valid(Uuid) when is_list(Uuid) ->
    is_valid_list(Uuid, 0);
is_valid(Uuid) when is_binary(Uuid), size(Uuid) =:= 36 ->
    is_valid_binary(Uuid, 0);
is_valid(_Uuid) ->
    false.

%% Determine if a list of characters is a valid UUID.
is_valid_list([Char | Tail], Offset) when Offset < 36 ->
    case is_valid_char(Char, Offset) of
        true -> 
            is_valid_list(Tail, Offset + 1);
        _ ->
            false
    end;
is_valid_list([], 36) ->
    true;
is_valid_list(_Uuid, _Offset) ->
    false.

%% Determine if a binary is a valid UUID.
is_valid_binary(Uuid, Offset) ->
    case Uuid of
        %% UUIDs have the following format: 6e5902ba-c09f-11dd-b9ab-001d7dd022f8
        <<_Head:Offset/binary, Char, _Tail/binary>> ->
            case is_valid_char(Char, Offset) of
                true -> 
                    is_valid_binary(Uuid, Offset + 1);
                _ ->
                    false
            end;
        _ ->
            %% We checked that the length of the string was 36 in the calling 
            %% function, so if the offset goes out of range the string is a 
            %% valid UUID.
            true
    end.

%% Determine if a characer in a specific offset is valid for a UUID.
is_valid_char(Char, Offset) ->
    if 
        (Offset =:= 8) orelse (Offset =:= 13) orelse 
        (Offset =:= 18) orelse (Offset =:= 23) ->
            (Char =:= $-);
        true ->
            bstr:is_xdigit_char(Char)
    end.
