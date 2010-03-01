%% @author Fernando Benavides <fbenavides@novamens.com>
%% @copyright 2009 Novamens S.R.L.
%% @doc Utility functions for ERFB
-module(erfb_utils).
-author('Fernando Benavides <fbenavides@novamens.com>').

-export([get_full_string/2, complete/3, complete/4, build_string/1,
         pf_to_binary/1, timestamp/0, floor/1, ceiling/1]).

-include("erfblog.hrl").
-include("erfb.hrl").

-spec get_full_string(binary(), port()) -> {binary(), binary()}.
get_full_string(<<Length:4/unit:8, Rest/binary>> = String, S) ->
    ?DEBUG("Getting full string from: ~p~n", [String]),
    case bstr:len(Rest) of
        L when L >= Length ->
            {bstr:substr(Rest, 1, Length),
             bstr:substr(Rest, Length + 1)};
        L ->
            ?DEBUG("String ~p needs completion of ~p bytes~n", [Rest, Length - L]),
            {complete(Rest, Length, S), <<>>}
    end.

%% @doc This function *must* be always invoked with a pasive socket!
-spec complete(binary(), integer(), port()) -> binary() | timeout.
complete(Prefix, FullLength, Socket) ->
    complete(Prefix, FullLength, Socket, false).

-spec complete(binary(), integer(), port(), boolean()) -> binary() | timeout.
complete(Prefix, FullLength, Socket, TrhowException) ->
    case FullLength - bstr:len(Prefix) of
        L when L =< 0 ->
            Prefix;
        SuffixLength ->
            Result =
                case gen_tcp:recv(Socket, SuffixLength, ?COMPLETION_TIMEOUT) of
                    {ok, Bytes} ->
                        bstr:join([Prefix, Bytes]);
                    {error, timeout} ->
                        case TrhowException of
                            false ->
                                timeout;
                            true ->
                                throw(not_enough_data)
                        end
                end,
            Result
    end.

-spec build_string(binary()) -> binary().
build_string(String) ->
    Length = bstr:len(String),
    <<Length:4/integer-unit:8, String/binary>>.

-spec pf_to_binary(#pixel_format{}) -> binary().
pf_to_binary(#pixel_format{bits_per_pixel = PFBits,
                           depth          = PFDepth,
                           big_endian     = PFBigEndian,
                           true_colour    = PFTrueColour,
                           red_max        = PFRedMax,
                           green_max      = PFGreenMax,
                           blue_max       = PFBlueMax,
                           red_shift      = PFRedShift,
                           green_shift    = PFGreenShift,
                           blue_shift     = PFBlueShift}) ->
    PFBigEndianByte = case PFBigEndian of
                          true -> 1;
                          false -> 0
                      end,
    PFTrueColourByte = case PFTrueColour of
                           true -> 1;
                           false -> 0
                       end,
    <<PFBits:1/unit:8,
      PFDepth:1/unit:8,
      PFBigEndianByte:1/unit:8,
      PFTrueColourByte:1/unit:8,
      PFRedMax:2/unit:8,
      PFGreenMax:2/unit:8,
      PFBlueMax:2/unit:8,
      PFRedShift:1/unit:8,
      PFGreenShift:1/unit:8,
      PFBlueShift:1/unit:8,
      0:3/unit:8>>.

-spec floor(float()) -> integer().
floor(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T - 1;
        Pos when Pos > 0 -> T;
        _ -> T
    end.

-spec ceiling(float()) -> integer().
ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.

-spec timestamp() -> non_neg_integer().
timestamp() ->
    {Mgs,S,Ms} = erlang:now(),
    Mgs * 1000000000000 + S * 1000000 + Ms.
