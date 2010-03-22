%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fbenavides@novamens.com>
%%% @copyright (C) 2010 Novamens S.A.
%%% @doc RFB Utility Functions
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(erfb_utils).
-author('Fernando Benavides <fbenavides@novamens.com>').

-export([get_full_string/2, complete/3, complete/4, build_string/1,
         pf_to_binary/1, timestamp/0, floor/1, ceiling/1,
         default_pixel_format/1]).

-include("erfblog.hrl").
%% @headerfile "erfb.hrl"
-include("erfb.hrl").

%% @spec get_full_string(binary(), port()) -> {Result :: binary(), Rest :: binary()}
%% @doc  Gets the complete string from its prefix and the socket.
%%       The first 32 bits in RFB prtocol strings tell the length of the string.  This function reads
%%       all the bytes it needs from the socket until the string is complete and returns any extra
%%       byte read if there were enough bytes already in the stream
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

%% @spec complete(binary(), integer(), port()) -> binary() | timeout
%% @doc  Completes Prefix until it gets FullLength bytes from the Socket.
%%       <em>Note:</em> This function *must* be always invoked with a pasive socket
-spec complete(binary(), integer(), port()) -> binary() | timeout.
complete(Prefix, FullLength, Socket) ->
    complete(Prefix, FullLength, Socket, false).

%% @spec complete(binary(), integer(), port(), boolean()) -> binary() | timeout
%% @doc  Completes Prefix until it gets FullLength bytes from the Socket.
%%       If it takes more than 5 seconds, and ThrowException is true, throws a
%%       timeout exception, otherwise returns <code>timeout</code>
%%       <em>Note:</em> This function *must* be always invoked with a pasive socket
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

%% @spec build_string(binary()) -> binary()
%% @doc  Converts a binary string into an RFB string: <code> &lt;&lt;Length:4/integer-unit:8, String/binary&gt;&gt; </code>
-spec build_string(binary()) -> binary().
build_string(String) ->
    Length = bstr:len(String),
    <<Length:4/integer-unit:8, String/binary>>.

%% @spec pf_to_binary(#pixel_format{}) -> binary()
%% @doc  Converts a <code>#pixel_format{}</code> record into its binary form
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

%% @spec floor(float()) -> integer()
%% @doc  Returns the floor (next smallest integer) of <code>X</code>
-spec floor(float()) -> integer().
floor(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T - 1;
        Pos when Pos > 0 -> T;
        _ -> T
    end.

%% @spec ceiling(float()) -> integer()
%% @doc  Returns the ceiling (next bigest integer) of <code>X</code>
-spec ceiling(float()) -> integer().
ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.

%% @spec timestamp() -> non_neg_integer()
%% @doc  Returns the current timestamp in POSIX usecs
-spec timestamp() -> non_neg_integer().
timestamp() ->
    {Mgs,S,Ms} = erlang:now(),
    Mgs * 1000000000000 + S * 1000000 + Ms.

%% @spec default_pixel_format(integer()) -> #pixel_format{}
%% @doc  Returns a #pixel_format that matches the received Bits Per Pixel
%%       These pixel formats are taken from those used by Chicken of the VNC
-spec default_pixel_format(integer()) -> #pixel_format{}.
default_pixel_format(32) ->
    #pixel_format{bits_per_pixel= 32,
                  depth         = 24,
                  big_endian    = false,
                  true_colour   = true,
                  red_max       = 255,
                  green_max     = 255,
                  blue_max      = 255,
                  red_shift     = 16,
                  green_shift   = 8,
                  blue_shift    = 0};
default_pixel_format(16) ->
    #pixel_format{bits_per_pixel= 16,
                  depth         = 16,
                  big_endian    = false,
                  true_colour   = true,
                  red_max       = 15,
                  green_max     = 15,
                  blue_max      = 15,
                  red_shift     = 4,
                  green_shift   = 0,
                  blue_shift    = 12};
default_pixel_format(8) ->
    #pixel_format{bits_per_pixel= 8,
                  depth         = 8,
                  big_endian    = false,
                  true_colour   = true,
                  red_max       = 3,
                  green_max     = 3,
                  blue_max      = 3,
                  red_shift     = 6,
                  green_shift   = 4,
                  blue_shift    = 2}.