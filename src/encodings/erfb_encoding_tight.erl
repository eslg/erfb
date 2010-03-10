%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fbenavides@novamens.com>
%%% @copyright (C) 2010 Novamens S.A.
%%% @doc Tight RFB Encoding implementation
%%% @reference <a href="http://www.tigervnc.com/cgi-bin/rfbproto#tight-encoding">More Information</a>
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(erfb_encoding_tight).
-author('Fernando Benavides <fbenavides@novamens.com>').

-behaviour(erfb_encoding).

-export([init/0, read/5, write/4, terminate/2]).

-include("erfblog.hrl").
-include("erfb.hrl").

-record(state, {zstreams    :: [zlib:zstream()],
                state       :: undefined | reading | writing}).

%% ====================================================================
%% Server functions
%% ====================================================================
%% @hidden
-spec init() -> {ok, #state{}}.
init() ->
    %%NOTE: This encoding uses 4 zstreams
    ZS = [zlib:open(), zlib:open(), zlib:open(), zlib:open()],
    {ok, #state{zstreams    = ZS,
                state       = undefined}}.

%% @hidden
-spec read(#pixel_format{}, #box{}, binary(), port(), #state{}) -> {ok, Data :: iolist(), Read::binary(), Rest::binary(), #state{}}.
read(PF, Box, <<CompCtrl:1/unit:8, NextBytes/binary>>, Socket,
     State = #state{zstreams    = ZS,
                    state       = ZState}) ->
    ?DEBUG("Tight reader starting for ~p~n", [Box]),
    case ZState of
        writing ->
            lists:foreach(fun zlib:deflateEnd/1, ZS),
            lists:foreach(fun zlib:inflateInit/1, ZS);
        reading ->
            void;
        undefined ->
            lists:foreach(fun zlib:inflateInit/1, ZS)
    end,

    %%NOTE: The Tight encoding makes use of a new type TPIXEL (Tight pixel).
    %%      This is the same as a PIXEL for the agreed pixel format, except
    %%      where true-colour-flag is non-zero, bits-per-pixel is 32, depth
    %%      is 24 and all of the bits making up the red, green and blue
    %%      intensities are exactly 8 bits wide. In this case a TPIXEL
    %%      is only 3 bytes long, where the first byte is the red component,
    %%      the second byte is the green component, and the third byte is
    %%      the blue component of the pixel color value.
    PixelSize =
        case PF of
            #pixel_format{true_colour   = true,
                          bits_per_pixel= 32,
                          depth         = 24,
                          red_max       = RM,
                          green_max     = GM,
                          blue_max      = BM,
                          red_shift     = RS,
                          green_shift   = GS,
                          blue_shift    = BS}
              when (RM bsl RS =:= 16#ff000000),
                   (GM bsl GS =:= 16#ff000000),
                   (BM bsl BS =:= 16#ff000000) ->
                3;
            #pixel_format{bits_per_pixel = BPP} ->
                erlang:trunc(BPP / 8)
        end,
    
    <<ControlBits:4/binary-unit:1,
      ResetZS:4/binary-unit:1>> = CompCtrl,
    lists:zipwith(fun(Z, 1) ->
                          zlib:inflateReset(Z);
                     (_Z, 0) ->
                          ok
                  end, ZS, get_reset_zstreams(ResetZS)),

    {Data, RectBytes, Rest} =
        case ControlBits of
            <<8:4>> -> %%NOTE: Fill Compression: only pixel value follows, in TPIXEL format. This value applies to all pixels of the rectangle.
                {Pixel, MoreBytes} =
                    case bstr:len(NextBytes) of
                        L when L < PixelSize ->
                            {erfb_utils:complete(NextBytes, PixelSize,
                                                 Socket, true),
                             <<>>};
                        _ ->
                            {bstr:substr(NextBytes, 1, PixelSize),
                             bstr:substr(NextBytes, PixelSize + 1)}
                    end,
                {#tight_data{reset_zstreams  = ResetZS,
                             compression     = fill,
                             data            = Pixel},
                 Pixel, MoreBytes};

            <<9:4>> -> %%NOTE: JPEG Compression
                {Length, LengthBytes, MoreBytes} =
                    read_length(NextBytes, Socket),
                {D, R} =
                    case bstr:len(MoreBytes) of
                        L when L < Length ->
                            {erfb_utils:complete(MoreBytes, Length,
                                                 Socket, true),
                             <<>>};
                        _ ->
                            {bstr:substr(MoreBytes, 1, Length),
                             bstr:substr(MoreBytes, Length + 1)}
                    end,
                {#tight_data{reset_zstreams  = ResetZS,
                             compression     = jpeg,
                             data            = D},
                 <<LengthBytes/binary, D/binary>>, R};
            
            <<0:1, ReadFillerId:1, ZN:2>> -> %%NOTE: Basic Compression
                Z = lists:nth(ZS, ZN + 1),
                {Filter, FilterBytes, MoreBytes} =
                    case ReadFillerId of
                        0 ->
                            {copy, <<>>, NextBytes};
                        1 ->
                            <<FB:1/binary, M/binary>> =
                               erfb_utils:complete(NextBytes, 1, Socket, true),
                            case FB of
                                <<0>> ->
                                    {copy, FB, M};
                                <<1>> ->
                                    <<Size:1/unit:8, MM/binary>> =
                                        erfb_utils:complete(M, 1, Socket, true),
                                    {{palette, Size+1},
                                     <<FB/binary, Size:1/unit:8>>,
                                     MM};
                                <<2>> ->
                                    {gradient, FB, M}
                            end
                    end,

                Length =
                    case Filter of
                        {palette, PS} when PS =< 2 ->
                            (Box#box.width + 7) / 8 * Box#box.height;
                        {palette, _} ->
                            Box#box.width * Box#box.height;
                        _ ->
                            PixelSize * Box#box.width * Box#box.height
                    end,
                

                case Length of
                    Length when Length =< 12 ->
                        {D, R} =
                            case bstr:len(MoreBytes) of
                                L when L < Length ->
                                    {erfb_utils:complete(MoreBytes, Length,
                                                         Socket, true),
                                     <<>>};
                                _ ->
                                    {bstr:substr(MoreBytes, 1, Length),
                                     bstr:substr(MoreBytes, Length + 1)}
                            end,
                        {#tight_data{reset_zstreams  = ResetZS,
                                     compression     = basic,
                                     data            = D},
                         <<FilterBytes/binary, D/binary>>, R};
                    _ ->
                        {ZLength, ZLengthBytes, ZMoreBytes} =
                            read_length(MoreBytes, Socket),
                        {D, R} =
                            case bstr:len(ZMoreBytes) of
                                L when L < ZLength ->
                                    {erfb_utils:complete(ZMoreBytes, ZLength,
                                                         Socket, true),
                                     <<>>};
                                _ ->
                                    {bstr:substr(ZMoreBytes, 1, ZLength),
                                     bstr:substr(ZMoreBytes, ZLength + 1)}
                            end,
                        Decompressed = zlib:inflate(Z, D),
                        {#tight_data{reset_zstreams  = ResetZS,
                                     compression     = {basic, ZN + 1},
                                     data            = Decompressed},
                         <<FilterBytes/binary, ZLengthBytes/binary, D/binary>>,
                         R}
                end
        end,
    {ok, Data, <<CompCtrl:1/unit:8, RectBytes/binary>>, Rest,
     State#state{state = reading}};
read(PF, Box, Bytes, Socket, State) ->
    ?DEBUG("Tight reader starting for ~p~n", [Box]),
    read(PF, Box, erfb_utils:complete(Bytes, 1, Socket, true), Socket, State).

%% @hidden
%%TODO: implement!!!!
-spec write(#pixel_format{}, #box{}, iolist(), #state{}) -> {ok, binary(), #state{}} | {error, invalid_data, #state{}}.
write(_PF, _Box, Data,
      State = #state{zstreams   = ZS,
                     state      = ZState}) ->
    case ZState of
        writing ->
            lists:foreach(fun zlib:inflateEnd/1, ZS),
            lists:foreach(fun zlib:deflateInit/1, ZS);
        reading ->
            void;
        undefined ->
            lists:foreach(fun zlib:deflateInit/1, ZS)
    end,
    FinalData = bstr:bstr(zlib:deflate(lists:nth(ZS, 1), Data, sync)),
    {ok, erfb_utils:build_string(FinalData), State#state{state = writing}}.

%% @hidden
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, #state{zstreams = ZS}) ->
    lists:foreach(fun({_, Z}) -> zlib:close(Z) end, ZS).


%% ====================================================================
%% Internal functions
%% ====================================================================
-spec get_reset_zstreams(binary()) -> [0|1].
get_reset_zstreams(ResetZS) ->
    %%NOTE: Reversed to match nth in ZS
    lists:reverse([RZ || <<RZ:1>> <= ResetZS]).

-spec read_length(binary(), port()) -> {Length :: integer(), LengthBytes :: binary(), MoreBytes :: binary}.
read_length(NextBytes, Socket) ->
    case erfb_utils:complete(NextBytes, 1, Socket, true) of
        <<0:1, L1:7, M/binary>> ->
            {L1, <<0:1, L1:7>>, M};
        _ ->
            case erfb_utils:complete(NextBytes, 2, Socket, true) of
                <<1:1, L1:7, 0:1, L2:7, M/binary>> ->
                    {L2 * 128 + L1,
                     <<1:1, L1:7, 0:1, L2:7>>,
                     M};
                _ ->
                    <<1:1, L1:7, 1:1, L2:7, L3:1/unit:8, M/binary>> =
                        erfb_utils:complete(NextBytes, 3, Socket, true),
                    {L3 * 128 * 128 + L2 * 128 + L1,
                     <<1:1, L1:7, 1:1, L2:7, L3:1/unit:8>>,
                     M}
            end
    end.