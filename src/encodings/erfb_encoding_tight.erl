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
-spec read(#pixel_format{}, #box{}, binary(), port(), #state{}) -> {ok, Data :: #tight_data{}, Read::binary(), Rest::binary(), #state{}}.
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

    PixelSize = get_pixel_size(PF),
        
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
                                    PLength = (Size+1)*PixelSize,
                                    <<PaletteBytes:PLength/unit:8, MMM/binary>> =
                                        erfb_utils:complete(MM, 1, Socket, true),
                                    Palette =
                                        [Colour || <<Colour:PixelSize>> <= PaletteBytes],
                                    {{palette, Palette},
                                     <<FB/binary, Size:1/unit:8>>,
                                     MMM};
                                <<2>> ->
                                    {gradient, FB, M}
                            end
                    end,

                Length =
                    case Filter of
                        {palette, P} when erlang:length(P) =< 2 ->
                            erlang:trunc((Box#box.width + 7) / 8) * Box#box.height;
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
                                     filter          = Filter,
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
                                     filter          = Filter,
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
-spec write(#pixel_format{}, #box{}, #tight_data{}, #state{}) -> {ok, binary(), #state{}} | {error, invalid_data, #state{}}.
write(PF, _Box, Data = #tight_data{reset_zstreams = ResetZS},
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
    
    lists:zipwith(fun(Z, 1) ->
                          zlib:deflateReset(Z);
                     (_Z, 0) ->
                          ok
                  end, ZS, get_reset_zstreams(ResetZS)),

    try
        FinalData = internal_write(PF, Data, ZS),
        {ok, FinalData, State#state{state = writing}}
    catch
        _:invalid_data ->
            {error, invalid_data, State#state{state = writing}}
    end.

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

-spec write_length(integer() | binary()) -> binary().
write_length(Data) when is_binary(Data) ->
    write_length(bstr:len(Data));
write_length(Length) ->
    if
        Length =< 127 ->
            <<Length>>;
        true ->
            if
                Length =< 16383 ->
                    L1 = Length rem 128,
                    L2 = erlang:trunc((Length - L1) / 128),
                    <<1:1, L1:7, L2:8>>;
                true ->
                    L1 = Length rem 128,
                    L23= erlang:trunc((Length - L1) / 128),
                    L2 = L23 rem 128,
                    L3 = erlang:trunc((L23 - L2) / 128),
                    <<1:1, L1:7, 1:1, L2:7, L3:8>>
            end
    end.

%% @doc The Tight encoding makes use of a new type TPIXEL (Tight pixel).
%%      This is the same as a PIXEL for the agreed pixel format, except
%%      where true-colour-flag is non-zero, bits-per-pixel is 32, depth
%%      is 24 and all of the bits making up the red, green and blue
%%      intensities are exactly 8 bits wide. In this case a TPIXEL
%%      is only 3 bytes long, where the first byte is the red component,
%%      the second byte is the green component, and the third byte is
%%      the blue component of the pixel color value.
-spec get_pixel_size(#pixel_format{}) -> integer().
get_pixel_size(#pixel_format{true_colour= true, bits_per_pixel= 32, depth = 24,
                             red_max    = RM, green_max     = GM, blue_max  = BM,
                             red_shift  = RS, green_shift   = GS, blue_shift= BS})
  when (RM bsl RS =:= 16#ff000000), (GM bsl GS =:= 16#ff000000), (BM bsl BS =:= 16#ff000000) ->
    3;
get_pixel_size(#pixel_format{bits_per_pixel = BPP}) ->
    erlang:trunc(BPP / 8).

-spec internal_write(#pixel_format{}, #tight_data{}, [zlib:zstream()]) -> binary().
internal_write(_PF, #tight_data{reset_zstreams  = ResetZS,
                                compression     = fill,
                                data            = Pixel}, _ZS) ->
    <<8:4, ResetZS/binary, Pixel/binary>>;
internal_write(_PF, #tight_data{reset_zstreams  = ResetZS,
                                compression     = jpeg,
                                data            = Data}, _ZS) ->
    LengthBytes = write_length(Data),
    <<9:4, ResetZS/binary, LengthBytes/binary, Data/binary>>;
internal_write(_PF, #tight_data{reset_zstreams  = ResetZS,
                                compression     = basic,
                                filter          = copy,
                                data            = Uncompressed}, _ZS) ->
    %%NOTE: It says "use zstream 0", but it doesn't matter... data is not compressed anyway
    <<0:4, ResetZS/binary, Uncompressed/binary>>;
internal_write(PF, #tight_data{reset_zstreams  = ResetZS,
                               compression     = basic,
                               filter          = {palette, Palette},
                               data            = Uncompressed}, _ZS) ->
    PixelSize   = get_pixel_size(PF),
    Size        = erlang:length(Palette) - 1,
    <<4:4,              %% read-filter-id = 1
      ResetZS/binary,   
      1:1/unit:8,       %% filter-id = palette
      Size:1/unit:8,    %% palette size -1
      << <<Colour:PixelSize>> || Colour <- Palette >>,
      Uncompressed/binary>>;
internal_write(_PF, #tight_data{reset_zstreams  = ResetZS,
                                compression     = basic,
                                filter          = gradient,
                                data            = Uncompressed}, _ZS) ->
    <<4:4,              %% read-filter-id = 1
      ResetZS/binary,
      2:1/unit:8,       %% filter-id = gradient
      Uncompressed/binary>>;
internal_write(_PF, #tight_data{reset_zstreams  = ResetZS,
                                compression     = {basic, ZN},
                                filter          = copy,
                                data            = Uncompressed}, ZS) ->
    Compressed  = bstr:bstr(zlib:deflate(lists:nth(ZS, ZN), Uncompressed, sync)),
    LengthBytes = write_length(Compressed),
    <<0:2,              %% read-filter-id = 0
      (ZN-1):2,         %% use zstream ZN-1 
      ResetZS/binary,
      LengthBytes/binary,
      Compressed/binary>>;
internal_write(PF, #tight_data{reset_zstreams  = ResetZS,
                               compression     = {basic, ZN},
                               filter          = {palette, Palette},
                               data            = Uncompressed}, ZS) ->
    PixelSize   = get_pixel_size(PF),
    Compressed  = bstr:bstr(zlib:deflate(lists:nth(ZS, ZN), Uncompressed, sync)),
    LengthBytes = write_length(Compressed),
    Size        = erlang:length(Palette) - 1,
    <<1:2,              %% read-filter-id = 1
      (ZN-1):2,         %% use zstream ZN-1 
      ResetZS/binary,   
      1:1/unit:8,       %% filter-id = palette
      Size:1/unit:8,    %% palette size -1
      << <<Colour:PixelSize>> || Colour <- Palette >>,
      LengthBytes/binary,
      Compressed/binary>>;
internal_write(_PF, #tight_data{reset_zstreams  = ResetZS,
                                compression     = {basic, ZN},
                                filter          = gradient,
                                data            = Uncompressed}, ZS) ->
    Compressed = bstr:bstr(zlib:deflate(lists:nth(ZS, ZN), Uncompressed, sync)),
    LengthBytes = write_length(Compressed),
    <<1:2,              %% read-filter-id = 1
      (ZN-1):2,         %% use zstream ZN-1 
      ResetZS/binary,
      2:1/unit:8,       %% filter-id = gradient
      LengthBytes/binary,
      Compressed/binary>>;
internal_write(_PF, Data, _ZS) ->
    ?ERROR("Invalid tight data: ~p~n", [Data]),
    throw(invalid_data).