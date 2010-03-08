%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fbenavides@novamens.com>
%%% @copyright (C) 2010 Novamens S.A.
%%% @doc Hextile RFB Encoding implementation
%%% @reference <a href="http://www.tigervnc.com/cgi-bin/rfbproto#hextile-encoding">More Information</a>
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(erfb_encoding_hextile).
-author('Fernando Benavides <fbenavides@novamens.com>').

-behaviour(erfb_encoding).

-export([init/0, read/5, write/4, terminate/2]).

-include("erfblog.hrl").
-include("erfb.hrl").

-record(state, {}).

%% ====================================================================
%% Server functions
%% ====================================================================
%% @hidden
-spec init() -> {ok, #state{}}.
init() -> {ok, #state{}}.

%% @hidden
-spec read(#pixel_format{}, #box{}, binary(), port(), #state{}) -> {ok, [#rectangle{}], Read::binary(), Rest::binary(), #state{}}.
read(PF, Box = #box{x = X, y = Y, width = W, height = H}, Bytes, Socket, State) ->
    WLast = case W rem 16 of
                0 -> 16;
                Wrem16 -> Wrem16
            end,
    HLast = case H rem 16 of
                0 -> 16;
                Hrem16 -> Hrem16
            end,
    Tiles = [ #box{x = TileX,
                   y = TileY,
                   width =
                       case TileX + 16 of
                           Next when Next >= (X+W) ->
                               WLast;
                           _ ->
                               16
                       end,
                   height =
                       case TileY + 16 of
                           Next when Next >= (Y+H) ->
                               HLast;
                           _ ->
                               16
                       end
                  } ||
                  TileY <- lists:seq(Y, Y+H-HLast, 16),
                  TileX <- lists:seq(X, X+W-WLast, 16) ],
    ?DEBUG("Hextile reader starting for ~p boxes in:~n\t~p~n", [{(erlang:trunc(W/16) +
                                                                      erfb_utils:ceiling((W rem 16)/16)) *
                                                                     (erlang:trunc(H/16) +
                                                                          erfb_utils:ceiling((H rem 16)/16)),
                                                                 erlang:length(Tiles)},
                                                                Box]),
    read(Tiles, Box, PF, Bytes, Socket, [], <<>>, State).

%% @hidden
-spec write(#pixel_format{}, #box{}, binary(), #state{}) -> {ok, binary(), #state{}} | {error, invalid_data, #state{}}.
write(#pixel_format{bits_per_pixel = BPP}, _Box, Tiles, State) when is_list(Tiles) ->
    PixelSize = erlang:trunc(BPP / 8),
    RectBytes = << <<(write(Tile, PixelSize))/binary>> || Tile <- Tiles >>,
    {ok, RectBytes, State};
write(_PF, _, Data, State) ->
    ?ERROR("Invalid data for hextile encoding:~p~n", [Data]),
    {error, invalid_data, State}.

%% @hidden
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) -> ok.


%% ====================================================================
%% Internal functions
%% ====================================================================
-spec read([#box{}], #box{}, #pixel_format{}, binary(), port(), [#rectangle{}], binary(), #state{}) -> {ok, #rectangle{}, Read::binary(), Rest::binary(), #state{}}.
read([], _OutsideBox, _PF, Rest, _Socket, Tiles, Read, State) ->
    ?TRACE("Last: ~p~nAll boxes read~n", [{(hd(Tiles))#rectangle.box, (hd(Tiles))#rectangle.encoding}]),
    {ok, lists:reverse(Tiles), Read, Rest, State};
read(Boxes, OutsideBox, PF, <<>>, Socket, Tiles, BytesRead, State) ->
    ?TRACE("Empty Stream, missing ~p boxes...~n", [erlang:length(Boxes)]),
    read(Boxes, OutsideBox, PF, erfb_utils:complete(<<>>, 1, Socket, true),
         Socket, Tiles, BytesRead, State);
read([Box | Boxes], OutsideBox, PF = #pixel_format{bits_per_pixel = BPP},
     <<Byte:1/binary, Bytes/binary>>, Socket, Tiles, BytesRead, State) ->
    ?TRACE("Last: ~p - Reading ~p box: ~p from ~p bytes~n", [case Tiles of
                                                                 [] ->
                                                                     none;
                                                                 Tiles ->
                                                                     {(hd(Tiles))#rectangle.box, (hd(Tiles))#rectangle.encoding}
                                                             end,
                                                             erlang:length(Tiles)+1,
                                                             Box,
                                                             {Byte,
                                                              erlang:size(Bytes)}]),
    <<_Padding:3/unit:1,
      SubrectsColoured:1/unit:1,
      AnySubrects:1/unit:1,
      ForegroundSpecified:1/unit:1,
      BackgroundSpecified:1/unit:1,
      Raw:1/unit:1>> = Byte,
    case Raw of
        1 ->
            {ok, RawState} = erfb_encoding_raw:init(),
            {ok, Tile, Read, Rest, NewRawState} =
                erfb_encoding_raw:read(PF, Box, Bytes, Socket, RawState),
            ok = erfb_encoding_raw:terminate(normal, NewRawState),
            read(
              Boxes, OutsideBox, PF, Rest, Socket, [#rectangle{box = Box,
                                                               encoding = ?ENCODING_RAW,
                                                               data = Tile} | Tiles],
              <<BytesRead/binary, Byte/binary, Read/binary>>,
              State);
        0 ->
            PixelSize = erlang:trunc(BPP / 8),
            HeaderLength =
                case BackgroundSpecified of
                    1 -> PixelSize;
                    0 -> 0
                end + case ForegroundSpecified of
                          1 -> PixelSize;
                          0 -> 0
                end +
                    AnySubrects, %%NOTE: 1 => 1 more byte
            AllBytes = 
            case bstr:len(Bytes) of
                HL when HL < HeaderLength ->
                    erfb_utils:complete(Bytes, HeaderLength, Socket, true);
                _ ->
                    Bytes
            end,
            <<Header:HeaderLength/binary, Rest/binary>> = AllBytes,
            {Background, Foreground, Count} =
                case {BackgroundSpecified, ForegroundSpecified, AnySubrects} of
                    {1,1,1} ->
                        <<B:PixelSize/unit:8, F:PixelSize/unit:8, C:1/unit:8>> = Header,
                        {B, F, C};
                    {1,1,0} ->
                        <<B:PixelSize/unit:8, F:PixelSize/unit:8>> = Header,
                        {B, F, 0};
                    {1,0,1} ->
                        <<B:PixelSize/unit:8, C:1/unit:8>> = Header,
                        {B, undefined, C};
                    {1,0,0} ->
                        <<B:PixelSize/unit:8>> = Header,
                        {B, undefined, 0};
                    {0,1,1} ->
                        <<F:PixelSize/unit:8, C:1/unit:8>> = Header,
                        {undefined, F, C};
                    {0,1,0} ->
                        <<F:PixelSize/unit:8>> = Header,
                        {undefined, F, 0};
                    {0,0,1} ->
                        <<C:1/unit:8>> = Header,
                        {undefined, undefined, C};
                    {0,0,0} ->
                        {undefined, undefined, 0}
                end,
            BodyLength = Count * (2 + (SubrectsColoured * PixelSize)), %%NOTE: if set, each subrect comes with its own colour
            AllRest =
                case bstr:len(Rest) of
                    BL when BL < BodyLength andalso Boxes =:= [] ->
                        erfb_utils:complete(Rest, BodyLength, Socket, true);
                    BL when BL < BodyLength ->
                        erfb_utils:complete(Rest, BodyLength + 1, Socket, true); %%NOTE: +1 to get the first byte of the next tile
                    _ ->
                        Rest
                end,
            Body = bstr:substr(AllRest, 1, BodyLength),
            Rectangles =
                hextile_subreader(Body, PixelSize, SubrectsColoured),
            read(
              Boxes, OutsideBox, PF,
              bstr:substr(AllRest, BodyLength + 1), Socket,
              [#rectangle{box     = Box,
                          data    =
                              #hextile_data{background = Background,
                                            foreground = Foreground,
                                            rectangles = Rectangles}} | Tiles],
              <<BytesRead/binary, Byte/binary, Header/binary, Body/binary>>,
              State)
    end.

-spec hextile_subreader(binary(), integer(), 0 | 1) -> [#rectangle{}].
hextile_subreader(Body, PixelSize, 1) ->
    [#rectangle{box     = #box{x = X,
                               y = Y,
                               width = W+1,
                               height = H+1},
                data    = Pixel} ||
               <<Pixel:PixelSize/unit:8,
                 X:1/unit:4,
                 Y:1/unit:4,
                 W:1/unit:4,
                 H:1/unit:4>> <= Body];
hextile_subreader(Body, _PixelSize, 0) ->
    [#rectangle{box     = #box{x = X,
                               y = Y,
                               width = W+1,
                               height = H+1}} ||
               <<X:1/unit:4,
                 Y:1/unit:4,
                 W:1/unit:4,
                 H:1/unit:4>> <= Body].

-spec write(#rectangle{}, integer()) -> binary().
write(#rectangle{encoding = ?ENCODING_RAW, data = Data}, _PixelSize) ->
    <<1:1/unit:8, Data/binary>>;
write(#rectangle{data = #hextile_data{background = Background,
                                      foreground = Foreground,
                                      rectangles = Rectangles}},
               PixelSize) ->
    ForegroundSpecified =
        case Foreground of
            undefined -> 0;
            _ -> 1
        end,
    BackgroundSpecified =
        case Background of
            undefined -> 0;
            _ -> 1
        end,
    {AnySubrects, SubrectsColoured} =
        case Rectangles of
            [] ->
                {0, 0};
            [#rectangle{data = undefined} | _] ->
                {1, 0};
            _ ->
                {1, 1}
        end,
    bstr:join([
               <<0:3/unit:1,
                 SubrectsColoured:1/unit:1,
                 AnySubrects:1/unit:1,
                 ForegroundSpecified:1/unit:1,
                 BackgroundSpecified:1/unit:1,
                 0:1/unit:1>>,
               case BackgroundSpecified of
                   1 -> <<Background:PixelSize/unit:8>>;
                   0 -> <<>>
               end,
               case ForegroundSpecified of
                   1 -> <<Foreground:PixelSize/unit:8>>;
                   0 -> <<>>
               end,
               case AnySubrects of
                   1 -> <<(erlang:length(Rectangles)):PixelSize/unit:8>>;
                   0 -> <<>>
               end |
                   [case Pixel of
                        undefined ->
                            <<X:1/unit:4,
                              Y:1/unit:4,
                              (W-1):1/unit:4,
                              (H-1):1/unit:4>>;
                        Pixel ->
                            <<Pixel:PixelSize/unit:8,
                              X:1/unit:4,
                              Y:1/unit:4,
                              (W-1):1/unit:4,
                              (H-1):1/unit:4>>
                    end ||
                    #rectangle{box = #box{x = X,
                                          y = Y,
                                          width = W,
                                          height = H},
                               data = Pixel} <- Rectangles]]).