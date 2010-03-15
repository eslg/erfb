-define(FSM_TIMEOUT, 120000).
-define(COMPLETION_TIMEOUT, 5000).

%% @type ip() = string() | {integer(), integer(), integer(), integer()}.  IP Address, either as a string or parsed.
-type(ip() :: string() | {integer(), integer(), integer(), integer()}).

%% == FSMS =====================================================================
%% @type async_state_result() = {next_state, atom(), term()} | {next_state, atom(), term(), integer() | infinity} | {stop, term(), term()}
-type(async_state_result() :: {next_state, atom(), term()} | {next_state, atom(), term(), integer() | infinity} | {stop, term(), term()}).
%% @type sync_state_result() = {next_state, atom(), term()} | {next_state, atom(), term(), integer() | infinity} | {reply, any(), atom(), term()} | {reply, any(), atom(), term(), integer() | infinity} | {stop, term(), any(), term()} | {stop, term(), term()}
-type(sync_state_result() :: {next_state, atom(), term()} | {next_state, atom(), term(), integer() | infinity} | {reply, any(), atom(), term()} | {reply, any(), atom(), term(), integer() | infinity} | {stop, term(), any(), term()} | {stop, term(), term()}).
%% @type fsmref() = pid() | atom(). FSM Reference: a way to identify an ERFB client or server process
-type(fsmref() :: pid() | atom()).

%% == MAGIC NUMBERS ============================================================
-define(FALSE, 0).
-define(MIN_VERSION, {3,3}).
-define(MID_VERSION, {3,7}).
-define(MAX_VERSION, {3,8}).
-define(PROTOCOL_NAME, "RFB").
-define(SECURITY_INVALID, 0).
-define(SECURITY_NONE, 1).
-define(SECURITY_VNC, 2).
-define(SECURITY_RESULT_OK, 0).
-define(SECURITY_RESULT_NOK, 1).
-define(MSG_FRAMEBUFFER_UPDATE, 0).
-define(MSG_SET_COLOUR_MAP_ENTRIES, 1).
-define(MSG_BELL, 2).
-define(MSG_SERVER_CUT_TEXT, 3).
-define(MSG_SET_PIXEL_FORMAT, 0).
-define(MSG_SET_ENCODINGS, 2).
-define(MSG_FRAMEBUFFER_UPDATE_REQUEST, 3).
-define(MSG_KEY_EVENT, 4).
-define(MSG_POINTER_EVENT, 5).
-define(MSG_CLIENT_CUT_TEXT, 6).
-define(ENCODING_RAW, 0).
-define(ENCODING_ZLIB, 6).
-define(ENCODING_WMVI, 1464686185).

%% == RFB ======================================================================
-record(colour, {red    :: non_neg_integer(),
                 green  :: non_neg_integer(),
                 blue   :: non_neg_integer()}).
-record(pixel_format, {bits_per_pixel   :: integer(),
                       depth            :: integer(),
                       big_endian       :: boolean(),
                       true_colour      :: boolean(),
                       red_max          :: integer(),
                       green_max        :: integer(),
                       blue_max         :: integer(),
                       red_shift        :: integer(),
                       green_shift      :: integer(),
                       blue_shift       :: integer()
                      }).
%% @type security_kind() = none | {vnc, Password :: binary()}. The supported security types
-type(security_kind() :: none | {vnc, binary()}).
-record(session, {server = uuid:as_bstr()   :: binary(),
                  client = uuid:as_bstr()   :: binary(),
                  version    = {3,8}        :: {3, 3 | 7 | 8},
                  name                      :: binary(),
                  width                     :: integer(),
                  height                    :: integer(),
                  pixel_format              :: #pixel_format{},
                  security = [none]         :: [security_kind()]
                 }).
-record(box, {x        :: integer(),
              y        :: integer(),
              width    :: integer() | all,
              height   :: integer() | all}).
-record(rectangle, {box                  :: #box{},
                    encoding = undefined :: integer() | undefined,
                    data                 :: term()}). %%NOTE: depends on the encoding
-record(rre_data, {background   :: integer(),
                   rectangles   :: [#rectangle{}]}).
-record(hextile_data, {background   :: integer() | undefined,
                       foreground   :: integer() | undefined,
                       rectangles   :: [#rectangle{}]}).
-record(zlibhex_data, {background   :: integer() | undefined,
                       foreground   :: integer() | undefined,
                       compressed   :: boolean(),
                       rectangles   :: [#rectangle{}]}).
-record(tight_data, {reset_zstreams :: binary(),
                     compression    :: fill | jpeg | basic | {basic, 1..4},
                     filter         :: undefined | copy | gradient | {palette, [integer()]},
                     data           :: integer() | binary()}).
-record(cursor_data, {pixels    :: binary(),
                      bitmask   :: binary()}).

%% == EVENTS ===================================================================
-define(ERFB_EVENT_BASE, sender    = self()                 :: fsmref(),
                         server                             :: binary(),
                         client                             :: binary(),
                         timestamp = erfb_utils:timestamp() :: integer(),
                         raw_data  = undefined              :: undefined | binary()).
-record(server_connected, {?ERFB_EVENT_BASE,
                           session :: #session{}}).
-record(server_disconnected, {?ERFB_EVENT_BASE,
                              reason :: term()}).
-record(update, {?ERFB_EVENT_BASE,
                 rectangles = [] :: [#rectangle{}]}).
-record(set_colour_map_entries, {?ERFB_EVENT_BASE,
                                 first_colour   :: integer(),
                                 colours = []   :: [#colour{}]}).
-record(bell, {?ERFB_EVENT_BASE}).
-record(server_cut_text, {?ERFB_EVENT_BASE,
                          text  :: binary()}).

-record(listener_disconnected, {?ERFB_EVENT_BASE,
                                reason :: term()}).

-record(client_connected, {?ERFB_EVENT_BASE,
                           session :: #session{}}).
-record(client_disconnected, {?ERFB_EVENT_BASE,
                              reason :: term()}).
-record(set_pixel_format, {?ERFB_EVENT_BASE,
                           pixel_format :: #pixel_format{}}).
-record(set_encodings, {?ERFB_EVENT_BASE,
                        encodings = [] :: [{integer(), atom()}]}).
-record(update_request, {?ERFB_EVENT_BASE,
                         incremental = true :: boolean(),
                         box                :: #box{}}).
-record(key, {?ERFB_EVENT_BASE,
              down  = true  :: boolean(),
              code          :: integer()}).
-record(pointer, {?ERFB_EVENT_BASE,
                  button_mask   :: byte(),
                  x             :: integer(),
                  y             :: integer()}).
-record(client_cut_text, {?ERFB_EVENT_BASE,
                          text  :: binary()}).

-record(unknown_message, {?ERFB_EVENT_BASE,
                          type :: integer()}).

%% @type server_event() = #server_connected{} | #server_disconnected{} | #set_colour_map_entries{} | #update{} | #bell{} | #server_cut_text{} | #unknown_message{}
-type(server_event() :: #server_connected{} | #server_disconnected{} | #set_colour_map_entries{} | #update{} | #bell{} | #server_cut_text{} | #unknown_message{}).
%% @type client_event() = #client_connected{} | #listener_disconnected{} | #client_disconnected{} | #set_pixel_format{} | #set_encodings{} | #update_request{} | #key{} | #pointer{} | #client_cut_text{} | #unknown_message{}
-type(client_event() :: #client_connected{} | #listener_disconnected{} | #client_disconnected{} | #set_pixel_format{} | #set_encodings{} | #update_request{} | #key{} | #pointer{} | #client_cut_text{} | #unknown_message{}).