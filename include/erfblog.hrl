%%%-------------------------------------------------------------------
%%% @doc Macros used to selectively send debugging information
%%%
%%% Slightly copied from enovalog, but that's not a public project yet
%%% This macros work with the enovalog handlers or with any event_handler
%%% attached to error_logger in this way:
%%%     error_logger:add_report_handler(?MODULE, [LogLevel]);
%%%-------------------------------------------------------------------

%%NOTE: We do this because we want to step over other definitions of these macros
-undef(LOG_LEVEL_TRACE).
-undef(LOG_LEVEL_DEBUG).
-undef(LOG_LEVEL_INFO).
-undef(LOG_LEVEL_WARN).
-undef(LOG_LEVEL_ERROR).
-undef(LOG_LEVEL_FATAL).
-undef(LOG_LEVELS).
-undef(LOG).
-undef(DEBUG).
-undef(TRACE).
-undef(INFO).
-undef(WARN).
-undef(ERROR).
-undef(FATAL).

-define(LOG_LEVEL_TRACE, trace).
-define(LOG_LEVEL_DEBUG, debug).
-define(LOG_LEVEL_INFO,  info).
-define(LOG_LEVEL_WARN,  warning).
-define(LOG_LEVEL_ERROR, error).
-define(LOG_LEVEL_FATAL, fatal_error).
-define(LOG_LEVELS, [?LOG_LEVEL_TRACE, ?LOG_LEVEL_DEBUG, ?LOG_LEVEL_INFO,
                     ?LOG_LEVEL_WARN, ?LOG_LEVEL_ERROR, ?LOG_LEVEL_FATAL]).
-type loglevel() :: trace | debug | info | warning | error | fatal_error.

-record(log, {time = erlang:universaltime()     :: {{integer(),integer(),integer()},{integer(),integer(),integer()}},
              timestamp = erfb_utils:timestamp():: integer(),
              node = node()                     :: atom(),
              pid = self()                      :: pid(),
              module                            :: atom(),
              line                              :: integer(),
              stacktrace = []                   :: [term()],
              text = ""                         :: string(),
              args = []                         :: [term()]}).

-define(LOG(LOGLevel, LOGStr, LOGArgs, LOGStack),
        try erlang:apply(
              error_logger,
              case LOGLevel of
                  ?LOG_LEVEL_FATAL -> error_report;
                  ?LOG_LEVEL_ERROR -> error_report;
                  ?LOG_LEVEL_WARN ->  warning_report;
                  _ ->                info_report
              end,
              [#log{module      = ?MODULE,
                    line        = ?LINE,
                    stacktrace  = LOGStack,
                    text        = LOGStr,
                    args        = LOGArgs}]) of
            ok ->
                ok;
            _ ->
                error_logger:error_msg("Error trying to log a message:~p~n",
                                       [{LOGStr, LOGArgs}])
        catch
            _:_ ->
                error_logger:error_msg("Exception trying to log a message:~p~n",
                                       [{LOGStr, LOGArgs}])
        end.
-define(DEBUG(Str, Args), ?LOG(?LOG_LEVEL_DEBUG, Str, Args, [])).
-define(TRACE(Str, Args), ?LOG(?LOG_LEVEL_TRACE, Str, Args, [])).
-define(INFO(Str, Args),  ?LOG(?LOG_LEVEL_INFO,  Str, Args, [])).
-define(WARN(Str, Args),  ?LOG(?LOG_LEVEL_WARN,  Str, Args, [])).
-define(ERROR(Str, Args), ?LOG(?LOG_LEVEL_ERROR, Str, Args, erlang:get_stacktrace())).
-define(FATAL(Str, Args), ?LOG(?LOG_LEVEL_FATAL, Str, Args, erlang:get_stacktrace())).