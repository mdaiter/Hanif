-module(pin).
-behaviour(gen_server).
-define(GPIO_EXPORT_FILE, "/sys/class/gpio/export").
-define(GPIO_PIN_BASE, "/sys/class/gpio/gpio").
-export([init/1, handle_info/2, handle_cast/2, handle_call/3, code_change/3, terminate/2]).
-record(pinState, {
         name,
         dir_name,
         power,
         direction = out
         }).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

turnOn(PinNum, Direction) ->
  gen_server:cast(?MODULE, {turnOn, PinNum, Direction}).

turnOff(PinNum) ->
  gen_server:cast(?MODULE, {turnOn, PinNum}).
  

%%%
%%% API Funcs
%%%
init(Pin, Direction) ->
  exportPin(Pin),
  PinStruct = #pinState{name = integer_to_list(Pin), dir_name = ?GPIO_PIN_BASE ++ integer_to_list(Pin),
                       direction = Direction},
  {ok, PinStruct}.

handle_cast({turnOn, Power, Direction}, State) ->
  State2 = State#pinState{power = Power, direction = Direction},
  {noreply, State2}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
handle_info(_Info, State) ->
  {noreply, State}.

%%%
%%% Private functions
%%%
turnPinOn(PinNum, Direction) ->
  exportPin(PinNum),
  PinFileName = "/sys/class/gpio/gpio" ++ integer_to_list(PinNum) ++ "/direction",
  file:open(PinFileName, [write]),
  file:write(PinFileName, Direction),
  file:close(PinFileName).
exportPin(PinNum) ->
  ExportFile = file:open(?GPIO_EXPORT_FILE, [append]),
  file:write(ExportFile, integer_to_list(PinNum)),
  file:close(ExportFile).
