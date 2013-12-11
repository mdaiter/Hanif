-module(node).
-behaviour(gen_server).
-compile(export_all).

-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).

%%
%%API
%%
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_server() ->
  gen_server:cast(?MODULE, {start}).

location(IPOrAddress) ->
  gen_server:call(?MODULE, {location, IPOrAddress}).

retrieve(Data) ->
  gen_server:call(?MODULE, {retrieve, Data}).

init([]) ->
  erlang:process_flag(trap_exit, true),
  init(),
  {ok, []}.


%%
%% GEN_SERVER STUFF
%%
handle_cast({start}, State) ->
  start(),
  {noreply, started}.

handle_call({location, IPOrAddress},_From, State) ->
  Location = getLocation(IPOrAddress),
  {reply, {IPOrAddress, Location}, State};

handle_call({retrieve, Data}, _From, State) ->
  {reply, retrieveI(Data), State}.

 %Stuff I'm not using but still need to export :P
handle_info(_Message, State) -> {noreply, State}.
terminate(_Reason, State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

%%
%%Hidden Functions
%%

init() ->
  application:start(crypto),
  application:start(asn1),
  application:start(public_key),
  application:start(ssl),
  application:start(sasl),
  application:start(ibrowse),
  application:start(couchbeam).

restarterListening() ->
  process_flag(trap_exit, true),
  spawn_link(?MODULE, startListeningRemote, []),
  receive
    {'EXIT', _, normal} -> %not a crash
      ok;
    {'EXIT', _, shutdown} -> %I killed it
      ok;
    {'EXIT', _, _} ->
      restarterListening()
  end.

start() ->
  {spawn(?MODULE, restarterListening, []), spawn(?MODULE, restarterUploading, [])}.

startListeningRemote() ->
  receive
    {Pid, {retrieve, Message}} ->
      Pid ! retrieveI(Message);
    {Pid, location, IPOrAddress} ->
      Pid ! getLocation(IPOrAddress)
  end,
  startListeningRemote().

create_connection(Ip, Port) ->
  S = couchbeam:server_connection(Ip, Port, "", []),
  couchbeam:open_db(S, "testdb", []).

retrieveI(Data) ->
  {ok, Db} = create_connection("localhost", 5984),
  {Response, Doc2} = couchbeam_view:fetch(Db),
  case Response of
  	ok ->
  	  Doc2;
  	error ->
      error
  end.

restarterUploading() ->
  process_flag(trap_exit, true),
  UploaderPID = spawn_link(?MODULE, startUploadPID, []),
  register(uploader, UploaderPID),
  receive
    {'EXIT', _, normal} ->
      ok;
    {'EXIT', _, shutdown} ->
      ok;
    {'EXIT', _, _} ->
      restarterUploading()
  end.

startUploadPID() ->
  receive
    {Pid, upload, {Type, Tray, Id}} ->
      {ok, Db} = create_connection("localhost", 5984),
      TempData = scrape(),
      {{Year, Month, Day}, {Hour, Min, Sec}} = erlang:localtime(),
      Doc = {[
              {<<"type">>, list_to_binary(Type)},
              {<<"content">>, <<TempData>>},
              {<<"time">>, {[
                      {<<"year">>, Year},
                      {<<"month">>, Month},
                      {<<"day">>, Day},
                      {<<"hour">>, Hour},
                      {<<"min">>, Min},
                      {<<"sec">>, Sec}
                  ]}
              },
              {<<"tray">>, list_to_binary(Tray)},
              {<<"id">>, Id}
      ]},
      {AtomReceived, _} = couchbeam:save_doc(Db, Doc),
      Pid ! AtomReceived;
    {Pid, _} ->
      Pid ! invalid
  end,
  startUploadPID().

scrape() ->
  42.

getLocation(IPOrAddress) ->
  {ok, [{IP, _, _}, _]} = inet:getif(),
  case IPOrAddress of
    ip ->
      IP;
    address ->
      ipToAddress(inet_parse:ntoa(IP))
  end.

ipToAddress(IP) ->
  application:start(jiffy),
  inets:start(),
  {Err, {{_, 200, _}, _, Body}} = httpc:request(get, {"http://freegeoip.net/json/" ++ IP, []}, [], []),
  case Err of
    error ->
      error;
    ok ->
      JSON = jiffy:decode(Body),
      {[{_,_},
         {_,CountryAbbreviation},
         {_,Country},
         {_,StateAbbreviation},
         {_,State},
         {_,City},
         {_,ZipCode},
         {_,Latitude},
         {_,Longitude},
         {_,MetroCode},
         {_,AreaCode}]} = JSON,
      {CountryAbbreviation, Country, StateAbbreviation, State, City, ZipCode, Latitude, Longitude, MetroCode, AreaCode}
  end.
