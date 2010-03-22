RFB Library for Erlang
======================

A library to start, manage and stop [RFB](http://www.tigervnc.com/cgi-bin/rfbproto) client and/or server connections.
Server side works with [any VNC client](http://www.dmoz.org/Computers/Software/Networking/Thin_Clients/Virtual_Network_Computing/).
Client side works with servers that support reverse connections.


Requirements
------------

You should only need a somewhat recent version of Erlang/OTP, though the library
has only been tested with Erlang R13B.


Installation
------------

To compile the library you simply run ``make``.


Usage
-----

Follow this steps to build a RFB *Client*:

  - start the client listener process:
>        erfb_client:start_link(Ip, Port, Backlog)
>    where the parameters are:
>
>  * ``Ip :: ip()``: The IP where you want the client listener process to listen for server connections
>  * ``Port :: integer()``: (Usually 5500) The port where you want the client listener process to listen for server connections
>  * ``Backlog :: integer()``: The [backlog](http://ftp.sunet.se/pub/lang/erlang/doc/man/gen_tcp.html#listen-2) for the client listener process socket

  - subscribe to the client event dispatcher:
>        erfb_client_event_dispatcher:subscribe_link(GeneralHandler, Args)
>   where the parameters are equivalent to those of [gen_event:add_sup_handler/3](http://demo.erlang.org/doc/man/gen_event.html#add_sup_handler-3)

  - wait for ``#server_connected{event_dispatcher = Dispatcher :: pid()}`` events
  
  - subscribe to the particular event dispatcher:
>        erfb_client_event_dispatcher:subscribe_link(Dispatcher, Handler, Args)
>   where the parameters are equivalent to those of [gen_event:add_sup_handler/3](http://demo.erlang.org/doc/man/gen_event.html#add_sup_handler-3)

  - start listening for events with your ``Handler``
>    All events are implemented using records, and all of them have the following common fields:
>
>  * ``sender :: pid()``: The pid of the process that sent the event
>  * ``server :: binary()``: The id of the server (assigned by *erfb* automatically)
>  * ``raw_data :: binary()``: The actual bytes sent by the RFB server
>
>    Every event has its own fields too.  The possible events are (check the RFB protocol definition for their meanings):
>
>  * `#set_colour_map_entries{}`
>  * `#update{}`
>  * `#bell{}`
>  * `#server_cut_text{}`
>  * `#unknown_message{}`

  - to send data to the server, use the functions exported by the ``erfb_client_process`` module

Follow this steps to build a RFB *Server*:

  - start the server listener manager process:
>        erfb_server:start_link()

  - start a new listener:
>        erfb_server_listener_manager:start_listener(Session, Encodings, Ip, Port, Backlog)
>    where the parameters are:
>
>  * ``Session :: #session{}``: The description of the server parameters (including window size, name, pixel format and others)
>  * ``Encodings :: [{integer(), atom()}]``: The list of encodings that the server will support (each one with the module that will be used to interpret it, this modules must comply to the erfb_encoding behaviour)
>  * ``Ip :: ip()``: The IP where you want the server listener process to listen for client connections
>  * ``Port :: integer()``: (Usually 59xx) The port where you want the server listener process to listen for client connections
>  * ``Backlog :: integer()``: The [backlog](http://ftp.sunet.se/pub/lang/erlang/doc/man/gen_tcp.html#listen-2) for the server listener process socket

  - subscribe to the server event dispatcher:
>        erfb_server_event_dispatcher:subscribe_link(GeneralHandler, Args)
>   where the parameters are equivalent to those of [gen_event:add_sup_handler/3](http://demo.erlang.org/doc/man/gen_event.html#add_sup_handler-3)

  - wait for ``#client_connected{event_dispatcher = Dispatcher :: pid()}`` events
  
  - subscribe to the particular event dispatcher:
>        erfb_server_event_dispatcher:subscribe_link(Dispatcher, Handler, Args)
>   where the parameters are equivalent to those of [gen_event:add_sup_handler/3](http://demo.erlang.org/doc/man/gen_event.html#add_sup_handler-3)

  - start listening for events with your ``Handler``
>    All events are implemented using records, and all of them have the following common fields:
>
>  * ``sender :: pid()``: The pid of the process that sent the event
>  * ``client :: binary()``: The id of the client (assigned by *erfb* automatically) *Note:* a server may have more than just one client
>  * ``raw_data :: binary()``: The actual bytes sent by the RFB server
>
>    Every event has its own fields too.  The possible events are (check the RFB protocol definition for their meanings):
>
>  * `#set_pixel_format{}`
>  * `#set_encodings{}`
>  * `#update_request{}`
>  * `#key{}`
>  * `#pointer{}`
>  * `#client_cut_text{}`
>  * `#unknown_message{}`

  - to send data to the clients, use the functions exported by the ``erfb_server_process`` module