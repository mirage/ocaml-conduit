open Async
open Async_ssl
open Conduit_async

type 'flow with_ssl = {
  connection : Ssl.Connection.t;
  reader : Reader.t;
  writer : Writer.t;
  underlying : 'flow;
}

type context = {
  version : Ssl.Version.t option;
  options : Ssl.Opt.t list option;
  name : string option;
  hostname : string option;
  allowed_ciphers :
    [ `Only of string list | `Openssl_default | `Secure ] option;
  ca_file : string option;
  ca_path : string option;
  crt_file : string option;
  key_file : string option;
  session : Ssl.Session.t option;
  verify_modes : Verify_mode.t list option;
  verify : (Ssl.Connection.t -> bool Async.Deferred.t) option;
}

val context :
  ?version:Ssl.Version.t ->
  ?options:Ssl.Opt.t list ->
  ?name:string ->
  ?hostname:string ->
  ?allowed_ciphers:[ `Only of string list | `Openssl_default | `Secure ] ->
  ?ca_file:string ->
  ?ca_path:string ->
  ?crt_file:string ->
  ?key_file:string ->
  ?session:Ssl.Session.t ->
  ?verify_modes:Verify_mode.t list ->
  ?verify:(Ssl.Connection.t -> bool Async.Deferred.t) ->
  unit ->
  context

val protocol_with_ssl :
  key:'edn Conduit_async.key ->
  reader:('flow -> Reader.t) ->
  writer:('flow -> Writer.t) ->
  'flow Conduit_async.Witness.protocol ->
  (context * 'edn) Conduit_async.key
  * 'flow with_ssl Conduit_async.Witness.protocol

val service_with_ssl :
  key:'edn Conduit_async.key ->
  ('t * 'flow) Conduit_async.Witness.service ->
  reader:('flow -> Reader.t) ->
  writer:('flow -> Writer.t) ->
  'flow with_ssl Conduit_async.Witness.protocol ->
  (context * 'edn) Conduit_async.key
  * ((context * 't) * 'flow with_ssl) Conduit_async.Witness.service

module TCP : sig
  open Conduit_async_tcp

  val endpoint : (context * endpoint) key

  val protocol : Protocol.flow with_ssl Witness.protocol

  val configuration : (context * Conduit_async_tcp.configuration) key

  val service : ((context * Service.t) * Protocol.flow with_ssl) Witness.service

  val resolv_conf : port:int -> context:context -> (context * endpoint) resolver
end
