type t =
  | Domain of [ `host ] Domain_name.t
  | IP of Ipaddr.t  (** Type of endpoint. *)

val pp : Format.formatter -> t -> unit
(** Pretty-printer of {!t}. *)

val of_string : string -> (t, [> `Msg of string ]) result
(** [of_string str] returns an endpoint from the given [string]. We tried to
    parse the given [string] as an {i hostname} and if it fails, we try to
    consider it as an IP (V4 or V6).

    If the given [string] is neither a {i hostname} nor an IP, we return an
    error. *)

val v : string -> t
(** An alias of {!of_string}. In the case of an error, we raise an exception. *)

val to_string : t -> string
(** [to_string t] returns a valid string which represents the endpoint. By
    {i valid}, we means that the returned [string] can safely be used with {!v}. *)

val domain : [ `host ] Domain_name.t -> t
(** [domain domain_name] returns an endpoint from a {i hostname}. *)

val ip : Ipaddr.t -> t
(** [ip v] returns an endpoint from an {!Ipaddr.t}. *)

val compare : t -> t -> int
(** Comparison function for {!t}. *)

val equal : t -> t -> bool
(** Equal function for {!t}. *)
