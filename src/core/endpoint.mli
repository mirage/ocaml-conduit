type t = Domain of [ `host ] Domain_name.t | IP of Ipaddr.t

val pp : Format.formatter -> t -> unit

val of_string : string -> (t, [> `Msg of string ]) result

val v : string -> t

val to_string : t -> string

val domain : [ `host ] Domain_name.t -> t

val ip : Ipaddr.t -> t

val compare : t -> t -> int

val equal : t -> t -> bool
