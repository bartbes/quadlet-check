module Parser : sig
    type record
    val parse : string -> (record list, string) result
end

type t

(** Gets all values for a given section and key.
    Obeys the systemd standard that 'Key=' removes earlier assignments. **)
val get_all : t -> string -> string -> string Seq.t

(** Gets the last element in the result of {!get_all}, i.e. the last value specified.
    Returns None if the last entry was of the form 'Key=' **)
val get : t -> string -> string -> string option

(** Gets all sections specified. These can be empty. **)
val get_sections : t -> string Seq.t

(** Gets all keys. For all of these {!get} will return Some. **)
val get_keys : t -> string -> string Seq.t

(** Takes parser output and turns it into a {!t}.
    Mostly useful internally, but can be used to merge multiple
    unit files into one. **)
val of_parse_records : Parser.record list -> t

(** Parse a unit file into a {!t}. **)
val of_string : string -> (t, string) result

(** Parse multiple unit files or file parts into a {!t}. **)
val of_strings : string list -> (t, string) result
