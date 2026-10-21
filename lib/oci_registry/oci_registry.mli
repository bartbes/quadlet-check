type t
type blob_upload_session
type next_tag_list

type digest = string
type tag_or_digest = string

type error =
    | NetworkError
    | InvalidCredentials
    | NotPresent

type manifest_type =
    | Manifest
    | Index

val make : registry_base_url:Uri.t -> repository:string -> ?token:string option -> unit -> t
val of_image_name : ?insecure:bool -> ?token:string option -> string -> t
val get_token : t -> string option

val pull_manifest : ?manifest_type:manifest_type -> t -> tag_or_digest -> (string, error) result Lwt.t
val test_manifest : t -> tag_or_digest -> (manifest_type * digest, error) result Lwt.t

val push_manifest : manifest_type:manifest_type -> t -> tag_or_digest -> string -> (Uri.t, error) result Lwt.t

val pull_blob : t -> digest -> (string Lwt_stream.t, error) result Lwt.t
val test_blob : t -> digest -> (digest, error) result Lwt.t

val start_blob_upload_session : t -> (blob_upload_session, error) result Lwt.t
val upload_blob : t -> blob_upload_session -> digest:digest -> string -> (Uri.t, error) result Lwt.t

val list_tags : t -> ?last:string option -> ?max:int -> unit -> (string list * next_tag_list option, error) result Lwt.t
val list_next_tags : t -> ?max:int -> next_tag_list -> (string list * next_tag_list option, error) result Lwt.t
val stream_tags_paginated : t -> ?last:string option -> ?desired_page_size:int -> unit -> string list Lwt_stream.t
val stream_tags : t -> ?last:string option -> ?desired_page_size:int -> unit -> string Lwt_stream.t

(* TODO: Listing referrers
val list_referrers : t -> digest -> (string, error) result Lwt.t
*)

(* TODO: Deletion
val delete_tag : t -> string -> (unit, error) result Lwt.t
val delete_manifest : t -> digest -> (unit, error) result Lwt.t
val delete_blob : t -> digest -> (unit, error) result Lwt.t
*)
