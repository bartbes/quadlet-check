open Lwt

type credential_state =
    | Rejected
    | TryAnonymous
    | Token of Cohttp.Auth.credential

type t = {
    base_url: Uri.t;
    repository: string;
    mutable credential: credential_state;
}

type blob_upload_session = {
    upload_url: Uri.t;
}

type next_tag_list = {
    next_search_url: Uri.t;
}

type digest = string
type tag_or_digest = string
type blob = char array

type error =
    | NetworkError
    | InvalidCredentials
    | NotPresent

type manifest_type =
    | Manifest
    | Index

module YB = Yojson.Basic
module YBU = YB.Util

let make ~registry_base_url ~repository ?(token=None) () =
    {
        base_url = (Uri.path registry_base_url) ^ "/v2/" ^ repository |> Uri.with_path registry_base_url;
        repository;
        credential = match token with
        | Some(token) -> Token(Cohttp.Auth.credential_of_string @@ "Bearer " ^ token)
        | None -> TryAnonymous;
    }

let of_image_name ?(insecure=false) ?(token=None) name =
    let (registry, image) = match String.split_first ~sep:"/" name with
    | None -> raise @@ Invalid_argument(name)
    | Some(registry, image) -> (registry, image) in
    let host = match registry with
    | "docker.io" -> "registry-1.docker.io"
    | registry -> registry in
    let repository = match (host, image) with
    | ("registry-1.docker.io", image) when not @@ String.contains image '/' ->
        "library/" ^ image
    | (_, image) -> image in
    let scheme = if insecure then "http" else "https" in
    let registry_base_url : Uri.t = Uri.make ~scheme ~host () in
    make ~registry_base_url ~repository ~token ()

let get_token repository =
    let (let*) = Option.bind in
    let* token = match repository.credential with
    | Token(token) -> Some token
    | _ -> None in
    let token = Cohttp.Auth.string_of_credential token in
    let len = String.length token in
    if String.starts_with ~prefix:"Bearer " token then
        Some (String.sub token 7 (len - 7))
    else
        None

let authenticate repository (response : Http.Response.t) =
    let parse_auth_result body =
        try
            let token = YB.from_string body |> YBU.member "token" |> YBU.to_string in
            Ok ("Bearer " ^ token |> Cohttp.Auth.credential_of_string)
        with Yojson.Json_error _ | YBU.Type_error _ ->
            Error InvalidCredentials in
    let send_auth_request realm service scope =
        let scope = Printf.sprintf "repository:%s:pull" repository.repository in
        let uri = Uri.add_query_params' (Uri.of_string realm) [("service", service); ("scope", scope)] in
        let%lwt (response, body) = Cohttp_lwt_unix.Client.get uri in
        match response.status with
        | `OK -> Cohttp_lwt.Body.to_string body |> Lwt.map parse_auth_result
        | _ -> return_error InvalidCredentials in
    match Cohttp.Header.get response.headers "www-authenticate" with
    | None -> return_error NetworkError
    | Some authenticate ->
        Scanf.sscanf authenticate {|Bearer realm="%s@",service="%s@",scope="%s@"|} send_auth_request

let try_authenticated_request repository ?(headers=Http.Header.init ()) ?(body=`Empty) meth uri =
    let request_with_token token =
        let headers = Cohttp.Header.add_authorization headers token in
        let%lwt (response, body) = Cohttp_lwt_unix.Client.call ~headers ~body meth uri in
        match response.status with
        | `Unauthorized -> return_error InvalidCredentials
        | _ -> return_ok (response, body) in
    let request_anonymous_first () =
        let%lwt (response, body) = Cohttp_lwt_unix.Client.call ~headers ~body meth uri in
        match response.status with
        | `Unauthorized -> (
            match%lwt authenticate repository response with
            | Ok token -> repository.credential <- Token(token);
                request_with_token token
            | Error x -> repository.credential <- Rejected;
                return_error x
        )
        | _ -> return_ok (response, body) in
    match repository.credential with
    | Rejected -> return_error InvalidCredentials
    | Token token -> request_with_token token
    | TryAnonymous -> request_anonymous_first ()

let pull_manifest ?(manifest_type=Manifest) repository tag_or_digest =
    let (let*) = Lwt_result.bind in
    let uri = (Uri.path repository.base_url) ^ "/manifests/" ^ tag_or_digest |> Uri.with_path repository.base_url |> Uri.canonicalize in
    let mimetype = match manifest_type with
    | Manifest -> "application/vnd.oci.image.manifest.v1+json"
    | Index -> "application/vnd.oci.image.index.v1+json" in
    let headers = Cohttp.Header.init_with "Accept" mimetype in
    let* (response, body) = try_authenticated_request repository ~headers `GET uri in
    match response.status with
    | `OK ->
        let%lwt body = Cohttp_lwt.Body.to_string body in
        return_ok body
    | `Not_found -> return_error NotPresent
    | _ -> return_error NetworkError

let test_manifest repository tag_or_digest =
    let (let*) = Lwt_result.bind in
    let uri = (Uri.path repository.base_url) ^ "/manifests/" ^ tag_or_digest |> Uri.with_path repository.base_url |> Uri.canonicalize in
    let headers = Cohttp.Header.init_with "Accept" "application/vnd.oci.image.manifest.v1+json" in
    let headers = Cohttp.Header.add headers "Accept" "application/vnd.oci.image.index.v1+json" in
    let* (response, _body) = try_authenticated_request repository ~headers `HEAD uri in
    match response.status, Http.Header.get response.headers "Docker-Content-Digest", Http.Header.get response.headers "Content-Type" with
    | (`OK, Some digest, Some "application/vnd.oci.image.manifest.v1+json") -> return_ok (Manifest, digest)
    | (`OK, Some digest, Some "application/vnd.oci.image.index.v1+json") -> return_ok (Index, digest)
    | (`Not_found, _, _) -> return_error NotPresent
    | _ -> return_error NetworkError

let pull_blob repository digest =
    let (let*) = Lwt_result.bind in
    let uri = (Uri.path repository.base_url) ^ "/blobs/" ^ digest |> Uri.with_path repository.base_url |> Uri.canonicalize in
    let headers = Cohttp.Header.init_with "Accept" "application/octet-stream" in
    let* (response, body) = try_authenticated_request repository ~headers `GET uri in
    match response.status with
    | `OK -> Cohttp_lwt.Body.to_stream body |> return_ok
    | `Not_found -> return_error NotPresent
    | _ -> return_error NetworkError

let test_blob repository digest =
    let (let*) = Lwt_result.bind in
    let uri = (Uri.path repository.base_url) ^ "/blobs/" ^ digest |> Uri.with_path repository.base_url |> Uri.canonicalize in
    let headers = Cohttp.Header.init_with "Accept" "application/octet-stream" in
    let* (response, _body) = try_authenticated_request repository ~headers `HEAD uri in
    match response.status, Http.Header.get response.headers "Docker-Content-Digest" with
    | (`OK, Some digest) -> return_ok digest
    | (`Not_found, _) -> return_error NotPresent
    | _ -> return_error NetworkError

let start_blob_upload_session repository =
    let (let*) = Lwt_result.bind in
    let uri = (Uri.path repository.base_url) ^ "/blobs/uploads/" |> Uri.with_path repository.base_url |> Uri.canonicalize in
    let* (response, _body) = try_authenticated_request repository `POST uri in
    match response.status, Http.Header.get response.headers "Location" with
    | (`Accepted, Some location) ->
        let default_scheme = Option.value (Uri.scheme uri) ~default:"https" in
        let location = Uri.of_string location in
        let upload_url = Uri.resolve default_scheme uri location in
        return_ok {upload_url}
    | _ -> return_error NetworkError

let upload_blob repository session ~digest blob =
    let (let*) = Lwt_result.bind in
    let uri = Uri.add_query_param' session.upload_url ("digest", digest) in
    let headers = Cohttp.Header.init_with "Content-Type" "application/octet-stream" in
    let headers = Cohttp.Header.add headers "Content-Length" (String.length blob |> Int.to_string) in
    let* (response, _body) = try_authenticated_request repository ~headers ~body:(`String blob) `PUT uri in
    match response.status, Http.Header.get response.headers "Location" with
    | (`Created, Some location) -> Uri.of_string location |> return_ok
    | _ -> return_error NetworkError

let push_manifest ~manifest_type repository tag_or_digest manifest =
    let (let*) = Lwt_result.bind in
    let uri = (Uri.path repository.base_url) ^ "/manifests/" ^ tag_or_digest |> Uri.with_path repository.base_url |> Uri.canonicalize in
    let mimetype = match manifest_type with
    | Manifest -> "application/vnd.oci.image.manifest.v1+json"
    | Index -> "application/vnd.oci.image.index.v1+json" in
    let headers = Cohttp.Header.init_with "Content-Type" mimetype in
    let headers = Cohttp.Header.add headers "Content-Length" (String.length manifest |> Int.to_string) in
    let* (response, _body) = try_authenticated_request repository ~headers ~body:(`String manifest) `PUT uri in
    match response.status, Http.Header.get response.headers "Location" with
    | (`Created, Some location) -> Uri.of_string location |> return_ok
    | _ -> return_error NetworkError

let parse_tag_result body : (string list, error) result =
    try
        Ok(YB.from_string body |> YBU.member "tags" |> YBU.convert_each YBU.to_string)
    with YBU.Type_error _ | Yojson.Json_error _ ->
        Error(NetworkError)

let find_link_next_header current_request_uri headers =
    (* We cannot use the inbuilt Cohttp link parsing, because it dislikes relative urls *)
    let (let*) = Option.bind in
    let link_headers = Cohttp.Header.get_multi headers "link" in
    let next_headers = List.filter (String.includes ~affix:"rel=\"next\"") link_headers in
    let* next_header = match next_headers with
    | [] -> None
    | (first :: _) -> Some first in
    let* next_link = Scanf.sscanf_opt next_header "<%s@>; %_s" Fun.id in
    let next_link = Uri.of_string next_link in
    let scheme = Option.value (Uri.scheme current_request_uri) ~default:"https" in
    Some (Uri.resolve scheme current_request_uri next_link)

let list_next_tags repository ?(max=30) next_tag_list =
    let (let*) = Lwt_result.bind in
    let uri = Uri.remove_query_param next_tag_list.next_search_url "n" in
    let uri = Uri.add_query_param' uri ("n", string_of_int max) in
    let headers = Cohttp.Header.init_with "Accept" "application/json" in
    let* (response, body) = try_authenticated_request repository ~headers `GET uri in
    let construct_next_search_url tags =
        match find_link_next_header uri response.headers, List.length tags with
        | (Some next_search_url, _) -> Some {next_search_url}
        | (None, n) when n >= max -> Some {next_search_url = Uri.add_query_param' (Uri.remove_query_param uri "last" ) ("last", List.nth tags (n-1))}
        | (_, _) -> None in
    match response.status with
    | `OK -> (
        let* tags = Cohttp_lwt.Body.to_string body |> Lwt.map parse_tag_result in
        let next_tag_list = construct_next_search_url tags in
        return_ok (tags, next_tag_list))
    | `Not_found -> return_error NotPresent
    | _ -> return_error NetworkError

let create_next_tag_list ~repository ~last =
    let uri = (Uri.path repository.base_url) ^ "/tags/list" |> Uri.with_path repository.base_url in
    match last with
    | None -> { next_search_url=uri }
    | Some (last) -> {next_search_url=Uri.add_query_param' uri ("last", last)}

let list_tags repository ?(last=None) ?(max=30) () =
    let next_tag_list = create_next_tag_list ~repository ~last in
    list_next_tags repository ~max next_tag_list

let stream_tags_paginated repository ?(last=None) ?(desired_page_size=30) () =
    let next_tag_list = Some (create_next_tag_list ~repository ~last) |> ref in
    let get_next () =
        match !next_tag_list with
        | None -> return None
        | Some(next) ->
            match%lwt list_next_tags repository ~max:desired_page_size next with
            | Error(_) -> return None
            | Ok([], _) ->
                next_tag_list := None;
                return None
            | Ok(tags, next) ->
                next_tag_list := next;
                return_some tags in
    Lwt_stream.from get_next

let stream_tags repository ?(last=None) ?(desired_page_size=30) () =
    stream_tags_paginated repository ~last ~desired_page_size () |> Lwt_stream.flatten
