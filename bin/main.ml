module YB = Yojson.Basic
module YBU = Yojson.Basic.Util

module ProgressChar = struct
    type t = {
        mutable current : int
    }

    (* Assumption: all braille characters are printed as 3 utf-8 bytes *)
    let clear_str = "\b\b\b"
    let states = [| "⠋"; "⠇"; "⠦"; "⠴"; "⠸"; "⠙" |]

    let init_and_tick () =
        let state = { current = 0 } in
        Lwt_io.printf "%s" states.(state.current); %lwt
        Lwt_io.(flush stdout); %lwt
        Lwt.return state

    let tick state =
        state.current <- (match state.current + 1 with
        | n when n < Array.length states -> n
        | _ -> 0);
        Lwt_io.printf "%s%s" clear_str states.(state.current); %lwt
        Lwt_io.(flush stdout)

    let clear _state =
        Lwt_io.printf "%s" clear_str; %lwt
        Lwt_io.(flush stdout)
end

let parse_file filename =
    let to_unit content =
        match Sd_unit.of_string content with
        | Ok(unit) -> Ok(unit)
        | Error(error) -> Error("Error in '" ^ filename ^ "': " ^ error) in
    Lwt_io.chars_of_file filename |> Lwt_stream.to_string |> Lwt.map to_unit

let get_manifest_digest_from_index ~os ~arch index =
    let index = YB.from_string index in
    let combine_os_arch ~os ~arch =
        os ^ "/" ^ arch in
    let describe_manifest_entry m =
        let digest = m |> YBU.member "digest" |> YBU.to_string in
        let arch = m |> YBU.member "platform" |> YBU.member "architecture" |> YBU.to_string in
        let os = m |> YBU.member "platform" |> YBU.member "os" |> YBU.to_string in
        (combine_os_arch ~os ~arch, digest) in
    let manifests = index |> YBU.member "manifests" |> YBU.convert_each describe_manifest_entry in
    Lwt.return @@ List.assoc_opt (combine_os_arch ~os ~arch) manifests

let get_manifest_digest registry ~os ~arch tag =
    match%lwt Oci_registry.test_manifest registry tag with
    | Error(_) -> Lwt.return_none
    | Ok(Oci_registry.Manifest, digest) -> Lwt.return_some digest
    | Ok(Oci_registry.Index, _) ->
        match%lwt Oci_registry.pull_manifest ~manifest_type:Oci_registry.Index registry tag with
        | Error(_) -> Lwt.return_none
        | Ok(index) -> get_manifest_digest_from_index ~os ~arch index

(* Does it matter if we match the index instead of the manifest? I feel like it doesn't.
   Note that there is one edge case: if you're trying to compare a tagged manifest to a tagged index. *)
let get_manifest_digest registry tag =
    match%lwt Oci_registry.test_manifest registry tag with
    | Error(_) -> Lwt.return_none
    | Ok(Oci_registry.Manifest, digest) -> Lwt.return_some digest
    | Ok(Oci_registry.Index, digest) -> Lwt.return_some digest

let get_current_image file =
    let%lwt unit = Lwt_io.chars_of_file file |> Lwt_stream.to_string |> Lwt.map Sd_unit.of_string in
    match unit with
    | Ok(unit) ->
        Lwt.return @@ Sd_unit.get unit "Container" "Image"
    | Error(err) ->
        let%lwt () = Lwt_io.(fprintf stderr "Error parsing %s: %s\n" file err) in
        Lwt.return None

let determine_tags_of_digest ~known_tag ~digest ?(delay=0.2) registry =
    let tags = Oci_registry.stream_tags registry ~last:(Some known_tag) () in
    let%lwt progress = ProgressChar.init_and_tick () in
    let check_tag tag =
        Lwt_unix.sleep delay ;%lwt
        ProgressChar.tick progress ;%lwt
        match%lwt get_manifest_digest registry tag with
        | Some(tag_digest) when String.equal tag_digest digest -> Lwt.return_true
        | _ -> Lwt.return_false in
    let%lwt tags = Lwt_stream.filter_s check_tag tags |> Lwt_stream.to_list in
    ProgressChar.clear progress ;%lwt
    Lwt.return tags

let check_file file =
    let filename = Fpath.v file |> Fpath.normalize |> Fpath.filename in
    let%lwt () = Lwt_io.printf "\x1b[1mTrying file %s\x1b[0m\n" filename in
    let check_tagged_image image tag =
        let registry = Oci_registry.of_image_name image in
        let%lwt latest_digest = get_manifest_digest registry "latest" in
        let%lwt current_digest = get_manifest_digest registry tag in
        match (latest_digest, current_digest) with
        | (None, _) ->
            Lwt_io.printf "  \x1b[31mCould not determine digest of \x1b[3mlatest\x1b[0m\n"
        | (_, None) ->
            Lwt_io.printf "  \x1b[31mCould not determine digest of \x1b[3m%s\x1b[0m\n" tag
        | (Some(latest_digest), Some(current_digest)) when String.equal latest_digest current_digest ->
            Lwt_io.printf "  \x1b[32mDigest of \x1b[3m%s\x1b[23m matches \x1b[3mlatest\x1b[23m (%s)]\x1b[0m\n" tag current_digest
        | (Some(latest_digest), Some(current_digest)) ->
            Lwt_io.printf "  Digest of \x1b[3mlatest\x1b[23m is %s\n" latest_digest ;%lwt
            Lwt_io.printf "  Digest of \x1b[3m%s\x1b[23m is %s\n" tag current_digest ;%lwt
            match%lwt determine_tags_of_digest ~known_tag:tag ~digest:latest_digest registry with
            | [] -> Lwt_io.printf "  \x1b[31mNo tags found!\x1b[0m\n"
            | tags -> String.concat "\x1b[23m, \x1b[3m" tags |> Lwt_io.printf "  Matching tags: [\x1b[3m%s\x1b[23m]\n" in
    let check_image image =
        match String.split_last ~sep:":" image with
        | None -> Lwt_io.printf "  \x1b[31mFound image %s in %s, but could not find tag\x1b[0m\n" image filename
        | Some (image, tag) -> 
            Lwt_io.printf "  Found image \x1b[1m%s\x1b[0m with tag \x1b[3m%s\x1b[0m\n" image tag ;%lwt
            check_tagged_image image tag in
    try%lwt
        match%lwt get_current_image file with
        | None -> Lwt_io.printf "  \x1b[31mCould not find image in %s\x1b[0m\n" filename
        | Some image -> check_image image
    with Failure f ->
        Lwt_io.printf "  \x1b[31mCaught failure: %s\x1b[0m\n" f

let main () =
    let files = Array.to_seq Sys.argv |> Seq.drop 1 |> Lwt_seq.of_seq in
    Lwt_seq.iter_s check_file files

let () = Lwt_main.run (main ())
