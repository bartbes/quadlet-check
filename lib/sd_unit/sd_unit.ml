module Parser = Parser

module Stringtbl = Hashtbl.Make(struct
    type t = string
    let equal = String.equal
    let hash = Hashtbl.hash
end)

type t = string array Stringtbl.t Stringtbl.t

(* Internal function that returns the underlying array, without going through Seq.t *)
let get_array unit section key =
    match Stringtbl.find_opt unit section with
    | None -> [||]
    | Some (section) ->
            match Stringtbl.find_opt section key with
            | None -> [||]
            | Some (values) -> values

let get_all unit section key =
    get_array unit section key |> Array.to_seq

let get unit section key =
    match get_array unit section key with
    | [||] -> None
    | arr -> Some (((Array.length arr) - 1) |> Array.get arr)

let get_sections unit =
    Stringtbl.to_seq_keys unit

let get_keys unit section =
    match Stringtbl.find_opt unit section with
    | None -> Seq.empty
    | Some (section) -> Stringtbl.to_seq_keys section

let of_parse_records records =
    let unit = Stringtbl.create 1 in
    let write_record () {Parser.section = section_name; Parser.key; Parser.value} =
        let section = match Stringtbl.find_opt unit section_name with
        | Some (section) -> section
        | None -> (let new_section = Stringtbl.create 1 in Stringtbl.add unit section_name new_section; new_section) in
        let new_entry = match (Stringtbl.find_opt section key, value) with
        | (_, "") -> [||]
        | (None, value) | (Some [||], value) -> [|value|]
        | (Some old_entry, value) -> Array.append old_entry [|value|] in
        Stringtbl.replace section key new_entry in
    List.fold_left write_record () records;
    unit

let of_string s =
    Parser.parse s |> Result.map of_parse_records

let of_strings ss =
    let combine_result_of_list (acc : (Parser.record list, string) result) (item : (Parser.record list, string) result) =
        match acc, item with
        | Ok(prefix), Ok(suffix) -> Ok(prefix @ suffix)
        | Error(e), _
        | _, Error(e) -> Error(e) in
    List.map Parser.parse ss |>
    List.fold_left combine_result_of_list (Ok []) |>
    Result.map of_parse_records

let%test_module _ = (module struct
    let equals_unordered_string_seq expected_value value_to_test =
        let expected_value = List.stable_sort String.compare expected_value in
        let value_to_test = List.of_seq value_to_test |> List.stable_sort String.compare in
        List.equal String.equal expected_value value_to_test

    let equals_ordered_string_seq expected_value value_to_test =
        let value_to_test = List.of_seq value_to_test in
        List.equal String.equal expected_value value_to_test

    let%test _ =
        let v = of_parse_records [
            {Parser.section="a"; Parser.key="b"; Parser.value="c"};
        ] in
        get_sections v |>
        equals_unordered_string_seq ["a"]

    let%test _ =
        let v = of_parse_records [
            {Parser.section="a"; Parser.key="b"; Parser.value="c"};
            {Parser.section="c"; Parser.key="b"; Parser.value="c"};
            {Parser.section="a"; Parser.key="b"; Parser.value="c"};
        ] in
        get_sections v |>
        equals_unordered_string_seq ["a"; "c"]

    let%test _ =
        let v = of_parse_records [
            (* Sections with the same section name should be merged *)
            {Parser.section="a"; Parser.key="b"; Parser.value="c"};
            {Parser.section="c"; Parser.key="b"; Parser.value="c"};
            {Parser.section="a"; Parser.key="d"; Parser.value="c"};
        ] in
        (get_keys v "a" |> equals_unordered_string_seq ["b"; "d"])
        && (get_keys v "c" |> equals_unordered_string_seq ["b"])

    let%test _ =
        let v = of_parse_records [
            (* Duplicate keys should be joined into a list *)
            (* empty value means clear previous list *)
            {Parser.section="a"; Parser.key="b"; Parser.value="c"};
            {Parser.section="c"; Parser.key="b"; Parser.value="c"};
            {Parser.section="a"; Parser.key="b"; Parser.value="e"};
            {Parser.section="a"; Parser.key="d"; Parser.value="c"};
            {Parser.section="a"; Parser.key="d"; Parser.value="d"};
            {Parser.section="a"; Parser.key="d"; Parser.value=""};
            {Parser.section="a"; Parser.key="d"; Parser.value="x"};
        ] in
        (get_all v "a" "b" |> equals_ordered_string_seq ["c"; "e"])
        && (get_all v "a" "d" |> equals_ordered_string_seq ["x"])
        && (get_all v "a" "f" |> equals_ordered_string_seq [])

    let%test _ =
        let v = of_parse_records [
            (* get should return the last value if available *)
            (* and None if not *)
            (* Empty value should reset values, so return None after *)
            {Parser.section="a"; Parser.key="b"; Parser.value="c"};
            {Parser.section="a"; Parser.key="b"; Parser.value="e"};
            {Parser.section="a"; Parser.key="x"; Parser.value="g"};
            {Parser.section="a"; Parser.key="x"; Parser.value="h"};
            {Parser.section="a"; Parser.key="x"; Parser.value=""};
        ] in
        (get v "a" "b" |> Option.equal String.equal (Some "e"))
        && (get v "a" "c" |> Option.is_none)
        && (get v "a" "x" |> Option.is_none)
end)
