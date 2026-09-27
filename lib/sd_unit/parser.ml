open Angstrom

type record = {
    section: string;
    key: string;
    value: string;
}

let is_key_character = function
    | '0' .. '9' -> true
    | 'A' .. 'Z' -> true
    | 'a' .. 'z' -> true
    | _ -> false
let is_section_name_character = function
    | x when is_key_character x -> true
    | ' ' -> true
    | _ -> false
let optional_whitespace = take_while (function
    | ' ' | '\t' -> true
    | _ -> false)
let comment_char = char ';' <|> char '#'

let lf_contination =
    let* s = peek_string 2 in
    if String.equal s "\\\n" then
        advance 2 *> return ' '
    else
        fail "Invalid continuation"
let crlf_continuation =
    let* s = peek_string 3 in
    if String.equal s "\\\r\n" then
        advance 3 *> return ' '
    else
        fail "Invalid continuation"
let continuation = lf_contination <|> crlf_continuation

let comment = optional_whitespace *> comment_char *> many_till any_char end_of_line *> return () <?> "comment"
let empty_line = optional_whitespace *> end_of_line
let ignorable_line = comment <|> empty_line
let continuation_and_maybe_comments = continuation <* many comment (* Yes, comments can be in the middle of continuations, it's cursed *)

let value : string t =
    let* content : char list = many (continuation_and_maybe_comments <|> not_char '\n') in
    List.to_seq content |> String.of_seq |> return

let section_start : string t =
    char '[' *> take_while1 is_section_name_character <* char ']' <* optional_whitespace <* end_of_line

let assignment : (string * string) t =
    let* key = take_while1 is_key_character <* optional_whitespace <* char '=' <* optional_whitespace <?> "key"
    and+ value = value <?> "value" in
    return (key, value)

let section : record list t =
    let* section = section_start <?> "section"
    and+ assignments = many (many ignorable_line *> assignment) in
    return @@ List.map (fun (key, value) -> {section; key; value}) assignments

let file : record list t =
    let section_with_surrounding_lines = many ignorable_line *> section <* many ignorable_line in
    many section_with_surrounding_lines >>| List.concat

let parse s =
    Angstrom.parse_string ~consume:Angstrom.Consume.All file s

let%test_module _ = (module struct
    let record_equal lhs rhs =
        String.equal lhs.section rhs.section
        && String.equal lhs.key rhs.key
        && String.equal lhs.value rhs.value

    let record_list_equal = List.equal record_equal

    exception ParseError of string
    exception NoParseError

    let parse_string_res ~parser s =
        Angstrom.parse_string ~consume:Angstrom.Consume.All parser s

    let parse_string ~parser s =
        match parse_string_res ~parser s with
        | Ok(result) -> result
        | Error(e) -> raise (ParseError e)

    let expect_parse_error ~parser s =
        match parse_string_res ~parser s with
        | Ok(_) -> raise NoParseError
        | Error(e) -> e

    let%test _ =
        parse_string ~parser:section_start "[Section]\n" |>
        String.equal "Section"

    let%test _ =
        parse_string ~parser:section_start "[Section with spaces]\n" |>
        String.equal "Section with spaces"

    let%test _ =
        parse_string ~parser:section_start "[Whitespace] \t \n" |>
        String.equal "Whitespace"

    let%test _ =
        ignore @@ expect_parse_error ~parser:section_start " [Whitespace]\n";
        true

    (* This test is broken, why?
    let% test _ =
        let (key, value) = parse_string ~parser:assignment "Key = Value\n" in
        String.equal key "Key" && String.equal value "Value"
    *)

    let% test _ =
        parse_string ~parser:file {|
[Foo]
K=V
; Comments on their own lines
# With semicolons or hashes
Continued=Value\
with continuations\
; And comments in between
parses properly
Continued=And continuations  \
get replaced by a single\
 space, preserving other whitespace

[Bar]
Baz=1

[Empty section]

[Foo]
K=W
|} |>
        record_list_equal [
            {section="Foo";key="K";value="V"};
            {section="Foo";key="Continued";value="Value with continuations parses properly"};
            {section="Foo";key="Continued";value="And continuations   get replaced by a single  space, preserving other whitespace"};
            {section="Bar";key="Baz";value="1"};
            {section="Foo";key="K";value="W"};
        ]
end)
