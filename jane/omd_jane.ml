open! Base
module Ls = Soup

module Attrs = struct
  type t = (string * string) list [@@deriving sexp]
end

module List_type = struct
  type t = Omd.list_type =
    | Ordered of int * char
    | Bullet of char
end

module List_spacing = struct
  type t = Omd.list_spacing =
    | Loose
    | Tight
end

module rec Link : sig
  type t =
    { label : Inline.t
    ; destination : string
    ; title : string option
    }
end =
  Link

and Inline : sig
  type t =
    | Concat of Attrs.t * t list
    | Text of Attrs.t * string
    | Emph of Attrs.t * t
    | Strong of Attrs.t * t
    | Code of Attrs.t * string
    | Hard_break of Attrs.t
    | Soft_break of Attrs.t
    | Link of Attrs.t * Link.t
    | Image of Attrs.t * Link.t
    | Html of Attrs.t * string
end =
  Inline

and Def_elt : sig
  type t =
    { term : Inline.t
    ; defs : Inline.t list
    }
end =
  Def_elt

and Heading : sig
  type t =
    { attrs : Attrs.t
    ; level : int
    ; content : Inline.t
    }
end =
  Heading

and Block : sig
  type t =
    | Paragraph of Attrs.t * Inline.t
    | List of
        { attrs : Attrs.t
        ; kind : List_type.t
        ; spacing : List_spacing.t
        ; blocks : Block.t list list
        }
    | Blockquote of Attrs.t * Block.t list
    | Thematic_break of Attrs.t
    | Heading of Heading.t
    | Code_block of
        { attrs : Attrs.t
        ; language : string
        ; content : string
        }
    | Html_block of
        { tag : string
        ; attrs : Attrs.t
        ; content : t list
        ; content_raw : string
        }
    | Definition_list of Attrs.t * Def_elt.t list
end =
  Block

let rec parse_string_html s =
  match Ls.child_element (Ls.parse s) with
  | None -> assert false
  | Some element ->
      let tag = Ls.name element in
      let attrs =
        Ls.fold_attributes (fun acc key value -> (key, value) :: acc) [] element
      in
      let content =
        element
        |> Ls.children
        |> Ls.to_list
        |> List.map ~f:Ls.to_string
        |> String.concat ~sep:"\n"
        |> parse_doc
      in
      Block.Html_block { tag; attrs; content; content_raw = s }

and link_forward : Attrs.t Omd.link -> Link.t =
 fun { label; destination; title } ->
  let label = inline_forward label in
  { label; destination; title }

and inline_forward : Attrs.t Omd.inline -> Inline.t = function
  | Concat (attrs, inlines) -> Concat (attrs, List.map inlines ~f:inline_forward)
  | Text (attrs, text) -> Text (attrs, text)
  | Emph (attrs, t) -> Emph (attrs, inline_forward t)
  | Strong (attrs, t) -> Strong (attrs, inline_forward t)
  | Code (attrs, string) -> Code (attrs, string)
  | Hard_break attrs -> Hard_break attrs
  | Soft_break attrs -> Soft_break attrs
  | Link (attrs, link) -> Link (attrs, link_forward link)
  | Image (attrs, link) -> Image (attrs, link_forward link)
  | Html (attrs, string) -> Html (attrs, string)

and def_elt_forward : Attrs.t Omd.def_elt -> Def_elt.t =
 fun { term; defs } ->
  { term = inline_forward term; defs = List.map defs ~f:inline_forward }

and block_forward : Attrs.t Omd.block -> Block.t = function
  | Paragraph (attrs, inline) -> Paragraph (attrs, inline_forward inline)
  | List (attrs, kind, spacing, blocks) ->
      let blocks = List.map blocks ~f:(List.map ~f:block_forward) in
      List { attrs; kind; spacing; blocks }
  | Blockquote (attrs, blocks) ->
      let blocks = List.map blocks ~f:block_forward in
      Blockquote (attrs, blocks)
  | Thematic_break attrs -> Thematic_break attrs
  | Heading (attrs, level, content) ->
      let content = inline_forward content in
      Heading { attrs; level; content }
  | Code_block (attrs, language, content) ->
      Code_block { attrs; language; content }
  | Html_block (_, content) -> parse_string_html content
  | Definition_list (attrs, defs) ->
      let defs = List.map defs ~f:def_elt_forward in
      Definition_list (attrs, defs)

and conv_forward t = List.map t ~f:block_forward
and parse_doc s = conv_forward (Omd.of_string s)

let rec link_backward : Link.t -> Attrs.t Omd.link =
 fun { label; destination; title } ->
  let label = inline_backward label in
  { label; destination; title }

and inline_backward : Inline.t -> Attrs.t Omd.inline = function
  | Concat (attrs, inlines) ->
      Concat (attrs, List.map inlines ~f:inline_backward)
  | Text (attrs, text) -> Text (attrs, text)
  | Emph (attrs, t) -> Emph (attrs, inline_backward t)
  | Strong (attrs, t) -> Strong (attrs, inline_backward t)
  | Code (attrs, string) -> Code (attrs, string)
  | Hard_break attrs -> Hard_break attrs
  | Soft_break attrs -> Soft_break attrs
  | Link (attrs, link) -> Link (attrs, link_backward link)
  | Image (attrs, link) -> Image (attrs, link_backward link)
  | Html (attrs, string) -> Html (attrs, string)

and def_elt_backward : Def_elt.t -> Attrs.t Omd.def_elt =
 fun { term; defs } ->
  { term = inline_backward term; defs = List.map defs ~f:inline_backward }

and block_backward : Block.t -> Attrs.t Omd.block = function
  | Paragraph (attrs, inline) -> Paragraph (attrs, inline_backward inline)
  | List { attrs; kind; spacing; blocks } ->
      let blocks = List.map blocks ~f:(List.map ~f:block_backward) in
      List (attrs, kind, spacing, blocks)
  | Blockquote (attrs, blocks) ->
      let blocks = List.map blocks ~f:block_backward in
      Blockquote (attrs, blocks)
  | Thematic_break attrs -> Thematic_break attrs
  | Heading { attrs; level; content } ->
      let content = inline_backward content in
      Heading (attrs, level, content)
  | Code_block { attrs; language; content } ->
      Code_block (attrs, language, content)
  | Html_block { content_raw; _ } -> Html_block ([], content_raw)
  | Definition_list (attrs, defs) ->
      let defs = List.map defs ~f:def_elt_backward in
      Definition_list (attrs, defs)

module Document = struct
  type t = Block.t list

  let conv_forward t = List.map t ~f:block_forward
  let conv_backward t = List.map t ~f:block_backward
  let of_string s = conv_forward (Omd.of_string s)
  let from_in_channel c = conv_forward (Omd.of_channel c)

  let sexp_of_t t =
    t |> conv_backward |> Omd.to_sexp |> Parsexp.Single.parse_string_exn

  let table_of_contents ?start ?depth t =
    conv_forward (Omd.toc ?start ?depth (conv_backward t))

  let headers ?remove_links t =
    let headings = Omd.headers ?remove_links (conv_backward t) in
    List.map headings ~f:(fun (attrs, level, content) ->
        { Heading.attrs; level; content = inline_forward content })
end

module Html = struct
  module Omd = struct
    include Omd
    module Html = Omd.H
  end

  type t =
    | Element of
        { tag : string
        ; attributes : Attrs.t
        ; children : t list
        }
    | Text of string
    | Raw of string
  [@@deriving sexp]

  let kind_of_tag : string -> Omd.Html.element_type = function
    | "a"
    | "img"
    | "em"
    | "strong"
    | "code"
    | "br" ->
        Inline
    | _ -> Block

  let rec forward : Omd.Html.t -> t list = function
    | Text s -> [ Text s ]
    | Raw s -> [ Raw s ]
    | Null -> []
    | Concat (a, b) -> forward a @ forward b
    | Element (_kind, tag, attributes, content) ->
        let children =
          match content with
          | None -> []
          | Some t -> forward t
        in
        [ Element { tag; attributes; children } ]

  let rec backward : t list -> Omd.Html.t = function
    | [] -> Null
    | [ Text s ] -> Text s
    | [ Raw s ] -> Raw s
    | [ Element { tag; attributes; children } ] ->
        let kind = kind_of_tag tag in
        let children =
          match backward children with
          | Null -> None
          | other -> Some other
        in
        Element (kind, tag, attributes, children)
    | many ->
        let left, right = List.split_n many (List.length many / 2) in
        Concat (backward left, backward right)

  let of_document t = forward (Omd.Html.of_doc (Document.conv_backward t))

  let to_string ?(pretty = false) t =
    let s = t |> backward |> Omd.Html.to_string in
    if pretty then
      s |> Ls.parse |> Ls.pretty_print
    else
      s
end
