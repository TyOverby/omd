open! Base

module Attrs : sig
  type t = (string * string) list
end

module List_type : sig
  type t =
    | Ordered of int * char
    | Bullet of char
end

module List_spacing : sig
  type t =
    | Loose
    | Tight
end

module rec Link : sig
  type t =
    { label : Inline.t
    ; destination : string
    ; title : string option
    }
end

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
end

and Def_elt : sig
  type t =
    { term : Inline.t
    ; defs : Inline.t list
    }
end

and Heading : sig
  type t =
    { attrs : Attrs.t
    ; level : int
    ; content : Inline.t
    }
end

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
end

module Document : sig
  type t = Block.t list [@@deriving sexp_of]

  val of_string : string -> t
  val from_in_channel : Stdlib.in_channel -> t
  val table_of_contents : ?start:int list -> ?depth:int -> t -> t
  val headers : ?remove_links:bool -> t -> Heading.t list
end

module Html : sig
  type t =
    | Element of
        { tag : string
        ; attributes : Attrs.t
        ; children : t list
        }
    | Text of string
    | Raw of string
  [@@deriving sexp]

  val of_document : Document.t -> t list
  val to_string : ?pretty:bool -> t list -> string
end
