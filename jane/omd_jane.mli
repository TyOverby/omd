open! Base

module Attrs : sig
  type t = (string * string) list [@@deriving sexp, equal, compare]
end

module List_type : sig
  type t =
    | Ordered of int * char
    | Bullet of char
  [@@deriving sexp, equal, compare]
end

module List_spacing : sig
  type t =
    | Loose
    | Tight
  [@@deriving sexp, equal, compare]
end

module Inline : sig
  type t =
    | Text of Attrs.t * string
    | Emph of Attrs.t * t list
    | Strong of Attrs.t * t list
    | Code of Attrs.t * string
    | Hard_break of Attrs.t
    | Soft_break of Attrs.t
    | Link of Attrs.t * link
    | Image of Attrs.t * link
    | Html of Attrs.t * string

  and link =
    { label : t list
    ; destination : string
    ; title : string option
    }
  [@@deriving sexp, equal, compare]
end

module Def_elt : sig
  type t =
    { term : Inline.t list
    ; defs : Inline.t list list
    }
  [@@deriving sexp, equal, compare]
end

module Heading : sig
  type t =
    { attrs : Attrs.t
    ; level : int
    ; content : Inline.t list
    }
  [@@deriving sexp, equal, compare]
end

module Block : sig
  type t =
    | Paragraph of Attrs.t * Inline.t list
    | List of
        { attrs : Attrs.t
        ; kind : List_type.t
        ; spacing : List_spacing.t
        ; blocks : t list list
        }
    | Blockquote of Attrs.t * t list
    | Thematic_break of Attrs.t
    | Heading of Heading.t
    | Code_block of
        { attrs : Attrs.t
        ; language : string
        ; content : string
        }
    | Html_block of
        { attrs : Attrs.t
        ; content_raw : string
        }
    | Definition_list of Attrs.t * Def_elt.t list
  [@@deriving sexp, equal, compare]
end

module Document : sig
  type t = Block.t list [@@deriving sexp, equal, compare]

  val of_string : string -> t
  val from_in_channel : Stdlib.in_channel -> t
  val table_of_contents : ?start:int list -> ?depth:int -> t -> t
  val headers : ?remove_links:bool -> t -> Heading.t list

  val map :
       ?map_inline:(Inline.t -> Inline.t)
    -> ?map_block:(Block.t -> Block.t)
    -> t
    -> t
end

module Html : sig
  module Node : sig
    type t =
      | Element of
          { tag : string
          ; attributes : Attrs.t
          ; children : t list
          }
      | Text of string
      | Raw of string
    [@@deriving sexp]
  end

  type t

  val of_document : Document.t -> t
  val to_string : t -> string
  val reveal : t -> Node.t list
  val conceal : Node.t list -> t
  val map : Node.t list -> f:(Node.t -> Node.t) -> Node.t list
end
