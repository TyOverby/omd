(** A markdown parser in OCaml. *)

type attributes = (string * string) list

type list_type = Ast.list_type =
  | Ordered of int * char
  | Bullet of char

type list_spacing = Ast.list_spacing =
  | Loose
  | Tight

type 'attr link = 'attr Ast.link =
  { label : 'attr inline
  ; destination : string
  ; title : string option
  }

and 'attr inline = 'attr Ast.inline =
  | Concat of 'attr * 'attr inline list
  | Text of 'attr * string
  | Emph of 'attr * 'attr inline
  | Strong of 'attr * 'attr inline
  | Code of 'attr * string
  | Hard_break of 'attr
  | Soft_break of 'attr
  | Link of 'attr * 'attr link
  | Image of 'attr * 'attr link
  | Html of 'attr * string

type 'attr def_elt = 'attr Ast.def_elt = 
  { term : 'attr inline
  ; defs : 'attr inline list
  }

type 'attr block = 'attr Ast.block =
  | Paragraph of 'attr * 'attr inline
  | List of 'attr * list_type * list_spacing * 'attr block list list
  | Blockquote of 'attr * 'attr block list
  | Thematic_break of 'attr
  | Heading of 'attr * int * 'attr inline
  | Code_block of 'attr * string * string
  | Html_block of 'attr * string
  | Definition_list of 'attr * 'attr def_elt list

type doc = attributes block list
(** A markdown document *)

val of_channel : in_channel -> doc

val of_string : string -> doc

val to_html : doc -> string

val to_sexp : doc -> string

val headers :
  ?remove_links:bool -> 'attr block list -> ('attr * int * 'attr inline) list

val toc : ?start:int list -> ?depth:int -> doc -> doc

module H: module type of Html
