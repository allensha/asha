(** The main (abstract) type of graphics contexts. *)
type gctx

(** A type for colors, specified by red, green and blue values in 
    the range [0 .. 255] inclusive *) 		
type color = {r:int; g:int; b:int}

(* Some predefined colors *)	 
val black   : color
val white   : color
val red     : color
val green   : color
val blue    : color
val yellow  : color
val cyan    : color
val magenta : color

(*******************************)
(** {1 Basic Gctx operations } *)
(*******************************)

(** Open the graphics window *)
val open_graphics : unit -> unit

(** Clear the graphics window *)
val clear_graph : unit -> unit
  
(** The top-level graphics context *)
val top_level : gctx

(** Produce a new gctx shifted  by (dx,dy) *)
val translate : gctx -> int * int -> gctx

(** Produce a new gctx with a different pen color *)
val with_color : gctx -> color -> gctx

(** Produce a new gctx with a different pen thickness *)
val with_thickness : gctx -> int -> gctx

(** A widget-relative position *)
type position = int * int

(** A width and height paired together. *)
type dimension = int * int

(** Draw a line between the two specified positions *)
val draw_line : gctx -> position -> position -> unit

(** Display text at the given position *)
val draw_string : gctx -> position -> string -> unit


(** Draw a rectangle, with upper-left corner at position of the specified
    dimension. *)
val draw_rect : gctx -> position -> dimension -> unit

(** Display a filled rectangle with upper-left corner at position with the
    specified dimension. *)
val fill_rect : gctx -> position -> dimension -> unit

(** Draw an elipse, centered at position with given x and y radii. *)
val draw_ellipse : gctx -> position -> int -> int -> unit

val find_ellipse_center : position -> position -> position

val find_ellipse_rx : position -> position -> int

val find_ellipse_ry : position -> position -> int

(** Calculates the size of text when rendered. *)
val text_size : string -> dimension

val draw_point : gctx -> position -> unit

val draw_points : gctx -> position list -> unit

type event_type =
  | KeyPress of char     (* User pressed the following key     *)
  | MouseDown            (* Mouse button pressed, no movement  *)		  
  | MouseUp              (* Mouse button released, no movement *)
  | MouseMove            (* Mouse moved with button up         *)	 
  | MouseDrag            (* Mouse moved with button down       *)

(** Events produced by the user-interface. Each event contains a 
    type and a position. *)		
    
type event = event_type * position
		
val event_type : event -> event_type

val event_pos  : event -> gctx -> position

val make_test_event : event_type -> position -> event

