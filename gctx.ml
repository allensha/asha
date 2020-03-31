(** The "Graphics Context" component of the GUI library. *)

(** A graphics context represents a region of the window to which
   widgets will be drawn.

   The drawing primitives in this module are all relative to the
   graphics context. This means that when a widget needs to draw on
   the screen, it need not know its absolute position. The graphics
   context is responsible for translating the relative positions
   passed into the drawing routines into absolute positions on the
   screen.

   The graphics context also includes other information for basic
   drawing (such as the current pen color.)

   Note that this module defines a persistent (immutable) data
   structure. The operations here use a given graphics context to
   create a new one with the specified characteristics. They do not
   modify their arguments. *)

(* (We use the module name Graphics in this module to refer to a "shim
   module" that connects to either the native or the javascript
   graphics.  You do not need to understand the details of how this
   works.) *)
module Graphics = G

(****************)
(**   Colors    *)
(****************)

(** A type for colors *)
type color = {r:int; g:int; b:int}

let black   : color = {r=0;  g=0;  b=0}
let white   : color = {r=255;g=255;b=255}
let red     : color = {r=255;g=0;  b=0}
let green   : color = {r=0;  g=255;b=0}
let blue    : color = {r=0;  g=0;  b=255}
let yellow  : color = {r=255;g=255;b=0}
let cyan    : color = {r=0;  g=255;b=255}
let magenta : color = {r=255;g=0;  b=255}

(** A type for thickness*)

type thickness = { t: int}
(*******************************)
(**    Basic Gctx operations   *)
(*******************************)

(** The main type of graphics contexts. Note that none of the
   components are mutable. (TODO: You will need to modify this type
   definition when you get to Task 5.) *)
type gctx = {
  x: int;         (** offset from (0,0) in local coordinates *)
  y: int;
  color: color;   (** the current pen color *)
  thickness: int;
  }

(* Internal helper to set the text size *)
let set_text_size (text_size: int) (font: string) : unit =
  if Graphics.js_mode then Graphics.set_font font;
  Graphics.set_text_size text_size

let clear_graph () =
  Graphics.clear_graph ();
  set_text_size 20 (if Graphics.js_mode then "sans-serif" else "")

(* Has the graphics window been opened already? *)
let graphics_opened = {contents = false}

(** Open the graphics window (but only do it once) *)
let open_graphics () =
  if not graphics_opened.contents then
    begin
      graphics_opened.contents <- true;
      Graphics.open_graph
        (if Graphics.js_mode then "width=640,height=480" else "");
      Graphics.resize_window 640 480;
      if not Graphics.js_mode then Graphics.auto_synchronize false;
      clear_graph ()
    end

(** The top-level graphics context *)
let top_level : gctx =
  { x = 0;
    y = 0;
    color = black;
    thickness = 1;
     }

(** Shift the gctx by (dx,dy).  Used by widgets to translate (shift
   the origin of) a graphics context to obtain an appropriate graphics
   context for their children. *)
let translate (g: gctx) ((dx, dy): int * int) : gctx =
  { g with x = g.x + dx; y = g.y + dy }

(** Produce a new Gctx.t with a different pen color *)
let with_color (g: gctx) (c: color) : gctx =
  { g with color = c }
  
(** Produce a new gctx with a different pen thickness *)
let with_thickness (g: gctx) (t: int) : gctx =
  { g with thickness = t }



(** Set the OCaml graphics library's internal state according to the
   Gctx settings.  Initially, this just sets the current pen color. *)
let set_graphics_state (gc: gctx) : unit =
  let c = gc.color in
  let t = gc.thickness in
  Graphics.set_color (Graphics.rgb c.r c.g c.b);
  Graphics.set_line_width t


(************************************)
(*    Coordinate Transformations    *)
(************************************)

(* The default width and height of the graphics window that OCaml opens.   *)

let graphics_size_x () =
  if graphics_opened.contents then Graphics.size_x () else 640
let graphics_size_y () =
  if graphics_opened.contents then Graphics.size_y () else 480

(** A widget-relative position *)
type position = int * int


(** Convert widget-local coordinates (x,y) to OCaml graphics
    coordinates, relative to the graphics context. *)
let ocaml_coords (g: gctx) ((x, y): position) : (int * int) =
  (g.x + x, graphics_size_y () - 1 - (g.y + y))

(** Convert OCaml Graphics coordinates (x,y) to widget-local graphics
    coordinates, relative to the graphics context *)
let local_coords (g: gctx) ((x, y): int * int) : position =
  (x - g.x, (graphics_size_y () - 1 - y) - g.y)


(*****************)
(**    Drawing   *)
(*****************)

(** A width and height, paired together. *)
type dimension = int * int

(** Draw a line between the two specified positions *)
let draw_line (g: gctx) (p1: position) (p2: position) : unit =
  set_graphics_state g;
  let (x1, y1) = ocaml_coords g p1 in
  let (x2, y2) = ocaml_coords g p2 in
  Graphics.moveto x1 y1;
  Graphics.lineto x2 y2

(** Display text at the given position *)
let draw_string (g: gctx) (p: position) (s: string) : unit =
  set_graphics_state g;
  let (_, height) = Graphics.text_size s in
  let (x, y) = ocaml_coords g p in
  (* Web browser font rendering bounding box adjusment *)
  let fudge = if Graphics.js_mode then 3 else 0 in 
  (* subtract: working with Ocaml coordinates *)
  Graphics.moveto x (y - height + fudge);
  Graphics.draw_string s
                                 *)
let draw_rect (g: gctx) (p1: position) ((w, h): dimension) : unit =
  set_graphics_state g;
  let (x1, y1) = ocaml_coords g p1 in
  Graphics.draw_rect x1 (y1 - h) w h

let fill_rect (g: gctx) (p1: position) ((w, h): dimension) : unit =
  set_graphics_state g;
  let (x, y) = ocaml_coords g p1 in
  Graphics.fill_rect x (y - h) w h

let draw_ellipse (g: gctx) (p: position) (rx: int) (ry: int) : unit =
  set_graphics_state g;
  let (x1, y1) = ocaml_coords g p in
  Graphics.draw_ellipse x1 y1 rx ry
  
let find_ellipse_center (p1: position) (p2: position) : position =
  let (x1, y1) = p1 in
  let (x2, y2) = p2 in
  let xcenter = ((x2 - x1) / 2) + x1 in
  let ycenter = ((y2 - y1) / 2) + y1 in
  (xcenter, ycenter)
  
let find_ellipse_rx (p1: position) (p2: position) : int =
  let (x1, _) = p1 in
  let (x2, _) = p2 in
  abs (x2 - x1) / 2
  
let find_ellipse_ry (p1: position) (p2: position) : int =
  let (_, y1) = p1 in
  let (_, y2) = p2 in
  abs (y2 - y1) / 2
  
let draw_point (g: gctx) (p: position) : unit = 
  set_graphics_state g;
  let (x1, y1) = ocaml_coords g p in
  Graphics.plot x1 y1
  
let draw_points (g : gctx) (pl : position list) : unit =
  set_graphics_state g;
  List.iter (draw_point g) pl
  
(** Calculates the size of a text when rendered. *)
let text_size (text: string) : dimension =
  open_graphics ();
  let (w,h) = Graphics.text_size text in
  (w+1, h)  (* Web browser font widths seem to be smaller than desirable *)
  
(************************)
(**    Event Handling   *)
(************************)
type event_type =
  | KeyPress of char    (* Key pressed on the keyboard.      *)
  | MouseDown           (* Mouse button pressed.             *)
  | MouseUp             (* Mouse button released.            *)
  | MouseMove           (* Mouse moved with the button up.   *)
  | MouseDrag           (* Mouse moved with the button down. *)

let string_of_event_type (et : event_type) : string =
  begin match et with
    | KeyPress k -> "KeyPress at " ^ (String.make 1 k)
    | MouseDrag  -> "MouseDrag"
    | MouseMove  -> "MouseMove"
    | MouseUp    -> "MouseUp"
    | MouseDown  -> "MouseDown"
  end

(** An event records its type and the widget-local position of
    the mouse when the event occurred. *)
type event = event_type * position

(** Accessor for the type of an event. *)
let event_type (e: event) : event_type =
  fst e

(** Accessor for the widget-local position of an event. *)
let event_pos (e: event) (g : gctx) : position =
  local_coords g (snd e)

(** Convert an event to a string *)
let string_of_event ((ty, (x, y)): event) : string =
  (string_of_event_type ty) ^ " at "
     ^ (string_of_int x) ^ ","
     ^ (string_of_int y)

(** Make an event by hand for testing. *)
let make_test_event (et : event_type) ((x, y) : position) =
  (et, (x, graphics_size_y () - y))
