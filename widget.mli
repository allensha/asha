;; open Gctx

type widget = {
  repaint : Gctx.gctx -> unit;
  handle  : Gctx.gctx -> Gctx.event -> unit;
  size    : unit -> Gctx.dimension;
}

val space : Gctx.dimension -> widget

(** Adds a border around another widget *)
val border : widget -> widget

(** A pair of horizontally adjacent widgets *)
val hpair : widget -> widget -> widget

(** A pair of vertically adjacent widgets *)
val vpair : widget -> widget -> widget

(** A horizontal group of widgets *)
val hlist : widget list -> widget

(** A vertical group of widgets *)
val vlist : widget list -> widget

(** A record of functions that allows us to read and write the string 
    associated with a label. *)
type label_controller = { get_label : unit -> string;
                          set_label : string -> unit }         

(** Construct a label widget and its controller. *)
val label : string -> widget * label_controller

(** An event listener processes events as they "flow" through
   the widget hierarchy. *)
type event_listener = Gctx.gctx -> Gctx.event -> unit

(** Performs an action upon receiving a mouse click. *)
val mouseclick_listener : (unit -> unit) -> event_listener

(** A notifier_controller is associated with a notifier widget.
   It allows the program to add event listeners to the notifier.
*)
type notifier_controller = { add_event_listener : event_listener -> unit; }

(** Construct a notifier widget and its controller *)
val notifier : widget -> widget * notifier_controller

val button : string -> widget * label_controller * notifier_controller

val bare_canvas : Gctx.dimension -> (Gctx.gctx -> unit) -> widget
val canvas : Gctx.dimension -> (Gctx.gctx -> unit) -> widget * notifier_controller

type 'a value_controller = {
  add_change_listener : ('a -> unit) -> unit;
  get_value           : unit -> 'a;
  change_value        : 'a -> unit
  }


val helper_iterate : 'a -> ('a -> unit) list -> unit 
(** A utility function for creating a value_controller. *)
val make_controller : 'a -> 'a value_controller

(** A checkbox widget. *)     
val mk_box : (unit -> color) -> string -> widget * notifier_controller

val checkbox : bool -> string -> widget * bool value_controller

(** A slider widget. *)     
val mk_slider : int value_controller -> string -> widget * notifier_controller

val slider : int -> string -> widget * int value_controller
