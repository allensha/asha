;; open Gctx
(** A library of widgets for building GUIs. *)
(***********)
(* Widgets *)
(***********)

(** A widget is an object that provides three services:
   - it can repaint itself (given an appropriate graphics context)
   - it can handle events
   - it knows its dimensions (relative to a graphics context)  *)
type widget = {
  repaint : Gctx.gctx -> unit;
  handle  : Gctx.gctx -> Gctx.event -> unit;
  size    : unit -> Gctx.dimension;
}

(************************)
(** {1 Layout Widgets } *)
(************************)

(** Layout widgets are containers for other widgets
   They route events to the appropriate child widget, translating
   the events to the child's local coordinate system *)

(** A widget that does nothing but take up space *)
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


(*****************************)
(** {1    Label Widgets    } *)
(*****************************)


(** A record of functions that allows us to read and write the string 
    associated with a label. *)
type label_controller = { get_label : unit -> string;
                          set_label : string -> unit }         

(** Construct a label widget and its controller. *)
val label : string -> widget * label_controller


(*****************************************)
(** {1   Event Listeners               } *)
(*****************************************)

(** An event listener processes events as they "flow" through
   the widget hierarchy. *)
type event_listener = Gctx.gctx -> Gctx.event -> unit

(** Performs an action upon receiving a mouse click. *)
val mouseclick_listener : (unit -> unit) -> event_listener


(*****************************************)
(** {1   Notifier                      } *)
(*****************************************)

(** A notifier widget is a widget "wrapper" that doesn't take up any
   extra screen space -- it extends an existing widget with the
   ability to react to events.  It maintains a list of of "listeners"
   that eavesdrop on the events propagated through the notifier
   widget.

   When an event comes in to the notifier, it is passed to each
   event_listener in turn, and then pass to the child widget.
*)

(** A notifier_controller is associated with a notifier widget.
   It allows the program to add event listeners to the notifier.
*)
type notifier_controller = { add_event_listener : event_listener -> unit; }

(** Construct a notifier widget and its controller *)
val notifier : widget -> widget * notifier_controller


(*****************************************)
(** {1   Button                        } *)
(*****************************************)

(** A button has a string, which can be controlled by the
   corresponding label_controller, and an event listener, which can be
   controlled by the notifier_controller to add listeners (e.g. a
   mouseclick_listener) that will perform an action when the button is
   pressed.  *)
val button : string -> widget * label_controller * notifier_controller


(*****************************************)
(** {1   Canvas                        } *)
(*****************************************)
(** A widget that allows drawing to a graphics context *)

(** Canvases are just widgets with an associated notifier_controller.
   The repaint method of a canvas is a parameter of the widget
   constructor.
*)
val bare_canvas : Gctx.dimension -> (Gctx.gctx -> unit) -> widget
val canvas : Gctx.dimension -> (Gctx.gctx -> unit) -> widget * notifier_controller


(*****************************************)
(** {1   Checkbox                      } *)
(*****************************************)

(** A controller for a value associated with a widget. 

    This controller can read and write the value. It also allows 
    change listeners to be registered by the application. These listeners are 
    run whenever this value is set. *)
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



