;; open Gctx
(** A library of widgets for building GUIs. *)

type widget = {
  repaint: Gctx.gctx -> unit;
  handle: Gctx.gctx -> Gctx.event -> unit;
  size: unit -> Gctx.dimension
}

let space (p: Gctx.dimension) : widget =
  { repaint = (fun _ -> ());
    handle = (fun _ _ -> ());
    size = (fun _ -> p);
  }

(** A widget that adds a one-pixel border to an existing widget *)
let border (w: widget) : widget =
  { repaint = (fun (g: Gctx.gctx) ->
      let (width, height) = w.size () in
      let x = width + 3 in 
      let y = height + 3 in
      Gctx.draw_line g (0,0) (x,0);
      Gctx.draw_line g (0,0) (0, y);
      Gctx.draw_line g (x,0) (x, y);
      Gctx.draw_line g (0, y) (x, y);
      let g = Gctx.translate g (2,2) in
      w.repaint g);

    handle = (fun (g: Gctx.gctx) (e: Gctx.event) ->
      w.handle (Gctx.translate g (2,2)) e);

    size = (fun () ->
      let (width, height) = w.size () in
      width + 4, height + 4);
  }

(* A helper function that determines whether a given event is within a
   region of a widget whose upper-left hand corner is (0,0) with width
   w and height h.  *)
let event_within (g: Gctx.gctx) (e: Gctx.event)
    ((w, h): Gctx.dimension) : bool =
  let (mouse_x, mouse_y) = Gctx.event_pos e g in
  mouse_x >= 0 && mouse_x < w && mouse_y >= 0 && mouse_y < h

(** The hpair widget lays out two widgets horizontally, aligned at
   their top edges. *)
let hpair (w1:widget) (w2:widget) : widget = {
   repaint = (fun  (g:Gctx.gctx) -> w1.repaint g;
    let g = Gctx.translate g (fst (w1.size ()),0) in
      w2.repaint g);
   handle = (fun (g:Gctx.gctx) (e:Gctx.event) ->
    if event_within g e (w1.size ())
    then w1.handle g e
    else let g2 = (Gctx.translate g (fst (w1.size ()), 0)) in
         if event_within g2 e (w2.size ()) then w2.handle g2 e else ());
   size = (fun () -> let (x1,y1) = w1.size () in
          let (x2,y2) = w2.size () in (x1 + x2, max y1 y2))
   }

let vpair (w1: widget) (w2: widget) : widget = {
  repaint = (fun  (g:Gctx.gctx) -> w1.repaint g;
    let g = Gctx.translate g (0, snd (w1.size ())) in
      w2.repaint g);
   handle = (fun (g:Gctx.gctx) (e:Gctx.event) ->
    if event_within g e (w1.size ())
    then w1.handle g e
    else let g2 = (Gctx.translate g (0, snd (w1.size ()))) in
         if event_within g2 e (w2.size ()) then w2.handle g2 e else ());
   size = (fun () -> let (x1,y1) = w1.size () in
          let (x2,y2) = w2.size () in (max x1 x2, y1 + y2))
   }
   
let list_layout (pair: widget -> widget -> widget)
         (ws: widget list) : widget =
  List.fold_right (fun x acc -> pair x acc) ws (space (0, 0))
         
let hlist (ws: widget list) : widget = 
  list_layout hpair ws
  
let vlist (ws: widget list) : widget =
  list_layout vpair ws

type label_controller = { get_label : unit -> string;
                          set_label : string -> unit }

let label (s: string) : widget * label_controller =
  let r = { contents = s } in
  { repaint = (fun (g: Gctx.gctx) ->
        Gctx.draw_string g (0,0) r.contents);
    handle = (fun _ _ -> ());
    size = (fun () -> Gctx.text_size r.contents)
  },
  {
    get_label = (fun () -> r.contents);
    set_label = (fun (s: string) -> r.contents <- s);
  }



type event_listener = Gctx.gctx -> Gctx.event -> unit

(** Performs an action upon receiving a mouse click. *)
let mouseclick_listener (action: unit -> unit) : event_listener =
  fun (g: Gctx.gctx) (e: Gctx.event) ->
    if Gctx.event_type e = Gctx.MouseDown then action ()

(** Performs an action upon receiving a key press. *)
let key_listener (action: char -> unit) : event_listener =
  fun (g: Gctx.gctx) (e: Gctx.event) ->
    begin match Gctx.event_type e with
      | Gctx.KeyPress key -> action key
      | _ -> ()
    end

(** A notifier_controller is associated with a notifier widget.  It
   allows the program to add event listeners to the notifier. *)
type notifier_controller = {
  add_event_listener: event_listener -> unit
}

let notifier (w: widget) : widget * notifier_controller =
  let listeners = { contents = [] } in
  { repaint = w.repaint;
    handle =
      (fun (g: Gctx.gctx) (e: Gctx.event) ->
         List.iter (fun h -> h g e) listeners.contents;
         w.handle g e);
    size = w.size
  },
  { add_event_listener =
      fun (newl: event_listener) ->
        listeners.contents <- newl :: listeners.contents
  }

let button (s: string)
         : widget * label_controller * notifier_controller =
  let (w, lc) = label s in
  let (w', nc) = notifier w in
  (w', lc, nc)

let bare_canvas (dim: Gctx.dimension) (f : Gctx.gctx -> unit) 
         : widget =
  { repaint = f;
    handle = (fun _ _ -> ());
    size = (fun _ -> dim) }

let canvas (dim: Gctx.dimension) (f : Gctx.gctx -> unit)
         : widget * notifier_controller =
  let w = bare_canvas dim f in
  notifier (border w)

type 'a value_controller = {
  add_change_listener : ('a -> unit) -> unit;
  get_value           : unit -> 'a;
  change_value        : 'a -> unit
}

let rec helper_iterate (v: 'a) (listeners: ('a -> unit) list) : unit = 
  begin match listeners with
  | [] -> ()
  | x :: xs -> x v; helper_iterate v xs
  end
  
let make_controller (v: 'a) : 'a value_controller =
  let value = {contents = v } in
  let listeners = {contents = []} in
  { add_change_listener = (fun x -> listeners.contents <- 
                                    x :: listeners.contents);
    get_value = (fun () -> value.contents);
    change_value = (fun y -> value.contents <- y;
                             helper_iterate y listeners.contents)    
  }

let mk_box (switch: unit -> color) (s: string) : widget * notifier_controller =
  let (w_label,_) = label s in
  let (width, _) = w_label.size () in
  let repaint_box (g: gctx) : unit = 
    Gctx.draw_string g (0,0) s;
    let g1 = Gctx.translate g (width, 0) in
    Gctx.draw_rect g1 (0, 0) (20, 20);
    let g_new = switch () in
    draw_line (with_color g1 g_new) (0, 0) (20, 20);
    draw_line (with_color g1 g_new) (0, 20) (20, 0) in
  canvas (width + 20, 21) repaint_box
  
let checkbox (init: bool) (s: string) : widget * bool value_controller =
  let vcontroller = make_controller init in
  let (w, nc) = mk_box (fun () ->if vcontroller.get_value() then red else white)
                        (s) in
  nc.add_event_listener (mouseclick_listener (fun () -> vcontroller.change_value
                                              (not (vcontroller.get_value()))));
  (w, vcontroller)
  

let mk_slider (vcontroller : int value_controller) (s: string) : 
                                              widget * notifier_controller =
  let repaint_box (g: gctx) : unit = 
    Gctx.draw_rect g (0, 0) (100, 20);
    Gctx.fill_rect (with_color g black) (0, 0) 
                     (vcontroller.get_value (), 20) in
  canvas (100, 20) repaint_box

(** Slider widget*)
let slider (init: int) (s: string) : widget * int value_controller =
  let vcontroller = make_controller init in
  let (w, nc) = mk_slider vcontroller s in
  let listener () : event_listener = 
    fun (g : Gctx.gctx) (e: Gctx.event) -> 
      let (x_pos, y_pos) = event_pos e g in
      (*handles mouse dragging event*)
      begin match Gctx.event_type e with
      | MouseDrag -> if (x_pos < 3) (*avoids when thick < 1*)
                     then vcontroller.change_value (3) 
                     else  vcontroller.change_value (x_pos)
      | _ -> ()
      end in
  nc.add_event_listener (listener ());
  let (w_label, _) = label s in
  (hpair w_label w, vcontroller)
