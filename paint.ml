  (** The main paint application *)

;; open Gctx
;; open Widget

(** A location in the paint_canvas widget *)
type point = position (* from Gctx *)

type shape = 
  | Line of {color: color;  p1: point; p2: point; thickness: int}
  | Points of {color: Gctx.color; points: point list; thickness: int}
  | Ellipse of {color: Gctx.color;  p1: point; rx: int; ry: int; thickness: int}

type mode = 
  | LineStartMode
  | LineEndMode of point
  | PointMode
  | EllipseStartMode
  | EllipseEndMode of point
  
(** The state of the paint program. *)
type state = {
  (** The sequence of all shapes drawn by the user, in order from
      least recent (the head) to most recent (the tail). *)
  shapes : shape Deque.deque;

  (** The input mode the Paint program is in. *)
  mutable mode : mode;

  (** The currently selected pen color. *)
  mutable color : color;
  
  mutable preview: shape option;
  
  mutable thickness: int;
}

(** Initial values of the program state. *)
let paint : state = {
  shapes = Deque.create ();
  mode = LineStartMode;
  color = black;
  preview = None;
  thickness = 1;
}

(** This function creates a graphics context with the appropriate
    pen color.
*)
let with_params (g: gctx) (c: color) (t: int): gctx =
  let g = with_color g c in
  let g_t = with_thickness g t in
  g_t
  
(*********************************)
(**    MAIN CANVAS REPAINTING    *)
(*********************************)

let repaint (g: gctx) : unit =
  let draw_shape (s: shape) : unit =
    begin match s with
      | Line l -> draw_line (with_params g l.color l.thickness) l.p1 l.p2
      | Points p -> draw_points (with_params g p.color p.thickness) p.points
      | Ellipse e -> draw_ellipse (with_params g e.color e.thickness) e.p1 e.rx
        e.ry
    end in
  Deque.iterate draw_shape paint.shapes;
  begin match paint.preview with
  | Some x -> draw_shape x
  | None -> ()
  end

(** Create the actual paint_canvas widget and its associated
    notifier_controller . *)
let ((paint_canvas : widget), (paint_canvas_controller : notifier_controller)) =
  canvas (600, 350) repaint

(** The paint_action function processes all events that occur 
    in the canvas region. *)
let paint_action (gc:gctx) (event:event) : unit =
  let p  = event_pos event gc in  (* mouse position *)
  begin match (event_type event) with
    | MouseDown ->
      (begin match paint.mode with 
         | LineStartMode ->
           paint.mode <- LineEndMode p
         | PointMode -> paint.preview <- Some (Points {color = paint.color; 
                                               points = [p];
                                               thickness = paint.thickness})
         | EllipseStartMode ->
           paint.mode <- EllipseEndMode p
         | _ -> ()
          end)
    | MouseDrag ->
         (begin match paint.mode with 
         | LineStartMode ->
           paint.mode <- LineEndMode p
         | LineEndMode p1 ->
           paint.preview <- Some (Line {color=paint.color; p1=p1; p2=p;
                                        thickness = paint.thickness})
         | PointMode -> paint.preview <- 
           let points1 =
              begin match paint.preview with
              | Some (Points ps) -> ps.points
              | _ -> []
              end in
            Some (Points {color = paint.color; points = points1 @ (p :: []);
                          thickness = paint.thickness})
         | EllipseStartMode ->
           paint.mode <- EllipseEndMode p
         | EllipseEndMode p1 ->
           paint.preview <- Some (Ellipse {color=paint.color; 
                               p1 = (find_ellipse_center p1 p); 
                               rx = (find_ellipse_rx p1 p); 
                               ry = (find_ellipse_ry p1 p);
                               thickness = paint.thickness})
       end)
    | MouseUp ->
       (begin match paint.mode with 
         | LineEndMode p1 ->
           Deque.insert_tail (Line {color=paint.color; p1=p1; p2=p;
                                    thickness = paint.thickness}) 
           paint.shapes;
           paint.mode <- LineStartMode;
           paint.preview <- None
         | PointMode -> 
           let points1 =
              begin match paint.preview with
              | Some (Points ps) -> ps.points
              | _ -> []
              end in
              paint.preview <- None;
              Deque.insert_tail (Points {color = paint.color; points = points1;
                                         thickness = paint.thickness})
              paint.shapes
         | EllipseEndMode p1 ->
           Deque.insert_tail (Ellipse {color=paint.color; 
                               p1 = (find_ellipse_center p1 p); 
                               rx = (find_ellipse_rx p1 p); 
                               ry = (find_ellipse_ry p1 p);
                               thickness = paint.thickness}) 
           paint.shapes;
           paint.mode <- EllipseStartMode;
           paint.preview <- None
          | _ -> ()
       end)
    | _ -> ()
  end

(** Add the paint_action function as a listener to the paint_canvas *)
;; paint_canvas_controller.add_event_listener paint_action


(**************************************)
(** TOOLBARS AND PAINT PROGRAM LAYOUT *)
(**************************************)

(** Create the Undo button *)
let (w_undo, lc_undo, nc_undo) = button "Undo"

(** This function runs when the Undo button is clicked.
   It simply removes the last shape from the shapes deque. *)

let undo () : unit =
  paint.preview <- None;
  (begin match paint.mode with 
         | LineEndMode p1 ->
           paint.mode <- LineStartMode
         | EllipseEndMode p1 ->
           paint.mode <- EllipseStartMode
         | _ -> ()
       end);
       
  if Deque.is_empty paint.shapes then () else
    ignore (Deque.remove_tail paint.shapes)

;; nc_undo.add_event_listener (mouseclick_listener undo)

(** A line button *)
let (w_line, lc_line, nc_line) = button "Line"

let linenc () : unit =
  paint.mode <- LineStartMode
  
;; nc_line.add_event_listener (mouseclick_listener linenc)

(** A point button *)
let (w_point, lc_point, nc_point) = button "Point"

let pointnc () : unit =
  paint.mode <- PointMode
  
;; nc_point.add_event_listener (mouseclick_listener pointnc)

(** An ellipse button *)
let (w_ellipse, lc_ellipse, nc_ellipse) = button "Ellipse"

let ellipsenc () : unit =
  paint.mode <- EllipseStartMode
  
;; nc_ellipse.add_event_listener (mouseclick_listener ellipsenc)

(** A thickness checkbox*)
let (switch_w, switch_cb) =
  checkbox false "Thickness"

;;switch_cb.add_change_listener (fun b -> if b then paint.thickness <- 50 / 3
                                          else paint.thickness <- 1)
                                          
(** A thickness slider*)
let (slider_w, slider_cb) =
  slider 1 "Thickness Slider"
  
;;slider_cb.add_change_listener (fun t -> paint.thickness <- t / 3)
  
(** A spacer widget *)
let spacer : widget = space (10,10)


(** The mode toolbar, initially containing just the Undo button. *)

let mode_toolbar : widget = 
  let ws = [border w_point; spacer; border w_line; spacer; border w_ellipse; 
            spacer; border w_undo; spacer; switch_w] in
  hlist ws

(* The color selection toolbar. *)

let colored_square (width:int) (get_color:unit -> color)
  : widget * notifier_controller =
  let repaint_square (gc:gctx) =
    let c = get_color () in
    fill_rect (with_color gc c) (0, 0) (width-1, width-1) in   
  canvas (width,width) repaint_square

let color_indicator =
  let indicator,_ = colored_square 24 (fun () -> paint.color) in
  let lab, _ = label "Current Color" in
  border (hpair lab indicator)

let color_button (c: color) : widget =
  let w,nc = colored_square 10 (fun () -> c) in
  nc.add_event_listener (mouseclick_listener (fun () ->
      paint.color <- c ));
  w

(** The color selection toolbar. Contains the color indicator and 
    buttons for several different colors. *)
   let color_toolbar : widget =
     let ws = [color_indicator; spacer; color_button black; spacer;
               color_button white; spacer; color_button red; spacer; 
               color_button green; spacer; color_button blue; spacer;
               color_button yellow; spacer; color_button cyan; spacer;
               color_button magenta] in
   hlist ws
  

(** The top-level paint program widget: a combination of the
    mode_toolbar, the color_toolbar and the paint_canvas widgets.
*)

let paint_widget =
   let ws = [paint_canvas; mode_toolbar; spacer; slider_w; spacer; color_toolbar] in
   vlist ws

(** Run the event loop to process user events. *)
;; Eventloop.run paint_widget
