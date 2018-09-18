open Base
open Virtual_dom.Vdom

(* module Http = struct
 *   type method_ =
 *     | Get
 * 
 *   type t =
 *     { method_ : method_
 *     ; url : string
 *     }
 * end
 * 
 * module Command = struct
 *   type t = ..
 *   type t += Http of Http.t
 * end *)

let schedule f =
  Fn.ignore (
    Dom_html.window##requestAnimationFrame
      (Js.wrap_callback f))

let diff ~prev ~next =
  Node.Patch.create ~previous:prev ~current:next

let patch element diff =
  if not (Node.Patch.is_empty diff) then
    let _ = Node.Patch.apply diff (Js.Unsafe.coerce element) in
    ()

let append parent child =
  Fn.ignore (parent##appendChild (Js.Unsafe.coerce child))

let on_dom_load f =
  Fn.ignore (
    Dom_html.addEventListener
      Dom_html.window
      Dom_events.Typ.domContentLoaded
      (Dom.handler (fun _ -> f (); Js.bool false))
      (Js.bool false))

type ('m, 'a) update =
  'm -> 'a -> 'm

type ('m, 'a) view =
  ('a -> Event.t) -> 'm -> Node.t

type ('m, 'a) app =
  { view : ('m, 'a) view
  ; update : ('m, 'a) update
  }

type ('m, 'a) env =
  { vnode : Node.t
  ; element : Dom_html.element Js.t
  ; model: 'm
  }

type 'a ctx =
  { actions : 'a list
  ; advance : unit -> unit
  }

let start_app ?(compare=Caml.Pervasives.compare) ~model ~view ~update parent_element =
  let ctx = ref {actions = []; advance = fun () -> ()} in
  let inject action =
    ctx := {!ctx with actions = !ctx.actions @ [action]};
    schedule (fun _ -> !ctx.advance ());
    Event.Ignore
  in

  let vnode = view inject model in
  let element = Node.to_dom vnode in
  append parent_element element;

  let env = ref {vnode; element; model} in
  let advance () =
    let {vnode; element; model} = !env in
    match !ctx.actions with
    | [] -> ()
    | actions ->
      let model' = List.fold_left ~f:update ~init:model actions in
      ctx := {!ctx with actions = []};
      if compare model model' <> 0 then (
        let vnode' = view inject model' in
        patch element (diff ~prev:vnode ~next:vnode');
        env := {!env with model = model'; vnode = vnode'}
      );
  in
  ctx := {!ctx with advance};
  fun action -> Fn.ignore (inject action)
