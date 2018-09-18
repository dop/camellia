open Virtual_dom.Vdom

val start_app
  :  ?compare:('model -> 'model -> int)
  -> model:'model
  -> view:(('action -> Event.t) -> 'model -> Node.t)
  -> update:('model -> 'action -> 'model)
  -> Dom_html.element Js.t
  -> ('action -> unit)

val on_dom_load
  : (unit -> unit) -> unit
