#light

open System
open System.Drawing
open System.Windows.Forms
open Microsoft.Glee.Drawing
open Microsoft.Glee.GraphViewerGdi
open DigitalLogic
open Design
open Circuit
open List

let bin_op_str = function
  | B_add -> "+"
  | B_sub -> "-"
  | B_mulu -> "*"
  | B_muls -> "*+"
  | B_and -> "&"
  | B_or -> "|"
  | B_xor -> "^"
  | B_eq -> "=="
  | B_lt -> "<"
  | B_cat -> "++"

let viewer form_width form_height circuit = 
  (* build a simple graph *)
  let g = new Graph("HDFS RTL viewer")

  let dependants s = 
    match s with
    | Signal_reg(a,w,clk,rst,rstval,ena,d) -> [ ena; d ]
    | Signal_mem(a,dw,aw,size,clk,w,we,d,r) -> [ w; we; d; r ]
    | _ -> 
      dependants s

  let label_of_signal s = 
    match s with
    | Signal_empty    -> "empty", Shape.Box, Color.Red
    | Signal_const    (a,w,c) -> c, Shape.Box, Color.LightGray
    | Signal_binop    (a,w,op,s0,s1) ->  bin_op_str op, Shape.Circle, Color.LightGray
    | Signal_unop     (a,w,op,s) -> "~", Shape.Circle, Color.LightGray
    | Signal_wire     (a,w,n,d) -> wire_name s, Shape.Box, Color.LightGray
    | Signal_mux      (a,w,sel,d) -> "mux", Shape.Box, Color.LightGray
    | Signal_select   (a,hi,lo,s) -> "[" ^ string_of_int hi ^ ":" ^ string_of_int lo ^ "]", Shape.Box, Color.LightGray
    | Signal_reg      (a,w,clk,rst,rstval,ena,d) -> "reg", Shape.Box, Color.Yellow
    | Signal_mem      (a,dw,aw,size,clk,w,we,d,r) -> "mem", Shape.Box, Color.Yellow
    | Signal_behave   (a,w,b,d) -> "behave", Shape.Circle, Color.Orange
    | Signal_inst     (a,n,g,io,i,o) -> "inst:" ^ n, Shape.Circle, Color.Red
    | Signal_tri      (a,w,d) -> "Z", Shape.Circle, Color.Red
  
  let add_signal a s = 
    let id = string_of_int (uid s)
    let name, shape, fill_colour = label_of_signal s
    let deps = dependants s
    let dep_names = map (fun x -> string_of_int (uid x)) deps
    iter2 (fun n s -> 
      let e = g.AddEdge(n, id)
      e.EdgeAttr.Label <- string_of_int (width s)
    ) dep_names deps
    try
      let n = g.FindNode(id)
      n.NodeAttribute.Label <- name
      n.NodeAttribute.Shape <- shape
      n.NodeAttribute.Fillcolor <- fill_colour
    with _ -> ()
    
  visit_signal_list add_signal def_arg () (circuit_outputs circuit) |> ignore
  
  (* run through inputs/outputs and set their style *)
  let set_io fill_color ports = 
    iter (fun s ->
      try
        let name = wire_name s
        let id = string_of_int (uid s)
        let n = g.FindNode(id)
        n.NodeAttribute.Label <- name
        n.NodeAttribute.Shape <- Shape.Box
        n.NodeAttribute.Fillcolor <- fill_color
      with _ -> ()
    ) ports
  set_io Color.Green (circuit_inputs circuit)
  set_io Color.Blue (circuit_outputs circuit)

  let form = new Form() in
  form.Size <- new System.Drawing.Size(form_width, form_height); 
  form.Text <- "HDFS RTL viewer";
  let gview = new GViewer()
  form.Controls.Add(gview)
  gview.Dock <- DockStyle.Fill
  gview.Graph <- g
  ignore (form.ShowDialog());
  form.Dispose()
  

let test() =
  let inputs = [ for i in { 0 .. 3 } -> input ("i" ^ string_of_int i) 8 ]
  let outputs,_ = Circuits.Sort.bitonic (>:) (regc enable) inputs vdd
  let circuit = create (map (fun s -> s -- "o") outputs)
  viewer 400 500 circuit

//do test()

