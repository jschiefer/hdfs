#light "off"
(*
  HDFS Digital Logic Hardware Design Utility Library (hdfslib.dll)
  Copyright (C) 2006 Andy Ray.

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*)

(** Tree view of the circuit *)
module DigitalLogic.Hierform

open System
open System.Windows.Forms
open System.IO
open System.Drawing
open System.Drawing.Drawing2D
open System.Drawing.Imaging
open System.ComponentModel
open System.Threading
open DigitalLogic
open Signal
open Circuit

(** Draws a tree view of the circuit, starting at it's outputs.  Tree is generated dynamically as nodes are opened and closed. *)
let hierform outputs = 
  let form_width, form_height = 400, 300 in

  let string_of_signal (s : Signal) = 
    let wid = 
      match s.width with
      | 0 -> "no bits"
      | 1 -> "1 bit"
      | x -> string s.width ^ " bits" in
    let uid = "(uid: " ^ string s.uid ^ ")" in
    let sos (s:Signal) = 
      match s.signal with
      | Signal_empty -> "empty"
      | Signal_const (a,w,c) -> "constant \"" ^ c ^ "\""
      | Signal_binop (a,w,op,s0,s1) -> "\"" ^ (string_of_binop op) ^ "\""
      | Signal_unop (a,w,op,s) -> "\"" ^ (string_of_unop op) ^ "\""
      | Signal_wire (a,w,n,d) -> "wire" ^ (if wire_name s <> "" then " \"" ^ (wire_name s) ^ "\"" else "")
      | Signal_mux (a,w,sel,d) -> "mux " ^ string (List.length d) ^ " items (out of " ^ string (1<<<(sel.width)) ^ ")"
      | Signal_select (a,hi,lo,s) -> "select [" ^ string hi ^ ":" ^ string lo ^ "]"
      | Signal_reg (a,w,clk,rst,rstval,ena,d) -> "register"
      | Signal_mem (a,dw,aw,size,clk,w,we,d,r) -> "memory " ^ string size ^ " elements"
      | Signal_behave (a,w,b,d) -> "behave"
      | Signal_inst (a,n,m,g,io,i,o) -> 
        "instantiation " ^ 
        string (List.length g) ^ " generics, " ^ 
        string (List.length io) ^ " inouts, " ^ 
        string (List.length i) ^ " inputs, " ^ 
        string (List.length o) ^ " outputs "
      | Signal_tri (a,w,d) -> "tristate " ^ string (List.length d) ^ " drivers"
    in
    (sos s) ^ ", " ^ wid ^ " " ^ uid
  in

  let form = new Form() in
  form.Size <- new Size(form_width, form_height); 
  form.Text <- "Hierarchy viewer";

  let treeView = new TreeView() in
  treeView.Dock <- DockStyle.Fill;
  
  form.Controls.Add(treeView);
 
  let rootNode = new TreeNode("circuit") in
  ignore (treeView.Nodes.Add(rootNode));

  let add_children (parent : TreeNode) (signal : Signal) = 
    let deps = signal.dependants in
    List.iter (fun s ->
      let n = new TreeNode(string_of_signal s) in
      n.Tag <- s;
      ignore(parent.Nodes.Add(n))
    ) signal.dependants
  in
  
  List.iter (fun o ->
    let n = new TreeNode("output " ^ string_of_signal o) in 
    n.Tag <- o;
    ignore(rootNode.Nodes.Add(n));
    add_children n o
  ) outputs;
  rootNode.Expand();
  
  (* Apart from the root node and the list of output nodes, the tree is generated dynamically as it is
     expanded or collapsed.  It needs to be done this way as there are cycles in the graph which we do
     want to explore, but dont want to cause stack overflow *)

  let expand_handler (e : TreeViewCancelEventArgs) = 
    let node = e.Node in
    if node.Level > 0 then (
      (* run through the children of this node and add their nodes *)
      let rec iter_kids (nkid : TreeNode) = 
        if nkid = null then ()
        else (
          let signal = (nkid.Tag :?> Signal) in
          let deps = signal.dependants in
          add_children nkid signal;
          iter_kids (nkid.NextNode)
        )
      in
      iter_kids node.FirstNode
    )
  in
  treeView.BeforeExpand |> IEvent.listen expand_handler;

  let collapse_handler (e : TreeViewCancelEventArgs) = 
    let node = e.Node in
    if node.Level > 0 then (
      (* run through the kid nodes and remove their children *)
      let rec iter_kids (nkid : TreeNode) = 
        if nkid = null then ()
        else (
          nkid.Collapse();
          while (nkid.FirstNode <> null) do
            nkid.FirstNode.Remove()
          done;
          iter_kids (nkid.NextNode)
        )
      in
      iter_kids node.FirstNode
    )
  in
  treeView.BeforeCollapse |> IEvent.listen collapse_handler;

  let _ = form.ResumeLayout() in 
  ignore (form.ShowDialog());
  form.Dispose()
