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

(** Waveform viewer *)
module DigitalLogic.Waveform

open System
open System.Windows.Forms
open System.IO
open System.Drawing
open System.Drawing.Drawing2D
open System.Drawing.Imaging
open System.ComponentModel
open System.Threading

open DigitalLogic.SimulatorV01
open Printf
//open List

exception Waveform_error of string
let failwith s = raise (Waveform_error s)

/// Record configuring the wave viewer: colours, size of the form and size of the grid. 
/// The size of the grid is the most complex part: 
/// each grid element is xSize wide, and font height + 2*yMargin + 2*fontMargin high 
type waveconfig_t = {
  width : int;
  height : int;
  font : Font;
  xSize : int;
  yMargin : int;
  fontMargin : int;
  wavePenWidth : int;
  waveColour : Color;
  cursorColour : Color;
  bgColour : Color;
  gridColour : Color;
  textColour : Color;
  fitText : bool; (* Text is optionally "fitted" with a trailing ".", but it's expensive so can be turned off.  Without fitting a clipping rectantle is still used *)
}

(** Black and green *)
let wave_dark = {
  width = 640;
  height = 480;
  font = new Font("", 12.0f, GraphicsUnit.Pixel);
  xSize = 35;
  yMargin = 5;
  fontMargin = 3;
  wavePenWidth = 2;
  waveColour = Color.LightGreen;
  cursorColour = Color.Yellow;
  bgColour = Color.Black;
  gridColour = Color.DarkSlateGray;
  textColour = Color.White;
  fitText = true;
}

(** White and black *)
let wave_light = {
  width = 640;
  height = 480;
  font = new Font("", 12.0f, GraphicsUnit.Pixel);
  xSize = 35;
  yMargin = 5;
  fontMargin = 3;
  wavePenWidth = 2;
  waveColour = Color.Black;
  cursorColour = Color.Red;
  bgColour = Color.White;
  gridColour = Color.LightGray;
  textColour = Color.Blue;
  fitText = true;
}

type 'a wave_str_fmt_t = 
  | BinaryFormat
  | HexFormat
  | IntFormat
  | UserFormat of ('a -> int -> string)

let wave_default = wave_dark

(** Displays a modal dialog showing waveform data. *)
let drawf (dcfg : 'a sim_data_t) cfg signals = 
  let width = cfg.width in
  let height = cfg.height in
  let x_off, y_off = ref 0, ref 0 in
  let ySize = cfg.font.Height + (2*(cfg.yMargin+cfg.fontMargin)) in
  let x_cursor = ref 0 in
  
  let num_transitions = List.fold_left (fun a (_,_,b,_) -> let b = Array.length b in if a < b then b else a) 0 signals in
  let num_signals = List.length signals in
  (* create and array which indicates where transitions happen *)
  let signals = List.map (fun (name,wid,c,f) ->
    let len = (Array.length c) in
    let d = Array.zero_create len in
    let cur_run = ref 1 in
    let cur_val = dcfg.create wid in
    dcfg.copy cur_val c.(0) wid;
    for i=1 to (len-1) do
      if dcfg.eq cur_val c.(i) wid
      then (cur_run := !cur_run + 1)
      else (d.(i - !cur_run) <- !cur_run; cur_run := 1; dcfg.copy cur_val c.(i) wid)
    done;
    d.(len - !cur_run) <- !cur_run;
    (name,wid,c,d,f)) signals in
  
  (* main form *)
  let form = new Form() in
  let _ = form.SuspendLayout() in 
  form.Size <- new Size(width,height); 
  form.Text <- "Waveform viewer";

  let splitter = new SplitContainer() in
  splitter.Dock <- DockStyle.Fill;

  (* interior panel with scroll bars *)
  let panel = new Panel() in
  panel.Dock <- DockStyle.Fill;
  panel.BackColor <- cfg.bgColour;
  panel.AutoSize <- true;
  panel.AutoScroll <- true;
  panel.AutoScrollMinSize <- new Size(num_transitions * cfg.xSize,num_signals * ySize); 

  let panel_names = new Panel() in
  panel_names.Dock <- DockStyle.Fill;
  panel_names.BackColor <- cfg.bgColour;
  panel.AutoSize <- true;

  form.Controls.Add(splitter);
  splitter.Panel1.Controls.Add(panel_names);
  splitter.Panel2.Controls.Add(panel);

  let strFmt = new StringFormat(Enum.combine [StringFormatFlags.NoWrap]) in
  let draw_string (graphics : Graphics) text brush (rect : RectangleF) = 
    graphics.DrawString(text, cfg.font, brush, rect, strFmt) 
  in
  
  let fit_string (graphics : Graphics) (font : Font) text max_width = 
    let fits str = 
      let rendered_size = graphics.MeasureString(str, font) in
      rendered_size.Width < max_width
    in
    let rec fit str = (* if a string is known not to fit then it is padded with a "." and passed to this function *)
      let len = String.length str in
      match len with
      | 0 | 1 | 2 -> "."
      | n ->
        let shorter = (String.sub str 0 (len-2)) ^ "." in (* drop the last char and the ".", then put the "." back *)
        if fits shorter
        then shorter
        else fit shorter 
    in
    if fits text
    then text 
    else fit (text ^ ".")
  in
  
  let string_of_data data width format = 
    match format with
    | IntFormat -> string_of_int (dcfg.to_int data)
    | HexFormat -> dcfg.to_hex_str data width
    | BinaryFormat -> dcfg.to_bin_str data width
    | UserFormat f -> f data width
  in
  
  let draw_transition
    draw_string     (* NOTE: must pass the drawstring function as a paramter.  If it is called from within this
                       function then it crashes at runtime with the error: 
                        System.InvalidCastException: Unable to cast object of type 'draw_transition@137[Microsoft.FSharp.Ref[System.Int32]]' to type 'draw_transition@137[System.String]'.
                           at DigitalLogic.Waveform.draw_signal@169_3.Invoke(Int32 y_margin, Tuple`4 signal)
                           at Microsoft.FSharp.MLLib.List.iteri_aux@47[A](Int32 n, FastFunc`2 f, List x)
                           at DigitalLogic.Waveform.draw_signals@183.Invoke(SolidBrush textBrush)
                           at Microsoft.FSharp.Idioms.using[W35,B](W35 ie, FastFunc`2 f)
                           at Microsoft.FSharp.Idioms.using[W35,B](W35 ie, FastFunc`2 f)
                             .. *)
    (graphics : Graphics) (pen : Pen) brush 
    (x_off, y_off) (x_size, y_size) y_margin
    run prev cur nBits format = 
    let line (x0 : int) y0 x1 y1 = graphics.DrawLine(pen, x0+x_off, y0+y_off, x1+x_off, y1+y_off) in
    let down  () = line 0 y_margin          0      (y_size-y_margin)    in
    let up    () = line 0 (y_size-y_margin) 0      y_margin             in
    let top   () = line 0 y_margin          x_size y_margin             in
    let bottom() = line 0 (y_size-y_margin) x_size (y_size-y_margin)    in
    let text  r  = 
      let pnt = new PointF(Float32.of_int (x_off + cfg.fontMargin), Float32.of_int (y_off+y_margin+cfg.fontMargin)) in
      let rect = new SizeF(Float32.of_int ((run * cfg.xSize)-(2*cfg.fontMargin)), Float32.of_int (y_size-(2*(cfg.yMargin+cfg.fontMargin)))) in
      let text = string_of_data cur nBits format in
      let text = if cfg.fitText then fit_string graphics cfg.font text rect.Width else text in
      draw_string graphics text brush (new RectangleF(pnt, rect))
    in
    if nBits = 1 then
      match dcfg.to_int prev, dcfg.to_int cur with
      | 0, 0 -> bottom()
      | 1, 0 -> down(); bottom()
      | 0, 1 -> up(); top()
      | 1, 1 -> top()
      | _ -> failwith "not implemented"
    else
      if run = 0
      then (top(); bottom())
      else (up(); top(); bottom(); text run)
  in

  let draw_signal 
    (graphics : Graphics) (pen : Pen) brush
    first last
    (x_off, y_off) (x_size, y_size) y_margin
    signal = 
    let name, width, data, run, format = signal in
    let prev = dcfg.create width in
    for i = first to last - 1 do
      let cur = data.(i) in 
      draw_transition 
        draw_string 
        graphics pen brush (x_off + (i * x_size), y_off) (x_size, y_size) y_margin run.(i) prev cur width format;
        dcfg.copy prev cur width 
    done
  in
  
  let max_name_width = 
    using (panel.CreateGraphics()) (fun graphics -> 
      List.fold_left (fun max_width (name,_,_,_,_) ->
        let rendered_size = graphics.MeasureString(name ^ " ", cfg.font) in
        max rendered_size.Width max_width
      ) (Float32.of_float 0.0) signals
    ) in

  let draw_signals() = 
    using (panel.CreateGraphics()) (fun graphics -> 
      using (new Pen(cfg.waveColour, Float32.of_int cfg.wavePenWidth)) (fun wavePen ->
        using (new Pen(cfg.gridColour)) (fun gridPen ->
          using (new Pen(cfg.cursorColour)) (fun cursorPen ->
            using (new SolidBrush(cfg.textColour)) (fun textBrush ->
            
              (* Only draw the client area.  
                 These values are 1 bigger than the calculated grid to ensure the edges always get drawn.  
                 They must be clipped. 
                 This makes scrolling possible regardless of the dataset size.  *)
              let clientSize = panel.ClientSize in
              let gridXmin = ( - !x_off / cfg.xSize) - 1 in
              let gridYmin = ( - !y_off / ySize) - 1 in
              let gridXmax = gridXmin + (clientSize.Width / cfg.xSize) + 3 in
              let gridYmax = gridYmin + (clientSize.Height / ySize) + 1 in

              for i=0 to num_signals-1 do
                if i >= gridYmin && i <= gridYmax then 
                  graphics.DrawLine(gridPen, !x_off, !y_off + i*ySize, !x_off + num_transitions*cfg.xSize, !y_off + i*ySize);
              done;
              for i=(max 0 gridXmin) to (min (num_transitions-1) gridXmax) do
                graphics.DrawLine(gridPen, !x_off + i*cfg.xSize, !y_off, !x_off + i*cfg.xSize, !y_off + num_signals*ySize);
              done;
              
              List.iteri (fun i ((_,_,a,_,f) as x) -> 
                if i >= gridYmin && i <= gridYmax then (
                  let len = Array.length a in
                  draw_signal graphics wavePen textBrush 
                    //0 len 
                    (max 0 gridXmin) (min len gridXmax)
                    (!x_off, !y_off + (i * ySize)) (cfg.xSize, ySize) cfg.yMargin x
                )
              ) signals;
              
              graphics.DrawLine(cursorPen, !x_cursor + !x_off, !y_off, !x_cursor + !x_off, !y_off + num_signals*ySize);
            )
          )
        )
      )
    );
    using (panel_names.CreateGraphics()) (fun graphics -> 
      using (new SolidBrush(cfg.textColour)) (fun textBrush ->
          List.iteri (fun i (n,w,d,_,format) ->
            let pos = !x_cursor / cfg.xSize in
            let idx = min pos (Array.length d - 1) in
            let x = Float32.of_int cfg.fontMargin in
            let y = Float32.of_int (!y_off + (i*ySize) + cfg.yMargin + cfg.fontMargin) in
            let data = (string_of_data d.(idx) w format) in
            graphics.DrawString(n, cfg.font, textBrush, new PointF(x, y));
            graphics.DrawString(data, cfg.font, textBrush, new PointF(x+max_name_width, y))) signals; 
      )
    )
  in
  
  panel.MouseClick
    |> IEvent.filter (fun e -> e.Button = MouseButtons.Left)
    |> IEvent.listen (fun evArgs -> 
      let msx = evArgs.X in
      let msy = evArgs.Y in
      let s_x = (msx - !x_off) / cfg.xSize in
      let s_y = (msy - !y_off) / ySize in
      x_cursor := (msx - !x_off);
      panel.Invalidate();
      panel_names.Invalidate()
  ); 

  form.Resize 
    |> IEvent.listen (fun _ -> 
      panel.Invalidate();
      panel_names.Invalidate()
    ); (* odd things happen otherwise if the yscroll position has moved from 0 (???) *)

  splitter.KeyDown |> IEvent.listen (fun key -> 
    match key.KeyCode with
    | Keys.Escape -> form.Close()
    | _ -> ()
  );

  let scroll_handler (e : ScrollEventArgs) = 
    if e.ScrollOrientation = ScrollOrientation.HorizontalScroll 
    then x_off := - e.NewValue
    else y_off := - e.NewValue;
    panel.Invalidate();
    panel_names.Invalidate();
  in
  panel.Scroll 
    (* |> IEvent.filter (fun a -> a.Type = ScrollEventType.EndScroll) *) (* i would like to filter these events and only redraw on end scroll but the panel seems to do some of the scrolling for me...grrr *)
    |> IEvent.listen scroll_handler;
    
  panel.Paint.Add(fun e -> draw_signals());
  let _ = form.ResumeLayout() in 
  ignore (form.ShowDialog());
  form.Dispose()

(** Dispaly waveform using hex values *)
let draw_hex (dcfg : 'a sim_data_t) cfg signals = 
  let signals = List.map (fun (n,w,a) -> (n,w,a,HexFormat)) signals in
  drawf dcfg cfg signals

(** Dispaly waveform using binary values *)
let draw_bin (dcfg : 'a sim_data_t) cfg signals = 
  let signals = List.map (fun (n,w,a) -> (n,w,a,BinaryFormat)) signals in
  drawf dcfg cfg signals

(** Waveform drawing.  Default display format is hex *)
let draw = draw_hex

(** Waveform for int simulations *)
let draw_int = draw simDataInt
(** Waveform for uint32 simulations *)
let draw_uint32 = draw simDataUInt32 
(** Waveform for uint64 simulations *)
let draw_uint64 = draw simDataUInt64 
(** Waveform for array simulations *)
let draw_array = draw simDataArray

(* ----------------------------------------------------------- *)
(* Waveform viewer for use with the simulator2 data type *)
(* For now just adapt the one above so we can continue debugging, but *)
(* eventually we want this to be a new implementation *)
(* ----------------------------------------------------------- *)
open DigitalLogic.Simulator
open DigitalLogic.Numeric.Ops
open DigitalLogic.Numeric.ResizingArray
open DigitalLogic.Numeric.ArrayBits

let draw2 signal_data fmt = 
  drawf simDataArray wave_dark
    (List.map ( fun ((data : ResizingArray<ArrayBits>), (port : Port)) ->
      port.name, port.width, Array.map (fun (a:ArrayBits) -> a.data) data.data, fmt
    ) signal_data)

