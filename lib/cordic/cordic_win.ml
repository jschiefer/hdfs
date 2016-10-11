#light

(** Windows forms based cordic generator *)
open System
open System.Windows.Forms
open DigitalLogic
open Signal
open Circuits.Cordic

let version = "0.1"

type param_map_t = 
  | Constant of float
  | Vector of string
  | VectorX1
  | VectorX25
  | VectorX_1
  | VectorX_25
  | Gain1
  | Gain2

type cordic_cfg_t = 
  {
    cc_alg  : string
    cc_x    : param_map_t
    cc_y    : param_map_t
    cc_z    : param_map_t
    cc_vec  : param_map_t
    cc_type : cordic_type_t
  }


let cordic_cfgs = 
  [| 
    {
      cc_alg  = "generic (simplified linear)"
      cc_x    = Vector("X")
      cc_y    = Vector("Y")
      cc_z    = Vector("Z")
      cc_vec  = Vector("V")
      cc_type = Cordic0
    };
    {
      cc_alg  = "generic (linear)"
      cc_x    = Vector("X")
      cc_y    = Vector("Y")
      cc_z    = Vector("Z")
      cc_vec  = Vector("V")
      cc_type = Cordic1
    };
    {
      cc_alg  = "generic (hyperbolic)"
      cc_x    = Vector("X")
      cc_y    = Vector("Y")
      cc_z    = Vector("Z")
      cc_vec  = Vector("V")
      cc_type = Cordic2
    };
    {
      cc_alg  = "multiply"
      cc_x    = Vector("X")
      cc_y    = Constant(0.0)
      cc_z    = Vector("Y")
      cc_vec  = Constant(0.0)
      cc_type = Cordic0
    };
    {
      cc_alg  = "divide"
      cc_x    = Vector("Y")
      cc_y    = Vector("X")
      cc_z    = Constant(0.0)
      cc_vec  = Constant(0.0)
      cc_type = Cordic0
    }
    {
      cc_alg  = "atan"
      cc_x    = Constant(1.0)
      cc_y    = Vector("X")
      cc_z    = Constant(0.0)
      cc_vec  = Constant(0.0)
      cc_type = Cordic1
    }
    {
      cc_alg  = "sin and cos"
      cc_x    = Gain1
      cc_y    = Constant(0.0)
      cc_z    = Vector("X")
      cc_vec  = Constant(-1.0)
      cc_type = Cordic1
    }
    {
      cc_alg  = "magnitude and phase"
      cc_x    = Vector("X")
      cc_y    = Vector("Y")
      cc_z    = Constant(0.0)
      cc_vec  = Constant(0.0)
      cc_type = Cordic1
    }
    {
      cc_alg  = "polar to rect"
      cc_x    = Vector("X")
      cc_y    = Constant(0.0)
      cc_z    = Vector("Y")
      cc_vec  = Constant(-1.0)
      cc_type = Cordic1
    }
    {
      cc_alg  = "sinh and cosh"
      cc_x    = Gain2
      cc_y    = Constant(0.0)
      cc_z    = Vector("X")
      cc_vec  = Constant(-1.0)
      cc_type = Cordic2
    }
    {
      cc_alg  = "atanh"
      cc_x    = Constant(1.0)
      cc_y    = Vector("X")
      cc_z    = Constant(0.0)
      cc_vec  = Constant(0.0)
      cc_type = Cordic2
    }
    {
      cc_alg  = "exp"
      cc_x    = Gain2
      cc_y    = Constant(0.0)
      cc_z    = Vector("X")
      cc_vec  = Constant(-1.0)
      cc_type = Cordic2
    }
    {
      cc_alg  = "log"
      cc_x    = VectorX1
      cc_y    = VectorX_1
      cc_z    = Constant(0.0)
      cc_vec  = Constant(0.0)
      cc_type = Cordic2
    }
    {
      cc_alg  = "sqrt"
      cc_x    = VectorX25
      cc_y    = VectorX_25
      cc_z    = Constant(0.0)
      cc_vec  = Constant(0.0)
      cc_type = Cordic2
    }
  |]

let cordic_pre = [| "None", -1; "Type 0", 0; "Type 1", 1 |]
let cordic_hw = [| "Iterative",CordicIter(Cordic0); "Pipelined",CordicSeq(Cordic0); "Combinatorial",CordicComb(Cordic0) |]

let cordic_gen() = 
  let form = new Form() in

  let label1 = new System.Windows.Forms.Label();
  let comboBox1 = new System.Windows.Forms.ComboBox();
  let label2 = new System.Windows.Forms.Label();
  let textBox1 = new System.Windows.Forms.TextBox();
  let label3 = new System.Windows.Forms.Label();
  let label4 = new System.Windows.Forms.Label();
  let textBox2 = new System.Windows.Forms.TextBox();
  let label5 = new System.Windows.Forms.Label();
  let textBox3 = new System.Windows.Forms.TextBox();
  let label6 = new System.Windows.Forms.Label();
  let textBox4 = new System.Windows.Forms.TextBox();
  let menuStrip1 = new System.Windows.Forms.MenuStrip();
  let fileToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
  let calculateToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
  let simulateToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
  let generateVHDLToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
  let generateVerilogToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
  let generateCToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
  let generateCPPToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
  let generateCSToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
  let exitToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
  let label7 = new System.Windows.Forms.Label();
  let textBox5 = new System.Windows.Forms.TextBox();
  let textBox6 = new System.Windows.Forms.TextBox();
  let label8 = new System.Windows.Forms.Label();
  let label9 = new System.Windows.Forms.Label();
  let textBox7 = new System.Windows.Forms.TextBox();
  let comboBox2 = new System.Windows.Forms.ComboBox();
  let comboBox3 = new System.Windows.Forms.ComboBox();
  let label10 = new System.Windows.Forms.Label();
  let label11 = new System.Windows.Forms.Label();
  let label12 = new System.Windows.Forms.Label();
  let label13 = new System.Windows.Forms.Label();
  menuStrip1.SuspendLayout();
  form.SuspendLayout();
  
  // 
  // label1
  // 
  label1.AutoSize <- true;
  label1.Location <- new System.Drawing.Point(232, 89);
  label1.Name <- "label1";
  label1.Size <- new System.Drawing.Size(99, 13);
  label1.TabIndex <- 4;
  label1.Text <- "Pre-rototation mode";
  label1.Click |> IEvent.listen (fun _ -> ());
  // 
  // comboBox1
  // 
  comboBox1.FormattingEnabled <- true;
  Array.iter (fun x -> ignore (comboBox1.Items.Add(x))) (Array.map (fun x -> x.cc_alg) cordic_cfgs)
  comboBox1.Location <- new System.Drawing.Point(230, 63);
  comboBox1.Name <- "comboBox1";
  comboBox1.Size <- new System.Drawing.Size(142, 21);
  comboBox1.TabIndex <- 5;
  comboBox1.DropDownStyle <- ComboBoxStyle.DropDownList;
  // 
  // label2
  // 
  label2.AutoSize <- true;
  label2.Location <- new System.Drawing.Point(231, 42);
  label2.Name <- "label2";
  label2.Size <- new System.Drawing.Size(50, 13);
  label2.TabIndex <- 6;
  label2.Text <- "Algorithm";
  // 
  // textBox1
  // 
  textBox1.Location <- new System.Drawing.Point(115, 43);
  textBox1.Name <- "textBox1";
  textBox1.Size <- new System.Drawing.Size(78, 20);
  textBox1.TabIndex <- 10;
  textBox1.Text <- "0.0";
  // 
  // label3
  // 
  label3.AutoSize <- true;
  label3.Location <- new System.Drawing.Point(28, 46);
  label3.Name <- "label3";
  label3.Size <- new System.Drawing.Size(14, 13);
  label3.TabIndex <- 11;
  label3.Text <- "X";
  // 
  // label4
  // 
  label4.AutoSize <- true;
  label4.Location <- new System.Drawing.Point(28, 70);
  label4.Name <- "label4";
  label4.Size <- new System.Drawing.Size(14, 13);
  label4.TabIndex <- 13;
  label4.Text <- "Y";
  // 
  // textBox2
  // 
  textBox2.Location <- new System.Drawing.Point(115, 67);
  textBox2.Name <- "textBox2";
  textBox2.Size <- new System.Drawing.Size(78, 20);
  textBox2.TabIndex <- 12;
  textBox2.Text <- "0.0";
  // 
  // label5
  // 
  label5.AutoSize <- true;
  label5.Location <- new System.Drawing.Point(28, 94);
  label5.Name <- "label5";
  label5.Size <- new System.Drawing.Size(14, 13);
  label5.TabIndex <- 15;
  label5.Text <- "Z";
  // 
  // textBox3
  // 
  textBox3.Location <- new System.Drawing.Point(115, 91);
  textBox3.Name <- "textBox3";
  textBox3.Size <- new System.Drawing.Size(78, 20);
  textBox3.TabIndex <- 14;
  textBox3.Text <- "0.0";
  // 
  // label6
  // 
  label6.AutoSize <- true;
  label6.Location <- new System.Drawing.Point(28, 117);
  label6.Name <- "label6";
  label6.Size <- new System.Drawing.Size(81, 13);
  label6.TabIndex <- 17;
  label6.Text <- "Vectoring mode";
  label6.Click |> IEvent.listen (fun _ -> ());
  // 
  // textBox4
  // 
  textBox4.Location <- new System.Drawing.Point(115, 114);
  textBox4.Name <- "textBox4";
  textBox4.Size <- new System.Drawing.Size(78, 20);
  textBox4.TabIndex <- 16;
  textBox4.Text <- "0.0";
  // 
  // menuStrip1
  // 
  ignore (menuStrip1.Items.Add( fileToolStripMenuItem ));
  menuStrip1.Location <- new System.Drawing.Point(0, 0);
  menuStrip1.Name <- "menuStrip1";
  menuStrip1.Size <- new System.Drawing.Size(392, 24);
  menuStrip1.TabIndex <- 18;
  menuStrip1.Text <- "menuStrip1";
  // 
  // fileToolStripMenuItem
  // 
  let menuItems = [| calculateToolStripMenuItem; simulateToolStripMenuItem; generateVHDLToolStripMenuItem; 
                     generateVerilogToolStripMenuItem; generateCToolStripMenuItem; generateCSToolStripMenuItem;
                     generateCPPToolStripMenuItem; exitToolStripMenuItem |]
  Array.iter (fun x -> ignore (fileToolStripMenuItem.DropDownItems.Add(x :> ToolStripItem))) menuItems;
  fileToolStripMenuItem.Name <- "fileToolStripMenuItem";
  fileToolStripMenuItem.Size <- new System.Drawing.Size(35, 20);
  fileToolStripMenuItem.Text <- "File";
  // 
  // calculateToolStripMenuItem
  // 
  calculateToolStripMenuItem.Name <- "calculateToolStripMenuItem";
  calculateToolStripMenuItem.Size <- new System.Drawing.Size(156, 22);
  calculateToolStripMenuItem.Text <- "&Calculate";
  // 
  // simulateToolStripMenuItem
  // 
  simulateToolStripMenuItem.Name <- "simulateToolStripMenuItem";
  simulateToolStripMenuItem.Size <- new System.Drawing.Size(156, 22);
  simulateToolStripMenuItem.Text <- "&Simulate";
  // 
  // generateVHDLToolStripMenuItem
  // 
  generateVHDLToolStripMenuItem.Name <- "generateVHDLToolStripMenuItem";
  generateVHDLToolStripMenuItem.Size <- new System.Drawing.Size(156, 22);
  generateVHDLToolStripMenuItem.Text <- "&Generate VHDL";
  // 
  // generateVerilogToolStripMenuItem
  // 
  generateVerilogToolStripMenuItem.Name <- "generateVerilogToolStripMenuItem";
  generateVerilogToolStripMenuItem.Size <- new System.Drawing.Size(156, 22);
  generateVerilogToolStripMenuItem.Text <- "Generate &Verilog";
  // 
  // generateCToolStripMenuItem
  // 
  generateCToolStripMenuItem.Name <- "generateCToolStripMenuItem";
  generateCToolStripMenuItem.Size <- new System.Drawing.Size(156, 22);
  generateCToolStripMenuItem.Text <- "Generat&e C";
  // 
  // generateCPPToolStripMenuItem
  // 
  generateCPPToolStripMenuItem.Name <- "generateCPPToolStripMenuItem";
  generateCPPToolStripMenuItem.Size <- new System.Drawing.Size(156, 22);
  generateCPPToolStripMenuItem.Text <- "Genera&te C++";
  // 
  // generateCSToolStripMenuItem
  // 
  generateCSToolStripMenuItem.Name <- "generateCSToolStripMenuItem";
  generateCSToolStripMenuItem.Size <- new System.Drawing.Size(156, 22);
  generateCSToolStripMenuItem.Text <- "Generate C&#";
  // 
  // exitToolStripMenuItem
  // 
  exitToolStripMenuItem.Name <- "exitToolStripMenuItem";
  exitToolStripMenuItem.Size <- new System.Drawing.Size(156, 22);
  exitToolStripMenuItem.Text <- "E&xit";
  // 
  // label7
  // 
  label7.AutoSize <- true;
  label7.Location <- new System.Drawing.Point(28, 140);
  label7.Name <- "label7";
  label7.Size <- new System.Drawing.Size(58, 13);
  label7.TabIndex <- 20;
  label7.Text <- "Fixed point";
  // 
  // textBox5
  // 
  textBox5.Location <- new System.Drawing.Point(115, 137);
  textBox5.Name <- "textBox5";
  textBox5.Size <- new System.Drawing.Size(30, 20);
  textBox5.TabIndex <- 19;
  textBox5.Text <- "4";
  // 
  // textBox6
  // 
  textBox6.Location <- new System.Drawing.Point(163, 137);
  textBox6.Name <- "textBox6";
  textBox6.Size <- new System.Drawing.Size(30, 20);
  textBox6.TabIndex <- 21;
  textBox6.Text <- "12";
  // 
  // label8
  // 
  label8.AutoSize <- true;
  label8.Location <- new System.Drawing.Point(149, 140);
  label8.Name <- "label8";
  label8.Size <- new System.Drawing.Size(10, 13);
  label8.TabIndex <- 22;
  label8.Text <- ".";
  // 
  // label9
  // 
  label9.AutoSize <- true;
  label9.Location <- new System.Drawing.Point(28, 162);
  label9.Name <- "label9";
  label9.Size <- new System.Drawing.Size(50, 13);
  label9.TabIndex <- 24;
  label9.Text <- "Iterations";
  // 
  // textBox7
  // 
  textBox7.Location <- new System.Drawing.Point(115, 159);
  textBox7.Name <- "textBox7";
  textBox7.Size <- new System.Drawing.Size(78, 20);
  textBox7.TabIndex <- 23;
  textBox7.Text <- "16";
  // 
  // comboBox2
  // 
  comboBox2.FormattingEnabled <- true;
  Array.iter (fun (x,_) -> ignore (comboBox2.Items.Add(x))) cordic_pre
  comboBox2.Location <- new System.Drawing.Point(231, 109);
  comboBox2.Name <- "comboBox2";
  comboBox2.Size <- new System.Drawing.Size(142, 21);
  comboBox2.TabIndex <- 25;
  comboBox2.SelectedIndex <- 0;
  comboBox2.DropDownStyle <- ComboBoxStyle.DropDownList;
  // 
  // comboBox3
  // 
  comboBox3.FormattingEnabled <- true;
  Array.iter (fun (x,_) -> ignore (comboBox3.Items.Add(x))) cordic_hw
  comboBox3.Location <- new System.Drawing.Point(231, 158);
  comboBox3.Name <- "comboBox3";
  comboBox3.Size <- new System.Drawing.Size(142, 21);
  comboBox3.TabIndex <- 27;
  comboBox3.SelectedIndex <- 0;
  comboBox3.DropDownStyle <- ComboBoxStyle.DropDownList;
  // 
  // label10
  // 
  label10.AutoSize <- true;
  label10.Location <- new System.Drawing.Point(232, 138);
  label10.Name <- "label10";
  label10.Size <- new System.Drawing.Size(59, 13);
  label10.TabIndex <- 26;
  label10.Text <- "Circuit type";
  label10.Click |> IEvent.listen (fun _ -> ());
  // 
  // label11
  // 
  label11.AutoSize <- true;
  label11.Location <- new System.Drawing.Point(55, 212);
  label11.Name <- "label11";
  label11.Size <- new System.Drawing.Size(100, 13);
  label11.TabIndex <- 28;
  // 
  // label12
  // 
  label12.AutoSize <- true;
  label12.Location <- new System.Drawing.Point(55, 229);
  label12.Name <- "label12";
  label12.Size <- new System.Drawing.Size(100, 13);
  label12.TabIndex <- 29;
  // 
  // label13
  // 
  label13.AutoSize <- true;
  label13.Location <- new System.Drawing.Point(55, 246);
  label13.Name <- "label13";
  label13.Size <- new System.Drawing.Size(100, 13);
  label13.TabIndex <- 30;
  // 
  // Form1
  // 
  form.AutoScaleDimensions <- new System.Drawing.SizeF(6.0f, 13.0f);
  form.AutoScaleMode <- System.Windows.Forms.AutoScaleMode.Font;
  form.ClientSize <- new System.Drawing.Size(392, 280);
  form.Controls.Add(comboBox3);
  form.Controls.Add(label13);
  form.Controls.Add(label12);
  form.Controls.Add(label11);
  form.Controls.Add(label10);
  form.Controls.Add(comboBox2);
  form.Controls.Add(label9);
  form.Controls.Add(textBox7);
  form.Controls.Add(label8);
  form.Controls.Add(textBox6);
  form.Controls.Add(label7);
  form.Controls.Add(textBox5);
  form.Controls.Add(label6);
  form.Controls.Add(textBox4);
  form.Controls.Add(label5);
  form.Controls.Add(textBox3);
  form.Controls.Add(label4);
  form.Controls.Add(textBox2);
  form.Controls.Add(label3);
  form.Controls.Add(textBox1);
  form.Controls.Add(label2);
  form.Controls.Add(comboBox1);
  form.Controls.Add(label1);
  form.Controls.Add(menuStrip1);
  form.MainMenuStrip <- menuStrip1;
  form.Name <- "Form1";
  form.Text <- ("Cordic Generator (Version " ^ version ^ ", HDFS Version " ^ hdfs_version ^ ")");
  form.Load |> IEvent.listen (fun _ -> ());
  menuStrip1.ResumeLayout(false);
  menuStrip1.PerformLayout();
  form.ResumeLayout(false);
  form.PerformLayout();
  
  let validate_textbox_int (tb : TextBox) min max def = 
    try 
      let num = int_of_string tb.Text
      if num > max then max
      else if num < min then min
      else num
    with _ -> def

  let validate_textbox_float (tb : TextBox) min max def = 
    try 
      let num = float_of_string tb.Text
      if num > max then max
      else if num < min then min
      else num
    with _ -> def

  (* get configuration *)
  let get_params() = 
    let cfgidx = comboBox1.SelectedIndex
    let cfg = cordic_cfgs.(cfgidx) 
    let iters = int_of_string textBox7.Text
    let x = float_of_string textBox1.Text
    let y = float_of_string textBox2.Text
    let z = float_of_string textBox3.Text
    let v = float_of_string textBox4.Text
    let i = int_of_string textBox5.Text
    let f = int_of_string textBox6.Text
    let _,pre = cordic_pre.(comboBox2.SelectedIndex)
    let _,gen = cordic_hw.(comboBox3.SelectedIndex)
    cfgidx, cfg, iters, x, y, z, v, i, f, pre, 
    match gen with
    | CordicComb _ -> CordicComb(cfg.cc_type)
    | CordicSeq _  -> CordicSeq(cfg.cc_type)
    | CordicIter _ -> CordicIter(cfg.cc_type)
  
  (* Update user interface as different algorithms are selected *)
  let update_ui () = 
    try
      let cfgidx, cfg, iters, x, y, z, v, i, f, pre, gen = get_params()
      (* update the x, y, z, vec textboxes *)
      let set_tb (cc,(tb : TextBox),(lb : Label)) = 
        match cc with
        | Constant x -> 
          tb.Enabled <- false
          tb.Text <- string_of_float x
          lb.Text <- "Constant"
        | Gain1 -> 
          let gain1 = Cordic_ref.invGain1 iters in
          tb.Enabled <- false
          tb.Text <- string_of_float gain1
          lb.Text <- "Gain 1"
        | Gain2 -> 
          let gain2 = Cordic_ref.invGain2 iters in
          tb.Enabled <- false
          tb.Text <- string_of_float gain2
          lb.Text <- "Gain 2"
        | Vector x ->
          tb.Enabled <- true
          lb.Text <- x
        | VectorX1 ->
          tb.Enabled <- true
          lb.Text <- "X + 1.0"
        | VectorX25 ->
          tb.Enabled <- true
          lb.Text <- "X + 0.25"
        | VectorX_1 ->
          tb.Enabled <- false
          lb.Text <- "X - 1.0"
        | VectorX_25 ->
          tb.Enabled <- false
          lb.Text <- "X - 0.25"
          
      List.iter set_tb [ cfg.cc_x, textBox1, label3; cfg.cc_y, textBox2, label4; cfg.cc_z, textBox3, label5; cfg.cc_vec, textBox4, label6 ]
    with _ -> ()
  
  comboBox1.SelectedIndexChanged |> IEvent.listen (fun _ -> update_ui());
  textBox7.TextChanged |> IEvent.listen (fun _ -> update_ui());
  
  let circuit_create () = 
    Signal.signal_init() (* otherwise we get name clashes *)
    let cfgidx, cfg, iters, x, y, z, v, i, f, pre, gen = get_params()
    printf "(%f,%f,%f,%f) %i.%i / %i / %i\n" x y z v i f iters pre
    let bits = i + f
    let scale = 2.0 ** (float_of_int f) 
    let fconst f = consti bits (int_of_float (scale * f)) 
    let gain1 = Cordic_ref.invGain1 iters 
    let gain2 = Cordic_ref.invGain2 iters 
    let make_input xx p = 
      match p with
      | Constant x -> fconst x
      | Vector x   -> input x bits
      | VectorX1   -> ((input "X" bits) + (fconst 1.00))
      | VectorX25  -> ((input "X" bits) + (fconst 0.25))
      | VectorX_1  -> (xx - (fconst 1.00))
      | VectorX_25 -> (xx - (fconst 0.25))
      | Gain1      -> fconst gain1
      | Gain2      -> fconst gain2
    let x = make_input empty cfg.cc_x 
    let y = make_input x cfg.cc_y 
    let z = make_input x cfg.cc_z 
    let v = make_input x cfg.cc_vec 
    let vld_in = input "vld_in" 1
    printf "%i, %i, %i, %i\n" x.width y.width z.width v.width
    let xo, yo, zo, vld_out = cordic pre gen i iters clock reset enable vld_in x y z v in 
    let outputs = 
      match cfgidx with
      | 0  ->   [ "xo",   xo;  "yo", yo;  "zo", zo; ]
      | 1  ->   [ "xo",   xo;  "yo", yo;  "zo", zo; ]
      | 2  ->   [ "xo",   xo;  "yo", yo;  "zo", zo; ]
      | 3  ->   [ "mul",  yo; ]
      | 4  ->   [ "div",  yo; ]
      | 5  ->   [ "atan", zo; ]
      | 6  ->   [ "sin",  yo;  "cos", xo; ]
      | 7  ->   [ "mag",  xo;  "phase", zo; ]
      | 8  ->   [ "xo",   yo;  "yo", xo; ]
      | 9  ->   [ "sinh", yo;  "cosh", xo; ]
      | 10 ->   [ "atanh", zo ]
      | 11 ->   [ "exp", (xo + yo) ]
      | 12 ->   [ "log", (zo <<< 1) ]
      | 13 ->   [ "sqrt", xo ]
      | _  ->   []
    let outputs = (output "vld_out" vld_out) :: (List.map (fun (n,s) -> output n s) outputs)
    Circuit.create outputs

  let simulate draw = 
    try 
      let cfgidx, cfg, iters, x', y', z', v', i, f, pre, gen = get_params()
      let circuit = circuit_create()
      let scale = 2.0 ** (float_of_int f) 
      let fconst f = int_of_float (scale * f)
      let sim = Simulator.create circuit
      let sim, data= sim.record
      let find (name:String) = try sim.port name with _ -> failwith "Can't find port...does it need a default?"
      let x = find "X"
      let y = find "Y"
      let z = find "Z"
      let vec = find "V"
      let ena = find "enable"
      let vld_in = find "vld_in"
      let vld_out = find "vld_out"
      
      sim.reset
      x.i <- fconst x'
      y.i <- fconst y'
      z.i <- fconst z'
      vec.i <- fconst v'
      printf "sim inputs: %i %i %i %i\n" x.i y.i z.i vec.i
      ena.i <- 1
      vld_in.i <- 1
      sim.cycle 
      while vld_out.u <> 1 do
        vld_in.i <- 0
        sim.cycle 

      let idx = ref 0
      let labels = [| label11; label12; label13 |]
      Array.iter (fun (l : Label) -> l.Text <- "") labels
      List.iter (fun (p:Simulator.Port) -> 
        let name = p.port_name
        let width = p.width
        if name <> "vld_out" then
          printf "%s: %i\n" name p.i
          let data = p.i
          let data = if (data &&& (1<<<(width-1))) <> 0 then ((-1) <<< width) ||| data else data
          labels.(!idx).Text <- (name ^ ": " ^ string_of_float ((float_of_int data) / scale))
          incr idx
      ) (sim.outputs)
      
      if draw then
        Waveform.draw2 data Waveform.HexFormat
    with _ ->
      ignore (MessageBox.Show("Failed to run simulation.  Check arguments."))
      
  let write_hdl str write = 
    try
      let save = new SaveFileDialog()
      save.Filter <- str;
      save.FilterIndex <- 1;
      save.RestoreDirectory <- true;
      if save.ShowDialog() = DialogResult.OK then
        let f = open_out save.FileName
        let name = IO.Path.GetFileNameWithoutExtension(save.FileName)
        write f name (circuit_create())
        close_out f
    with _ ->
      ignore (MessageBox.Show("Failed to generate output file.  Check arguments."))

  calculateToolStripMenuItem.Click |> IEvent.listen (fun _ -> simulate false);
  simulateToolStripMenuItem.Click |> IEvent.listen (fun _ -> simulate true);
  generateVHDLToolStripMenuItem.Click |> IEvent.listen (fun _ -> write_hdl "VHDL files (*.vhd)|*.vhd" Vhdl.write);
  generateVerilogToolStripMenuItem.Click |> IEvent.listen (fun _ -> write_hdl "Verilog files (*.v)|*.v" Verilog.write);
  generateCToolStripMenuItem.Click |> IEvent.listen (fun _ -> write_hdl "C files (*.c)|*.c" C.write_c);
  generateCPPToolStripMenuItem.Click |> IEvent.listen (fun _ -> write_hdl "C++ files (*.cpp)|*.cpp" C.write_cpp);
  generateCSToolStripMenuItem.Click |> IEvent.listen (fun _ -> write_hdl "C# files (*.cs)|*.cs" C.write_cs);
  exitToolStripMenuItem.Click |> IEvent.listen (fun _ -> form.Close());

  comboBox1.SelectedIndex <- 1;  
  Application.Run(form)
  
[<STAThread>]    
do cordic_gen()
