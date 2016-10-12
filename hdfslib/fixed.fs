/// <P>Signed (<i>SFixed</i>) and unsigned (<i>UFixed</i>) fixed point arithmetic types.</P>
/// 
/// <P>The operators and functions defined on these types closely follow those of the Signal type.
///    These types are not objects so do not support a new() notation, nor do they have mutable
///    fields.  Operations, such as setting the rounding mode or moving the fixed point, will
///    return a new SFixed or UFixed.</P>
/// 
/// <P>The API works in a similar way to the proposed IEEE VHDL fixed point package.  Thus fixed 
///    point numbers are defined in terms of <i>high</i> and <i>low</i> bit indices where the
///    index may be negative and the fixed point lies between index <b>0</b> and <b>-1</b>.  An important
///    consequence of this design is that high may be negative, and low may be positive.  The only
///    restriction is that high must be >= low.</p>
///
/// <P>By default rounding and overflow control is applied when resizing the fixed point numbers.  
///    The default mode is to saturate and round to nearest (as per IEEE-754 definition) when 
///    applicable (ie the target has less precision than the argument).  The rounding and overflow 
///    mode is defined using a pair of functions and may be reconfigured so alternative 
///    definitions may be used.  The following implementations are already provided</P>
///    <ul>
///     <li> Rounding mode
///      <ul> 
///       <li>Round to -ve infinity</li>
///       <li>Round to +ve infinity</li>
///       <li>Round away from zero</li>
///       <li>Round towards from zero</li>
///       <li>Round to neareset even</li>
///      </ul>
///     </li>
///     <li> Overflow mode
///      <ul>
///       <li>Wrap</li>
///       <li>Truncate</li>
///      </ul>
///     </li>
///    </ul>
///    
///    <p>The rounding and overflow mode propogates though the various operators and functions.  For 
///    binary operators, the rounding mode is propogated from the left operand.</P>

#light 
module DigitalLogic.Fixed
open DigitalLogic.Numeric.Ops
open DigitalLogic.Signal

// ////////////////////////////////////////////////////////
// ////////////////////////////////////////////////////////

/// Specifies a fixed point type in terms of it's high and low indices.
type Range = { r_high : int; r_low : int }
  with
    member r.high = r.r_high
    member r.low = r.r_low
    member r.fix = { f_width = r.r_high - r.r_low + 1; f_fix = - r.r_low }
    member r.fixedPoint = r.fix.fixedPoint
    member r.width = r.fix.width
    
    (** Given a pair of ranges, return the range which covers both numbers. *)
    static member union ((r0 : Range), (r1 : Range)) = 
      let h = max r0.r_high r1.r_high
      let l = min r0.r_low r1.r_low
      {r_high=h; r_low=l}

    member r0.union (r1 : Range) = Range.union(r0,r1)

    static member union (ranges : Range list) = 
      List.fold_left (fun (rr : Range) (r : Range) -> rr.union(r)) (List.hd ranges) ranges

  end
  
/// Specifies a fixed point type in terms of it's width and an adjustment, called fix, to specify the fixed point.
/// The adjustment essentially takes the vector with it's least significant bit at zero and shifts it right (if fix 
/// is positive) or left by "fix".
and Fix = { f_width : int; f_fix : int }
  with
    member f.fixedPoint = f.f_fix
    member f.width = f.f_width
    member f.range = { r_high = f.width - f.f_fix - 1; r_low = - f.f_fix; }
    member f.high = f.range.r_high
    member f.low = f.range.r_low
  end

let range high low = { r_high=high; r_low=low }
let fix width fix = { f_width=width; f_fix=fix }

// ////////////////////////////////////////////////////////
// ////////////////////////////////////////////////////////

type Round = Signal -> Range -> int -> (Signal * Range)
type Overflow = Signal -> Range -> int -> (Signal * Range)

let default_ufixed_round, default_ufixed_overflow, set_default_ufixed_rounding_mode = 
  let boot_rounding_mode = (fun (s:Signal) (r:Range) (i:int) -> empty, range 0 0)
  let default_round = ref boot_rounding_mode
  let default_overflow = ref boot_rounding_mode
  (fun () -> !default_round),
  (fun () -> !default_overflow),
  (fun r o ->
    default_round := r
    default_overflow := o
  )

let default_sfixed_round, default_sfixed_overflow, set_default_sfixed_rounding_mode = 
  let boot_rounding_mode = (fun (s:Signal) (r:Range) (i:int) -> empty, range 0 0)
  let default_round = ref boot_rounding_mode
  let default_overflow = ref boot_rounding_mode
  (fun () -> !default_round),
  (fun () -> !default_overflow),
  (fun r o ->
    default_round := r
    default_overflow := o
  )
  
// ////////////////////////////////////////////////////////
// ////////////////////////////////////////////////////////

/// Unsigned fixed point type
type UFixed = 
  { 
    uf_signal : Signal;
    uf_range : Range;
    uf_round : Round;
    uf_overflow : Overflow;
  }
  with
    // ////////////////////////////////////////////////////////
    // Properties
    // ////////////////////////////////////////////////////////
    
    (** Internal signal *)
    member x.signal = x.uf_signal
    (** range *)
    member x.range = x.uf_range
    (* rounding mode *)
    member x.round = x.uf_round
    (* overflow mode *)
    member x.overflow = x.uf_overflow
    
    (** width of integer signal *)
    member x.width = x.signal.width
    (** Determine if the width of the signal and high and low are valid *)
    member x.valid = assert (x.signal.width = x.range.high - x.range.low + 1)
    (** Left most bit position *)
    member x.high = x.range.high
    (** Right most bit position *)
    member x.low = x.range.low
    (** range in fix_t format *)
    member x.fix = x.range.fix
    (** Position of fixed point *)
    member x.fixedPoint = x.fix.fixedPoint
    (** name of signal *)
    member x.name = x.signal.name

    // ////////////////////////////////////////////////////////
    // Rounding mode and fixed point manipulation
    // ////////////////////////////////////////////////////////
    
    member x.SetRoundingMode(r,o) = { x with uf_round = r; uf_overflow = o }

    (** return a ufixed with the fix point moved to the given position *)
    member x.SetFix f = UFixed.to_UFixed(x.signal, f, x.round, x.overflow)

    // ////////////////////////////////////////////////////////
    // Creation from signals
    // ////////////////////////////////////////////////////////

    (** Create a ufixed from a signal.  The fixed point is implicitly set to 0 *)
    static member of_Signal (s : Signal) = { uf_signal = s; uf_range = { r_high = s.width - 1; r_low = 0 }; uf_round = default_ufixed_round(); uf_overflow = default_ufixed_overflow(); }

    (** Create a ufixed from a signal with the given fixed point *)
    static member to_UFixed ((s : Signal), f, r, o) = { uf_signal = s; uf_range = { r_high = s.width - f - 1; r_low = -f }; uf_round = r; uf_overflow = o; }
    static member to_UFixed ((s : Signal), f) = { uf_signal = s; uf_range = { r_high = s.width - f - 1; r_low = -f }; uf_round = default_ufixed_round(); uf_overflow = default_ufixed_overflow(); }

    // ////////////////////////////////////////////////////////
    // Selection and resizing
    // ////////////////////////////////////////////////////////

    (* Select a range of bits.  Range must be within the UFixed.  Fixed point is kept *)
    member x.select ((a : int),(b : int)) = UFixed.to_UFixed(x.signal.[a+x.fixedPoint .. b+x.fixedPoint], x.fixedPoint, x.round, x.overflow)
    (* Select a range of bits.  Range must be within the UFixed.  New fixed point is applied *)
    member x.select ((a : int),(b : int),(c : int)) = UFixed.to_UFixed(x.signal.[a+x.fixedPoint .. b+x.fixedPoint], c, x.round, x.overflow)

    (** Resize UFixed to given range.  Vector is padded (with zeros) or truncated to fit range.  No rounding of overflow is applied. *)
    member x.resize(r : Range) = 
      let s = x.signal
      let s = 
        if r.high = x.high then s
        else if r.high > x.high then (Signal.zero (r.high - x.high)) ++ s
        else s.[s.width - (x.high - r.high) - 1 .. 0]
      let s =
        if r.low = x.low then s
        else if r.low > x.low then s.[s.width - 1 .. r.low-x.low]
        else s ++ (Signal.zero (x.low - r.low))
      let u = UFixed.to_UFixed (s, (-r.low), x.round, x.overflow)
      u.valid
      u
      
    (** Resizes the vector so that it is as least as big as the given range *)
    member x.resize_to(range : Range) = x.resize(x.range.union(range))
    
    (** Resizes the vector so that it is as least as big as the given range (in Fix format) *)
    member x.resize_to(fix : Fix) = x.resize(x.range.union(fix.range))
    
    (** Resizing of vector using the supplied rounding and overflow mode. *)
    member x.resize (r,o,high,low) = 
      let range = x.range
      let (x : Signal), (range : Range) = r x.signal range low
      let (x : Signal), (range : Range) = o x range high
      let x = UFixed.to_UFixed(x, range.fixedPoint)
      { x with uf_round = r; uf_overflow = o }
    
    (** Resizing of vector using the rounding and overflow modes set on the UFixed. *)
    member x.Item with get((high : int),(low : int)) = x.resize(x.round,x.overflow,high,low)

    // ////////////////////////////////////////////////////////
    // ////////////////////////////////////////////////////////

    (** Convert the given floating point number to a fixed point integer.  Rounding is performed if rnd is true.  *)
    static member fixed_of_float' (rnd, f, v) =
      let scale = 2.0 ** (float f)
      let rnd = if rnd then (if v >= 0.0 then 0.5 else - 0.5) else 0.0
      int ((v * scale) + rnd)

    (** Convert the given floating point number to a fixed point integer.  Rounding is performed.  *)
    member x.fixed_of_float v = UFixed.fixed_of_float' (true, x.fixedPoint, v)

    (** Convert the given fixed point integer to a floating point number *)
    member x.float_of_fixed v = 
      let v = float v
      let scale = 2.0 ** (float x.fixedPoint)
      (v / scale)

    (** Return a string encoding the range of this ufixed *)
    member x.to_string = 
      "[" ^ string x.high ^ ":" ^ string x.low ^ "] (f=" ^ string x.fixedPoint ^ " w=" ^ string x.width ^ ")"

    (** fixed point constant from an integer *)
    static member consti ((fix : Fix), i) = UFixed.to_UFixed(consti fix.width i, fix.fixedPoint)
    
    static member consti ((r : Range), i) = UFixed.consti(r.fix, i)
    
    (** fixed point constant from an integer *)
    member x.consti i = UFixed.consti(x.range, i).SetRoundingMode(x.round, x.overflow)
    
    (** fixed point constant from a float.  Rounding optional *)
    static member constf' (r, (fix : Fix), i) = UFixed.consti(fix, (UFixed.fixed_of_float' (r, fix.fixedPoint, i)))
    
    static member constf' (r, (range : Range), i) = UFixed.constf'(r, range.fix, i)
    
    (** fixed point constant from a float.  Performs rounding *)
    static member constf ((fix : Fix), i) = UFixed.constf'(true, fix, i)
    
    static member constf ((range : Range), i) = UFixed.constf'(true, range, i)

    (** fixed point constant from a float.  Performs rounding *)
    member x.constf i = UFixed.constf(x.range, i).SetRoundingMode(x.round, x.overflow)

    member x.zero = x.constf 0.0
    member x.one = x.resize_to(range 0 0).constf 1.0
    member x.two = x.resize_to(range 1 1).constf 2.0
    member x.half = x.resize_to(range (-1) (-1)).constf 0.5
    member x.quarter = x.resize_to(range (-2) (-2)).constf 0.25

    // ////////////////////////////////////////////////////////
    // Arithmetic.  Rounding and overflow modes propogated from left operand
    // ////////////////////////////////////////////////////////

    (** Addition.  The two input numbers are converted so that their ranges are compatible then extended by one bit. *)
    static member (+) ((a : UFixed), (b : UFixed)) = 
      let {r_high=high; r_low=low} = a.range.union(b.range)
      let a, b = a.resize (range (high + 1) low), b.resize (range (high + 1) low)
      UFixed.to_UFixed ((a.signal + b.signal), (-(min a.low b.low)), a.round, a.overflow)

    (** Addition with an integer operand on the right.  The integer operand is converted to the same fixed point format as the left operand *)
    static member (+) ((a : UFixed), (b : int)) = a + a.consti b

    (** Addition with a float operand on the right.  The integer operand is converted to the same fixed point format as the left operand *)
    static member (+) ((a : UFixed), (b : float)) = a + a.constf b

    // ////////////////////////////////////////////////////////
    // ////////////////////////////////////////////////////////
    
    static member sub ((a : int), (b : UFixed)) = b.consti a - b
    static member sub ((a : float), (b : UFixed)) = b.constf a - b

    (** Subtraction.  The two input numbers are converted so that their ranges are compatible then extended by one bit.  The result therefore cannot overflow *)
    static member (-) ((a : UFixed), (b : UFixed)) = 
      let {r_high=high; r_low=low} = a.range.union(b.range)
      let a, b = a.resize (range (high + 1) low), b.resize (range (high + 1) low)
      let s = UFixed.to_UFixed ((a.signal - b.signal), (-(min a.low b.low)), a.round, a.overflow)
      s
    
    (** Subtraction with an integer operand on the right.  The integer operand is converted to the same fixed point format as the left operand *)
    static member (-) ((a : UFixed), (b : int)) = a - a.consti b

    (** Subtraction with a float operand on the right.  The integer operand is converted to the same fixed point format as the left operand *)
    static member (-) ((a : UFixed), (b : float)) = a - a.constf b

    // ////////////////////////////////////////////////////////
    // ////////////////////////////////////////////////////////

    (** Multiplication. *)
    static member ( * ) ((a : UFixed), (b : UFixed)) = 
      UFixed.to_UFixed ((a.signal * b.signal), (a.fixedPoint + b.fixedPoint), a.round, a.overflow)
    
    (** Multiplication with an integer operand on the right.  The integer operand is converted to the same fixed point format as the left operand XXX is this the best way to do things? XXX *)
    static member ( * ) ((a : UFixed), (b : int)) = a * a.consti b

    (** Multiplication with a float operand on the right.  The float operand is converted to the same fixed point format as the left operand XXX is this the best way to do things? XXX *)
    static member ( * ) ((a : UFixed), (b : float)) = a * a.constf b

    // ////////////////////////////////////////////////////////
    // concantenation
    // ////////////////////////////////////////////////////////

    (** Concatentation.  The fixed point is lost by this operation and thus set to 0 *)
    static member (++) ((a : UFixed), (b : UFixed)) = 
      UFixed.to_UFixed(a.signal ++ b.signal, 0, a.round, b.overflow)

    (** Least significant bit.  Fixed point is kept *)
    member x.lsb = x.select(x.low,x.low)
    
    (** Most significant bit.  Fixed point is kept *)
    member x.msb = x.select(x.high,x.high)
    
    (** Least significant bits.  Fixed point is kept *)
    member x.lsbs = x.select(x.high-1,x.low)
    
    (** Most significant bits.  Fixed point is decremented *)
    member x.msbs = x.select(x.high,x.low+1,x.fixedPoint-1)

    // ////////////////////////////////////////////////////////
    // Bitwise operations
    // ////////////////////////////////////////////////////////

    // ////////////////////////////////////////////////////////
    // ////////////////////////////////////////////////////////

    (** Logical and. Both operands must have the same fixed point format *)
    static member (&&&) ((a : UFixed), (b : UFixed)) = 
      assert (a.high = b.high && a.low = b.low)
      UFixed.to_UFixed (a.signal &&& b.signal, a.fixedPoint, a.round, a.overflow)

    static member band ((a : UFixed), (b : UFixed)) = a &&& b
    static member band ((a : UFixed), (b : int)) = a &&& a.consti b
    static member band ((a : UFixed), (b : float)) = a &&& a.constf b

    static member (&~) ((a : UFixed), (b : UFixed)) = UFixed.band(a,b)
    static member (&~) ((a : UFixed), (b : int)) = UFixed.band(a,b)
    static member (&~) ((a : UFixed), (b : float)) = UFixed.band(a,b)

    // ////////////////////////////////////////////////////////
    // ////////////////////////////////////////////////////////

    (** Logical or. Both operands must have the same fixed point format *)
    static member (|||) ((a : UFixed), (b : UFixed)) = 
      assert (a.high = b.high && a.low = b.low)
      UFixed.to_UFixed (a.signal ||| b.signal, a.fixedPoint, a.round, a.overflow)

    static member bor ((a : UFixed), (b : UFixed)) = a ||| b
    static member bor ((a : UFixed), (b : int)) = a ||| a.consti b
    static member bor ((a : UFixed), (b : float)) = a ||| a.constf b

    static member (|~) ((a : UFixed), (b : UFixed)) = UFixed.bor(a,b)
    static member (|~) ((a : UFixed), (b : int)) = UFixed.bor(a,b)
    static member (|~) ((a : UFixed), (b : float)) = UFixed.bor(a,b)

    // ////////////////////////////////////////////////////////
    // ////////////////////////////////////////////////////////

    (** Logical xor. Both operands must have the same fixed point format *)
    static member (^^^) ((a : UFixed), (b : UFixed)) = 
      assert (a.high = b.high && a.low = b.low)
      UFixed.to_UFixed (a.signal ^^^ b.signal, a.fixedPoint, a.round, a.overflow)

    static member bxor ((a : UFixed), (b : UFixed)) = a ^^^ b
    static member bxor ((a : UFixed), (b : int)) = a ^^^ a.consti b
    static member bxor ((a : UFixed), (b : float)) = a ^^^ a.constf b

    static member (^~) ((a : UFixed), (b : UFixed)) = UFixed.bxor(a,b)
    static member (^~) ((a : UFixed), (b : int)) = UFixed.bxor(a,b)
    static member (^~) ((a : UFixed), (b : float)) = UFixed.bxor(a,b)

    // ////////////////////////////////////////////////////////
    // ////////////////////////////////////////////////////////

    (** Logical not. *)
    static member (~~~) (a : UFixed) = 
      UFixed.to_UFixed (~~~ a.signal, a.fixedPoint, a.round, a.overflow)

    // ////////////////////////////////////////////////////////
    // Comparison (note - it's not possible to overload the standard comparison operations unfortunately) 
    // ////////////////////////////////////////////////////////

    // ////////////////////////////////////////////////////////
    // ////////////////////////////////////////////////////////

    (** Equality.  Operands may have different fixed point types *)
    static member eq ((a : UFixed), (b : UFixed)) = 
      let r = a.range.union(b.range)
      let a, b = a.resize r, b.resize r
      UFixed.to_UFixed (a.signal ==~ b.signal, 0, a.round, a.overflow) 
    static member eq ((a : UFixed), (b : int)) = a ==~ (a.consti b)
    static member eq ((a : UFixed), (b : float)) = a ==~ (a.constf b)

    static member (==~) ((a : UFixed), (b : UFixed)) = UFixed.eq(a, b)
    static member (==~) ((a : UFixed), (b : int)) = UFixed.eq(a, b)
    static member (==~) ((a : UFixed), (b : float)) = UFixed.eq(a, b)

    // ////////////////////////////////////////////////////////
    // ////////////////////////////////////////////////////////

    (** Inequality.  Operands may have different fixed point types *)
    static member neq ((a : UFixed), (b : UFixed)) = 
      let r = a.range.union(b.range)
      let a, b = a.resize r, b.resize r
      UFixed.to_UFixed (a.signal /=~ b.signal, 0, a.round, a.overflow) 
    static member neq ((a : UFixed), (b : int)) = UFixed.(/=~) (a, (a.consti b))
    static member neq ((a : UFixed), (b : float)) = UFixed.(/=~) (a, (a.constf b))

    static member (/=~) ((a : UFixed), (b : UFixed)) = UFixed.neq(a,b) 
    static member (/=~) ((a : UFixed), (b : int)) = UFixed.neq(a,b)
    static member (/=~) ((a : UFixed), (b : float)) = UFixed.neq(a,b)

    // ////////////////////////////////////////////////////////
    // ////////////////////////////////////////////////////////
    
    (** Unsigned less than.  The arguments must have the same width. The returned UFixed is 1 bit wide. *)
    static member lsu ((a : UFixed), (b : UFixed)) = 
      let r = a.range.union(b.range)
      let a, b = a.resize r, b.resize r
      UFixed.to_UFixed (a.signal <~ b.signal, 0, a.round, a.overflow) 
    static member lsu ((a : UFixed), (b : int)) = UFixed.lsu(a, a.consti b)
    static member lsu ((a : UFixed), (b : float)) = UFixed.lsu(a, a.constf b)
    static member lsu ((a : int), (b : UFixed)) = UFixed.lsu(b.consti a, b)
    static member lsu ((a : float), (b : UFixed)) = UFixed.lsu(b.constf a, b)
    static member (<~) ((a : UFixed), (b : UFixed)) = UFixed.lsu(a, b)
    static member (<~) ((a : UFixed), (b : int)) = UFixed.lsu(a, b)
    static member (<~) ((a : UFixed), (b : float)) = UFixed.lsu(a, b)
    
    (** Unsigned greater than.  The arguments must have the same width. The returned UFixed is 1 bit wide. *)
    static member gtu ((a : UFixed), (b : UFixed)) = 
      let r = a.range.union(b.range)
      let a, b = a.resize r, b.resize r
      UFixed.to_UFixed (a.signal >~ b.signal, 0, a.round, a.overflow) 
    static member gtu ((a : UFixed), (b : int)) = UFixed.gtu(a, a.consti b)
    static member gtu ((a : UFixed), (b : float)) = UFixed.gtu(a, a.constf b)
    static member gtu ((a : int), (b : UFixed)) = UFixed.gtu(b.consti a, b)
    static member gtu ((a : float), (b : UFixed)) = UFixed.gtu(b.constf a, b)
    static member (>~) ((a : UFixed), (b : UFixed)) = UFixed.gtu(a, b)
    static member (>~) ((a : UFixed), (b : int)) = UFixed.gtu(a, b)
    static member (>~) ((a : UFixed), (b : float)) = UFixed.gtu(a, b)
  
    (** Unsigned less than or equal.  The arguments must have the same width. The returned UFixed is 1 bit wide. *)
    static member lsequ ((a : UFixed), (b : UFixed)) = 
      let r = a.range.union(b.range)
      let a, b = a.resize r, b.resize r
      UFixed.to_UFixed (a.signal <=~ b.signal, 0, a.round, a.overflow) 
    static member lsequ ((a : UFixed), (b : int)) = UFixed.lsequ(a, a.consti b)
    static member lsequ ((a : UFixed), (b : float)) = UFixed.lsequ(a, a.constf b)
    static member lsequ ((a : int), (b : UFixed)) = UFixed.lsequ(b.consti a, b)
    static member lsequ ((a : float), (b : UFixed)) = UFixed.lsequ(b.constf a, b)
    static member (<=~) ((a : UFixed), (b : UFixed)) = UFixed.lsequ(a, b)
    static member (<=~) ((a : UFixed), (b : int)) = UFixed.lsequ(a, b)
    static member (<=~) ((a : UFixed), (b : float)) = UFixed.lsequ(a, b)

    (** Unsigned greater than or equal.  The arguments must have the same width. The returned UFixed is 1 bit wide. *)
    static member gtequ ((a : UFixed), (b : UFixed)) = 
      let r = a.range.union(b.range)
      let a, b = a.resize r, b.resize r
      UFixed.to_UFixed (a.signal >=~ b.signal, 0, a.round, a.overflow) 
    static member gtequ ((a : UFixed), (b : int)) = UFixed.gtequ(a, a.consti b)
    static member gtequ ((a : UFixed), (b : float)) = UFixed.gtequ(a, a.constf b)
    static member gtequ ((a : int), (b : UFixed)) = UFixed.gtequ(b.consti a, b)
    static member gtequ ((a : float), (b : UFixed)) = UFixed.gtequ(b.constf a, b)
    static member (>=~) ((a : UFixed), (b : UFixed)) = UFixed.gtequ(a, b)
    static member (>=~) ((a : UFixed), (b : int)) = UFixed.gtequ(a, b)
    static member (>=~) ((a : UFixed), (b : float)) = UFixed.gtequ(a, b)

    // ////////////////////////////////////////////////////////
    // Shifting.  Note - no arithmetic shift right for unsigned terms
    // ////////////////////////////////////////////////////////

    (** Logical shift left.  Fix point is not moved *)
    static member (<<<) ((a : UFixed), (shift : int)) = UFixed.to_UFixed (a.signal <<< shift, a.fixedPoint, a.round, a.overflow)
    static member sll ((a : UFixed), (shift : int)) = a <<< shift
    static member sll ((a : UFixed), (shift : UFixed)) = UFixed.to_UFixed(shift_left a.signal shift.signal, a.fixedPoint, a.round, a.overflow)
    static member (<<~) ((a : UFixed), (shift : int)) = UFixed.sll(a,shift)
    static member (<<~) ((a : UFixed), (shift : UFixed)) = UFixed.sll(a,shift)

    (** Logical shift right.  Fix point is not moved *)
    static member (>>>) ((a : UFixed), (shift : int)) = UFixed.to_UFixed (a.signal >>> shift, a.fixedPoint, a.round, a.overflow)
    static member srl ((a : UFixed), (shift : int)) = a >>> shift
    static member srl ((a : UFixed), (shift : UFixed)) = UFixed.to_UFixed(shift_right_logical a.signal shift.signal, a.fixedPoint, a.round, a.overflow)
    static member (>>~) ((a : UFixed), (shift : int)) = UFixed.srl(a,shift)
    static member (>>~) ((a : UFixed), (shift : UFixed)) = UFixed.srl(a,shift)

    // ////////////////////////////////////////////////////////
    // registers 
    // ////////////////////////////////////////////////////////
    
    member d.reg(clock, reset, (reset_val : UFixed), enable) = 
      assert (reset_val.high = d.high && reset_val.low = d.low)
      UFixed.to_UFixed(d.signal.reg(clock, reset, reset_val.signal, enable), d.fixedPoint, d.round, d.overflow)

    member d.reg(clock, reset, (reset_val : int), enable) = d.reg(clock, reset, UFixed.consti(d.range, reset_val), enable)
    
    member d.reg(clock, reset, (reset_val : float), enable) = d.reg(clock, reset, UFixed.constf(d.range, reset_val), enable)

    (** Creates a synchronous write, asynchronous read memory array from which standard (FPGA) memories may be built *)
    member d.memory(size, clk, we, w, r) = UFixed.to_UFixed(d.signal.memory(size, clk, we, w, r), d.fixedPoint, d.round, d.overflow)

    member x.wire = UFixed.to_UFixed(Signal.wire x.width, x.fixedPoint, x.round, x.overflow)
    
    static member swire (range : Range) = UFixed.consti(range, 0).wire
    
    static member swire (fix : Fix) = UFixed.consti(fix, 0).wire
    
    static member (<==) ((a : UFixed), (b : UFixed)) = 
      a.signal <== b.[a.high,a.low].signal

    member s.set_name name = UFixed.to_UFixed(set_name s.signal name, s.fixedPoint, s.round, s.overflow);
    static member (--) ((s : UFixed), n) = s.set_name n

    // ////////////////////////////////////////////////////////
    // Muxing
    // ////////////////////////////////////////////////////////

    (* Mux a list of UFixed.  Format of each element may differ.  Rounding and overflow modes are propogated from first element in list. *)
    member x.mux (l : UFixed list) =
      let hd = List.hd l
      let high = List.fold_left (fun a (x : UFixed) -> max a x.high) (hd).high l
      let low = List.fold_left (fun a (x : UFixed) -> min a x.low) (hd).low l
      let r = range high low
      UFixed.to_UFixed ((x.signal.mux (List.map (fun (x : UFixed) -> (x.resize r).signal) l)), (-low), hd.round, hd.overflow)
      
    (* 2-input ufixed mux.   *)
    member x.mux2(a, b) = x.mux [ b; a ]

    // ////////////////////////////////////////////////////////
    // Behavioural code
    // ////////////////////////////////////////////////////////
    
    member x.b_if((on_true : Behave list), (on_false : Behave list)) = x.signal.b_if (on_true, on_false)
    member x.b_if on_true on_false = x.b_if(on_true, on_false)
    
    member x.b_switch (cases : (Signal * Behave list) list) = x.signal.b_switch cases
    
    member x.b_case (code : Behave list) = x.signal.b_case code
    
    member rstval.b_wire = 
      assert (rstval.width > 0)
      let f = rstval.fixedPoint
      let (B_assign_tgt(a,b,c,d,e)) = rstval.signal.b_wire
      let r,o = rstval.round,rstval.overflow
      UFixed_bassign_tgt(UFixed.to_UFixed(a,f,r,o),UFixed.to_UFixed(b,f,r,o),UFixed.to_UFixed(c,f,r,o),d,e)
      
    member rstval.b_reg(clock, reset, enable) = 
      assert (rstval.width > 0)
      let f = rstval.fixedPoint
      let (B_assign_tgt(a,b,c,d,e)) = rstval.signal.b_reg(clock, reset, enable)
      let r,o = rstval.round,rstval.overflow
      UFixed_bassign_tgt(UFixed.to_UFixed(a,f,r,o),UFixed.to_UFixed(b,f,r,o),UFixed.to_UFixed(c,f,r,o),d,e)

  end

/// Allows the use of UFixed types in behavioural code
and UFixedBehaveAssignTarget = UFixed_bassign_tgt of UFixed * UFixed * UFixed * int * int
  with
  
    member x.q = let (UFixed_bassign_tgt(q,_,_,_,_)) = x in q

    member target.assign (expr : UFixed) = 
      let (UFixed_bassign_tgt(a,b,c,d,e)) = target
      B_assign_tgt(a.signal,b.signal,c.signal,d,e) $== expr.signal
    member target.assign (expr : int) = target $== target.q.consti expr
    member target.assign (expr : float) = target $== target.q.constf expr

    static member ($==) ((target : UFixedBehaveAssignTarget), (expr : UFixed)) = target.assign expr
    
    static member ($==) ((target : UFixedBehaveAssignTarget), (expr : int)) = target.assign expr
    
    static member ($==) ((target : UFixedBehaveAssignTarget), (expr : float)) = target.assign expr
    
    member x.set_name name = 
      let (UFixed_bassign_tgt(a,b,c,d,e)) = x in
      UFixed_bassign_tgt(a.set_name name,b,c,d,e)

    static member (--) ((s : UFixedBehaveAssignTarget), n) = s.set_name n

  end

// ////////////////////////////////////////////////////////
// ////////////////////////////////////////////////////////

/// Signed fixed point type.  Note: regardless of the position of high, msb is always the sign bit.
type SFixed = 
  { 
    sf_signal : Signal;
    sf_range : Range;
    sf_round : Round;
    sf_overflow : Overflow;
  }
  with
    // ////////////////////////////////////////////////////////
    // Properties
    // ////////////////////////////////////////////////////////
    
    (** Internal signal *)
    member x.signal = x.sf_signal
    (** range *)
    member x.range = x.sf_range
    (* rounding mode *)
    member x.round = x.sf_round
    (* overflow mode *)
    member x.overflow = x.sf_overflow
    
    (** width of integer signal *)
    member x.width = x.signal.width
    (** Determine if the width of the signal and high and low are valid *)
    member x.valid = assert (x.signal.width = x.range.high - x.range.low + 1)
    (** Left most bit position *)
    member x.high = x.range.high
    (** Right most bit position *)
    member x.low = x.range.low
    (** range in fix_t format *)
    member x.fix = x.range.fix
    (** Position of fixed point *)
    member x.fixedPoint = x.fix.fixedPoint
    (** name of signal *)
    member x.name = x.signal.name

    member x.SetRoundingMode(r,o) = { x with sf_round = r; sf_overflow = o }
    
    (** return a sfixed with the fix point moved to the given position *)
    member x.SetFix f = SFixed.to_SFixed(x.signal, f, x.round, x.overflow)

    // ////////////////////////////////////////////////////////
    // Creation from signals
    // ////////////////////////////////////////////////////////

    (** Create a sfixed from a signal.  The fixed point is implicitly set to 0 *)
    static member of_Signal (s : Signal) = { sf_signal = s; sf_range = { r_high = s.width - 1; r_low = 0 }; sf_round = default_sfixed_round(); sf_overflow = default_sfixed_overflow(); }

    (** Create a sfixed from a signal with the given fixed point *)
    static member to_SFixed ((s : Signal), f, r, o) = { sf_signal = s; sf_range = { r_high = s.width - f - 1; r_low = -f }; sf_round = r; sf_overflow = o; }
    static member to_SFixed ((s : Signal), f) = { sf_signal = s; sf_range = { r_high = s.width - f - 1; r_low = -f }; sf_round = default_sfixed_round(); sf_overflow = default_sfixed_overflow(); }

    // ////////////////////////////////////////////////////////
    // Selection and resizing
    // ////////////////////////////////////////////////////////

    (* Select a range of bits.  Range must be within the SFixed.  Fixed point is kept *)
    member x.select ((a : int),(b : int)) = SFixed.to_SFixed(x.signal.[a+x.fixedPoint .. b+x.fixedPoint], x.fixedPoint, x.round, x.overflow)
    (* Select a range of bits.  Range must be within the SFixed.  New fixed point is applied *)
    member x.select ((a : int),(b : int),(c : int)) = SFixed.to_SFixed(x.signal.[a+x.fixedPoint .. b+x.fixedPoint], c, x.round, x.overflow)

    (** Resize SFixed to given range.  Vector is padded (with msb at left or zeros at right) or truncated to fit range.  No rounding of overflow is applied. *)
    member x.resize(r : Range) = 
      let s = x.signal
      let s = 
        if r.high = x.high then s
        else if r.high > x.high then (s.msb.repeat (r.high - x.high)) ++ s
        else s.[s.width - (x.high - r.high) - 1 .. 0]
      let s =
        if r.low = x.low then s
        else if r.low > x.low then s.[s.width - 1 .. r.low-x.low]
        else s ++ (Signal.zero (x.low - r.low))
      let u = SFixed.to_SFixed (s, (-r.low), x.round, x.overflow)
      u.valid
      u
      
    (** Resizes the vector so that it is as least as big as the given range *)
    member x.resize_to(range : Range) = x.resize(x.range.union(range))
    
    (** Resizes the vector so that it is as least as big as the given range (in Fix format) *)
    member x.resize_to(fix : Fix) = x.resize(x.range.union(fix.range))

    (** Resizing of vector using the supplied rounding and overflow mode. *)
    member x.resize (r,o,high,low) = 
      let range = x.range
      let (x : Signal), (range : Range) = r x.signal range low
      let (x : Signal), (range : Range) = o x range high
      let x = SFixed.to_SFixed(x, range.fixedPoint)
      { x with sf_round = r; sf_overflow = o }


    (** Resizing of vector using the rounding and overflow modes set on the SFixed. *)
    member x.Item with get((high : int),(low : int)) = x.resize(x.round,x.overflow,high,low)

    // ////////////////////////////////////////////////////////
    // ////////////////////////////////////////////////////////

    (** Convert the given floating point number to a fixed point integer.  Rounding is performed if rnd is true.  *)
    static member fixed_of_float' (rnd, f, v) =
      let scale = 2.0 ** (float f)
      let rnd = if rnd then (if v >= 0.0 then 0.5 else - 0.5) else 0.0
      int ((v * scale) + rnd)

    (** Convert the given floating point number to a fixed point integer.  Rounding is performed.  *)
    member x.fixed_of_float v = SFixed.fixed_of_float' (true, x.fixedPoint, v)

    (** Convert the given fixed point integer to a floating point number *)
    member x.float_of_fixed v = 
      let v = float v
      let scale = 2.0 ** (float x.fixedPoint)
      (v / scale)

    (** Return a string encoding the range of this sfixed *)
    member x.to_string = 
      "[" ^ string x.high ^ ":" ^ string x.low ^ "] (f=" ^ string x.fixedPoint ^ " w=" ^ string x.width ^ ")"

    (** fixed point constant from an integer *)
    static member consti ((fix : Fix), i) = SFixed.to_SFixed(consti fix.width i, fix.fixedPoint)
    
    static member consti ((r : Range), i) = SFixed.consti(r.fix, i)
    
    (** fixed point constant from an integer *)
    member x.consti i = SFixed.consti(x.range, i).SetRoundingMode(x.round, x.overflow)
    
    (** fixed point constant from a float.  Rounding optional *)
    static member constf' (r, (fix : Fix), i) = SFixed.consti(fix, (SFixed.fixed_of_float' (r, fix.fixedPoint, i)))
    
    static member constf' (r, (range : Range), i) = SFixed.constf'(r, range.fix, i)
    
    (** fixed point constant from a float.  Performs rounding *)
    static member constf ((fix : Fix), i) = SFixed.constf'(true, fix, i)
    
    static member constf ((range : Range), i) = SFixed.constf'(true, range, i)

    (** fixed point constant from a float.  Performs rounding *)
    member x.constf i = SFixed.constf(x.range, i).SetRoundingMode(x.round, x.overflow)

    member x.zero = x.constf 0.0
    member x.one = x.resize_to(range 0 0).constf 1.0
    member x.two = x.resize_to(range 1 1).constf 2.0
    member x.half = x.resize_to(range (-1) (-1)).constf 0.5
    member x.quarter = x.resize_to(range (-2) (-2)).constf 0.25

    // ////////////////////////////////////////////////////////
    // Arithmetic.  Rounding and overflow modes propogated from left operand
    // ////////////////////////////////////////////////////////

    (** Addition.  The two input numbers are converted so that their ranges are compatible then extended by one bit. *)
    static member (+) ((a : SFixed), (b : SFixed)) = 
      let {r_high=high; r_low=low} = a.range.union(b.range)
      let a, b = a.resize (range (high + 1) low), b.resize (range (high + 1) low)
      SFixed.to_SFixed ((a.signal + b.signal), (-(min a.low b.low)), a.round, a.overflow)

    (** Addition with an integer operand on the right.  The integer operand is converted to the same fixed point format as the left operand *)
    static member (+) ((a : SFixed), (b : int)) = a + a.consti b

    (** Addition with a float operand on the right.  The integer operand is converted to the same fixed point format as the left operand *)
    static member (+) ((a : SFixed), (b : float)) = a + a.constf b

    // ////////////////////////////////////////////////////////
    // ////////////////////////////////////////////////////////
    
    static member sub ((a : int), (b : SFixed)) = b.consti a - b
    static member sub ((a : float), (b : SFixed)) = b.constf a - b

    (** Subtraction.  The two input numbers are converted so that their ranges are compatible then extended by one bit.  The result therefore cannot overflow *)
    static member (-) ((a : SFixed), (b : SFixed)) = 
      let {r_high=high; r_low=low} = a.range.union(b.range)
      let a, b = a.resize (range (high + 1) low), b.resize (range (high + 1) low)
      let s = SFixed.to_SFixed ((a.signal - b.signal), (-(min a.low b.low)), a.round, a.overflow)
      s
    
    (** Subtraction with an integer operand on the right.  The integer operand is converted to the same fixed point format as the left operand *)
    static member (-) ((a : SFixed), (b : int)) = a - a.consti b

    (** Subtraction with a float operand on the right.  The integer operand is converted to the same fixed point format as the left operand *)
    static member (-) ((a : SFixed), (b : float)) = a - a.constf b

    // ////////////////////////////////////////////////////////
    // ////////////////////////////////////////////////////////

    (** Multiplication. *)
    static member ( * ) ((a : SFixed), (b : SFixed)) = 
      SFixed.to_SFixed ((a.signal *+ b.signal), (a.fixedPoint + b.fixedPoint), a.round, a.overflow)
    
    (** Multiplication with an integer operand on the right.  The integer operand is converted to the same fixed point format as the left operand XXX is this the best way to do things? XXX *)
    static member ( * ) ((a : SFixed), (b : int)) = a * a.consti b

    (** Multiplication with a float operand on the right.  The float operand is converted to the same fixed point format as the left operand XXX is this the best way to do things? XXX *)
    static member ( * ) ((a : SFixed), (b : float)) = a * a.constf b

    // ////////////////////////////////////////////////////////
    // concantenation
    // ////////////////////////////////////////////////////////

    (** Concatentation.  The fixed point is lost by this operation and thus set to 0 *)
    static member (++) ((a : SFixed), (b : SFixed)) = 
      SFixed.to_SFixed(a.signal ++ b.signal, 0, a.round, b.overflow)

    (** Least significant bit.  Fixed point is kept *)
    member x.lsb = x.select(x.low,x.low)
    
    (** Most significant bit.  Fixed point is kept *)
    member x.msb = x.select(x.high,x.high)
    
    (** Least significant bits.  Fixed point is kept *)
    member x.lsbs = x.select(x.high-1,x.low)
    
    (** Most significant bits.  Fixed point is decremented *)
    member x.msbs = x.select(x.high,x.low+1,x.fixedPoint-1)

    // ////////////////////////////////////////////////////////
    // Bitwise operations
    // ////////////////////////////////////////////////////////

    // ////////////////////////////////////////////////////////
    // ////////////////////////////////////////////////////////

    (** Logical and. Both operands must have the same fixed point format *)
    static member (&&&) ((a : SFixed), (b : SFixed)) = 
      assert (a.high = b.high && a.low = b.low)
      SFixed.to_SFixed (a.signal &&& b.signal, a.fixedPoint, a.round, a.overflow)

    static member band ((a : SFixed), (b : SFixed)) = a &&& b
    static member band ((a : SFixed), (b : int)) = a &&& a.consti b
    static member band ((a : SFixed), (b : float)) = a &&& a.constf b

    static member (&~) ((a : SFixed), (b : SFixed)) = SFixed.band(a,b)
    static member (&~) ((a : SFixed), (b : int)) = SFixed.band(a,b)
    static member (&~) ((a : SFixed), (b : float)) = SFixed.band(a,b)

    // ////////////////////////////////////////////////////////
    // ////////////////////////////////////////////////////////

    (** Logical or. Both operands must have the same fixed point format *)
    static member (|||) ((a : SFixed), (b : SFixed)) = 
      assert (a.high = b.high && a.low = b.low)
      SFixed.to_SFixed (a.signal ||| b.signal, a.fixedPoint, a.round, a.overflow)

    static member bor ((a : SFixed), (b : SFixed)) = a ||| b
    static member bor ((a : SFixed), (b : int)) = a ||| a.consti b
    static member bor ((a : SFixed), (b : float)) = a ||| a.constf b

    static member (|~) ((a : SFixed), (b : SFixed)) = SFixed.bor(a,b)
    static member (|~) ((a : SFixed), (b : int)) = SFixed.bor(a,b)
    static member (|~) ((a : SFixed), (b : float)) = SFixed.bor(a,b)

    // ////////////////////////////////////////////////////////
    // ////////////////////////////////////////////////////////

    (** Logical xor. Both operands must have the same fixed point format *)
    static member (^^^) ((a : SFixed), (b : SFixed)) = 
      assert (a.high = b.high && a.low = b.low)
      SFixed.to_SFixed (a.signal ^^^ b.signal, a.fixedPoint, a.round, a.overflow)

    static member bxor ((a : SFixed), (b : SFixed)) = a ^^^ b
    static member bxor ((a : SFixed), (b : int)) = a ^^^ a.consti b
    static member bxor ((a : SFixed), (b : float)) = a ^^^ a.constf b

    static member (^~) ((a : SFixed), (b : SFixed)) = SFixed.bxor(a,b)
    static member (^~) ((a : SFixed), (b : int)) = SFixed.bxor(a,b)
    static member (^~) ((a : SFixed), (b : float)) = SFixed.bxor(a,b)

    // ////////////////////////////////////////////////////////
    // ////////////////////////////////////////////////////////

    (** Logical not. *)
    static member (~~~) (a : SFixed) = 
      SFixed.to_SFixed (~~~ a.signal, a.fixedPoint, a.round, a.overflow)

    // ////////////////////////////////////////////////////////
    // Comparison (note - it's not possible to overload the standard comparison operations unfortunately) 
    // ////////////////////////////////////////////////////////

    // ////////////////////////////////////////////////////////
    // ////////////////////////////////////////////////////////

    (** Equality.  Operands may have different fixed point types *)
    static member eq ((a : SFixed), (b : SFixed)) = 
      let r = a.range.union (b.range)
      let a, b = a.resize r, b.resize r
      SFixed.to_SFixed (a.signal ==~ b.signal, 0, a.round, a.overflow) 
    static member eq ((a : SFixed), (b : int)) = a ==~ (a.consti b)
    static member eq ((a : SFixed), (b : float)) = a ==~ (a.constf b)

    static member (==~) ((a : SFixed), (b : SFixed)) = SFixed.eq(a, b)
    static member (==~) ((a : SFixed), (b : int)) = SFixed.eq(a, b)
    static member (==~) ((a : SFixed), (b : float)) = SFixed.eq(a, b)

    // ////////////////////////////////////////////////////////
    // ////////////////////////////////////////////////////////

    (** Inequality.  Operands may have different fixed point types *)
    static member neq ((a : SFixed), (b : SFixed)) = 
      let r = a.range.union (b.range)
      let a, b = a.resize r, b.resize r
      SFixed.to_SFixed (a.signal /=~ b.signal, 0, a.round, a.overflow) 
    static member neq ((a : SFixed), (b : int)) = SFixed.(/=~) (a, (a.consti b))
    static member neq ((a : SFixed), (b : float)) = SFixed.(/=~) (a, (a.constf b))

    static member (/=~) ((a : SFixed), (b : SFixed)) = SFixed.neq(a,b) 
    static member (/=~) ((a : SFixed), (b : int)) = SFixed.neq(a,b)
    static member (/=~) ((a : SFixed), (b : float)) = SFixed.neq(a,b)

    // ////////////////////////////////////////////////////////
    // ////////////////////////////////////////////////////////

    (** Signed less than.  The arguments must have the same width. The returned SFixed is 1 bit wide. *)
    static member lss ((a : SFixed), (b : SFixed)) =
      let r = a.range.union (b.range)
      let a, b = a.resize r, b.resize r
      SFixed.to_SFixed (a.signal <+ b.signal, 0, a.round, a.overflow) 
    static member lss ((a : SFixed), (b : int)) = SFixed.lss(a, a.consti b)
    static member lss ((a : SFixed), (b : float)) = SFixed.lss(a, a.constf b)
    static member lss ((a : int), (b : SFixed)) = SFixed.lss(b.consti a, b)
    static member lss ((a : float), (b : SFixed)) = SFixed.lss(b.constf a, b)
    static member (<+) ((a : SFixed), (b : SFixed)) = SFixed.lss(a, b)
    static member (<+) ((a : SFixed), (b : int)) = SFixed.lss(a, b)
    static member (<+) ((a : SFixed), (b : float)) = SFixed.lss(a, b)
    
    (** Signed greater than.  The arguments must have the same width. The returned SFixed is 1 bit wide. *)
    static member gts ((a : SFixed), (b : SFixed)) = 
      let r = a.range.union (b.range)
      let a, b = a.resize r, b.resize r
      SFixed.to_SFixed (a.signal >+ b.signal, 0, a.round, a.overflow) 
    static member gts ((a : SFixed), (b : int)) = SFixed.gts(a, a.consti b)
    static member gts ((a : SFixed), (b : float)) = SFixed.gts(a, a.constf b)
    static member gts ((a : int), (b : SFixed)) = SFixed.gts(b.consti a, b)
    static member gts ((a : float), (b : SFixed)) = SFixed.gts(b.constf a, b)
    static member (>+) ((a : SFixed), (b : SFixed)) = SFixed.gts(a, b)
    static member (>+) ((a : SFixed), (b : int)) = SFixed.gts(a, b)
    static member (>+) ((a : SFixed), (b : float)) = SFixed.gts(a, b)
  
    (** Signed less than or equal.  The arguments must have the same width. The returned SFixed is 1 bit wide. *)
    static member lseqs ((a : SFixed), (b : SFixed)) = 
      let r = a.range.union (b.range)
      let a, b = a.resize r, b.resize r
      SFixed.to_SFixed (a.signal <=+ b.signal, 0, a.round, a.overflow) 
    static member lseqs ((a : SFixed), (b : int)) = SFixed.lseqs(a, a.consti b)
    static member lseqs ((a : SFixed), (b : float)) = SFixed.lseqs(a, a.constf b)
    static member lseqs ((a : int), (b : SFixed)) = SFixed.lseqs(b.consti a, b)
    static member lseqs ((a : float), (b : SFixed)) = SFixed.lseqs(b.constf a, b)
    static member (<=+) ((a : SFixed), (b : SFixed)) = SFixed.lseqs(a, b)
    static member (<=+) ((a : SFixed), (b : int)) = SFixed.lseqs(a, b)
    static member (<=+) ((a : SFixed), (b : float)) = SFixed.lseqs(a, b)

    (** Signed greater than or equal.  The arguments must have the same width. The returned SFixed is 1 bit wide. *)
    static member gteqs ((a : SFixed), (b : SFixed)) = 
      let r = a.range.union (b.range)
      let a, b = a.resize r, b.resize r
      SFixed.to_SFixed (a.signal >=+ b.signal, 0, a.round, a.overflow) 
    static member gteqs ((a : SFixed), (b : int)) = SFixed.gteqs(a, a.consti b)
    static member gteqs ((a : SFixed), (b : float)) = SFixed.gteqs(a, a.constf b)
    static member gteqs ((a : int), (b : SFixed)) = SFixed.gteqs(b.consti a, b)
    static member gteqs ((a : float), (b : SFixed)) = SFixed.gteqs(b.constf a, b)
    static member (>=+) ((a : SFixed), (b : SFixed)) = SFixed.gteqs(a, b)
    static member (>=+) ((a : SFixed), (b : int)) = SFixed.gteqs(a, b)
    static member (>=+) ((a : SFixed), (b : float)) = SFixed.gteqs(a, b)

    // ////////////////////////////////////////////////////////
    // Shifting.  
    // ////////////////////////////////////////////////////////

    (** Logical shift left.  Fix point is not moved *)
    static member (<<<) ((a : SFixed), (shift : int)) = SFixed.to_SFixed (a.signal <<< shift, a.fixedPoint, a.round, a.overflow)
    static member sll ((a : SFixed), (shift : int)) = a <<< shift
    static member sll ((a : SFixed), (shift : SFixed)) = SFixed.to_SFixed(a.signal <<~ shift.signal, a.fixedPoint, a.round, a.overflow)
    static member (<<~) ((a : SFixed), (shift : int)) = SFixed.sll(a,shift)
    static member (<<~) ((a : SFixed), (shift : SFixed)) = SFixed.sll(a,shift)

    (** Logical shift right.  Fix point is not moved *)
    static member (>>>) ((a : SFixed), (shift : int)) = SFixed.to_SFixed (a.signal >>> shift, a.fixedPoint, a.round, a.overflow)
    static member srl ((a : SFixed), (shift : int)) = a >>> shift
    static member srl ((a : SFixed), (shift : SFixed)) = SFixed.to_SFixed(a.signal >>~ shift.signal, a.fixedPoint, a.round, a.overflow)
    static member (>>~) ((a : SFixed), (shift : int)) = SFixed.srl(a,shift)
    static member (>>~) ((a : SFixed), (shift : SFixed)) = SFixed.srl(a,shift)

    (** Arithmetic shift right.  Fix point is not moved *)
    static member (>>+) ((a : SFixed), (shift : int)) = SFixed.to_SFixed (a.signal >>+ shift, a.fixedPoint, a.round, a.overflow)
    static member sra ((a : SFixed), (shift : int)) = a >>+ shift 
    static member sra ((a : SFixed), (shift : SFixed)) = SFixed.to_SFixed(a.signal >>+ shift.signal, a.fixedPoint, a.round, a.overflow)
    static member (>>+) ((a : SFixed), (shift : SFixed)) = SFixed.sra(a,shift)

    // ////////////////////////////////////////////////////////
    // registers 
    // ////////////////////////////////////////////////////////
    
    member d.reg(clock, reset, (reset_val : SFixed), enable) = 
      assert (reset_val.high = d.high && reset_val.low = d.low)
      SFixed.to_SFixed(d.signal.reg(clock, reset, reset_val.signal, enable), d.fixedPoint, d.round, d.overflow)

    member d.reg(clock, reset, (reset_val : int), enable) = d.reg(clock, reset, SFixed.consti(d.range, reset_val), enable)
    
    member d.reg(clock, reset, (reset_val : float), enable) = d.reg(clock, reset, SFixed.constf(d.range, reset_val), enable)

    (** Creates a synchronous write, asynchronous read memory array from which standard (FPGA) memories may be built *)
    member d.memory(size, clk, we, w, r) = SFixed.to_SFixed(d.signal.memory(size, clk, we, w, r), d.fixedPoint, d.round, d.overflow)

    member x.wire = SFixed.to_SFixed(Signal.wire x.width, x.fixedPoint, x.round, x.overflow)
    
    static member swire (range : Range) = SFixed.consti(range, 0).wire
    
    static member swire (fix : Fix) = SFixed.consti(fix, 0).wire
    
    static member (<==) ((a : SFixed), (b : SFixed)) = 
      a.signal <== b.[a.high,a.low].signal

    member s.set_name name = SFixed.to_SFixed(set_name s.signal name, s.fixedPoint, s.round, s.overflow);
    static member (--) ((s : SFixed), n) = s.set_name n

    // ////////////////////////////////////////////////////////
    // Muxing
    // ////////////////////////////////////////////////////////

    (* Mux a list of SFixed.  Format of each element may differ.  Rounding and overflow modes are propogated from first element in list. *)
    member x.mux (l : SFixed list) =
      let hd = List.hd l
      let range = Range.union (List.map (fun (x : SFixed) -> x.range) l)
      SFixed.to_SFixed ((x.signal.mux (List.map (fun (x : SFixed) -> (x.resize_to range).signal) l)), range.fixedPoint, hd.round, hd.overflow)
      
    (* 2-input sfixed mux.   *)
    member x.mux2(a, b) = x.mux [ b; a ]

    // ////////////////////////////////////////////////////////
    // Behavioural code
    // ////////////////////////////////////////////////////////
    
    member x.b_if((on_true : Behave list), (on_false : Behave list)) = x.signal.b_if (on_true, on_false)
    member x.b_if on_true on_false = x.b_if(on_true, on_false)

    member x.b_switch (cases : (Signal * Behave list) list) = x.signal.b_switch cases
    
    member x.b_case (code : Behave list) = x.signal.b_case code
    
    member rstval.b_wire = 
      assert (rstval.width > 0)
      let f = rstval.fixedPoint
      let (B_assign_tgt(a,b,c,d,e)) = rstval.signal.b_wire
      let r,o = rstval.round,rstval.overflow
      SFixed_bassign_tgt(SFixed.to_SFixed(a,f,r,o),SFixed.to_SFixed(b,f,r,o),SFixed.to_SFixed(c,f,r,o),d,e)
      
    member rstval.b_reg(clock, reset, enable) = 
      assert (rstval.width > 0)
      let f = rstval.fixedPoint
      let (B_assign_tgt(a,b,c,d,e)) = rstval.signal.b_reg(clock, reset, enable)
      let r,o = rstval.round,rstval.overflow
      SFixed_bassign_tgt(SFixed.to_SFixed(a,f,r,o),SFixed.to_SFixed(b,f,r,o),SFixed.to_SFixed(c,f,r,o),d,e)

  end

/// Allows the use of SFixed types in behavioural code
and SFixedBehaveAssignTarget = SFixed_bassign_tgt of SFixed * SFixed * SFixed * int * int
  with
  
    member x.q = let (SFixed_bassign_tgt(q,_,_,_,_)) = x in q

    member target.assign (expr : SFixed) = 
      let (SFixed_bassign_tgt(a,b,c,d,e)) = target
      B_assign_tgt(a.signal,b.signal,c.signal,d,e) $== expr.signal
    member target.assign (expr : int) = target $== target.q.consti expr
    member target.assign (expr : float) = target $== target.q.constf expr

    static member ($==) ((target : SFixedBehaveAssignTarget), (expr : SFixed)) = target.assign expr
    
    static member ($==) ((target : SFixedBehaveAssignTarget), (expr : int)) = target.assign expr
    
    static member ($==) ((target : SFixedBehaveAssignTarget), (expr : float)) = target.assign expr
    
    member x.set_name name = 
      let (SFixed_bassign_tgt(a,b,c,d,e)) = x in
      SFixed_bassign_tgt(a.set_name name,b,c,d,e)

    static member (--) ((s : UFixedBehaveAssignTarget), n) = s.set_name n

  end
  

// ////////////////////////////////////////////////////////
// Properly initialise the rounding mode logic
// ////////////////////////////////////////////////////////

let truncate (x : Signal) (xr : Range) low = 
  let x = UFixed.to_UFixed(x, xr.fixedPoint)
  let fix = - low
  let range = range (max x.high low) low in
  let x : UFixed = x.resize_to range in
  let x = x.select(x.high, low, fix)
  x.signal, x.range

(** Round towards +ve infinity *)
let round_inf_u (x : Signal) (xr : Range) low = 
  let x = UFixed.to_UFixed(x, xr.fixedPoint)
  let fix = - low
  let range = range (max x.high low) low in
  let x : UFixed = x.resize_to range in
  let x = 
    if fix < x.fixedPoint then
      let round : UFixed = UFixed.consti({r_high=range.high; r_low=range.low-1}, 1) in
      let x : UFixed = x + round in
      x.select(x.high, low, fix)
    else
      x.select(x.high, low, fix)
  x.signal, x.range

(** Round towards -ve infinity *)
let round_ninf_u x r l = truncate x r l

(** Round towards zero *)
let round_zero_u x r l = truncate x r l

(** Round away from zero *)
let round_away_zero_u x r l = round_inf_u x r l

(** Round to nearest.  The results are rounded to the nearest representable value. 
    If the result is midway between two representable values, the even representable 
    is chosen. Even here means the lowest-order bit is zero *)
let round_nearest_u (x : Signal) (xr : Range) low = 
  let x = UFixed.to_UFixed(x, xr.fixedPoint)
  let x =
    if x.low >= low then
      (x.resize_to {r_high=x.high; r_low=low})
    else
      let fix = - low
      let min_range = {r_high=low+1; r_low=low-2} in
      let x = x.resize_to min_range
      let lsb : UFixed = x.select(low,low,0) in
      let msb = x.select(low-1,low-1,0) in
      let lsbs = x.select(low-2,x.low,0) in
      let rnd = (msb &&& (lsb ||| (lsbs /=~ 0))).mux2(UFixed.consti({r_high=low-1; r_low=low-1}, 1), UFixed.consti({r_high=low-1; r_low=low-1}, 0)) in
      let x : UFixed = x + rnd in
      x.select(x.high,low,fix)
  x.signal, x.range

let round_s (op : SFixed -> int -> SFixed) (x : Signal) (xr : Range) low =
  let x = SFixed.to_SFixed(x, xr.fixedPoint)
  let fix = - low
  let range = range (max x.high low) low in
  let x : SFixed = x.resize_to range in
  let x = 
    if fix < x.fixedPoint then
      let x : SFixed = op x range.low
      x.select(x.high, low, fix)
    else
      x.select(x.high, low, fix)
  x.signal, x.range

(** Round towards +ve infinity *)
let round_inf_s (x : Signal) (xr : Range) low = round_s (fun (x : SFixed) low -> x + SFixed.consti({r_high=low; r_low=low-1}, 1)) x xr low

(** Round towards -ve infinity *)
let round_ninf_s x r l = truncate x r l

(** Round towards zero *)
let round_zero_s (x : Signal) (xr : Range) low = 
  round_s (fun (x : SFixed) low -> 
    let c i = SFixed.consti({r_high=low; r_low=low-1}, i)
    x + x.msb.mux2(c 1, c 0)) x xr low
    
(** Round away from zero *)
let round_away_zero_s (x : Signal) (xr : Range) low  = 
  round_s (fun (x : SFixed) low -> 
    let c i = SFixed.consti({r_high=low; r_low=low-1}, i)
    x + x.msb.mux2(c 0, c 1)) x xr low

(** Round to nearest.  The results are rounded to the nearest representable value. 
    If the result is midway between two representable values, the even representable 
    is chosen. Even here means the lowest-order bit is zero *)
let round_nearest_s (x : Signal) (xr : Range) low = 
  let x = SFixed.to_SFixed(x, xr.fixedPoint)
  let x =
    if x.low >= low then
      (x.resize_to {r_high=x.high; r_low=low})
    else
      let fix = - low
      let min_range = {r_high=low+1; r_low=low-2} in
      let x = x.resize_to min_range
      let lsb : SFixed = x.select(low,low,0) in
      let msb = x.select(low-1,low-1,0) in
      let lsbs = x.select(low-2,x.low,0) in
      let rnd = (msb &&& (lsb ||| (lsbs /=~ 0))).mux2(SFixed.consti({r_high=low; r_low=low-1}, 1), SFixed.consti({r_high=low; r_low=low-1}, 0)) in      ////???? not always -1??
      let x : SFixed = x + rnd in
      x.select(x.high,low,fix)
  x.signal, x.range

let wrap (x : Signal) (xr : Range) high = 
  let x = UFixed.to_UFixed(x, xr.fixedPoint)
  let range = range x.high (min high x.low) in
  let x = x.resize_to range in
  let x = 
    if x.high > high then x.select(high, x.low, -x.low)
    else (x.resize_to ({r_high=high; r_low=x.low}))
  x.signal, x.range

let saturate_u (x : Signal) (xr : Range) high = 
  let x = UFixed.to_UFixed(x, xr.fixedPoint)
  let range = range x.high (min high x.low) in
  let x : UFixed = x.resize_to range in
  let x = 
    if x.high > high then
      let resized : UFixed = x.select(high, x.low, -x.low)
      let cond : UFixed = (x.select(x.high,high+1,0) ==~ 0)
      cond.mux2(resized, resized.consti (-1))
    else 
      (x.resize_to ({r_high=high; r_low=x.low}))
  x.signal, x.range

let saturate_s (x : Signal) (xr : Range) high = 
  let x = SFixed.to_SFixed(x, xr.fixedPoint)
  let range = range x.high (min high x.low) in
  let x : SFixed = x.resize_to range in
  let x = 
    if x.high > high then
      let resized : SFixed = x.select(high, x.low, -x.low)
      let ovfl = x.select(x.high,high+1,0).signal /=~ resized.msb.signal.repeat (x.high-high)
      let ovfl = SFixed.to_SFixed(ovfl,0)
      let min = resized.consti 0
      let min = ((~~~ min.msb) ++ min.lsbs).SetFix resized.fixedPoint
      let max = resized.consti (-1)
      let max = ((~~~ max.msb) ++ max.lsbs).SetFix resized.fixedPoint
      ovfl.mux2(x.msb.mux2(min, max), resized)
    else 
      (x.resize_to ({r_high=high; r_low=x.low}))
  x.signal, x.range

do set_default_ufixed_rounding_mode round_nearest_u saturate_u
do set_default_sfixed_rounding_mode round_nearest_s saturate_s
