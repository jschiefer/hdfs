#light

(*

    Unit tests for the IntBits and StringBits types (can also be used to test the ArrayBit type)
    
    We test against integers so none of the tests can exceed 32 bits.  In fact due to the random
    number generator this is actually 30 bits.  This is ok - the IntBits sturture is based on a
    bit list representation so it should be safe to infer the tests are ok for larger bit precisions.
    
    With this type tested the idea is to use it as the number representation to test the simulator.

 *)

open NUnit.Framework
open DigitalLogic
open Numeric
open Ops
open IntBits
open StringBits
open ArrayBits

(* Choose one of the types below for the test.  
   IntBits provides the implementation for StringBits and is the more important thing to check. 
   ArrayBits is only checked up to about 30 bits which isnt actually all that useful.  *)  

type T = IntBits
module TM = IntBits

//type T = StringBits
//module TM = StringBits

//type T = ArrayBits
//module TM = ArrayBits

[<TestFixture>]
type HdfsRegressIntBits = 
  class

    val complete_tests : int list;
    val random_tests : int list;
    val complete_tests2 : (int*int) list;
    val random_tests2 : (int*int) list;
    val num_random_tests : int;

    new() = 
      {
        complete_tests = [ 1; 2; 3; 4; 5; 6 ];
        random_tests = [ 16; 29; 30 ];
        complete_tests2 = [ 1,1; 1,2; 1,2; 3,2; 4,5; 5,3; 6,5 ];
        random_tests2 = [ 16,13; 12,13; 4,18 ];
        num_random_tests = 1000;
      }

    (* -------------------------------------------------------------------------- *)
    (* -------------------------------------------------------------------------- *)

    (* A op B: A.Width = b.Width. Output width specified (either arg width or 1) *)
    
    member x.TestBinaryOpSameSizeArgsComplete (op0 : T -> T -> T) (op1 : int -> int -> int) w wo = 
      let mask = ~~~(-1 <<< wo)
      assert (w <= 32)
      for i = 0 to (1<<<w) - 1 do
        for j = 0 to (1<<<w) - 1 do
          let a = (op0 (TM.of_int w i) (TM.of_int w j)).to_int
          let b = ((op1 i j) &&& mask)
          let report = ("(" ^ string_of_int w ^ "," ^ string_of_int wo ^ ") " ^ string_of_int i ^ " op " ^ string_of_int j ^ " = " ^ string_of_int a ^ " <> " ^ string_of_int b)
          let test = a = b
          Assertion.Assert(report, test)

    member x.TestBinaryOpSameSizeArgsRandom (op0 : T -> T -> T) (op1 : int -> int -> int) w wo n = 
      let mask = ~~~(-1 <<< wo)
      assert (w <= 32)
      for i = 0 to n-1 do
        let i = rand 0 ((1<<<w)-1)
        let j = rand 0 ((1<<<w)-1)
        let a = (op0 (TM.of_int w i) (TM.of_int w j)).to_int
        let b = ((op1 i j) &&& mask)
        let report = ("(" ^ string_of_int w ^ "," ^ string_of_int wo ^ ") " ^ string_of_int i ^ " op " ^ string_of_int j ^ " = " ^ string_of_int a ^ " <> " ^ string_of_int b)
        let test = a = b
        Assertion.Assert(report, test)

    (* A op B: A.Width <> b.Width. Output width = A.Width + B.Width *)

    member x.TestBinaryOpDifferentSizeArgsComplete (op0 : T -> T -> T) (op1 : int -> int -> int) w0 w1 = 
      let mask = ~~~(-1 <<< (w0 + w1))
      assert (w0 + w1 <= 32)
      for i = 0 to (1<<<w0) - 1 do
        for j = 0 to (1<<<w1) - 1 do
          let a = (op0 (TM.of_int w0 i) (TM.of_int w1 j)).to_int
          let b = ((op1 i j) &&& mask)
          let report = ("(" ^ string_of_int w0 ^ "," ^ string_of_int w1 ^ ") " ^ string_of_int i ^ " op " ^ string_of_int j ^ " = " ^ string_of_int a ^ " <> " ^ string_of_int b)
          let test = a = b
          Assertion.Assert(report, test)

    member x.TestBinaryOpDifferentSizeArgsRandom (op0 : T -> T -> T) (op1 : int -> int -> int) w0 w1 n = 
      let mask = ~~~(-1 <<< (w0 + w1))
      assert (w0 + w1 <= 32)
      for i = 0 to n-1 do
        let i = rand 0 ((1<<<w0)-1)
        let j = rand 0 ((1<<<w1)-1)
        let a = (op0 (TM.of_int w0 i) (TM.of_int w1 j)).to_int
        let b = ((op1 i j) &&& mask)
        let report = ("(" ^ string_of_int w0 ^ "," ^ string_of_int w1 ^ ") " ^ string_of_int i ^ " op " ^ string_of_int j ^ " = " ^ string_of_int a ^ " <> " ^ string_of_int b)
        let test = a = b
        Assertion.Assert(report, test)

    (* Shift operators *)

    member x.TestShiftOpComplete (op0 : T -> int -> T) (op1 : int -> int -> int) w = 
      let mask = ~~~(-1 <<< w)
      assert (w <= 32)
      for i = 0 to (1<<<w) - 1 do
        for j = 0 to w - 1 do
          let a = (op0 (TM.of_int w i) j).to_int
          let b = ((op1 i j) &&& mask)
          let report = ("(" ^ string_of_int w ^ ") " ^ string_of_int i ^ " op " ^ string_of_int j ^ " = " ^ string_of_int a ^ " <> " ^ string_of_int b)
          let test = a = b
          Assertion.Assert(report, test)

    member x.TestShiftOpRandom (op0 : T -> int -> T) (op1 : int -> int -> int) w n = 
      let mask = ~~~(-1 <<< w)
      assert (w <= 32)
      for i = 0 to n-1 do
        let i = rand 0 ((1<<<w)-1)
        let j = rand 0 (w-1)
        let a = (op0 (TM.of_int w i) j).to_int
        let b = ((op1 i j) &&& mask)
        let report = ("(" ^ string_of_int w ^ ") " ^ string_of_int i ^ " op " ^ string_of_int j ^ " = " ^ string_of_int a ^ " <> " ^ string_of_int b)
        let test = a = b
        Assertion.Assert(report, test)
        
    (* op A: Output width = A.Width *)

    member x.TestUnaryOpComplete (op0 : T -> T) (op1 : int -> int) w = 
      let mask = ~~~(-1 <<< w)
      assert (w <= 32)
      for i = 0 to (1<<<w) - 1 do
        let a = (op0 (TM.of_int w i)).to_int
        let b = ((op1 i) &&& mask)
        let report = ("(" ^ string_of_int w ^ ") " ^ "op" ^ string_of_int i ^ " = " ^ string_of_int a ^ " <> " ^ string_of_int b)
        let test = a = b
        Assertion.Assert(report, test)

    member x.TestUnaryOpRandom (op0 : T -> T) (op1 : int -> int) w n = 
      let mask = ~~~(-1 <<< w)
      assert (w <= 32)
      for i = 0 to n-1 do
        let i = rand 0 ((1<<<w)-1)
        let a = (op0 (TM.of_int w i)).to_int
        let b = ((op1 i) &&& mask)
        let report = ("(" ^ string_of_int w ^ ") " ^ "op" ^ string_of_int i ^ " = " ^ string_of_int a ^ " <> " ^ string_of_int b)
        let test = a = b
        Assertion.Assert(report, test)

    (* -------------------------------------------------------------------------- *)
    (* -------------------------------------------------------------------------- *)

    [<Test>]
    member x.ConvInt() = 
      let Assert w v = 
        let s = "width " ^ string_of_int w ^ " value " ^ string_of_int v
        Assertion.Assert (s, (of_int w v).to_int = (v &&& (~~~((-1)<<<w))))
      Assert 1 0
      Assert 1 1
      Assert 2 0
      Assert 2 1
      Assert 2 2
      Assert 2 3
      Assert 3 0
      Assert 3 1
      Assert 3 2
      Assert 3 3
      Assert 3 4
      Assert 3 5
      Assert 3 6
      Assert 3 7
      Assert 3 (-4)
      Assert 3 (-3)
      Assert 3 (-2)
      Assert 3 (-1)
      Assert 3 0
      Assert 3 1
      Assert 3 2
      Assert 3 3
      Assert 16 65535
      Assert 16 22323

    [<Test>]
    member x.ConvString() = 
      let Assert v = 
        let t = (TM.of_string v).to_string
        let s = v ^ " <> " ^ t
        Assertion.Assert (s, (t = v))
      Assert "0"
      Assert "1"
      Assert "00"
      Assert "01"
      Assert "10"
      Assert "11"
      Assert "000"
      Assert "001"
      Assert "010"
      Assert "011"
      Assert "100"
      Assert "101"
      Assert "110"
      Assert "111"
      Assert "10101001010101101010100010101010101010101001"
      Assert "000100010001"
      Assert "0010011101010100000"
      Assert "11111000111110001"
      Assert "100010100010101000010100101010001001011010101011010101010101010101010101010010101010101"

    [<Test>]
    member x.Repeat() = 
      Assertion.Assert ( (TM.repeat TM.gnd 1).to_int_bits = [ 0 ] )
      Assertion.Assert ( (TM.repeat TM.vdd 1).to_int_bits = [ 1 ] )
      Assertion.Assert ( (TM.repeat TM.gnd 2).to_int_bits = [ 0;0 ] )
      Assertion.Assert ( (TM.repeat TM.vdd 2).to_int_bits = [ 1;1 ] )
      Assertion.Assert ( (TM.repeat TM.gnd 3).to_int_bits = [ 0;0;0 ] )
      Assertion.Assert ( (TM.repeat TM.vdd 3).to_int_bits = [ 1;1;1 ] )      
      Assertion.Assert ( (TM.repeat (TM.of_int_bits [0;1]) 1).to_int_bits = [ 0;1 ] )
      Assertion.Assert ( (TM.repeat (TM.of_int_bits [0;1]) 2).to_int_bits = [ 0;1;0;1 ] )
      Assertion.Assert ( (TM.repeat (TM.of_int_bits [0;1]) 3).to_int_bits = [ 0;1;0;1;0;1 ] )

    [<Test>]
    member x.And() = 
      List.iter (fun w -> x.TestBinaryOpSameSizeArgsComplete (&&&) (&&&) w w) x.complete_tests
      List.iter (fun w -> x.TestBinaryOpSameSizeArgsRandom (&&&) (&&&) w w x.num_random_tests) x.random_tests

    [<Test>]
    member x.Or() = 
      List.iter (fun w -> x.TestBinaryOpSameSizeArgsComplete (|||) (|||) w w) x.complete_tests
      List.iter (fun w -> x.TestBinaryOpSameSizeArgsRandom (|||) (|||) w w x.num_random_tests) x.random_tests

    [<Test>]
    member x.Xor() = 
      List.iter (fun w -> x.TestBinaryOpSameSizeArgsComplete (^^^) (^^^) w w) x.complete_tests
      List.iter (fun w -> x.TestBinaryOpSameSizeArgsRandom (^^^) (^^^) w w x.num_random_tests) x.random_tests

    [<Test>]
    member x.Not() = 
      List.iter (fun w -> x.TestUnaryOpComplete (~~~) (~~~) w) x.complete_tests
      List.iter (fun w -> x.TestUnaryOpRandom (~~~) (~~~) w x.num_random_tests) x.random_tests

    [<Test>]
    member x.Add() = 
      List.iter (fun w -> x.TestBinaryOpSameSizeArgsComplete (+) (+) w w) x.complete_tests
      List.iter (fun w -> x.TestBinaryOpSameSizeArgsRandom (+) (+) w w x.num_random_tests) x.random_tests

    [<Test>]
    member x.Sub() = 
      List.iter (fun w -> x.TestBinaryOpSameSizeArgsComplete (-) (-) w w) x.complete_tests
      List.iter (fun w -> x.TestBinaryOpSameSizeArgsRandom (-) (-) w w x.num_random_tests) x.random_tests

    [<Test>]
    member x.Eq() = 
      let compare x y = if x = y then 1 else 0
      List.iter (fun w -> x.TestBinaryOpSameSizeArgsComplete (==:) compare w 1) x.complete_tests
      List.iter (fun w -> x.TestBinaryOpSameSizeArgsRandom (==:) compare w 1 x.num_random_tests) x.random_tests

    [<Test>]
    member x.Neq() = 
      let compare x y = if x <> y then 1 else 0
      List.iter (fun w -> x.TestBinaryOpSameSizeArgsComplete (/=:) compare w 1) x.complete_tests
      List.iter (fun w -> x.TestBinaryOpSameSizeArgsRandom (/=:) compare w 1 x.num_random_tests) x.random_tests


    [<Test>]
    member x.Lsu() = 
      let compare x y = if x < y then 1 else 0
      List.iter (fun w -> x.TestBinaryOpSameSizeArgsComplete (<:) compare w 1) x.complete_tests
      List.iter (fun w -> x.TestBinaryOpSameSizeArgsRandom (<:) compare w 1 x.num_random_tests) x.random_tests

    [<Test>]
    member x.Lsequ() = 
      let compare x y = if x <= y then 1 else 0
      List.iter (fun w -> x.TestBinaryOpSameSizeArgsComplete (<=:) compare w 1) x.complete_tests
      List.iter (fun w -> x.TestBinaryOpSameSizeArgsRandom (<=:) compare w 1 x.num_random_tests) x.random_tests

    [<Test>]
    member x.Gtu() = 
      let compare x y = if x > y then 1 else 0
      List.iter (fun w -> x.TestBinaryOpSameSizeArgsComplete (>:) compare w 1) x.complete_tests
      List.iter (fun w -> x.TestBinaryOpSameSizeArgsRandom (>:) compare w 1 x.num_random_tests) x.random_tests

    [<Test>]
    member x.Gteq() = 
      let compare x y = if x >= y then 1 else 0
      List.iter (fun w -> x.TestBinaryOpSameSizeArgsComplete (>=:) compare w 1) x.complete_tests
      List.iter (fun w -> x.TestBinaryOpSameSizeArgsRandom (>=:) compare w 1 x.num_random_tests) x.random_tests


    [<Test>]
    member x.Lss() = 
      let se w x = if x &&& (1<<<(w-1)) <> 0 then x ||| ((-1) <<< w) else x
      let compare w x y = if se w x < se w y then 1 else 0
      List.iter (fun w -> x.TestBinaryOpSameSizeArgsComplete (<+) (compare w) w 1) x.complete_tests
      List.iter (fun w -> x.TestBinaryOpSameSizeArgsRandom (<+) (compare w) w 1 x.num_random_tests) x.random_tests

    [<Test>]
    member x.Lseqs() = 
      let se w x = if x &&& (1<<<(w-1)) <> 0 then x ||| ((-1) <<< w) else x
      let compare w x y = if se w x <= se w y then 1 else 0
      List.iter (fun w -> x.TestBinaryOpSameSizeArgsComplete (<=+) (compare w) w 1) x.complete_tests
      List.iter (fun w -> x.TestBinaryOpSameSizeArgsRandom (<=+) (compare w) w 1 x.num_random_tests) x.random_tests

    [<Test>]
    member x.Gts() = 
      let se w x = if x &&& (1<<<(w-1)) <> 0 then x ||| ((-1) <<< w) else x
      let compare w x y = if se w x > se w y then 1 else 0
      List.iter (fun w -> x.TestBinaryOpSameSizeArgsComplete (>+) (compare w) w 1) x.complete_tests
      List.iter (fun w -> x.TestBinaryOpSameSizeArgsRandom (>+) (compare w) w 1 x.num_random_tests) x.random_tests

    [<Test>]
    member x.Gteqs() = 
      let se w x = if x &&& (1<<<(w-1)) <> 0 then x ||| ((-1) <<< w) else x
      let compare w x y = if se w x >= se w y then 1 else 0
      List.iter (fun w -> x.TestBinaryOpSameSizeArgsComplete (>=+) (compare w) w 1) x.complete_tests
      List.iter (fun w -> x.TestBinaryOpSameSizeArgsRandom (>=+) (compare w) w 1 x.num_random_tests) x.random_tests

    [<Test>]
    member x.Cat() = 
      let cat w1 a b = (a <<< w1) ||| b
      List.iter (fun (w0, w1) -> x.TestBinaryOpDifferentSizeArgsComplete (++) (cat w1) w0 w1) x.complete_tests2
      List.iter (fun (w0, w1) -> x.TestBinaryOpDifferentSizeArgsRandom (++) (cat w1) w0 w1 x.num_random_tests) x.random_tests2

    [<Test>]
    member x.Mulu() = 
      List.iter (fun (w0, w1) -> x.TestBinaryOpDifferentSizeArgsComplete ( * ) ( * ) w0 w1) x.complete_tests2
      List.iter (fun (w0, w1) -> x.TestBinaryOpDifferentSizeArgsRandom ( * ) ( * ) w0 w1 x.num_random_tests) x.random_tests2
 
    [<Test>]
    member x.Muls() = 
      let se w x = if x &&& (1<<<(w-1)) <> 0 then x ||| ((-1) <<< w) else x
      let muls w0 w1 a b = se w0 a * se w1 b
      List.iter (fun (w0, w1) -> x.TestBinaryOpDifferentSizeArgsComplete ( *+ ) (muls w0 w1) w0 w1) x.complete_tests2
      List.iter (fun (w0, w1) -> x.TestBinaryOpDifferentSizeArgsRandom ( *+ ) (muls w0 w1) w0 w1 x.num_random_tests) x.random_tests2
    
    [<Test>]
    member x.Sll() = 
      List.iter (fun w -> x.TestShiftOpComplete (<<<) (<<<) w) x.complete_tests
      List.iter (fun w -> x.TestShiftOpRandom (<<<) (<<<) w x.num_random_tests) x.random_tests
    
    [<Test>]
    member x.Srl() = 
      List.iter (fun w -> x.TestShiftOpComplete (>>>) (>>>) w) x.complete_tests
      List.iter (fun w -> x.TestShiftOpRandom (>>>) (>>>) w x.num_random_tests) x.random_tests

    [<Test>]
    member x.Sra() = 
      let se w x = if x &&& (1<<<(w-1)) <> 0 then x ||| ((-1) <<< w) else x
      let sra w a b = (se w a) >>> b
      List.iter (fun w -> x.TestShiftOpComplete (>>+) (sra w) w) x.complete_tests
      List.iter (fun w -> x.TestShiftOpRandom (>>+) (sra w) w x.num_random_tests) x.random_tests

    [<Test>]
    member x.Select() = 
      let Assert a hi lo =
        let w = String.length a
        let s = String.sub a (w-1-hi) (hi-lo+1)
        let a = ((TM.of_string a).[hi,lo]).to_string
        let r = a ^ ".[" ^ string_of_int hi ^ "," ^ string_of_int lo ^ "] = " ^ a ^ " <> " ^ s
        let b = a = s
        Assertion.Assert(r, b)
      Assert "1" 0 0
      Assert "01" 0 0
      Assert "01" 1 0
      Assert "01" 1 1
      Assert "1001" 1 0
      Assert "1001101000101011" 10 2
      Assert "1101000101010111001" 18 0
      Assert "10010011" 4 4
      Assert "100110100010101" 11 1
      Assert "1000101010101001" 12 7

  end

