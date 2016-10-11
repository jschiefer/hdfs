#light
open NUnit.Framework
open DigitalLogic
open Numeric
open Ops
open IntBits
open ArrayBits

[<TestFixture>]
type HdfsRegressArrayBits = 
  class

    (* Select a bit range within these bounds *)
    val min_bits : int;
    val max_bits : int;

    (* Number of specific bit ranges to test *)
    val num_tests : int;
    
    (* Number of tests per bit range *)
    val num_tests_per_size : int;

    new() = 
      {
        min_bits = 1;
        max_bits = 128;
        num_tests = 25;
        num_tests_per_size = 25;
      }

    member x.rand_bits b = IntBits.of_int_bits [ for i in { 0 to b-1 } -> rand 0 1 ]
    
    member x.check_mask (a:ArrayBits) = 
      let bit = a.width % 32
      let word = a.words - 1
      let mask = 
        match bit with
        | 0 -> 0ul
        | n -> 0xFFFFFFFFul <<< bit
      Assertion.Assert("Bit overflow in result", (a.data.(word) &&& mask) = 0ul)

    member x.Binop1 (op_a:ArrayBits->ArrayBits->ArrayBits) (op_i:IntBits->IntBits->IntBits) = 
      for i = 0 to x.num_tests - 1 do
        let bits = rand x.min_bits x.max_bits
        for j = 0 to x.num_tests_per_size - 1 do
          let a = x.rand_bits bits
          let b = x.rand_bits bits
          let c = op_i a b
          let a' = ArrayBits.of_int_bits a.to_int_bits
          let b' = ArrayBits.of_int_bits b.to_int_bits
          let c' = op_a a' b'
          let str = a.to_string ^ " op " ^ b.to_string ^ " = " ^ c.to_string ^ " <> " ^ c'.to_string
          x.check_mask c'
          Assertion.Assert(str, (c.to_string = c'.to_string))

    member x.Binop2 (op_a:ArrayBits->ArrayBits->ArrayBits) (op_i:IntBits->IntBits->IntBits) = 
      for i = 0 to x.num_tests - 1 do
        let bits1 = rand x.min_bits x.max_bits
        let bits2 = rand x.min_bits x.max_bits
        for j = 0 to x.num_tests_per_size - 1 do
          let a = x.rand_bits bits1
          let b = x.rand_bits bits2
          let c = op_i a b
          let a' = ArrayBits.of_int_bits a.to_int_bits
          let b' = ArrayBits.of_int_bits b.to_int_bits
          let c' = op_a a' b'
          let str = a.to_string ^ " op " ^ b.to_string ^ " = " ^ c.to_string ^ " <> " ^ c'.to_string
          x.check_mask c'
          Assertion.Assert(str, (c.to_string = c'.to_string))

    member x.Sftop (op_a:ArrayBits->int->ArrayBits) (op_i:IntBits->int->IntBits) = 
      for i = 0 to x.num_tests - 1 do
        let bits = rand x.min_bits x.max_bits
        for j = 0 to x.num_tests_per_size - 1 do 
          let a = x.rand_bits bits
          let sft = rand 0 (bits-1)
          let c = op_i a sft
          let a' = ArrayBits.of_int_bits a.to_int_bits
          let c' = op_a a' sft
          let str = a.to_string ^ " sft " ^ string_of_int sft ^ " = " ^ c.to_string ^ " <> " ^ c'.to_string
          x.check_mask c'
          Assertion.Assert(str, (c.to_string = c'.to_string))

    member x.Unop (op_a:ArrayBits->ArrayBits) (op_i:IntBits->IntBits) = 
      for i = 0 to x.num_tests - 1 do
        let bits = rand x.min_bits x.max_bits
        for j = 0 to x.num_tests_per_size - 1 do
          let a = x.rand_bits bits
          let c = op_i a 
          let a' = ArrayBits.of_int_bits a.to_int_bits
          let c' = op_a a' 
          let str = "op " ^ a.to_string ^ " = " ^ c.to_string ^ " <> " ^ c'.to_string
          x.check_mask c'
          Assertion.Assert(str, (c.to_string = c'.to_string))

    [<Test>]
    member x.Add() = x.Binop1 (ArrayBits.add) (IntBits.add)

    [<Test>]
    member x.Sub() = x.Binop1 (ArrayBits.sub) (IntBits.sub)

    [<Test>]
    member x.And() = x.Binop1 (ArrayBits.band) (IntBits.band)

    [<Test>]
    member x.Or() = x.Binop1 (ArrayBits.bor) (IntBits.bor)

    [<Test>]
    member x.Xor() = x.Binop1 (ArrayBits.bxor) (IntBits.bxor)

    [<Test>]
    member x.Not() = x.Unop (ArrayBits.bnot) (IntBits.bnot)

    [<Test>]
    member x.Cat() = x.Binop2 (ArrayBits.cat) (IntBits.cat)

    [<Test>]
    member x.Mulu() = x.Binop2 (ArrayBits.mulu) (IntBits.mulu)

    [<Test>]
    member x.Muls() = x.Binop2 (ArrayBits.muls) (IntBits.muls)

    [<Test>]
    member x.Lsu() = x.Binop1 (ArrayBits.lsu) (IntBits.lsu)

    [<Test>]
    member x.Lsequ() = x.Binop1 (ArrayBits.lsequ) (IntBits.lsequ)
    
    [<Test>]
    member x.Gtu() = x.Binop1 (ArrayBits.gtu) (IntBits.gtu)
    
    [<Test>]
    member x.Gteq() = x.Binop1 (ArrayBits.gtequ) (IntBits.gtequ)
    
    [<Test>]
    member x.Lss() = x.Binop1 (ArrayBits.lss) (IntBits.lss)
    
    [<Test>]
    member x.Lseqs() = x.Binop1 (ArrayBits.lseqs) (IntBits.lseqs)
    
    [<Test>]
    member x.Gts() = x.Binop1 (ArrayBits.gts) (IntBits.gts)
    
    [<Test>]
    member x.Gteqs() = x.Binop1 (ArrayBits.gteqs) (IntBits.gteqs)

    [<Test>]
    member x.Eq() = x.Binop1 (ArrayBits.eq) (IntBits.eq)
    
    [<Test>]
    member x.Neq() = x.Binop1 (ArrayBits.neq) (IntBits.neq)

    [<Test>]
    member x.Repeat() = 
      for i = 0 to x.num_tests - 1 do
        let bits = rand x.min_bits x.max_bits
        for j = 0 to x.num_tests_per_size - 1 do
          let n = rand 1 10
          let a = x.rand_bits bits
          let c = IntBits.repeat a n
          let a' = ArrayBits.of_int_bits a.to_int_bits
          let c' = ArrayBits.repeat a' n
          let str = a.to_string ^ ".repeat " ^ string_of_int n ^ " = " ^ c.to_string ^ " <> " ^ c'.to_string
          x.check_mask c'
          Assertion.Assert(str, (c.to_string = c'.to_string))

    [<Test>]
    member x.Sll() = x.Sftop (ArrayBits.sll) (IntBits.sll)
    
    [<Test>]
    member x.Srl() = x.Sftop (ArrayBits.srl) (IntBits.srl)

    [<Test>]
    member x.Sra() = x.Sftop (ArrayBits.sra) (IntBits.sra)

    [<Test>]
    member x.Select() = 
      for i = 0 to x.num_tests - 1 do
        let bits = rand x.min_bits x.max_bits
        for j = 0 to x.num_tests_per_size - 1 do
          let lo = rand 0 (bits-1)
          let hi = rand lo (bits-1)
          let a = x.rand_bits bits
          let c = IntBits.select a hi lo
          let a' = ArrayBits.of_int_bits a.to_int_bits
          let c' = ArrayBits.select a' hi lo
          let str = a.to_string ^ ".[" ^ string_of_int hi ^ "," ^ string_of_int lo ^ "] = " ^ c.to_string ^ " <> " ^ c'.to_string
          x.check_mask c'
          Assertion.Assert(str, (c.to_string = c'.to_string))

    (* Test the custom api functions used by the simulator *)
    [<Test>]
    member x.Copying() = 
      let d0 = make 7
      let d1 = make 63
      let e = make 1
      d0.data.(0) <- 255ul;
      d1.data.(0) <- 0xFFFFFFFFul;
      d1.data.(1) <- 0xFFFFFFFFul;
      Assertion.Assert("copy", ArrayBits.copy d0 = d0)
      Assertion.Assert("copy", ArrayBits.copy d1 = d1)
      Assertion.Assert("copy_mask", (ArrayBits.copy_mask d0).to_int = 127)
      Assertion.Assert("copy_mask", (ArrayBits.copy_mask d1).to_uint64 = 0x7FFFFFFFFFFFFFFFUL)
      Assertion.Assert("copy_ena", (ArrayBits.copy_ena e d0).to_int = 0)
      e.data.(0) <- 1ul
      Assertion.Assert("copy_ena", (ArrayBits.copy_ena e d0) = d0)

    [<Test>]
    member x.Mem() = 
      let d_bits = 64
      let a_bits = 9
      let lookup = f_mem 8 4 d_bits a_bits 
      let addr = make a_bits
      let lookup i = 
        addr.data.(0) <- UInt32.of_int i
        let d = lookup addr
        Assertion.Assert("mem addr", addr.to_int = i)
        d
      (lookup 1).data.(0) <- 5ul;
      (lookup 256).data.(0) <- 6ul;
      (lookup 257).data.(0) <- 7ul;
      Assertion.Assert("mem lookup", (lookup 1).data.(0) = 5ul)
      Assertion.Assert("mem lookup", (lookup 256).data.(0) = 6ul)
      Assertion.Assert("mem lookup", (lookup 257).data.(0) = 7ul)

    [<Test>]
    member x.Mux() = 
      let d_bits = 10 
      let a_bits = 4
      let rec build i =
        if i = (1 <<< a_bits) - 2 then []
        else (of_int d_bits i) :: build (i+1)
      let dlist = build 0
      let lookup = f_mux 3 a_bits dlist     (* build the mux with a map ... generally we'd use something like 31 which means mux's will pretty much always be build with maps *)
      let addr = make a_bits
      let lookup i = 
        addr.data.(0) <- UInt32.of_int i
        let d = lookup addr
        let ii = min ((1<<<a_bits)-3) i
        let str = "mux @ " ^ string_of_int i ^ " : " ^ string_of_int d.to_int ^ " <> " ^ string_of_int ii
        Assertion.Assert(str, d.to_int = ii)
      for i=0 to (1<<<a_bits)-1 do
        lookup i
          

  end
