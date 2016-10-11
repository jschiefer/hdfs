#light
open NUnit.Framework
open DigitalLogic
open Numeric
open Numeric.Ops
open Numeric.Conversions
open IntBits
open ArrayBits
open Signal
open Simulator
     
[<TestFixture>]
type HdfsRegressSim =
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
        num_tests = 1;
        num_tests_per_size = 1;
      }

    (* ------------------------------------------------------------ *)
    (* ------------------------------------------------------------ *)

    member x.rand_bits b = IntBits.of_int_bits [ for i in { 0 to b-1 } -> rand 0 1 ]
    
    member x.check_mask (a:ArrayBits) = 
      let bit = a.width % 32
      let word = a.words - 1
      let mask = 
        match bit with
        | 0 -> 0ul
        | n -> 0xFFFFFFFFul <<< bit
      Assertion.Assert("Bit overflow in result", (a.data.(word) &&& mask) = 0ul)

    member x.Binop1 (op_s:Signal->Signal->Signal) (op_i:IntBits->IntBits->IntBits) = 
      for i = 0 to x.num_tests - 1 do
        let bits = rand x.min_bits x.max_bits
        let a' = input "a" bits
        let b' = input "b" bits
        let c' = (op_s a' b').output "c"
        let sim = create (Circuit.create [ c' ])
        sim.reset
        for j = 0 to x.num_tests_per_size - 1 do
          let a = x.rand_bits bits
          let b = x.rand_bits bits
          let c = op_i a b
          (ArrayBits.f_copy (sim.[a'].port_data) (ArrayBits.of_int_bits a.to_int_bits))()
          (ArrayBits.f_copy (sim.[b'].port_data) (ArrayBits.of_int_bits b.to_int_bits))()
          sim.cycle
          let str = a.to_string ^ " op " ^ b.to_string ^ " = " ^ c.to_string ^ " <> " ^ (sim.[c'].port_data.to_string)
          x.check_mask sim.[c'].port_data
          Assertion.Assert(str, (c.to_string = sim.[c'].port_data.to_string))

    member x.Binop2 (op_s:Signal->Signal->Signal) (op_i:IntBits->IntBits->IntBits) = 
      for i = 0 to x.num_tests - 1 do
        let bits1 = rand x.min_bits x.max_bits
        let bits2 = rand x.min_bits x.max_bits
        let a' = input "a" bits1
        let b' = input "b" bits2
        let c' = (op_s a' b').output "c"
        let sim = create (Circuit.create [ c' ])
        sim.reset
        for j = 0 to x.num_tests_per_size - 1 do
          let a = x.rand_bits bits1
          let b = x.rand_bits bits2
          let c = op_i a b
          (ArrayBits.f_copy (sim.[a'].port_data) (ArrayBits.of_int_bits a.to_int_bits))()
          (ArrayBits.f_copy (sim.[b'].port_data) (ArrayBits.of_int_bits b.to_int_bits))()
          sim.cycle
          let str = a.to_string ^ " op " ^ b.to_string ^ " = " ^ c.to_string ^ " <> " ^ (sim.[c'].port_data.to_string)
          x.check_mask sim.[c'].port_data
          Assertion.Assert(str, (c.to_string = sim.[c'].port_data.to_string))

    member x.Sftop (op_s:Signal->Signal->Signal) (op_i:IntBits->int->IntBits) = 
      for i = 0 to x.num_tests - 1 do
        let bits1 = rand x.min_bits x.max_bits
        let a' = input "a" bits1
        let bits2 = rand 1 (ubits bits1)
        let b' = input "b" bits2
        let c' = (op_s a' b').output "c"
        let sim = create (Circuit.create [ c' ])
        sim.reset
        for j = 0 to x.num_tests_per_size - 1 do 
          let a = x.rand_bits bits1
          let sft = rand 0 (bits2-1)
          let c = op_i a sft
          (ArrayBits.f_copy (sim.[a'].port_data) (ArrayBits.of_int_bits a.to_int_bits))()
          (ArrayBits.f_copy (sim.[b'].port_data) (ArrayBits.of_int bits2 sft))()
          sim.cycle
          let str = a.to_string ^ " sft " ^ string_of_int sft ^ " = " ^ c.to_string ^ " <> " ^ (sim.[c'].port_data.to_string)
          x.check_mask sim.[c'].port_data
          Assertion.Assert(str, (c.to_string = sim.[c'].port_data.to_string))

    member x.Unop (op_s:Signal->Signal) (op_i:IntBits->IntBits) = 
      for i = 0 to x.num_tests - 1 do
        let bits = rand x.min_bits x.max_bits
        let a' = input "a" bits
        let c' = (op_s a').output "c"
        let sim = create (Circuit.create [ c' ])
        sim.reset
        for j = 0 to x.num_tests_per_size - 1 do
          let a = x.rand_bits bits
          let c = op_i a 
          (ArrayBits.f_copy (sim.[a'].port_data) (ArrayBits.of_int_bits a.to_int_bits))()
          sim.cycle
          let str = "op " ^ a.to_string ^ " = " ^ c.to_string ^ " <> " ^ (sim.[c'].port_data.to_string)
          x.check_mask sim.[c'].port_data
          Assertion.Assert(str, (c.to_string = sim.[c'].port_data.to_string))

    (* ------------------------------------------------------------ *)
    (* Test the basic hdfs operators *)
    (* ------------------------------------------------------------ *)

    [<Test>]
    member x.Add() = x.Binop1 (Signal.add) (IntBits.add)

    [<Test>]
    member x.Sub() = x.Binop1 (Signal.sub) (IntBits.sub)

    [<Test>]
    member x.And() = x.Binop1 (Signal.band) (IntBits.band)

    [<Test>]
    member x.Or() = x.Binop1 (Signal.bor) (IntBits.bor)

    [<Test>]
    member x.Xor() = x.Binop1 (Signal.bxor) (IntBits.bxor)

    [<Test>]
    member x.Not() = x.Unop (Signal.bnot) (IntBits.bnot)

    [<Test>]
    member x.Cat() = x.Binop2 (Signal.cat) (IntBits.cat)

    [<Test>]
    member x.Mulu() = x.Binop2 (Signal.mulu) (IntBits.mulu)

    [<Test>]
    member x.Muls() = x.Binop2 (Signal.muls) (IntBits.muls)

    [<Test>]
    member x.Lsu() = x.Binop1 (Signal.lsu) (IntBits.lsu)

    [<Test>]
    member x.Lsequ() = x.Binop1 (Signal.lsequ) (IntBits.lsequ)
    
    [<Test>]
    member x.Gtu() = x.Binop1 (Signal.gtu) (IntBits.gtu)
    
    [<Test>]
    member x.Gteq() = x.Binop1 (Signal.gtequ) (IntBits.gtequ)
    
    [<Test>]
    member x.Lss() = x.Binop1 (Signal.lss) (IntBits.lss)
    
    [<Test>]
    member x.Lseqs() = x.Binop1 (Signal.lseqs) (IntBits.lseqs)
    
    [<Test>]
    member x.Gts() = x.Binop1 (Signal.gts) (IntBits.gts)
    
    [<Test>]
    member x.Gteqs() = x.Binop1 (Signal.gteqs) (IntBits.gteqs)

    [<Test>]
    member x.Eq() = x.Binop1 (Signal.eq) (IntBits.eq)
    
    [<Test>]
    member x.Neq() = x.Binop1 (Signal.neq) (IntBits.neq)

    [<Test>]
    member x.Repeat() = 
      for i = 0 to x.num_tests - 1 do
        let bits = rand x.min_bits x.max_bits
        let a' = input "a" bits
        let n = rand 1 10
        let c' = (a'.repeat n).output "c"
        let sim = create (Circuit.create [ c' ])
        sim.reset
        for j = 0 to x.num_tests_per_size - 1 do
          let a = x.rand_bits bits
          let c = IntBits.repeat a n
          (ArrayBits.f_copy (sim.[a'].port_data) (ArrayBits.of_int_bits a.to_int_bits))()
          sim.cycle
          let str = a.to_string ^ ".repeat " ^ string_of_int n ^ " = " ^ c.to_string ^ " <> " ^ (sim.[c'].port_data.to_string)
          x.check_mask sim.[c'].port_data
          Assertion.Assert(str, (c.to_string = sim.[c'].port_data.to_string))


    [<Test>]
    member x.Sll() = x.Sftop (Signal.shift_left) (IntBits.sll)
    
    [<Test>]
    member x.Srl() = x.Sftop (Signal.shift_right_logical) (IntBits.srl)

    [<Test>]
    member x.Sra() = x.Sftop (Signal.shift_right_arithmetic) (IntBits.sra)

    [<Test>]
    member x.Select() = 
      for i = 0 to x.num_tests - 1 do
        let bits = rand x.min_bits x.max_bits
        let a' = input "a" bits
        let lo = rand 0 (bits-1)
        let hi = rand lo (bits-1)
        let c' = (a'.[hi,lo]).output "c"
        let sim = create (Circuit.create [ c' ])
        sim.reset
        for j = 0 to x.num_tests_per_size - 1 do
          let a = x.rand_bits bits
          let c = IntBits.select a hi lo
          let str = a.to_string ^ ".[" ^ string_of_int hi ^ "," ^ string_of_int lo ^ "] = " ^ c.to_string ^ " <> " ^ (sim.[c'].port_data.to_string)
          (ArrayBits.f_copy (sim.[a'].port_data) (ArrayBits.of_int_bits a.to_int_bits))()
          sim.cycle
          x.check_mask sim.[c'].port_data
          Assertion.Assert(str, (c.to_string = sim.[c'].port_data.to_string))

    (* ------------------------------------------------------------ *)
    (* More specific hand crafted test cases *)
    (* ------------------------------------------------------------ *)
    [<Test>]
    member x.Test0() = 
      let a = input "a" 8
      let b = input "b" 8
      let a' = regc enable a
      let b' = regc enable b
      let c = (a' + b').output "c"
      let sim = create (Circuit.create [ c ])
      sim.reset
      sim.[enable].[0] <- 1ul
      sim.[a].[0] <- 10ul
      sim.[b].[0] <- 20ul
      sim.cycle
      printf "c = %u\n" sim.[c].[0]
      sim.cycle
      printf "c = %u\n" sim.[c].[0]

    (* Accessing the simulator using different types of value.  Need more tests to ensure sign bits are properly cleared and set *)
    [<Test>]
    member x.SimTypeAccess() = 
      let a = input "a" 64
      let b = input "b" 64
      let c = (a |: b).output "c"
      let circuit = Circuit.create [ c ]
      let sim = Simulator.create circuit

      // Base array access
      sim.[a].[0] <- 1ul
      printf "%8x%8x\n" sim.[a].[1] sim.[a].[0]

      // int
      sim.[a].i <- (-2)
      printf "%i\n" sim.[a].i
      printf "%8x%8x\n" sim.[a].[1] sim.[a].[0]

      // byte       
      sim.[a].uy <- 4uy
      printf "%i\n" (Byte.to_int sim.[a].uy)
      printf "%8x%8x\n" sim.[a].[1] sim.[a].[0]

      // sbyte      
      sim.[a].y <- -3y
      printf "%i\n" (SByte.to_int sim.[a].y)
      printf "%8x%8x\n" sim.[a].[1] sim.[a].[0]

      // uint16     
      sim.[a].us <- 6us
      printf "%i\n" (UInt16.to_int sim.[a].us)
      printf "%8x%8x\n" sim.[a].[1] sim.[a].[0]

      // int16      
      sim.[a].s <- -5s
      printf "%i\n" (Int16.to_int sim.[a].s)
      printf "%8x%8x\n" sim.[a].[1] sim.[a].[0]

      // uint32     
      sim.[a].ul <- 8ul
      printf "%i\n" sim.[a].ul
      printf "%8x%8x\n" sim.[a].[1] sim.[a].[0]

      // int32      
      sim.[a].l <- -7l
      printf "%i\n" sim.[a].l
      printf "%8x%8x\n" sim.[a].[1] sim.[a].[0]

      // uint64     
      sim.[a].UL <- 9UL
      printf "%i\n" sim.[a].UL
      printf "%8x%8x\n" sim.[a].[1] sim.[a].[0]

      // int64      
      sim.[a].L <- -10L
      printf "%i\n" sim.[a].L
      printf "%8x%8x\n" sim.[a].[1] sim.[a].[0]


 end
  
