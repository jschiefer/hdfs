-- $Source: /evtfs/home/tres/vhdl/ref_design/RCS/uart.vhd,v $
-- $Revision: 1.49 $     $Date: 2006/08/10 17:00:40 $
-------------------------------------------------------------------------------
-- Procedural Template Comparisons for Synthesis: UART Example
-- 
-- Fri Jan 28 15:34:46 2005           Mike Treseler
-------------------------------------------------------------------------------
-- This source file will be maintained for a while at
--   http://home.comcast.net/~mike_treseler/uart.vhd
-- A testbench and comments will be maintained for a while at
--   http://home.comcast.net/~mike_treseler/test_uart.vhd
-------------------------------------------------------------------------------
-- This vhdl reference design is a reformatting of the well written
-- and superbly documented single process UART example by Ian Lang
-- http://www.designabstraction.co.uk/EXAMPLE/HTML/rtl.htm
-------------------------------------------------------------------------------
-- Ian benchmarks his single process style in vhdl and verilog against
-- a more conventional process per register style and shows that all
-- of the source styles are equivalent for the purposes of synthesis.
-------------------------------------------------------------------------------
-- One reason to make another pass at Ian's example is to document a
-- procedural template coding style that is easy to read while still
-- providing efficient synthesis and simulation. At the end of this
-- file, I compare performance between three synthesis templates: two
-- with asynch reset and one with synch reset.
--
-- A UART is a simple yet non-trivial hardware block suitable for this
-- demonstration. The example design entity is coded using a single
-- process without any signal declarations. Procedures are used to
-- collect, name, test, and shuffle related statement sequences.  All
-- functions and procedures are intended to be short enough that their
-- purpose is obvious but long enough to be useful and nameable. All
-- variable declarations represent registers.  Start reading from the
-- end of this file for a "top-down" view.
-------------------------------------------------------------------------------
-- The introduction below is from Ian's paper:
-- http://www.designabstraction.co.uk/Articles/Advanced%20Synthesis%20Techniques.htm
-------------------------------------------------------------------------------
-- "Now let's design ourselves a very simple UART circuit. The model
-- has serial data input, serial data output and a simple eight bit
-- read/write bus (via which a microprocessor would send and receive
-- bytes of serial data). Transmission protocol is START BIT (logic
-- '0'), EIGHT DATA BITS (lsb 1st), STOP BIT (logic '1'). The memory
-- interface is synchronous with a memory-map comprising two addresses
-- as shown below"

--   +----------------------------------------+
--   |    Table 1 UART Memory Map             |
--   +------+-----+-----+----------+----------+
--   |Name  |Add  |R/W  |Data      |Comment   |
--   +------+-----+-----+----------+----------+
--   |TXQ   |0    |W    |7_downto_0| Transmit |
--   |      |     |     |          | Data     |
--   +------+-----+-----+----------+----------+
--   |RXQ   |0    |R    |7_downto 0| Receive  |
--   |      |     |     |          | Data     |
--   +------+-----+-----+----------+----------+
--   |StatQ |1    |R    |2->TxReady| Status   |
--   |      |     |     |1->RxError| Data     |
--   |      |     |     |0->RxReady|          |
--   +------+-----+-----+----------+----------+

-- Updated Mon Jul 17 14:26:18 2006 to center low baud rate samples
-- and to improve transmit handshaking. Added 13 ALUTS.
library ieee;
use ieee.std_logic_1164.all;
-------------------------------------------------------------------------------
package uart_pkg is
-- port types and register assigments shared with test_uart.vhd
-- see case template_g at the end of this file      
   type     template_t is (v_rst, a_rst, s_rst); -- style types

   type     wire_t is (loopback, stuck_hi, stuck_low, random); -- tb port type
   constant t_rdy_c : natural := 2; -- status bit assignments
   constant r_err_c : natural := 1; --        use read_data_v(r_rdy_c) 
   constant r_rdy_c : natural := 0; -- instead of read_data_v(0)
   constant data_ptr_c   : std_ulogic := '0';
   constant status_ptr_c : std_ulogic := '1';
end package uart_pkg;
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all; -- nothing fancy
use work.uart_pkg.all;       -- see above
entity uart is
   
-- An instance without generic map gets these constant values: 
   generic (template_g    : template_t := v_rst;
            char_len_g    : natural    := 8;   
            tic_per_bit_g : natural    := 3);  -- roll_c eliminated in rev 1.45 
   port (
      clock     : in  std_ulogic;  -- port maps to std_logic with no conversion
      reset     : in  std_ulogic;
      address   : in  std_ulogic;
      writeData : in  std_logic_vector(char_len_g-1 downto 0);  -- generic type
      write_stb : in  std_ulogic;
      readData  : out std_logic_vector(char_len_g-1 downto 0);
      read_stb  : in  std_ulogic;
      serialIn  : in  std_ulogic;
      serialOut : out std_ulogic
      );
end uart;
-------------------------------------------------------------------------------
architecture templates of uart is
--           Note: No signal declarations.
begin
   main : process (clock, reset) is  --  templates 0, 2
-- main : process (clock) is         --  template 1
--                (no functional difference)
      type RxState_t is (
         IDLE,
         START,
         RECEIVE,
         STOP,
         FULL,
         ERR
         );
      variable RxState_v : RxState_t;
-------------------------------------------------------------------------------
      type TxState_t is (
         IDLE,
         START,
         SEND,
         STOP
         );
      variable TxState_v : TxState_t;
-------------------------------------------------------------------------------
-- RETIME is a four node procedural component for input
-- synchronization.  It is a variable of type retime_t that is UPDATED
-- by the procedure retime declared below. These types and procedures
-- could be packaged, but are shown here as a tutorial

-- USAGE: Declare a variable of the type retime_t for each instance.
-- Output options(sequence constraints):
-- Registered Node   = USE the variable.f3 first -- required in this case
-- Unregistered Node = UPDATE the variable first -- would drop a sync stage

   type retime_t
      is
   record
      f1 : std_ulogic;  -- .f1 output covers races only
      f2 : std_ulogic;  -- .f2 output node covers metastable events also
      f3 : std_ulogic;  -- .f3 output covers synthesis register duplication
   end record;

-- RETIME Register Instance:
   variable serialInRetimed_v : retime_t;  -- input synchronizer nodes
-- RETIME Register Update Routine:
   procedure retime
      (arg_in : in    std_ulogic;       -- input value
       update : inout retime_t          -- 3 bit shifter
       )
   is
   begin
      update.f3 := update.f2;           -- f2[DQ]f3
                                        --  \__
                                        --      \
      update.f2 := update.f1;           -- f1[DQ]f2
                                        --  \__
                                        --      \
      update.f1 := arg_in;              -- in[DQ]f1

      -- Note that reversing the order the statements above would make
      -- wires instead of registers.
   end procedure retime;
-------------------------------------------------------------------------------
   -- Data Registers
   subtype  char_t is std_logic_vector(readData'range);  -- match port width
   variable read_data_v    : char_t;
   variable Tx_v           : char_t; -- output buffer
   variable Rx_v           : char_t; -- input shifter
   variable serial_out_v   : std_ulogic;
   -- Counter Registers
   subtype  tic_count_t is natural range 0 to tic_per_bit_g;
   -- max count output is tic_per_bit_g-1 carry value is tic_per_bit_g;
   -- Mon Jul 17 10:15:13 2006 Fixed tic_per_bit_g -- was short by one
   -- one tick per bit. Elimianted roll_c in the process.
   variable RxBitSampleCount_v : tic_count_t;
   variable TxBitSampleCount_v : tic_count_t;
   subtype  bit_count_t is natural range 0 to char_len_g; -- 0 to carry value
   variable RxBitCount_v       : bit_count_t;
   variable TxBitCount_v       : bit_count_t;
   -- sim testpoints -- ignored for synthesis
   variable sample_tp_sim_only_v :boolean;  -- ck centering of sample
-------------------------------------------------------------------------------
   procedure zero_read_data is -- used in init_regs and cpu_regs
   begin
      read_data_v := (others => '0');
   end procedure zero_read_data;
-------------------------------------------------------------------------------
   procedure init_regs is
   begin
      zero_read_data;
      serial_out_v    := '1'; -- no premature start bit
      RxState_v       := IDLE;
      TxState_v       := IDLE;
      Tx_v            := (others => '0');
      Rx_v            := (others => '0');
      serialInRetimed_v  := ('1', '1', '1'); -- no premature start bit
      RxBitSampleCount_v := 0;
      RxBitCount_v       := 0;
      TxBitSampleCount_v := 0;
      TxBitCount_v       := 0;
      sample_tp_sim_only_v := false;
   end procedure init_regs;
-------------------------------------------------------------------------------
   procedure update_ports is
   begin -- purpose: synthesize a wire from the register to the port
      serialOut <= serial_out_v;
      readData  <= read_data_v;
   end procedure update_ports;
-------------------------------------------------------------------------------
   procedure tx_state is -- Transmit State Register Update Routine
   -- local subprograms: Collect statements used more than once:
      procedure inc_tic_count is
      begin
         TxBitSampleCount_v := TxBitSampleCount_v+1;
      end procedure inc_tic_count;
      --
      impure function bit_done return boolean is
         begin
             return TxBitSampleCount_v = tic_per_bit_g;   -- counter rollover
      end function bit_done;
-------------------------------------------------------------------------------
      begin  --  procedure tx_state
         case TxState_v is
            when IDLE => if write_stb = '1' and
                            address = data_ptr_c then
                            TxState_v := START;  -- kick off transmit
                         end if;
                         
            when START => Start_bit : serial_out_v := '0';
                          TXQ  : Tx_v        := writeData;
                                                -- grab a byte from the bus 
                          TxBitSampleCount_v := 0;
                          TxBitCount_v       := 0;
                          TxState_v          := SEND;
------------------------------------------------------------------------------- 
            when SEND => inc_tic_count;
                         if bit_done then 
                            TxBitSampleCount_v          := 0;
                            bit_sel      : serial_out_v := Tx_v(TxBitCount_v);
                            inc_tx_count : TxBitCount_v := TxBitCount_v+1;
                            char_done    : if TxBitCount_v = char_len_g then
                               TxState_v := STOP;
                            end if char_done;
                         end if;

            when STOP => inc_tic_count;
                         if bit_done then
                            stop_bit     : serial_out_v := '1';
                            byte_restart : TxState_v    := IDLE;
                         end if;
                         
         end case;
      end procedure tx_state;
-------------------------------------------------------------------------------
   procedure rx_state is -- Receive State Register Update Routine
      --    local procedures for statements used more than once:
      procedure inc_tic_count is
      begin
         RxBitSampleCount_v := RxBitSampleCount_v+1;
      end procedure inc_tic_count;
      --
      impure function bit_done return boolean is
      begin
         return RxBitSampleCount_v = tic_per_bit_g;  -- center of input
      end function bit_done;
-------------------------------------------------------------------------------
      begin
         sample_tp_sim_only_v := false;                    
         case RxState_v is
            when IDLE =>            -- wait for start bit
               RxBitSampleCount_v := 0;
               RxBitCount_v := 0;                -- else wastes a count
               Rx_v         := (others => '0');  -- makes sim easier to follow
               if SerialInRetimed_v.F3 = '0' then 
               RxState_v     := START;
               end if;
            when START =>
               inc_tic_count;
               mid_bit:if RxBitSampleCount_v = tic_per_bit_g/2 then
                  RxState_v := RECEIVE;
                  RxBitSampleCount_v := 0;  -- Thanks to Richard Brady
                  --  Wed Jul 12 16:02:48 2006: This centers the subsequent
                  --  rx_v samples just right of bit.f3 center.
                  -- in.f3 -------------___---___---___ ...
                  -- count 000000000000012012012012  ...
                  -- rx_v       00       |01|03 ...
                  -------------------------------------------------------------
               end if mid_bit;
            when RECEIVE =>
               inc_tic_count;
               sample_time:if bit_done then  -- at center of bit
                  sample_tp_sim_only_v := true;  -- to check sample centering
                  RxBitSampleCount_v := 0;   -- This overwrites the carry value
                  grab_a_bit: Rx_v(RxBitCount_v) := serialInRetimed_v.F3;
                  inc_rx_count:RxBitCount_v := RxBitCount_v+1;
                  end_of_payload:if RxBitCount_v = char_len_g then
                     RxState_v := STOP;
                  end if end_of_payload;
                  else
                     sample_tp_sim_only_v := false;   -- sim only
               end if sample_time;
            when STOP =>
               inc_tic_count;
               if bit_done then
                  check_stop_bit:if serialInRetimed_v.F3 = '1' then 
                     RxState_v := FULL;
                  else
                     RxState_v := ERR;
                  end if check_stop_bit;
               end if;        
            when FULL => null; -- wait for read strobe
            when ERR  => null; -- these states covered in cpu_regs, not here; 
         end case;               
   end procedure rx_state;
-------------------------------------------------------------------------------
   procedure cpu_regs is -- CPU  Register Update Routine
      -- this procedure does some post processing on the rx and tx
      -- state variables and is intended *follow* those procedures.
   begin
--   +----------------------------------------+
--   |    Table 1 UART Memory Map             |
--   +------+-----+-----+----------+----------+
--   |Name  |Add  |R/W  |Data      |Comment   |
--   +------+-----+-----+----------+----------+
--   |TXQ   |0    |W    |7_downto_0| Transmit |
--   |      |     |     |          | Data     |
--   +------+-----+-----+----------+----------+
--   |RXQ   |0    |R    |7_downto 0| Receive  |
--   |      |     |     |          | Data     |
--   +------+-----+-----+----------+----------+
--   |StatQ |1    |R    |2->TxReady| Status   |
--   |      |     |     |1->RxError| Data     |
--   |      |     |     |0->RxReady|          |
--   +------+-----+-----+----------+----------+

    they_want_data: if read_stb = '1' then
         case address is     -- reads anytime, expects smart,handshaking reader
            when data_ptr_c =>    -- Collect a byte from the input shifter
               RXQ : read_data_v := Rx_v; 
               RxState_v         := IDLE; -- read is complete
            when status_ptr_c =>
               zero_read_data; -- zero fill old values and unused bits
               StatQ : if RxState_v = FULL then            -- Update rx status
                  RxReady : read_data_v(r_rdy_c) := '1';   -- drive data ready
               elsif RxState_v = ERR then
                RxError : read_data_v(1) := '1';      -- bad stop bit flag
                  RxState_v                := IDLE;   -- start over
               end if;
               if TxState_v = IDLE then
                  TX_Ready : read_data_v(t_rdy_c) := '1';  -- Drive tx ready
               else
                  TX_Busy  : read_data_v(t_rdy_c) := '0';
               end if;
            when others => null;
         end case;
      end if they_want_data;
      
   end procedure cpu_regs;
-------------------------------------------------------------------------------
   procedure update_regs is
   begin  -- purpose: call the procedures above in the desired order
      rx_state;                         -- tx first is ok too
      tx_state;
      cpu_regs;                         -- follows rx, tx
      retime(                           -- keep last to get all three flops
         arg_in => serialIn,            -- entity port raw value
         update => serialInRetimed_v    -- UPDATE variable every tick
         );
   end procedure update_regs;
-------------------------------------------------------------------------------
--    Synchronous Templates for Synthesis: The following templates use
--    all of the declarations above to make a UART. With other
--    declarations, the template could make anything else.  Relative
--    synthesis performance will be compared below for each template.
--    If I were not making comparisons, the first template would be
--    pasted into the main process.
-------------------------------------------------------------------------------
   procedure template_v_rst is -- My default. 
   begin                      -- a_rst is logically equivalent  
      if reset = '1' then     -- Assumes synched trailing edge reset pulse
         init_regs;           -- reg_v := init_c;  Variables only, ports below.
      elsif rising_edge(clock) then
         update_regs;         -- reg_v := f(reg_v);Variables only, ports below.
      end if;                 -- Synchronous init optional (state_v = idle_c)
      update_ports;           -- will infer port wires ok for reset and clock
   end procedure template_v_rst; -- out_port <= reg_v; ports only, no signals
-------------------------------------------------------------------------------
   procedure template_s_rst is -- for use in template comparisons
   begin
      if rising_edge(clock) then
         if reset = '1' then
            init_regs;
            update_ports;
         else
            update_regs;
            update_ports;
         end if;
      end if;
   end procedure template_s_rst;
-------------------------------------------------------------------------------
   procedure template_a_rst is -- for use in template comparisons
   begin                       -- Has proven equivalent to v_rst for synthesis.
      if reset = '1' then
         init_regs;
         update_ports;
      elsif rising_edge(clock) then
         update_regs;
         update_ports;
      end if;
   end procedure template_a_rst;
-------------------------------------------------------------------------------
   begin  -- process main
      case template_g is
         when a_rst  => template_a_rst;
         when s_rst  => template_s_rst;
         when others => template_v_rst;
      end case;
   end process main;
-------------------------------------------------------------------------------
-- Synthesis Results for default constraints and various devices
-------------------------------------------------------------------------------
-- template_a_rst;
-------------------------------------------------------------------------------
-- Quartus 5.0 SP2    216 MHz  50 FF   73 ALUT ep2s15sf484c3 by Kevin Jennings
-- Synplify 8.4 +Q5.0 405 MHz  50 FF   74 ALUT ep2s15sf484c3 by Kevin Jennings
-- Quartus 5.1        356 MHz  52 FF   72 ALUT ep2s15sf484C3 by Mike Treseler
-- Quartus 6 src=1.48 363 MHz  52 FF   85 ALUT ep2s15sf484C3 "" [with bug fix]  
-- Synplify Pro  8.4  408 MHz  93 FF  126 LUTS xc4vlx15sf363-12 by Rick North
-- Synplify Pro  8.0  216 MHz  -? FF   94 LUTS xc3s50tq144-4 by Martin Thompson
-- Libero/Synpify8.5b 155 MHz  49 FF  113 TILE a3pe600-2 208pqfp by Anonymous1
-------------------------------------------------------------------------------
-- template_s_rst;
-------------------------------------------------------------------------------
-- Quartus 5.0 SP2    222 MHz  50 FF   77 ALUT ep2s15sf484c3 by Kevin Jennings
-- Synplify 8.4 +Q5.0 381 MHz  50 FF   77 ALUT ep2s15sf484c3 by Kevin Jennings
-- Quartus 5.1        317 MHz  50 FF   80 ALUT ep2s15sf484c3 by Mike Treseler
-- Synplify Pro  8.4  357 MHz 108 FF  126 LUTS xc4vlx15sf363-12 retime R.North
-- Synplify Pro  8.0  171 MHz  -? FF   76 LUTS xc3s50tq144-4 by Martin Thompson
-- Libero/Synpify8.5b 123 MHz  48 FF  168 TILE a3pe600-2 208pqfp by Anonymous1
-------------------------------------------------------------------------------
-- template_v_rst;
-------------------------------------------------------------------------------
-- Quartus 5.0 SP2    216 MHz  50 FF   73 ALUT ep2s15sf484c3 by Kevin Jennings
-- Synplify 8.4+5.0   405 MHz  50 FF   74 ALUT ep2s15sf484c3 by Kevin Jennings
-- Quartus 5.1        356 MHz  52 FF   72 ALUT eps15sf484c3 by Mike Treseler
-- Leo 2005b.24 est   194 MHz  48 FF   62 ALUT default device by Mike Treseler
-- Synplify Pro 8.4   408 MHz  93 FF  126 LUTS xc4vlx15sf363-12 retime R.North
-- Synplify Pro 8.0   216 MHz  -? FF   94 LUTS xc3s50tq144-4 by Martin Thompson
-- ISE 7.1            288 MHz  50 FF  107 LUTS xc2vp40fg676-6 by Mark Norton
-- ISE 8.1            277 MHz  50 FF   84 LUTS xc2v40cs144-6 by Mike Treseler
-- ISE 8.1            388 MHz  50 FF   80 LUTS xc4vfx12sf363-12 Mike Treseler
-------------------------------------------------------------------------------
-- Note that the synthesis results for the a_rst and v_rst templates
-- are virtually identical.
--                          -- Mike Treseler Mon Jun 19 14:51:42 2006
-------------------------------------------------------------------------------
end architecture templates;

