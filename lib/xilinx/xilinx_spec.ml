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

open List

(*************************************************************************)
(*************************************************************************)
let virtex4_p = [

  "arithmetic", [
    "DSP48"               , "18x18 Signed Multiplier Followed by a Three-Input Adder with Optional Pipeline Registers";
  ];

  "clock", [
    "BUFG            "    , "Global Clock Buffer";
    "BUFGCE          "    , "Global Clock MUX with Clock Enable and Output State 0";
    "BUFGCE_1        "    , "Global Clock MUX Buffer with Clock Enable and Output State 1";
    "BUFGCTRL        "    , "Global Clock MUX Buffer";
    "BUFGMUX         "    , "Global Clock MUX Buffer with Output State 0";
    "BUFGMUX_1       "    , "Global Clock MUX with Output State 1";
    "BUFGMUX_VIRTEX4 "    , "Global Clock MUX Buffer";
    "BUFIO           "    , "Local Clock Buffer for I/O";
    "BUFR            "    , "Local Clock Buffer for I/O and CLB";
    "DCM_ADV         "    , "Digital Clock Manager with Advanced Features";
    "DCM_BASE        "    , "Digital Clock Manager with Basic Features";
    "DCM_PS          "    , "Digital Clock Manager with Basic and Phase-Shift Features";
    "PMCD            "    , "Phase-Matched Clock Divider";
  ];

  "config/bscan", [
    "BSCAN_VIRTEX4      " , "Provides Access to the BSCAN Sites on Virtex-4 Devices";
    "CAPTURE_VIRTEX4    " , "Virtex-4 Boundary Scan Logic Control Circuit";
    "FRAME_ECC_VIRTEX4  " , "Reads a Single, Virtex-4 Configuration Frame and Computes a Hamming, Single-Error Correction, Double-Error Detection Syndrome";
    "ICAP_VIRTEX4       " , "Virtex-4 Internal Configuration Access Port";
    "STARTUP_VIRTEX4    " , "Virtex-4 User Interface to Configuration Clock, Global Reset, Global 3-State Controls, and Other Configuration Signals";
    "USR_ACCESS_VIRTEX4 " , "32-Bit Register with a 32-Bit DATA Bus and a DATAVALID Port";
  ];

  "Gigabit Transceivers", [
    "GT11_CUSTOM  "       , "RocketIO MGTs with 622 Mb/s to 11.1 Gb/s data rates, 8 to 24 transceivers per FPGA, and 2.5 GHz – 5.55 GHz VCO, less than 1ns RMS jitter";
    "GT11_DUAL    "       , "RocketIO MGT Tile (contains 2 GT11_CUSTOM) with 622 Mb/s to 11.1 Gb/s data rates, 8 to 24 transceivers per FPGA, and 2.5 GHz – 5.55 GHz VCO, less than 1ns RMS jitter";
    "GT11CLK      "       , "A MUX That Can Select Fom Differential Package Input Clock, refclk From the Fabric, or rxbclk to Drive the Two Vertical Reference Clock Buses for the Column of MGTs";
    "GT11CLK_MGT  "       , "Allows Differential Package Input to Drive the Two Vertical Reference Clock Buses for the Column of MGTs";
  ];

  "I/O components", [
    "BUFIO       "        , "Local Clock Buffer for I/O";
    "DCIRESET    "        , "DCI State Machine Reset (After Configuration Has Been Completed)";
    "IBUF        "        , "Single-Ended Input Buffer with Selectable I/O Standard and Capacitance";
    "IBUFDS      "        , "Differential Signaling Input Buffer with Selectable I/O Interface";
    "IBUFG       "        , "Dedicated Input Buffer with Selectable I/O Interface";
    "IBUFGDS     "        , "Dedicated Differential Signaling Input Buffer with Selectable I/O Interface";
    "IDDR        "        , "A Dedicated Input Register to Receive External Dual Data Rate (DDR) Signals into Virtex-4 FPGAs";
    "IDELAY      "        , "Dedicated input variable-tap delay chain";
    "IDELAYCTRL  "        , "IDELAY tap delay value control";
    "IOBUF       "        , "Bi-Directional Buffer with Selectable I/0 Interface (multiple primitives)";
    "IOBUFDS     "        , "3-State Differential Signaling I/O Buffer with Active Low Output Enable";
    "ISERDES     "        , "Dedicated I/O Buffer Input Deserializer";
    "KEEPER      "        , "KEEPER Symbol";
    "OBUF        "        , "Single-ended Output Buffer";
    "OBUFT       "        , " 3-State Output Buffer with Active Low Output Enable and with Selectable I/O Interface";
    "OBUFDS      "        , "Differential Signaling Output Buffer with Selectable I/O Interface";
    "OBUFTDS     "        , "3-State Output Buffer with Differential Signaling, Active-Low Output Enable, and Selectable I/O Interface";
    "ODDR        "        , "A dedicated output registers to transmit dual data rate (DDR) signals from Virtex-4 FPGAs";
    "OSERDES     "        , "Provides a way for the user to easily implement source synchronous interface by using the OSERDES module";
    "PULLDOWN    "        , "Resistor to GND for Input Pads";
    "PULLUP      "        , "Resistor to VCC for Input PADs, Open-Drain, and 3-State Outputs";
  ];             

  "Processor Components", [
    "EMAC        "        , "Fully integrated 10/100/1000 Mb/s Ethernet Media Access Controller (Ethernet MAC)";
    "PPC405_ADV  "        , "Primitive for the Power PC Core";
  ];

  "RAM/ROM", [
    "FIFO16         "     , "Virtex-4 Block RAM based built-in FIFO";
    "RAM16X1D       "     , "16-Deep by 1-Wide Static Dual Port Synchronous RAM";
    "RAM16X1S       "     , "16-Deep by 1-Wide Static Synchronous RAM";
    "RAM32X1S       "     , "32-Deep by 1-Wide Static Synchronous RAM";
    "RAM64X1S       "     , "64-Deep by 1-Wide Static Synchronous RAM";
    "RAMB16         "     , "16384-Bit Data Memory and 2048-Bit Parity Memory, Single-Port Synchronous Block RAM with Port Width (n) Configured to 1, 2, 4, 9, 18, or 36 Bits";
    "RAMB32_S64_ECC "     , "512 Deep by 64-Bit Wide Synchronous, Two-Port, Block RAM with Built-In Error Correction";
    "ROM16X1        "     , "16-Deep by 1-Wide ROM";
    "ROM32X1        "     , "32-Deep by 1-Wide ROM";
    "ROM64X1        "     , "64-Deep by 1-Wide ROM";
    "ROM128X1       "     , "128-Deep by 1-Wide ROM";
    "ROM256X1       "     , "256-Deep by 1-Wide ROM";
  ];

  "Registers & Latches", [
    "FDCPE"               , "D Flip-Flop with Clock Enable and Asynchronous Preset and Clear";
    "FDRSE"               , "D Flip-Flop with Synchronous Reset and Set and Clock Enable";
    "LDCPE"               , "Transparent Data Latch with Asynchronous Clear and Preset and Gate Enable";
  ];

  "Shift registers", [
    "SRL16     "          , "16-Bit Shift Register Look-Up Table (LUT)";
    "SRL16_1   "          , "16-Bit Shift Register Look-Up Table (LUT) with Negative-Edge Clock";
    "SRL16E    "          , "16-Bit Shift Register Look-Up Table (LUT) with Clock Enable";
    "SRL16E_1  "          , "16-Bit Shift Register Look-Up Table (LUT) with Negative-Edge Clock and Clock Enable";
    "SRLC16    "          , "16-Bit Shift Register Look-Up Table (LUT) with Carry";
    "SRLC16_1  "          , "16-Bit Shift Register Look-Up Table (LUT) with Carry and Negative-Edge Clock";
    "SRLC16E   "          , "16-Bit Shift Register Look-Up Table (LUT) with Carry and Clock Enable";
    "SRLC16E_1 "          , "16-Bit Shift Register Look-Up Table (LUT) with Negative-Edge Clock and Clock Enable";
  ];

  "Slice/CLB Primitives", [
    "BUFCF    "           , "Fast Connect Buffer";
    "LUT1     "           , "1-Bit Look-Up Table with General Output";
    "LUT2     "           , "2-Bit Look-Up Table with General Output";
    "LUT3     "           , "3-Bit Look-Up Table with General Output";
    "LUT4     "           , "4-Bit Look-Up Table with General Output";
    "LUT1_D   "           , "1-Bit Look-Up Table with Dual Output";
    "LUT2_D   "           , "2-Bit Look-Up Table with Dual Output";
    "LUT3_D   "           , "3-Bit Look-Up Table with Dual Output";
    "LUT4_D   "           , "4-Bit Look-Up Table with Dual Output";
    "LUT1_L   "           , "1-Bit Look-Up Table with Local Output";
    "LUT2_L   "           , "2-Bit Look-Up Table with Local Output";
    "LUT3_L   "           , "3-Bit Look-Up Table with Local Output";
    "LUT4_L   "           , "4-Bit Look-Up Table with Local Output";
    "MULT_AND "           , "Fast Multiplier AND";
    "MUXCY    "           , "2-to-1 Multiplexer for Carry Logic with General Output";
    "MUXCY_D  "           , "2-to-1 Multiplexer for Carry Logic with Dual Output";
    "MUXCY_L  "           , "2-to-1 Multiplexer for Carry Logic with Local Output";
    "MUXF5    "           , "2-to-1 Lookup Table Multiplexer with General Output";
    "MUXF5_D  "           , "2-to-1 Lookup Table Multiplexer with Dual Output";
    "MUXF5_L  "           , "2-to-1 Lookup Table Multiplexer with Local Output";
    "MUXF6    "           , "2-to-1 Lookup Table Multiplexer with General Output";
    "MUXF6_D  "           , "2-to-1 Lookup Table Multiplexer with Dual Output";
    "MUXF6_L  "           , "2-to-1 Lookup Table Multiplexer with Local Output";
    "MUXF7    "           , "2-to-1 Lookup Table Multiplexer with General Output";
    "MUXF7_D  "           , "2-to-1 Lookup Table Multiplexer with Dual Output";
    "MUXF7_L  "           , "2-to-1 Lookup Table Multiplexer with Local Output";
    "MUXF8    "           , "2-to-1 Lookup Table Multiplexer with General Output";
    "MUXF8_D  "           , "2-to-1 Lookup Table Multiplexer with Dual Output";
    "MUXF8_L  "           , "2-to-1 Lookup Table Multiplexer with Local Output";
    "XORCY    "           , "XOR for Carry Logic with General Output";
    "XORCY_D  "           , "XOR for Carry Logic with Dual Output";
    "XORCY_L  "           , "XOR for Carry Logic with Local Output";
  ];
]

(*************************************************************************)
(*************************************************************************)
let virtex5_p = [

  "Arithmetic Functions", [
    "DSP48E"              , "25x18 Twos Complement Multiplier with Integrated 48-Bit, 3-Input Adder/Subtracter/Accumulator or 2-Input Logic Unit"
  ];

  "Clock Components", [
    "BUFG         "       , "Global Clock Buffer";
    "BUFGCE       "       , "Global Clock MUX with Clock Enable and Output State 0";
    "BUFGCTRL     "       , "Global Clock MUX Buffer";
    "BUFGMUX_CTRL "       , "2-to-1 Global Clock MUX Buffer";
    "BUFIO        "       , "Local Clock Buffer for I/O";
    "BUFR         "       , "Local Clock Buffer for I/O and CLB";
    "DCM_ADV      "       , "Digital Clock Manager with Advanced Features";
    "DCM_BASE     "       , "Digital Clock Manager with Basic Features";
    "PLL_ADV      "       , "Advanced Phase Locked Loop Clock Circuit";
    "PLL_BASE     "       , "Basic Phase Locked Loop Clock Circuit";
  ];

  "Config/BSCAN Components", [
    "BSCAN_VIRTEX5     "  , "Virtex-5 JTAG Boundary-Scan Logic Access";
    "CAPTURE_VIRTEX5   "  , "Virtex-5 Readback Register Capture Control";
    "ICAP_VIRTEX5      "  , "Internal Configuration Access Port";
    "KEY_CLEAR         "  , "Virtex-5 Configuration Encryption Key Erase";
    "STARTUP_VIRTEX5   "  , "Virtex-5 Configuration Start-Up Sequence Interface";
    "USR_ACCESS_VIRTEX5"  , "Virtex-5 User Access Register";
  ];

  "I/O Components", [
    "DCIRESET       "     , "DCI State Machine Reset (After Configuration Has Been Completed)";
    "IBUF           "     , "Single-Ended Input Buffer with Selectable I/O Standard and Capacitance";
    "IBUFDS         "     , "Differential Signaling Input Buffer with Selectable I/O Interface and Optional Delay";
    "IBUFG          "     , "Dedicated Input Buffer with Selectable I/O Interface";
    "IBUFGDS        "     , "Dedicated Differential Signaling Input Buffer with Selectable I/O Interface";
    "IDDR           "     , "Input Dual Data-Rate Register";
    "IDELAY         "     , "Dedicated input variable-tap delay chain";
    "IDELAYCTRL     "     , "IDELAY tap delay value control";
    "IOBUF          "     , "Bi-Directional Buffer with Selectable I/0 Interface (multiple primitives)";
    "IOBUFDS        "     , "3-State Differential Signaling I/O Buffer with Active Low Output Enable and with Selectable I/O Interface";
    "ISERDES        "     , "Dedicated I/O Buffer Input Deserializer";
    "ISERDES_NODELAY"     , "Input SERial/DESerializer";
    "KEEPER         "     , "KEEPER Symbol";
    "OBUF           "     , "Single-ended Output Buffer";
    "OBUFDS         "     , "Differential Signaling Output Buffer with Selectable I/O Interface";
    "OBUFT          "     , "3-State Output Buffer with Active Low Output Enable and with Selectable I/O Interface";
    "OBUFTDS        "     , "3-State Output Buffer with Differential Signaling, Active-Low Output Enable, and Selectable I/O Interface";
    "ODDR           "     , "A dedicated output registers to transmit dual data rate (DDR) signals from Virtex-5 FPGAs";
    "OSERDES        "     , "Provides a way for the user to easily implement source synchronous interface by using the OSERDES module";
    "PULLDOWN       "     , "Resistor to GND for Input Pads";
    "PULLUP         "     , "Resistor to Vcc for Input PADs, Open-Drain, and 3-State Outputs";
  ];

  "RAM/ROM", [
    "FIFO18    "          , "18KB FIFO (First In, First Out) Block RAM Memory";
    "FIFO18_36 "          , "36-bit Wide by 512 Deep 18KB FIFO (First In, First Out) Block RAM Memory";
    "FIFO36    "          , "36KB FIFO (First In, First Out) Block RAM Memory";
    "FIFO36_72 "          , "72-Bit Wide by 512 Deep 36KB FIFO (First In, First Out) Block RAM Memory with ECC (Error Detection and Correction Circuitry)";
    "RAM32M    "          , "32-Deep by 8-bit Wide Multi Port Random Access Memory (Select RAM)";
    "RAM32X1S  "          , "32-Deep by 1-Wide Static Synchronous RAM";
    "RAM64M    "          , "64-Deep by 4-bit Wide Multi Port Random Access Memory (Select RAM)";
    "RAM64X1D  "          , "64-Deep by 1-Wide Dual Port Static Synchronous RAM";
    "RAM64X1S  "          , "64-Deep by 1-Wide Static Synchronous RAM";
    "RAM128X1D "          , "128-Deep by 1-Wide Dual Port Random Access Memory (Select RAM)";
    "RAM128X1S "          , "128-Deep by 1-Wide Static Synchronous RAM";
    "RAM256X1S "          , "256-Deep by 1-Wide Random Access Memory (Select RAM)";
    "RAMB18    "          , "18KB Configurable Synchronous Dual Port Block RAM";
    "RAMB18SDP "          , "36-bit by 512 Deep, 18KB Synchronous Simple Dual Port Block RAM";
    "RAMB36    "          , "Multi-functional, Cascadable, 48-bit Output Arithmetic Logic Unit";
    "RAMB36SDP "          , "72-bit by 512 Deep, 36KB Synchronous Simple Dual Port Block RAM with ECC (Error Correction Circuitry)";
    "ROM16X1   "          , "16-Deep by 1-Wide ROM";
    "ROM32X1   "          , "32-Deep by 1-Wide ROM";
    "ROM64X1   "          , "64-Deep by 1-Wide ROM";
    "ROM128X1  "          , "128-Deep by 1-Wide ROM";
    "ROM256X1  "          , "256-Deep by 1-Wide ROM";
  ];

  "Registers & Latches", [
    "FDCPE     "          , "D Flip-Flop with Clock Enable and Asynchronous Preset and Clear";
    "FDRSE     "          , "D Flip-Flop with Synchronous Reset and Set and Clock Enable";
    "IDDR_2CLK "          , "Input Dual Data-Rate Register with Dual Clock Inputs";
    "LDCPE     "          , "Transparent Data Latch with Asynchronous Clear and Preset and Gate Enable";
  ];

  "Shift Registers", [
    "SRLC32E"             , "32 Clock Cycle, Variable Length Shift Register Look-Up Table (LUT) with Clock Enable";
  ];

  "Slice/CLB Primitives", [
    "BUFCF  "             , "Fast Connect Buffer";
    "CARRY4 "             , "Fast Carry Logic with Look Ahead";
    "CFGLUT5"             , "5-input Dynamically Reconfigurable Look-Up Table";
    "LUT1   "             , "1-Bit Look-Up Table with General Output";
    "LUT2   "             , "2-Bit Look-Up Table with General Output";
    "LUT3   "             , "3-Bit Look-Up Table with General Output";
    "LUT4   "             , "4-Bit Look-Up Table with General Output";
    "LUT1_D "             , "1-Bit Look-Up Table with Dual Output";
    "LUT2_D "             , "2-Bit Look-Up Table with Dual Output";
    "LUT3_D "             , "3-Bit Look-Up Table with Dual Output";
    "LUT4_D "             , "4-Bit Look-Up Table with Dual Output";
    "LUT1_L "             , "1-Bit Look-Up Table with Local Output";
    "LUT2_L "             , "2-Bit Look-Up Table with Local Output";
    "LUT3_L "             , "3-Bit Look-Up Table with Local Output";
    "LUT4_L "             , "4-Bit Look-Up Table with Local Output";
    "LUT5   "             , "5 Input, 1 Output, Look-Up-Table";
    "LUT5_D "             , "5 Input, 1 Output, Look-Up-Table";
    "LUT5_L "             , "5 Input, 1 Output, Look-Up-Table";
    "LUT6   "             , "6 Input, 1 Output, Look-Up-Table";
    "LUT6_D "             , "6 Input, 1 Output, Look-Up-Table";
    "LUT6_L "             , "6 Input, 1 Output, Look-Up-Table";
    "MUXCY  "             , "2-to-1 Multiplexer for Carry Logic with General Output";
    "MUXF7  "             , "2-to-1 Look-Up Table Multiplexer with General Output";
    "MUXF7_D"             , "2-to-1 Look-Up Table Multiplexer with Dual Output";
    "MUXF7_L"             , "2-to-1 Look-Up Table Multiplexer with Local Output";
    "MUXF8  "             , "2-to-1 Look-Up Table Multiplexer with General Output";
    "MUXF8_D"             , "2-to-1 Look-Up Table Multiplexer with Dual Output";
    "MUXF8_L"             , "2-to-1 Look-Up Table Multiplexer with Local Output";
    "XORCY  "             , "XOR for Carry Logic with General Output";
  ];
]

(*************************************************************************)
(* a lot of copy/paste/replace work went on here to do with the list of supported
   parts.  There may be mistakes.  If there is one then it'll probably
   apply to all definitions with the same spec *)
(*************************************************************************)
type prim_type_t = No | Primitive | Macro

let virtex_virtex2_spartan_cpld_p = [

  "Arithmetic", [
    "ACC1      ",  "1-Bit Loadable Cascadable Accumulator with Carry-In, Carry-Out, and Synchronous Reset", [No; No; No; No; Primitive; Primitive; Primitive];
    "ACC4      ",  "4-Bit Loadable Cascadable Accumulator with Carry-In, Carry-Out, and Synchronous Reset", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "ACC8      ",  "8-Bit Loadable Cascadable Accumulator with Carry-In, Carry-Out, and Synchronous Reset", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "ACC16     ",  "16-Bit Loadable Cascadable Accumulator with Carry-In, Carry-Out, and Synchronous Reset", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "ADD1      ",  "1-Bit Full Adder with Carry-In and Carry-Out", [No; No; No; No; Primitive; Primitive; Primitive];
    "ADD4      ",  "4-Bit Cascadable Full Adder with Carry-In, Carry-Out, and Overflow", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "ADD8      ",  "8-Bit Cascadable Full Adder with Carry-In, Carry-Out, and Overflow", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "ADD16     ",  "16-Bit Cascadable Full Adder with Carry-In, Carry-Out, and Overflow", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "ADSU1     ",  "1-Bit Cascadable Adder/Subtracter with Carry-In, Carry-Out", [No; No; No; No; Primitive; Primitive; Primitive];
    "ADSU4     ",  "4-Bit Cascadable Adder/Subtracter with Carry-In, Carry-Out, and Overflow", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "ADSU8     ",  "8-Bit Cascadable Adder/Subtracter with Carry-In, Carry-Out, and Overflow", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "ADSU16    ",  "16-Bit Cascadable Adder/Subtracter with Carry-In, Carry-Out, and Overflow", [Macro; Macro; Macro; Macro; Macro; Macro; Macro];
    "MULT18X18 ",  "18 x 18 Signed Multiplier", [No; Primitive; No; Primitive; No; No; No];
    "MULT18X18S",  "18 x 18 Signed Multiplier -- Registered Version", [No; Primitive; No; Primitive; No; No; No];
  ];

  "buffers", [
    "BUF      ",   "General Purpose Buffer", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "BUF4     ",   "4-Bit General Purpose Buffer", [Macro; No; No; No; Primitive; Primitive; Primitive];
    "BUF8     ",   "8-Bit General Purpose Buffer", [Macro; No; No; No; Primitive; Primitive; Primitive];
    "BUF16    ",   "16-Bit General Purpose Buffer", [Macro; No; No; No; Primitive; Primitive; Primitive];
    "BUFCF    ",   "Fast Connect Buffer", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "BUFE     ",   "Internal 3-State Buffer with Active High Enable", [ Primitive; No; Primitive; Primitive; Primitive; No; No];
    "BUFE4    ",   "Internal 3-State Buffer with Active High Enable", [ Primitive; No; Macro; Macro; Primitive; No; No];
    "BUFE8    ",   "Internal 3-State Buffer with Active High Enable", [ Primitive; No; Macro; Macro; Primitive; No; No];
    "BUFE16   ",   "Internal 3-State Buffer with Active High Enable", [ Primitive; No; Macro; Macro; Primitive; No; No];
    "BUFG     ",   "Global Clock Buffer", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "BUFGCE   ",   "Global Clock MUX with Clock Enable and Output State 0", [No; Primitive; No; Primitive; No; No; No];
    "BUFGCE_1 ",   "Global Clock MUX Buffer with Clock Enable and Output State 1", [No; Primitive; No; Primitive; No; No; No];
    "BUFGDLL  ",   "Clock Delay Locked Loop Buffer", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "BUFGMUX  ",   "Global Clock MUX Buffer with Output State 0", [No; Primitive; No; Primitive; No; No; No];
    "BUFGMUX_1",   "Global Clock MUX with Output State 1", [No; Primitive; No; Primitive; No; No; No];
    "BUFGP    ",   "Primary Global Buffer for Driving Clocks or Longlines (Four per PLD Device)", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "BUFGSR   ",   "Global Set/Reset Input Buffer", [No; No; No; No; Primitive; Primitive; Primitive];
    "BUFGTS   ",   "Global 3-State Input Buffer", [No; No; No; No; Primitive; Primitive; Primitive];
    "BUFT     ",   "Internal 3-State Buffer with Active-Low Enable", [Primitive; No; Primitive; Primitive; Primitive; No; No];
    "BUFT4    ",   "Internal 3-State Buffer with Active-Low Enable", [Macro; No; Macro; Macro; Primitive; No; No;];
    "BUFT8    ",   "Internal 3-State Buffer with Active-Low Enable", [Macro; No; Macro; Macro; Primitive; No; No;];
    "BUFT16   ",   "Internal 3-State Buffer with Active-Low Enable", [Macro; No; Macro; Macro; Primitive; No; No;];
  ];

  "Comparators", [
    "COMP2   ",    "2-Bit Identity Comparator", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "COMP4   ",    "4-Bit Identity Comparator", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "COMP8   ",    "8-Bit Identity Comparator", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "COMP16  ",    "16-Bit Identity Comparator", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "COMPM2  ",    "2-Bit Magnitude Comparator", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "COMPM4  ",    "4-Bit Magnitude Comparator", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "COMPM8  ",    "8-Bit Magnitude Comparator", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "COMPM16 ",    "16-Bit Magnitude Comparator", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "COMPMC8 ",    "8-Bit Magnitude Comparator", [Macro; Macro; Macro; Macro; No; No; No];
    "COMPMC16",    "16-Bit Magnitude Comparator", [Macro; Macro; Macro; Macro; No; No; No];
  ];

  "counters", [
    "CB2CE      ", "2-Bit Cascadable Binary Counter with Clock Enable and Asynchronous Clear", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "CB4CE      ", "4-Bit Cascadable Binary Counter with Clock Enable and Asynchronous Clear", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "CB8CE      ", "8-Bit Cascadable Binary Counter with Clock Enable and Asynchronous Clear", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "CB16CE     ", "16-Bit Cascadable Binary Counter with Clock Enable and Asynchronous Clear", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "CB2CLE     ", "2-Bit Loadable Cascadable Binary Counters with Clock Enable and Asynchronous Clear", [No; No; No; No; Primitive; Primitive; Primitive];
    "CB4CLE     ", "4-Bit Loadable Cascadable Binary Counters with Clock Enable and Asynchronous Clear", [No; No; No; No; Primitive; Primitive; Primitive];
    "CB8CLE     ", "8-Bit Loadable Cascadable Binary Counters with Clock Enable and Asynchronous Clear", [No; No; No; No; Primitive; Primitive; Primitive];
    "CB16CLE    ", "16-Bit Loadable Cascadable Binary Counters with Clock Enable and Asynchronous Clear", [No; No; No; No; Primitive; Primitive; Primitive];
    "CB2CLED    ", "2-, 4-, 8-, 16-Bit Loadable Cascadable Bidirectional Binary Counters with Clock Enable and Asynchronous Clear", [No; No; No; No; Primitive; Primitive; Primitive];
    "CB4CLED    ", "2-, 4-, 8-, 16-Bit Loadable Cascadable Bidirectional Binary Counters with Clock Enable and Asynchronous Clear", [No; No; No; No; Primitive; Primitive; Primitive];
    "CB8CLED    ", "2-, 4-, 8-, 16-Bit Loadable Cascadable Bidirectional Binary Counters with Clock Enable and Asynchronous Clear", [No; No; No; No; Primitive; Primitive; Primitive];
    "CB16CLED   ", "2-, 4-, 8-, 16-Bit Loadable Cascadable Bidirectional Binary Counters with Clock Enable and Asynchronous Clear", [No; No; No; No; Primitive; Primitive; Primitive];
    "CB2RE      ", "2-Bit Cascadable Binary Counter with Clock Enable and Synchronous Reset", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "CB4RE      ", "4-Bit Cascadable Binary Counter with Clock Enable and Synchronous Reset", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "CB8RE      ", "8-Bit Cascadable Binary Counter with Clock Enable and Synchronous Reset", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "CB16RE     ", "16-Bit Cascadable Binary Counter with Clock Enable and Synchronous Reset", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "CB2RLE     ", "2-Bit Loadable Cascadable Binary Counter with Clock Enable and Synchronous Reset", [No; No; No; Macro; Primitive; Primitive; Primitive];
    "CB4RLE     ", "4-Bit Loadable Cascadable Binary Counter with Clock Enable and Synchronous Reset", [No; No; No; Macro; Primitive; Primitive; Primitive];
    "CB8RLE     ", "8-Bit Loadable Cascadable Binary Counter with Clock Enable and Synchronous Reset", [No; No; No; Macro; Primitive; Primitive; Primitive];
    "CB16RLE    ", "16-Bit Loadable Cascadable Binary Counter with Clock Enable and Synchronous Reset", [No; No; No; Macro; Primitive; Primitive; Primitive];
    "CB2X1      ", "2-Bit Loadable Cascadable Bidirectional Binary Counter with Clock Enable and Asynchronous Clear", [No; No; No; Macro; Primitive; Primitive; Primitive];
    "CB4X1      ", "4-Bit Loadable Cascadable Bidirectional Binary Counter with Clock Enable and Asynchronous Clear", [No; No; No; Macro; Primitive; Primitive; Primitive];
    "CB8X1      ", "8-Bit Loadable Cascadable Bidirectional Binary Counter with Clock Enable and Asynchronous Clear", [No; No; No; Macro; Primitive; Primitive; Primitive];
    "CB16X1     ", "16-Bit Loadable Cascadable Bidirectional Binary Counter with Clock Enable and Asynchronous Clear", [No; No; No; Macro; Primitive; Primitive; Primitive];
    "CB2X2      ", "2-Bit Loadable Cascadable Bidirectional Binary Counter with Clock Enable and Synchronous Reset", [No; No; No; Macro; Primitive; Primitive; Primitive];
    "CB4X2      ", "4-Bit Loadable Cascadable Bidirectional Binary Counter with Clock Enable and Synchronous Reset", [No; No; No; Macro; Primitive; Primitive; Primitive];
    "CB8X2      ", "8-Bit Loadable Cascadable Bidirectional Binary Counter with Clock Enable and Synchronous Reset", [No; No; No; Macro; Primitive; Primitive; Primitive];
    "CB16X2     ", "16-Bit Loadable Cascadable Bidirectional Binary Counter with Clock Enable and Synchronous Reset", [No; No; No; Macro; Primitive; Primitive; Primitive];
    "CBD2CE     ", "2-Bit Cascadable Dual Edge Triggered Binary Counter with Clock Enable and Asynchronous Clear", [No; No; No; No; No; No; Primitive];
    "CBD4CE     ", "4-Bit Cascadable Dual Edge Triggered Binary Counter with Clock Enable and Asynchronous Clear", [No; No; No; No; No; No; Primitive];
    "CBD8CE     ", "8-Bit Cascadable Dual Edge Triggered Binary Counter with Clock Enable and Asynchronous Clear", [No; No; No; No; No; No; Primitive];
    "CBD16CE    ", "16-Bit Cascadable Dual Edge Triggered Binary Counter with Clock Enable and Asynchronous Clear", [No; No; No; No; No; No; Primitive];
    "CBD2CLE    ", "2-Bit Loadable Cascadable Dual Edge Triggered Binary Counter with Clock Enable and Asynchronous Clear", [No; No; No; No; No; No; Primitive];
    "CBD4CLE    ", "4-Bit Loadable Cascadable Dual Edge Triggered Binary Counter with Clock Enable and Asynchronous Clear", [No; No; No; No; No; No; Primitive];
    "CBD8CLE    ", "8-Bit Loadable Cascadable Dual Edge Triggered Binary Counter with Clock Enable and Asynchronous Clear", [No; No; No; No; No; No; Primitive];
    "CBD16CLE   ", "16-Bit Loadable Cascadable Dual Edge Triggered Binary Counter with Clock Enable and Asynchronous Clear", [No; No; No; No; No; No; Primitive];
    "CBD2CLED   ", "2-Bit Loadable Cascadable Bidirectional Dual Edge Triggered Binary Counter with Clock Enable and Asynchronous Clear", [No; No; No; No; No; No; Primitive];
    "CBD4CLED   ", "4-Bit Loadable Cascadable Bidirectional Dual Edge Triggered Binary Counter with Clock Enable and Asynchronous Clear", [No; No; No; No; No; No; Primitive];
    "CBD8CLED   ", "8-Bit Loadable Cascadable Bidirectional Dual Edge Triggered Binary Counter with Clock Enable and Asynchronous Clear", [No; No; No; No; No; No; Primitive];
    "CBD16CLED  ", "16-Bit Loadable Cascadable Bidirectional Dual Edge Triggered Binary Counter with Clock Enable and Asynchronous Clear", [No; No; No; No; No; No; Primitive];
    "CBD2RE     ", "2-Bit Cascadable Dual Edge Triggered Binary Counter with Clock Enable and Synchronous Reset", [No; No; No; No; No; No; Primitive];
    "CBD4RE     ", "4-Bit Cascadable Dual Edge Triggered Binary Counter with Clock Enable and Synchronous Reset", [No; No; No; No; No; No; Primitive];
    "CBD8RE     ", "8-Bit Cascadable Dual Edge Triggered Binary Counter with Clock Enable and Synchronous Reset", [No; No; No; No; No; No; Primitive];
    "CBD16RE    ", "16-Bit Cascadable Dual Edge Triggered Binary Counter with Clock Enable and Synchronous Reset", [No; No; No; No; No; No; Primitive];
    "CBD2RLE    ", "2-Bit Loadable Cascadable Dual Edge Triggered Binary Counter with Clock Enable and Synchronous Reset", [No; No; No; No; No; No; Primitive];
    "CBD4RLE    ", "4-Bit Loadable Cascadable Dual Edge Triggered Binary Counter with Clock Enable and Synchronous Reset", [No; No; No; No; No; No; Primitive];
    "CBD8RLE    ", "8-Bit Loadable Cascadable Dual Edge Triggered Binary Counter with Clock Enable and Synchronous Reset", [No; No; No; No; No; No; Primitive];
    "CBD16RLE   ", "16-Bit Loadable Cascadable Dual Edge Triggered Binary Counter with Clock Enable and Synchronous Reset", [No; No; No; No; No; No; Primitive];
    "CBD2X1     ", "2-Bit Loadable Cascadable Bidirectional Dual Edge Triggered Binary Counter with Clock Enable and Asynchronous Clear", [No; No; No; No; No; No; Primitive];
    "CBD4X1     ", "4-Bit Loadable Cascadable Bidirectional Dual Edge Triggered Binary Counter with Clock Enable and Asynchronous Clear", [No; No; No; No; No; No; Primitive];
    "CBD8X1     ", "8-Bit Loadable Cascadable Bidirectional Dual Edge Triggered Binary Counter with Clock Enable and Asynchronous Clear", [No; No; No; No; No; No; Primitive];
    "CBD16X1    ", "16-Bit Loadable Cascadable Bidirectional Dual Edge Triggered Binary Counter with Clock Enable and Asynchronous Clear", [No; No; No; No; No; No; Primitive];
    "CBD2X2     ", "2-Bit Loadable Cascadable Bidirectional Dual Edge Triggered Binary Counter with Clock Enable and Synchronous Reset", [No; No; No; No; No; No; Primitive];
    "CBD4X2     ", "4-Bit Loadable Cascadable Bidirectional Dual Edge Triggered Binary Counter with Clock Enable and Synchronous Reset", [No; No; No; No; No; No; Primitive];
    "CBD8X2     ", "8-Bit Loadable Cascadable Bidirectional Dual Edge Triggered Binary Counter with Clock Enable and Synchronous Reset", [No; No; No; No; No; No; Primitive];
    "CBD16X2    ", "16-Bit Loadable Cascadable Bidirectional Dual Edge Triggered Binary Counter with Clock Enable and Synchronous Reset", [No; No; No; No; No; No; Primitive];
    "CC8CE      ", "8-Bit Cascadable Binary Counter with Clock Enable and Asynchronous Clear", [Macro; Macro; Macro; Macro; No; No; No];
    "CC16CE     ", "16-Bit Cascadable Binary Counter with Clock Enable and Asynchronous Clear", [Macro; Macro; Macro; Macro; No; No; No];
    "CC8CLE     ", "8-Bit Loadable Cascadable Binary Counter with Clock Enable and Asynchronous Clear", [Macro; Macro; Macro; Macro; No; No; No];
    "CC16CLE    ", "16-Bit Loadable Cascadable Binary Counter with Clock Enable and Asynchronous Clear", [Macro; Macro; Macro; Macro; No; No; No];
    "CC8CLED    ", "8-Bit Loadable Cascadable Bidirectional Binary Counter with Clock Enable and Asynchronous Clear", [Macro; Macro; Macro; Macro; No; No; No];
    "CC16CLED   ", "16-Bit Loadable Cascadable Bidirectional Binary Counter with Clock Enable and Asynchronous Clear", [Macro; Macro; Macro; Macro; No; No; No];
    "CC8RE      ", "8-Bit Cascadable Binary Counter with Clock Enable and Synchronous Reset", [Macro; Macro; Macro; Macro; No; No; No];
    "CC16RE     ", "16-Bit Cascadable Binary Counter with Clock Enable and Synchronous Reset", [Macro; Macro; Macro; Macro; No; No; No];
    "CD4CE      ", "4-Bit Cascadable BCD Counter with Clock Enable and Asynchronous Clear", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "CD4CLE     ", "4-Bit Loadable Cascadable BCD Counter with Clock Enable and Asynchronous Clear", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "CD4RE      ", "4-Bit Cascadable BCD Counter with Clock Enable and Synchronous Reset", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "CD4RLE     ", "4-Bit Cascadable BCD Counter with Clock Enable And Synchronous Reset", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "CDD4CE     ", "4-Bit Cascadable Dual Edge Triggered BCD Counter with Clock Enable and Asynchronous Clear", [No; No; No; No; No; No; Primitive];
    "CDD4CLE    ", "4-Bit Loadable Cascadable Dual Edge Triggered BCD Counter with Clock Enable and Asynchronous Clear", [No; No; No; No; No; No; Primitive];
    "CDD4RE     ", "4-Bit Cascadable Dual Edge Triggered BCD Counter with Clock Enable and Synchronous Reset", [No; No; No; No; No; No; Primitive];
    "CDD4RLE    ", "4-Bit Loadable Cascadable Dual Edge Triggered BCD Counter with Clock Enable and Synchronous Reset", [No; No; No; No; No; No; Primitive];
    "CD4RE      ", "4-Bit Cascadable BCD Counter with Clock Enable and Synchronous Reset", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "CD4RLE     ", "4-Bit Loadable Cascadable BCD Counter with Clock Enable and Synchronous Reset", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "CJ4CE      ", "4-Bit Johnson Counter with Clock Enable and Asynchronous Clear", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "CJ5CE      ", "5-Bit Johnson Counter with Clock Enable and Asynchronous Clear", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "CJ8CE      ", "8-Bit Johnson Counter with Clock Enable and Asynchronous Clear", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "CJ4RE      ", "4-Bit Johnson Counter with Clock Enable and Synchronous Reset", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "CJ5RE      ", "5-Bit Johnson Counter with Clock Enable and Synchronous Reset", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "CJ8RE      ", "8-Bit Johnson Counter with Clock Enable and Synchronous Reset", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "CJD4CE     ", "4-Bit Dual Edge Triggered Johnson Counter with Clock Enable and Asynchronous Clear", [No; No; No; No; No; No; Primitive];
    "CJD5CE     ", "5-Bit Dual Edge Triggered Johnson Counter with Clock Enable and Asynchronous Clear", [No; No; No; No; No; No; Primitive];
    "CJD8CE     ", "8-Bit Dual Edge Triggered Johnson Counter with Clock Enable and Asynchronous Clear", [No; No; No; No; No; No; Primitive];
    "CJD4RE     ", "4-Bit Dual Edge Triggered Johnson Counter with Clock Enable and Synchronous Reset", [No; No; No; No; No; No; Primitive];
    "CJD5RE     ", "5-Bit Dual Edge Triggered Johnson Counter with Clock Enable and Synchronous Reset", [No; No; No; No; No; No; Primitive];
    "CJD8RE     ", "8-Bit Dual Edge Triggered Johnson Counter with Clock Enable and Synchronous Reset", [No; No; No; No; No; No; Primitive];
    "CR8CE      ", "8-Bit Negative-Edge Binary Ripple Counter with Clock Enable and Asynchronous Clear", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "CR16CE     ", "16-Bit Negative-Edge Binary Ripple Counter with Clock Enable and Asynchronous Clear", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "CRD8CE     ", "8-Bit Dual-Edge Triggered Binary Ripple Counter with Clock Enable and Asynchronous Clear", [No; No; No; No; No; No; Primitive];
    "CRD16CE    ", "16-Bit Dual-Edge Triggered Binary Ripple Counter with Clock Enable and Asynchronous Clear", [No; No; No; No; No; No; Primitive];
  ];

  "decoders", [
    "D2_4E    ",       "2- to 4-Line Decoder/Demultiplexer with Enable", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "D3_8E    ",       "3- to 8-Line Decoder/Demultiplexer with Enable", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "D4_16E   ",       "4- to 16-Line Decoder/Demultiplexer with Enable", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "DEC_CC4  ",       "4-Bit Active Low Decoder", [Macro; Macro; Macro; Macro; No; No; No];
    "DEC_CC8  ",       "8-Bit Active Low Decoder", [Macro; Macro; Macro; Macro; No; No; No];
    "DEC_CC16 ",       "16-Bit Active Low Decoder", [Macro; Macro; Macro; Macro; No; No; No];
  ];

  "edge decoders", [
    "DECODE4  ",       "4-Bit Active-Low Decoder   ", [Macro; Macro; Macro; Macro; No; No; No];
    "DECODE8  ",       "8-Bit Active-Low Decoder   ", [Macro; Macro; Macro; Macro; No; No; No];
    "DECODE16 ",       "16-Bit Active-Low Decoder  ", [Macro; Macro; Macro; Macro; No; No; No];
    "DECODE32 ",       "32-Bit Active-Low Decoder  ", [Macro; Macro; Macro; Macro; No; No; No];
    "DECODE64 ",       "64-Bit Active-Low Decoder  ", [Macro; Macro; Macro; Macro; No; No; No];
  ];

  "flip flop", [
    "FD D      ",      "Flip-Flop", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "FD_1      ",      "D Flip-Flop with Negative-Edge Clock", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "FD4       ",      "Multiple D Flip-Flop", [No; No; No; No; Primitive; Primitive; Primitive];
    "FD8       ",      "Multiple D Flip-Flop", [No; No; No; No; Primitive; Primitive; Primitive];
    "FD16      ",      "Multiple D Flip-Flop", [No; No; No; No; Primitive; Primitive; Primitive];
    "FD4CE     ",      "4-Bit Data Register with Clock Enable and Asynchronous Clear", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "FD8CE     ",      "8-Bit Data Register with Clock Enable and Asynchronous Clear", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "FD16CE    ",      "16-Bit Data Register with Clock Enable and Asynchronous Clear", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "FD4RE     ",      "4-Bit Data Register with Clock Enable and Synchronous Reset", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "FD8RE     ",      "8-Bit Data Register with Clock Enable and Synchronous Reset", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "FD16RE    ",      "16-Bit Data Register with Clock Enable and Synchronous Reset", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "FDC D     ",      "Flip-Flop with Asynchronous Clear", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "FDC_1     ",      "D Flip-Flop with Negative-Edge Clock and Asynchronous Clear", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "FDCE      ",      "D Flip-Flop with Clock Enable and Asynchronous Clear", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "FDCE_1    ",      "D Flip-Flop with Negative-Edge Clock, Clock Enable, and Asynchronous Clear", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "FDCP      ",      "D Flip-Flop with Asynchronous Preset and Clear", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "FDCP_1    ",      "D Flip-Flop with Negative-Edge Clock and Asynchronous Preset and Clear", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "FDCPE     ",      "D Flip-Flop with Clock Enable and Asynchronous Preset and Clear", [Primitive; Primitive; Primitive; Primitive; Macro; Macro; Primitive ];
    "FDCPE_1   ",      "D Flip-Flop with Negative-Edge Clock, Clock Enable, and Asynchronous Preset and Clear", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "FDD       ",      "Dual Edge Triggered D Flip-Flop", [No; No; No; No; No; No; Primitive];
    "FDD4      ",      "Multiple Dual Edge Triggered D Flip-Flop", [No; No; No; No; No; No; Primitive];
    "FDD8      ",      "Multiple Dual Edge Triggered D Flip-Flop", [No; No; No; No; No; No; Primitive];
    "FDD16     ",      "Multiple Dual Edge Triggered D Flip-Flop", [No; No; No; No; No; No; Primitive];
    "FDD4CE    ",      "4-Bit Dual Edge Triggered Data Register with Clock Enable and Asynchronous Clear", [No; No; No; No; No; No; Primitive];
    "FDD8CE    ",      "8-Bit Dual Edge Triggered Data Register with Clock Enable and Asynchronous Clear", [No; No; No; No; No; No; Primitive];
    "FDD16CE   ",      "16-Bit Dual Edge Triggered Data Register with Clock Enable and Asynchronous Clear", [No; No; No; No; No; No; Primitive];
    "FDD4RE    ",      "4-Bit Dual Edge Triggered Data Register with Clock Enable and Synchronous Reset", [No; No; No; No; No; No; Primitive];
    "FDD8RE    ",      "8-Bit Dual Edge Triggered Data Register with Clock Enable and Synchronous Reset", [No; No; No; No; No; No; Primitive];
    "FDD16RE   ",      "16-Bit Dual Edge Triggered Data Register with Clock Enable and Synchronous Reset", [No; No; No; No; No; No; Primitive];
    "FDDC      ",      "D Dual Edge Triggered Flip-Flop with Asynchronous Clear", [No; No; No; No; No; No; Primitive];
    "FDDCE     ",      "Dual Edge Triggered D Flip-Flop with Clock Enable and Asynchronous Clear", [No; No; No; No; No; No; Primitive];
    "FDDCP     ",      "Dual Edge Triggered D Flip-Flop Asynchronous Preset and Clear", [No; No; No; No; No; No; Primitive];
    "FDDCPE    ",      "Dual Edge Triggered D Flip-Flop with Clock Enable and Asynchronous Preset and Clear", [No; No; No; No; No; No; Primitive];
    "FDDP      ",      "Dual Edge Triggered D Flip-Flop with Asynchronous Preset", [No; No; No; No; No; No; Primitive];
    "FDDPE     ",      "Dual Edge Triggered D Flip-Flop with Clock Enable and Asynchronous Preset", [No; No; No; No; No; No; Primitive];
    "FDDR      ",      "Dual Edge Triggered D Flip-Flop with Synchronous Reset", [No; No; No; No; No; No; Primitive];
    "FDDRCPE   ",      "Dual Data Rate D Flip-Flop with Clock Enable and Asynchronous Preset and Clear", [No; Primitive; No; Primitive; No; No; No];
    "FDDRE     ",      "Dual Edge Triggered D Flip-Flop with Clock Enable and Synchronous Reset", [No; No; No; No; No; No; Primitive];
    "FDDRSE    ",      "Dual Data Rate D Flip-Flop with Clock Enable and Synchronous Reset and Set", [No; Primitive; No; Primitive; No; No; No];
    "FDDRS     ",      "Dual Edge Triggered D Flip-Flop with Synchronous Reset and Set", [No; No; No; No; No; No; Primitive];
    "FDDRSE    ",      "Dual Edge Triggered D Flip-Flop with Synchronous Reset and Set and Clock Enable", [No; No; No; No; No; No; Primitive];
    "FDDS      ",      "Dual Edge Triggered D Flip-Flop with Synchronous Set", [No; No; No; No; No; No; Primitive];
    "FDDSE     ",      "D Flip-Flop with Clock Enable and Synchronous Set", [No; No; No; No; No; No; Primitive];
    "FDDSR     ",      "Dual Edge Triggered D Flip-Flop with Synchronous Set and Reset", [No; No; No; No; No; No; Primitive];
    "FDDSRE    ",      "Dual Edge Triggered D Flip-Flop with Synchronous Set and Reset and Clock Enable", [No; No; No; No; No; No; Primitive];
    "FDE       ",      "D Flip-Flop with Clock Enable", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "FDE_1     ",      "D Flip-Flop with Negative-Edge Clock and Clock Enable", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "FDP       ",      "D Flip-Flop with Asynchronous Preset", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "FDP_1     ",      "D Flip-Flop with Negative-Edge Clock and Asynchronous Preset", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "FDPE      ",      "D Flip-Flop with Clock Enable and Asynchronous Preset", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "FDPE_1    ",      "D Flip-Flop with Negative-Edge Clock, Clock Enable, and Asynchronous Preset", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "FDR       ",      "D Flip-Flop with Synchronous Reset", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "FDR_1     ",      "D Flip-Flop with Negative-Edge Clock and Synchronous Reset", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "FDRE      ",      "D Flip-Flop with Clock Enable and Synchronous Reset", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "FDRE_1    ",      "D Flip-Flop with Negative-Clock Edge, Clock Enable, and Synchronous Reset", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "FDRS D    ",      "Flip-Flop with Synchronous Reset and Set", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "FDRS_1    ",      "D Flip-Flop with Negative-Clock Edge and Synchronous Reset and Set", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "FDRSE     ",      "D Flip-Flop with Synchronous Reset and Set and Clock Enable", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "FDRSE_1   ",      "D Flip-Flop with Negative-Clock Edge, Synchronous Reset and Set, and Clock Enable", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "FDS D     ",      "Flip-Flop with Synchronous Set", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "FDS_1 D   ",      "Flip-Flop with Negative-Edge Clock and Synchronous Set", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "FDSE D    ",      "Flip-Flop with Clock Enable and Synchronous Set", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "FDSE_1    ",      "D Flip-Flop with Negative-Edge Clock, Clock Enable, and Synchronous Set", [Primitive; Primitive; Primitive; No; No; No; No];
    "FDSR D    ",      "Flip-Flop with Synchronous Set and Reset", [No; No; No; No; Primitive; Primitive; Primitive];
    "FDSRE D   ",      "Flip-Flop with Synchronous Set and Reset and Clock Enable", [No; No; No; No; Primitive; Primitive; Primitive];
    "FJKC      ",      "J-K Flip-Flop with Asynchronous Clear", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "FJKCE     ",      "J-K Flip-Flop with Clock Enable and Asynchronous Clear", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "FJKCP     ",      "J-K Flip-Flop with Asynchronous Clear and Preset", [No; No; No; No; Primitive; Primitive; Primitive];
    "FJKCPE    ",      "J-K Flip-Flop with Asynchronous Clear and Preset and Clock Enable", [No; No; No; No; Primitive; Primitive; Primitive];
    "FJKP      ",      "J-K Flip-Flop with Asynchronous Preset", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "FJKPE     ",      "J-K Flip-Flop with Clock Enable and Asynchronous Preset", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "FJKRSE    ",      "J-K Flip-Flop with Clock Enable and Synchronous Reset and Set", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "FJKSRE    ",      "J-K Flip-Flop with Clock Enable and Synchronous Set and Reset", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "FTC       ",      "Toggle Flip-Flop with Toggle Enable and Asynchronous Clear", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "FTCE      ",      "Toggle Flip-Flop with Toggle and Clock Enable and Asynchronous Clear", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "FTCLE     ",      "Toggle/Loadable Flip-Flop with Toggle and Clock Enable and Asynchronous Clear", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "FTCLEX    ",      "Toggle/Loadable Flip-Flop with Toggle and Clock Enable and Asynchronous Clear", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "FTCP      ",      "Toggle Flip-Flop with Toggle Enable and Asynchronous Clear and Preset", [No; No; No; No; Primitive; Primitive; Primitive];
    "FTCPE     ",      "Toggle Flip-Flop with Toggle and Clock Enable and Asynchronous Clear and Preset", [No; No; No; No; Primitive; Primitive; Primitive];
    "FTCPLE    ",      "Loadable Toggle Flip-Flop with Toggle and Clock Enable and Asynchronous Clear and Preset", [No; No; No; No; Primitive; Primitive; Primitive];
    "FTDCE     ",      "Dual Edge Triggered Toggle Flip-Flop with Toggle and Clock Enable and Asynchronous Clear", [No; No; No; No; No; No; Primitive];
    "FTDCLE    ",      "Dual Edge Triggered Toggle/Loadable Flip-Flop with Toggle and Clock Enable and Asynchronous Clear", [No; No; No; No; No; No; Primitive];
    "FTDCLEX   ",      "Dual Edge Triggered Toggle/Loadable Flip-Flop with Toggle and Clock Enable and Asynchronous Clear", [No; No; No; No; No; No; Primitive];
    "FTDCP     ",      "Toggle Flip-Flop with Toggle Enable and Asynchronous Clear and Preset", [No; No; No; No; Primitive; Primitive; Primitive];
    "FTDRSE    ",      "Dual Edge Triggered Toggle Flip- Flop with Toggle and Clock Enable and Synchronous Reset and Set", [No; No; No; No; No; No; Primitive];
    "FTDRSLE   ",      "Dual Edge Triggered Toggle/Loadable Flip-Flop with Toggle and Clock Enable and Synchronous Reset and Set", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "FTP       ",      "Toggle Flip-Flop with Toggle Enable and Asynchronous Preset", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "FTPE      ",      "Toggle Flip-Flop with Toggle and Clock Enable and Asynchronous Preset", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "FTPLE     ",      "Toggle/Loadable Flip-Flop with Toggle and Clock Enable and Asynchronous Preset", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "FTRSE     ",      "Toggle Flip-Flop with Toggle and Clock Enable and Synchronous Reset and Set", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "FTRSLE    ",      "Toggle/Loadable Flip-Flop with Toggle and Clock Enable and Synchronous Reset and Set", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "FTSRE     ",      "Toggle Flip-Flop with Toggle and Clock Enable and Synchronous Set and Reset", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "FTSRLE    ",      "Toggle/Loadable Flip-Flop with Toggle and Clock Enable and Synchronous Set and Reset", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
  ];

  "general", [
    "BSCAN_SPARTAN2        ",  "Spartan-II Boundary Scan Logic Control Circuit", [Primitive; No; No; No; No; No; No];
    "BSCAN_SPARTAN3        ",  "Spartan-3 Boundary Scan Logic Control Circuit", [No; Primitive; No; No; No; No; No];
    "BSCAN_VIRTEX          ",  "Virtex Boundary Scan Logic Control Circuit", [Primitive; No; Primitive; No; No; No; No];
    "BSCAN_VIRTEX2         ",  "Virtex2 Boundary Scan Logic Control Circuit", [No; No; No; Primitive; No; No; No];
    "CAPTURE_SPARTAN2      ",  "Spartan-II Register State Capture for Bitstream Readback", [Primitive; No; No; No; No; No; No];
    "CAPTURE_SPARTAN3      ",  "Spartan-3 Register State Capture for Bitstream Readback", [No; Primitive; No; No; No; No; No];
    "CAPTURE_VIRTEX        ",  "Virtex Register State Capture for Bitstream Readback", [No; No; Primitive; No; No; No; No];
    "CAPTURE_VIRTEX2       ",  "Virtex-II Register State Capture for Bitstream Readback", [No; No; No; Primitive; No; No; No];
    "CLK_DIV2              ",  "Global Clock Divider", [No; No; No; No; No; No; Primitive];
    "CLK_DIV4              ",  "Global Clock Divider", [No; No; No; No; No; No; Primitive];
    "CLK_DIV6              ",  "Global Clock Divider", [No; No; No; No; No; No; Primitive];
    "CLK_DIV8              ",  "Global Clock Divider", [No; No; No; No; No; No; Primitive];
    "CLK_DIV10             ",  "Global Clock Divider", [No; No; No; No; No; No; Primitive];
    "CLK_DIV12             ",  "Global Clock Divider", [No; No; No; No; No; No; Primitive];
    "CLK_DIV14             ",  "Global Clock Divider", [No; No; No; No; No; No; Primitive];
    "CLK_DIV16             ",  "Global Clock Divider", [No; No; No; No; No; No; Primitive];
    "CLK_DIV2R             ",  "Global Clock Divider with Synchronous Reset", [No; No; No; No; No; No; Primitive];
    "CLK_DIV4R             ",  "Global Clock Divider with Synchronous Reset", [No; No; No; No; No; No; Primitive];
    "CLK_DIV6R             ",  "Global Clock Divider with Synchronous Reset", [No; No; No; No; No; No; Primitive];
    "CLK_DIV8R             ",  "Global Clock Divider with Synchronous Reset", [No; No; No; No; No; No; Primitive];
    "CLK_DIV10R            ",  "Global Clock Divider with Synchronous Reset", [No; No; No; No; No; No; Primitive];
    "CLK_DIV12R            ",  "Global Clock Divider with Synchronous Reset", [No; No; No; No; No; No; Primitive];
    "CLK_DIV14R            ",  "Global Clock Divider with Synchronous Reset", [No; No; No; No; No; No; Primitive];
    "CLK_DIV16R            ",  "Global Clock Divider with Synchronous Reset", [No; No; No; No; No; No; Primitive];
    "CLK_DIV2RSD           ",  "Global Clock Divider with Synchronous Reset and Start Delay", [No; No; No; No; No; No; Primitive];
    "CLK_DIV4RSD           ",  "Global Clock Divider with Synchronous Reset and Start Delay", [No; No; No; No; No; No; Primitive];
    "CLK_DIV6RSD           ",  "Global Clock Divider with Synchronous Reset and Start Delay", [No; No; No; No; No; No; Primitive];
    "CLK_DIV8RSD           ",  "Global Clock Divider with Synchronous Reset and Start Delay", [No; No; No; No; No; No; Primitive];
    "CLK_DIV10RSD          ",  "Global Clock Divider with Synchronous Reset and Start Delay", [No; No; No; No; No; No; Primitive];
    "CLK_DIV12RSD          ",  "Global Clock Divider with Synchronous Reset and Start Delay", [No; No; No; No; No; No; Primitive];
    "CLK_DIV14RSD          ",  "Global Clock Divider with Synchronous Reset and Start Delay", [No; No; No; No; No; No; Primitive];
    "CLK_DIV16RSD          ",  "Global Clock Divider with Synchronous Reset and Start Delay", [No; No; No; No; No; No; Primitive];
    "CLK_DIV2SD            ",  "Global Clock Divider with Start Delay", [No; No; No; No; No; No; Primitive];
    "CLK_DIV4SD            ",  "Global Clock Divider with Start Delay", [No; No; No; No; No; No; Primitive];
    "CLK_DIV6SD            ",  "Global Clock Divider with Start Delay", [No; No; No; No; No; No; Primitive];
    "CLK_DIV8SD            ",  "Global Clock Divider with Start Delay", [No; No; No; No; No; No; Primitive];
    "CLK_DIV10SD           ",  "Global Clock Divider with Start Delay", [No; No; No; No; No; No; Primitive];
    "CLK_DIV12SD           ",  "Global Clock Divider with Start Delay", [No; No; No; No; No; No; Primitive];
    "CLK_DIV14SD           ",  "Global Clock Divider with Start Delay", [No; No; No; No; No; No; Primitive];
    "CLK_DIV16SD           ",  "Global Clock Divider with Start Delay", [No; No; No; No; No; No; Primitive];
    "CLKDLL                ",  "Clock Delay Locked Loop", [Primitive; No; Primitive; No; No; No; No]; 
    "CLKDLLE               ",  "Clock Delay Locked Loop with Expanded Output", [Primitive; No; Primitive; No; No; No; No];
    "CLKDLLHF              ",  "High Frequency Clock Delay Locked Loop", [Primitive; No; Primitive; No; No; No; No];
    "DCM                   ",  "Digital Clock Manager", [No; Primitive; No; Primitive; No; No; No];
    "GND                   ",  "Ground-Connection Signal Tag", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "ICAP_VIRTEX2          ",  "User Interface to Virtex-II Internal Configuration Access Port", [No; No; No; Primitive; No; No; No];
    "JTAGPPC               ",  "JTAG Primitive for the Power PC", [No; No; No; Primitive; No; No; No];
    "KEEPER                ",  "KEEPER Symbol", [Primitive; Primitive; Primitive; Primitive; Primitive; No; Primitive];
    "LUT1                  ",  "1-Bit Look-Up Table with General Output", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "LUT2                  ",  "2-Bit Look-Up Table with General Output", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "LUT3                  ",  "3-Bit Look-Up Table with General Output", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "LUT4                  ",  "4-Bit Look-Up Table with General Output", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "LUT1_D                ",  "1-Bit Look-Up Table with Dual Output", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "LUT2_D                ",  "2-Bit Look-Up Table with Dual Output", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "LUT3_D                ",  "3-Bit Look-Up Table with Dual Output", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "LUT4_D                ",  "4-Bit Look-Up Table with Dual Output", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "LUT1_L                ",  "1-Bit Look-Up Table with Local Output", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "LUT2_L                ",  "2-Bit Look-Up Table with Local Output", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "LUT3_L                ",  "3-Bit Look-Up Table with Local Output", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "LUT4_L                ",  "4-Bit Look-Up Table with Local Output", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "PPC405                ",  "Primitive for the Power PC Core", [No; No; No; Primitive; No; No; No];
    "PULLDOWN              ",  "Resistor to GND for Input Pads", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "PULLUP                ",  "Resistor to VCC for Input PADs, Open-Drain, and 3-State Outputs", [Primitive; Primitive; Primitive; Primitive; No; Primitive; Primitive];
    "ROC                   ",  "Reset On Configuration", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "STARTBUF_architecture ",  "VHDL Simulation of FPGA Designs", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "STARTUP_SPARTAN2      ",  "Spartan-II User Interface to Global Clock, Reset, and 3-State Controls", [Primitive; No; No; No; No; No; No];
    "STARTUP_SPARTAN3      ",  "Spartan-3 User Interface to Global Clock, Reset, and 3-State Controls", [No; Primitive; No; No; No; No; No];
    "STARTUP_VIRTEX        ",  "Virtex User Interface to Global Clock, Reset, and 3-State Controls", [Primitive; No; Primitive; No; No; No; No];
    "STARTUP_VIRTEX2       ",  "Virtex-II User Interface to Global Clock, Reset, and 3-State Controls", [No; No; No; Primitive; No; No; No];
    "TOC                   ",  "Three-State On Configuration", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "TOCBUF                ",  "Three-State On Configuration Buffer", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "VCC                   ",  "VCC-Connection Signal Tag", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
  ];

  "input latches", [
    "ILD    ", "Transparent Input Data Latch", [Macro; Macro; Macro; Macro; No; No; No];
    "ILD4   ", "Transparent Input Data Latch", [Macro; Macro; Macro; Macro; No; No; No];
    "ILD8   ", "Transparent Input Data Latch", [Macro; Macro; Macro; Macro; No; No; No];
    "ILD16  ", "Transparent Input Data Latch", [Macro; Macro; Macro; Macro; No; No; No];
    "ILD_1  ", "Transparent Input Data Latch with Inverted Gate", [Macro; Macro; Macro; Macro; No; No; No];
    "ILDI   ", "Transparent Input Data Latch (Asynchronous Preset)", [Macro; Macro; Macro; Macro; No; No; No]; 
    "ILDI_1 ", "Transparent Input Data Latch with Inverted Gate (Asynchronous Preset)", [Macro; Macro; Macro; Macro; No; No; No];
    "ILDX   ", "Transparent Input Data Latch", [Macro; Macro; Macro; Macro; No; No; No];
    "ILDX4  ", "Transparent Input Data Latch", [Macro; Macro; Macro; Macro; No; No; No];
    "ILDX8  ", "Transparent Input Data Latch", [Macro; Macro; Macro; Macro; No; No; No];
    "ILDX16 ", "Transparent Input Data Latch", [Macro; Macro; Macro; Macro; No; No; No];
    "ILDX_1 ", "Transparent Input Data Latch with Inverted Gate", [Macro; Macro; Macro; Macro; No; No; No];
    "ILDXI  ", "Transparent Input Data Latch (Asynchronous Preset)", [Macro; Macro; Macro; Macro; No; No; No];
    "ILDXI_1", "Transparent Input Data Latch with Inverted Gate (Asynchronous Preset)", [Macro; Macro; Macro; Macro; No; No; No];
  ];

  "input/output flip flops", [
    "IFD       ",  "Single- and Multiple-Input D Flip-Flop", [Macro; Macro; Macro; Macro; No; No; No];
    "IFD_1     ",  "Input D Flip-Flop with Inverted Clock", [Macro; Macro; Macro; Macro; No; No; No];
    "IFD4      ",  "Single- and Multiple-Input D Flip-Flop", [Macro; Macro; Macro; Macro; No; No; No];
    "IFD8      ",  "Single- and Multiple-Input D Flip-Flop", [Macro; Macro; Macro; Macro; No; No; No];
    "IFD16     ",  "Single- and Multiple-Input D Flip-Flop", [Macro; Macro; Macro; Macro; No; No; No];
    "IFDDRCPE  ",  "Dual Data Rate Input D Flip-Flop with Clock Enable and Asynchronous Preset and Clear", [No; Primitive; No; Primitive; No; No; No];
    "IFDDRRSE  ",  "Dual Data Rate Input D Flip-Flop with Synchronous Reset and Set and Clock Enable", [No; Primitive; No; Primitive; No; No; No];
    "IFDI      ",  "Input D Flip-Flop (Asynchronous Preset)", [Macro; Macro; Macro; Macro; No; No; No];
    "IFDI_1    ",  "Input D Flip-Flop with Inverted Clock (Asynchronous Preset)", [Macro; Macro; Macro; Macro; No; No; No];
    "IFDX      ",  "Single- and Multiple-Input D Flip-Flop with Clock Enable", [Macro; Macro; Macro; Macro; No; No; No];
    "IFDX_1    ",  "Input D Flip-Flop with Inverted Clock and Clock Enable", [Macro; Macro; Macro; Macro; No; No; No];
    "IFDX4     ",  "Single- and Multiple-Input D Flip-Flop with Clock Enable", [Macro; Macro; Macro; Macro; No; No; No];
    "IFDX8     ",  "Single- and Multiple-Input D Flip-Flop with Clock Enable", [Macro; Macro; Macro; Macro; No; No; No];
    "IFDX16    ",  "Single- and Multiple-Input D Flip-Flops with Clock Enable", [Macro; Macro; Macro; Macro; No; No; No];
    "IFDXI     ",  "Input D Flip-Flop with Clock Enable (Asynchronous Preset)", [Macro; Macro; Macro; Macro; No; No; No];
    "ILDXI_1   ",  "Input D Flip-Flop with Inverted Clock and Clock Enable (Asynchronous Preset)", [Macro; Macro; Macro; Macro; No; No; No];
    "OFD       ",  "Single- and Multiple- Output D Flip-Flops", [Macro; Macro; Macro; Macro; No; No; No];
    "OFD4      ",  "Single- and Multiple- Output D Flip-Flops", [Macro; Macro; Macro; Macro; No; No; No];
    "OFD8      ",  "Single- and Multiple- Output D Flip-Flops", [Macro; Macro; Macro; Macro; No; No; No];
    "OFD16     ",  "Single- and Multiple-Output D Flip-Flops", [Macro; Macro; Macro; Macro; No; No; No];
    "OFD_1     ",  "Output D Flip-Flop with Inverted Clock", [Macro; Macro; Macro; Macro; No; No; No];
    "OFDDRCPE  ",  "Dual Data Rate Output D Flip-Flop with Clock Enable and Asynchronous Preset and Clear", [No; Primitive; No; Primitive; No; No; No];
    "OFDDRRSE  ",  "Dual Data Rate Output D Flip-Flop with Synchronous Reset and Set and Clock Enable", [No; Primitive; No; Primitive; No; No; No];
    "OFDDRTCPE ",  "Dual Data Rate D Flip-Flop with Active-Low 3--State Output Buffer, Clock Enable, and Asynchro-nous Preset and Clear", [No; Primitive; No; Primitive; No; No; No];
    "OFDDRTRSE ",  "Dual Data Rate D Flip-Flop with Active -Low 3-State Output Buffer, Synchronous Reset and Set, and Clock Enable", [No; Primitive; No; Primitive; No; No; No];
    "OFDE      ",  "D Flip-Flop with Active-High Enable Output Buffers", [Macro; Macro; Macro; Macro; No; No; No];
    "OFDE4     ",  "D Flip-Flop with Active-High Enable Output Buffers", [Macro; Macro; Macro; Macro; No; No; No];
    "OFDE8     ",  "D Flip-Flop with Active-High Enable Output Buffers", [Macro; Macro; Macro; Macro; No; No; No];
    "OFDE16    ",  "D Flip-Flop with Active- High Enable Output Buffers", [Macro; Macro; Macro; Macro; No; No; No];
    "OFDE_1    ",  "D Flip-Flop with Active- High Enable Output Buffer and Inverted Clock", [Macro; Macro; Macro; Macro; No; No; No];
    "OFDI      ",  "Output D Flip-Flop (Asynchronous Preset)", [Macro; Macro; Macro; Macro; No; No; No];
    "OFDI_1    ",  "Output D Flip-Flop with Inverted Clock (Asynchronous Preset)", [Macro; Macro; Macro; Macro; No; No; No];
    "OFDT      ",  "Single and Multiple D Flip-Flop with Active-Low 3-State Output Buffers", [Macro; Macro; Macro; Macro; No; No; No];
    "OFDT4     ",  "Single and Multiple D Flip-Flop with Active-Low 3-State Output Buffers", [Macro; Macro; Macro; Macro; No; No; No];
    "OFDT8     ",  "Single and Multiple D Flip-Flop with Active-Low 3-State Output Buffers", [Macro; Macro; Macro; Macro; No; No; No];
    "OFDT16    ",  "Single and Multiple D Flip-Flop with Active-Low 3-State Output Buffers", [Macro; Macro; Macro; Macro; No; No; No];
    "OFDT_1    ",  "D Flip-Flop with Active-Low 3-State Output Buffer and Inverted Clock", [Macro; Macro; Macro; Macro; No; No; No];
    "OFDX      ",  "Single- and Multiple-Output D Flip-Flop with Clock Enable", [Macro; Macro; Macro; Macro; No; No; No];
    "OFDX4     ",  "Single- and Multiple-Output D Flip-Flop with Clock Enable", [Macro; Macro; Macro; Macro; No; No; No];
    "OFDX8     ",  "Single- and Multiple- Output D Flip-Flop with Clock Enable", [Macro; Macro; Macro; Macro; No; No; No];
    "OFDX16    ",  "Single- and Multiple- Output D Flip-Flop with Clock Enable", [Macro; Macro; Macro; Macro; No; No; No];
    "OFDX_1    ",  "Output D Flip-Flop with Inverted Clock and Clock Enable", [Macro; Macro; Macro; Macro; No; No; No];
    "OFDXI     ",  "Output D Flip-Flop with Clock Enable (Asynchronous Preset)", [Macro; Macro; Macro; Macro; No; No; No];
    "OFDXI_1   ",  "Output D Flip-Flop with Inverted Clock and Clock Enable (Asynchronous Preset)", [Macro; Macro; Macro; Macro; No; No; No];
  ];

  "input/output functions", [
    "GT_AURORA_n       ",  "Gigabit Transceiver for High-Speed I/O", [No; No; No; Primitive; No; No; No];
    "GT_CUSTOM         ",  "Gigabit Transceiver for High-Speed I/O", [No; No; No; Primitive; No; No; No];
    "GT_ETHERNET_n     ",  "Gigabit Transceiver for High-Speed I/O", [No; No; No; Primitive; No; No; No];
    "GT_FIBRE_CHAN_n   ",  "Gigabit Transceiver for High-Speed I/O", [No; No; No; Primitive; No; No; No];
    "GT_INFINIBAND_n   ",  "10-Gigabit Transceiver for High-Speed I/O", [No; No; No; Primitive; No; No; No];
    "GT_XAUI_n         ",  "10-Gigabit Transceiver for High-Speed I/O", [No; No; No; Primitive; No; No; No];
    "GT10_10GE_n       ",  "10-Gigabit Transceiver for High-Speed I/O", [No; No; No; Primitive; No; No; No];
    "GT10_10GFC_n      ",  "10-Gigabit Transceiver for High-Speed I/O", [No; No; No; Primitive; No; No; No];
    "GT10_AURORA_n     ",  "10-Gigabit Transceiver for High-Speed I/O", [No; No; No; Primitive; No; No; No];
    "GT10_AURORAX_n    ",  "10-Gigabit Transceiver for High-Speed I/O", [No; No; No; Primitive; No; No; No];
    "GT10_CUSTOM       ",  "10-Gigabit Transceiver for High-Speed I/O", [No; No; No; Primitive; No; No; No];
    "GT10_OC48_n       ",  "10-Gigabit Transceiver for High-Speed I/O", [No; No; No; Primitive; No; No; No];
    "GT10_OC192_n      ",  "10-Gigabit Transceiver for High-Speed I/O", [No; No; No; Primitive; No; No; No];
    "GT10_PCI_EXPRESS_n",  "10-Gigabit Transceiver for High-Speed I/O", [No; No; No; Primitive; No; No; No];
    "IBUF              ",  "Single- and Multiple- Input Buffer", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "IBUF4             ",  "Single- and Multiple- Input Buffer", [Macro; No; Macro; Macro; Primitive; Primitive; Primitive];
    "IBUF8             ",  "Single- and Multiple- Input Buffer", [Macro; No; Macro; Macro; Primitive; Primitive; Primitive];
    "IBUF16            ",  "Single- and Multiple- Input Buffer", [Macro; No; Macro; Macro; Primitive; Primitive; Primitive];
    "IBUFDS            ",  "Differential Signaling Input Buffer with Selectable I/O Interface", [No; Primitive; No; Primitive; No; No; No];
    "IBUFG             ",  "Dedicated Input Buffer with Selectable I/O Interface", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "IBUFGDS           ",  "Dedicated Differential Signaling Input Buffer with Selectable I/O Interface", [No; Primitive; No; Primitive; No; No; No];
    "IOBUF             ",  "Bi-Directional Buffer with Selectable I/0 Interface (multiple primitives)", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "IOBUFDS           ",  "3-State Differential Signaling I/O Buffer with Active Low Output Enable", [No; Primitive; No; Primitive; No; No; No];
    "IOPAD             ",  "Single- and Multiple- Input/Output Pad", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "IOPAD4            ",  "Single- and Multiple- Input/Output Pad", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "IOPAD8            ",  "Single- and Multiple- Input/Output Pad", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "IOPAD16           ",  "Single- and Multiple- Input/Output Pad", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "IPAD              ",  "Single- and Multiple- Input Pad", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "IPAD4             ",  "Single- and Multiple- Input Pad", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "IPAD8             ",  "Single- and Multiple-Input Pad", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "IPAD16            ",  "Single- and Multiple-Input Pad", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "OBUF              ",  "Single- and Multiple-Output Buffer", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "OBUF4             ",  "Single- and Multiple-Output Buffer", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "OBUF8             ",  "Single- and Multiple-Output Buffer", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "OBUF16            ",  "Single- and Multiple-Output Buffer", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "OBUFDS            ",  "Differential Signaling Output Buffer with Selectable I/O Interface", [No; Primitive; No; Primitive; No; No; No];
    "OBUFE             ",  "3-State Output Buffers with Active-High Output Enable", [Macro; No; Macro; Macro; Primitive; Primitive; Primitive];
    "OBUFE4            ",  "3-State Output Buffers with Active-High Output Enable", [Macro; No; Macro; Macro; Primitive; Primitive; Primitive];
    "OBUFE8            ",  "3-State Output Buffers with Active-High Output Enable", [Macro; No; Macro; Macro; Primitive; Primitive; Primitive];
    "OBUFE16           ",  "3-State Output Buffers with Active-High Output Enable", [Macro; No; Macro; Macro; Primitive; Primitive; Primitive];
    "OBUFT             ",  "Single and Multiple 3- State Output Buffer with Active Low Output Enable", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "OBUFT4            ",  "Single and Multiple 3- State Output Buffer with Active Low Output Enable", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "OBUFT8            ",  "Single and Multiple 3- State Output Buffer with Active Low Output Enable", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "OBUFT16           ",  "Single and Multiple 3- State Output Buffer with Active Low Output Enable", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "OBUFTDS           ",  "3-State Output Buffer with Differential Signaling, Active-Low Output Enable, and Selectable I/O Interface", [No; Primitive; No; Primitive; No; No; No];
    "OPAD              ",  "Single- and Multiple- Output Pad", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "OPAD4             ",  "Multiple-Output Pad", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ]; 
    "OPAD8             ",  "Multiple-Output Pad", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "OPAD16            ",  "Multiple-Output Pad", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "UPAD              ",  "Connects the I/O Node of an IOB to the Internal PLD Circuit", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
  ];

  "latches", [
    "LD      ",    "Transparent Data Latch", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "LD_1    ",    "Transparent Data Latch with Inverted Gate", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "LD4     ",    "Multiple Transparent Data Latch", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "LD8     ",    "Multiple Transparent Data Latch", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "LD16    ",    "Multiple Transparent Data Latch", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "LDC     ",    "Transparent Data Latch with Asynchronous Clear", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "LDC_1   ",    "Transparent Data Latch with Asynchronous Clear and Inverted Gate", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "LD4CE   ",    "Transparent Data Latch with Asynchronous Clear and Gate Enable", [Macro; Macro; Macro; Macro; No; No; No];
    "LD8CE   ",    "Transparent Data Latch with Asynchronous Clear and Gate Enable", [Macro; Macro; Macro; Macro; No; No; No];
    "LD16CE  ",    "Transparent Data Latch with Asynchronous Clear and Gate Enable", [Macro; Macro; Macro; Macro; No; No; No];
    "LDCE    ",    "Transparent Data Latch with Asynchronous Clear and Gate Enable", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "LDCE_1  ",    "Transparent Data Latch with Asynchronous Clear, Gate Enable, and Inverted Gate", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "LDCP    ",    "Transparent Data Latch with Asynchronous Clear and Preset", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "LDCP_1  ",    "Transparent Data Latch with Asynchronous Clear and Preset and Inverted Gate", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "LDCPE   ",    "Transparent Data Latch with Asynchronous Clear and Preset and Gate Enable", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "LDCPE_1 ",    "Transparent Data Latch with Asynchronous Clear and Preset, Gate Enable, and Inverted Gate", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "LDE     ",    "Transparent Data Latch with Gate Enable", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "LDE_1   ",    "Transparent Data Latch with Gate Enable and Inverted Gate", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "LDG     ",    "Transparent Datagate Latch", [No; No; No; No; No; No; Primitive]; 
    "LDG4    ",    "Multiple Transparent Datagate Latch", [No; No; No; No; No; No; Primitive];
    "LDG8    ",    "Multiple Transparent Datagate Latch", [No; No; No; No; No; No; Primitive];
    "LDG16   ",    "Multiple Transparent Datagate Latch", [No; No; No; No; No; No; Primitive];
    "LDP     ",    "Transparent Data Latch with Asynchronous Preset", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "LDP_1   ",    "Transparent Data Latch with Asynchronous Preset and Inverted Gate", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "LDPE    ",    "Transparent Data Latch with Asynchronous Preset and Gate Enable", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "LDPE_1  ",    "Transparent Data Latch with Asynchronous Preset, Gate Enable, and Inverted Gate", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
  ];

  "logic primitives", [
    "AND2      ",  "2-Input AND Gate with Inverted and Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "AND2B1    ",  "2-Input AND Gate with Inverted and Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "AND2B2    ",  "2-Input AND Gate with Inverted and Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "AND3      ",  "3-Input AND Gate with Inverted and Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "AND3B1    ",  "3-Input AND Gate with Inverted and Non-Inverted Inputs", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "AND3B2    ",  "3-Input AND Gate with Inverted and Non-Inverted Inputs", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "AND3B3    ",  "3-Input AND Gate with Inverted and Non-Inverted Inputs", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "AND4      ",  "4-Input AND Gate with Inverted and Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "AND4B1    ",  "4-Input AND Gate with Inverted and Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "AND4B2    ",  "4-Input AND Gate with Inverted and Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "AND4B3    ",  "4-Input AND Gate with Inverted and Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "AND4B4    ",  "4-Input AND Gate with Inverted and Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "AND5      ",  "5-Input AND Gate with Inverted and Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "AND5B1    ",  "5-Input AND Gate with Inverted and Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "AND5B2    ",  "5-Input AND Gate with Inverted and Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "AND5B3    ",  "5-Input AND Gate with Inverted and Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "AND5B4    ",  "5-Input AND Gate with Inverted and Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "AND5B5    ",  "5-Input AND Gate with Inverted and Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "AND6      ",  "6-Input AND Gate with Inverted and Non-Inverted Inputs", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "AND7      ",  "7-Input AND Gate with Inverted and Non-Inverted Inputs", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "AND8      ",  "8-Input AND Gate with Inverted and Non-Inverted Inputs", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "AND9      ",  "9-Input AND Gate with Inverted and Non-Inverted Inputs", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "AND12     ",  "12- Input AND Gate with Non-Inverted Inputs", [Macro; Macro; Macro; Macro; No; No; No];
    "AND16     ",  "16- Input AND Gate with Non-Inverted Inputs", [Macro; Macro; Macro; Macro; No; No; No];
    "INV       ",  "Single and Multiple Inverters", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "INV4      ",  "Single and Multiple Inverters", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "INV8      ",  "Single and Multiple Inverters", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "INV16     ",  "Single and Multiple Inverters", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "MULT_AND  ",  "Fast Multiplier AND", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "NAND2     ",  "2-Input NAND Gate with Inverted and Non-Inverted Inputs.", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "NAND2B1   ",  "2-Input NAND Gate with Inverted and Non-Inverted Inputs.", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "NAND2B2   ",  "2-Input NAND Gate with Inverted and Non-Inverted Inputs.", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "NAND3     ",  "3-Input NAND Gate with Inverted and Non-Inverted Inputs.", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "NAND3B1   ",  "3-Input NAND Gate with Inverted and Non-Inverted Inputs.", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "NAND3B2   ",  "3-Input NAND Gate with Inverted and Non-Inverted Inputs.", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "NAND3B3   ",  "3-Input NAND Gate with Inverted and Non-Inverted Inputs.", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "NAND4     ",  "4-Input NAND Gate with Inverted and Non-Inverted Inputs.", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "NAND4B1   ",  "4-Input NAND Gate with Inverted and Non-Inverted Inputs.", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "NAND4B2   ",  "4-Input NAND Gate with Inverted and Non-Inverted Inputs.", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "NAND4B3   ",  "4-Input NAND Gate with Inverted and Non-Inverted Inputs.", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "NAND4B4   ",  "4-Input NAND Gate with Inverted and Non-Inverted Inputs.", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "NAND5     ",  "5-Input NAND Gate with Inverted and Non-Inverted Inputs.", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "NAND5B1   ",  "5-Input NAND Gate with Inverted and Non-Inverted Inputs.", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "NAND5B2   ",  "5-Input NAND Gate with Inverted and Non-Inverted Inputs.", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "NAND5B3   ",  "5-Input NAND Gate with Inverted and Non-Inverted Inputs.", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "NAND5B4   ",  "5-Input NAND Gate with Inverted and Non-Inverted Inputs.", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "NAND5B5   ",  "5-Input NAND Gate with Inverted and Non-Inverted Inputs.", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "NAND6     ",  "6-Input NAND Gate with Inverted and Non-Inverted Inputs.", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "NAND7     ",  "7-Input NAND Gate with Inverted and Non-Inverted Inputs.", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "NAND8     ",  "8-Input NAND Gate withInverted and Non-Inverted  Inputs.", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "NAND9     ",  "9-Input NAND Gate with Inverted and Non-Inverted Inputs.", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "NAND12    ",  "12- Input NAND Gate with Non-Inverted Inputs.", [Macro; Macro; Macro; Macro; No; No; No]; 
    "NAND16    ",  "16- Input NAND Gate with Non-Inverted Inputs.", [Macro; Macro; Macro; Macro; No; No; No]; 
    "NOR2      ",  "2- Input NOR Gate with Inverted and Non-Inverted Inputs.", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "NOR2B1    ",  "2- Input NOR Gate with Inverted and Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "NOR2B2    ",  "2- Input NOR Gate with Inverted and Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "NOR3      ",  "3- Input NOR Gate with Inverted and Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "NOR3B1    ",  "3- Input NOR Gate with Inverted and Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "NOR3B2    ",  "3- Input NOR Gate with Inverted and Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "NOR3B3    ",  "3- Input NOR Gate with Inverted and Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "NOR4      ",  "4- Input NOR Gate with Inverted and Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "NOR4B1    ",  "4- Input NOR Gate with Inverted and Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "NOR4B2    ",  "4- Input NOR Gate with Inverted and Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "NOR4B3    ",  "4- Input NOR Gate with Inverted and Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "NOR4B4    ",  "4- Input NOR Gate with Inverted and Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "NOR5      ",  "5- Input NOR Gate with Inverted and Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "NOR5B1    ",  "5- Input NOR Gate with Inverted and Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "NOR5B2    ",  "5- Input NOR Gate with Inverted and Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "NOR5B3    ",  "5- Input NOR Gate with Inverted and Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "NOR5B4    ",  "5- Input NOR Gate with Inverted and Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "NOR5B5    ",  "5- Input NOR Gate with Inverted and Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "NOR6      ",  "6- Input NOR Gate with Inverted and Non-Inverted Inputs", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "NOR7      ",  "7- Input NOR Gate with Inverted and Non-Inverted Inputs", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "NOR8      ",  "8- Input NOR Gate with Inverted and Non-Inverted Inputs", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "NOR9      ",  "9- Input NOR Gate with Inverted and Non-Inverted Inputs", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "NOR12     ",  "12-Input NOR Gate with Non-Inverted Inputs", [Macro; Macro; Macro; Macro; No; No; No];
    "NOR16     ",  "16-Input NOR Gate with Non-Inverted Inputs", [Macro; Macro; Macro; Macro; No; No; No];
    "OR2       ",  "2-Input OR Gate with Inverted and Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "OR2B1     ",  "2-Input OR Gate with Inverted and Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "OR2B2     ",  "2-Input OR Gate with Inverted and Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "OR3       ",  "3-Input OR Gate with Inverted and Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "OR3B1     ",  "3-Input OR Gate with Inverted and Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "OR3B2     ",  "3-Input OR Gate with Inverted and Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "OR3B3     ",  "3Input OR Gate with Inverted and Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "OR4       ",  "4-Input OR Gate with Inverted and Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "OR4B1     ",  "4-Input OR Gate with Inverted and Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "OR4B2     ",  "4-Input OR Gate with Inverted and Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "OR4B3     ",  "4-Input OR Gate with Inverted and Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "OR5B1     ",  "5-Input OR Gate with Inverted and Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "OR5B2     ",  "12-Input OR Gate with Inverted and Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "OR5B3     ",  "5-Input OR Gate with Inverted and Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "OR5B4     ",  "5-Input OR Gate with Inverted and Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "OR5B5     ",  "5-Input OR Gate with Inverted and Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "OR6       ",  "6-Input OR Gate with Inverted and Non-Inverted Inputs", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "OR7       ",  "6-Input OR Gate with Inverted and Non-Inverted Inputs", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "OR8       ",  "8-Input OR Gate with Inverted and Non-Inverted Inputs", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "OR9       ",  "9-Input OR Gate with Inverted and Non-Inverted Inputs", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "OR12      ",  "12-Input OR Gate with Inverted and Non-Inverted Inputs", [Macro; Macro; Macro; Macro; No; No; No];
    "OR16      ",  "16-Input OR Gate with Inverted and Non-Inverted Inputs", [Macro; Macro; Macro; Macro; No; No; No];
    "ORCY      ",  "OR with Carry Logic", [No; No; No; Primitive; No; No; No]; 
    "SOP3      ",  "Sum of Products", [Macro; Macro; Macro; Macro; No; No; No];
    "SOP3B1A   ",  "Sum of Products", [Macro; Macro; Macro; Macro; No; No; No];
    "SOP3B1B   ",  "Sum of Products", [Macro; Macro; Macro; Macro; No; No; No];
    "SOP3B2A   ",  "Sum of Products", [Macro; Macro; Macro; Macro; No; No; No];
    "SOP3B2B   ",  "Sum of Products", [Macro; Macro; Macro; Macro; No; No; No];
    "SOP3B3    ",  "Sum of Products", [Macro; Macro; Macro; Macro; No; No; No];
    "SOP4      ",  "Sum of Products", [Macro; Macro; Macro; Macro; No; No; No];
    "SOP4B3    ",  "Sum of Products", [Macro; Macro; Macro; Macro; No; No; No];
    "SOP4B4    ",  "Sum of Products", [Macro; Macro; Macro; Macro; No; No; No];
    "SOP4B1    ",  "Sum of Products", [Macro; Macro; Macro; Macro; No; No; No];
    "SOP4B2A   ",  "Sum of Products", [Macro; Macro; Macro; Macro; No; No; No];
    "SOP4B2B   ",  "Sum of Products", [Macro; Macro; Macro; Macro; No; No; No];
    "XNOR2     ",  "2-Input XNOR Gate with Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "XNOR3     ",  "3-Input XNOR Gate with Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "XNOR4     ",  "4-Input XNOR Gate with Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "XNOR5     ",  "5-Input XNOR Gate with Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "XNOR6     ",  "6-Input XNOR Gate with Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "XNOR7     ",  "7-Input XNOR Gate with Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "XNOR8     ",  "8-Input XNOR Gate with Non-Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "XNOR9     ",  "9-Input XNOR Gate with Non-Inverted Inputs", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "XOR2      ",  "2-Input XOR Gate with Non- Inverted Inputs", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "XOR3      ",  "3-Input XOR Gate with Non- Inverted Inputs", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "XOR4      ",  "4-Input XOR Gate with Non- Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "XOR5      ",  "5-Input XOR Gate with Non- Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "XOR6      ",  "6-Input XOR Gate with Non- Inverted Inputs", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "XOR7      ",  "7-Input XOR Gate with Non- Inverted Inputs", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "XOR8      ",  "8-Input XOR Gate with Non- Inverted Inputs", [Primitive; Primitive; Primitive; Primitive; Primitive; Primitive; Primitive ];
    "XOR9      ",  "9-Input XOR Gate with Non- Inverted Inputs", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "XORCY     ",  "XOR for Carry Logic with General Output", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "XORCY_D   ",  "XOR for Carry Logic with Dual Output", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "XORCY_L   ",  "XOR for Carry Logic with Local Output", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
  ];

  "map elements", [
    "FMAP", "F Function Generator Partitioning Control Symbol", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
  ];

  "memory", [
    "RAM16X1D     ",   "16-Deep by 1-Wide Static Dual Port Synchronous RAM", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "RAM16X1D_1   ",   "16-Deep by 1-Wide Static Dual Port Synchronous RAM with Negative-Edge Clock", [Primitive; Primitive; Primitive; Primitive; No; No; No ]; 
    "RAM16X1S     ",   "16-Deep by 1-Wide Static Synchronous RAM", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "RAM16X1S_1   ",   "16-Deep by 1-Wide Static Synchronous RAM with Negative-Edge Clock", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "RAM16X2D     ",   "16-Deep by 2-Wide Static Dual Port Synchronous RAM", [Macro; No; Macro; Macro; No; No; No ];
    "RAM16X2S     ",   "16-Deep by 2-Wide Static Synchronous RAM", [Macro; Primitive; Macro; Primitive; No; No; No ];
    "RAM16X4D     ",   "16-Deep by 4-Wide Static Dual Port Synchronous RAM", [Macro; No; Macro; Macro; No; No; No ];
    "RAM16X4S     ",   "16-Deep by 4-Wide Static Synchronous RAM", [Macro; Primitive; Macro; Primitive; No; No; No ];
    "RAM16X8D     ",   "16-Deep by 8-Wide Static Dual Port Synchronous RAM", [Macro; No; Macro; Macro; No; No; No ];
    "RAM16X8S     ",   "16-Deep by 8-Wide Static Synchronous RAM", [Macro; No; Macro; Primitive; No; No; No];
    "RAM32X1D     ",   "32-Deep by 1-Wide Static Dual Static Port Synchronous RAM", [No; No; No; Primitive; No; No; No];
    "RAM32X1D_1   ",   "32-Deep by 1-Wide Static Dual Port Synchronous RAM with Negative-Edge Clock", [No; No; No; Primitive; No; No; No];
    "RAM32X1S     ",   "32-Deep by 1-Wide Static Synchronous RAM", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "RAM32X1S_1   ",   "32-Deep by 1-Wide Static Synchronous RAM with Negative-Edge Clock", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "RAM32X2S     ",   "32-Deep by 2-Wide Static Synchronous RAM", [Macro; Primitive; Macro; Primitive; No; No; No ];
    "RAM32X4S     ",   "32-Deep by 4-Wide Static Synchronous RAM", [Macro; No; Macro; Primitive; No; No; No];
    "RAM32X8S     ",   "32-Deep by 8-Wide Static Synchronous RAM", [Macro; No; Macro; Primitive; No; No; No];
    "RAM64X1D     ",   "64-Deep by 1-Wide Dual Port Static Synchronous RAM", [No; No; No; Primitive; No; No; No];
    "RAM64X1D_1   ",   "64-Deep by 1-Wide Dual Port Static Synchronous RAM with Negative-Edge Clock", [No; No; No; Primitive; No; No; No];
    "RAM64X1S     ",   "64-Deep by 1-Wide Static Synchronous RAM", [No; Primitive; No; Primitive; No; No; No];
    "RAM64X1S_1   ",   "64-Deep by 1-Wide Static Synchronous RAM with Negative-Edge Clock", [No; Primitive; No; Primitive; No; No; No];
    "RAM64X2S     ",   "64-Deep by 2-Wide Static Synchronous RAM", [No; No; No; Primitive; No; No; No];
    "RAM128X1S    ",   "128-Deep by 1-Wide Static Synchronous RAM", [No; No; No; Primitive; No; No; No];
    "RAM128X1S_1  ",   "128-Deep by 1-Wide Static Synchronous RAM with Negative-Edge Clock", [No; No; No; Primitive; No; No; No];
    "RAMB4_Sm_Sn  ",   "4096-Bit Dual-Port Synchronous Block RAM with Port Width (m or n) Configured to 1, 2, 4, 8, or 16 Bits", [Primitive; No; Primitive; No; No; No; No];
    "RAMB4_Sn     ",   "4096-Bit Single-Port Synchronous Block RAM with Port Width (n) Configured to 1, 2, 4, 8, or 16 Bits", [Primitive; No; Primitive; No; No; No; No];
    "RAMB16_Sm_Sn ",   "6384-Bit Data Memory and 2048-Bit Parity Memory, Dual- Port Synchronous Block RAM with Port Width (m or n) Configured to 1, 2, 4, 9, 18, or 36 Bits", [No; Primitive; No; Primitive; No; No; No];
    "RAMB16_Sn    ",   "16384-Bit Data Memory and 2048-Bit Parity Memory, Single-Port Synchronous Block RAM with Port Width (n) Configured to 1, 2, 4, 9, 18, or 36 Bits", [No; Primitive; No; Primitive; No; No; No];
    "ROC          ",   "Reset On Configuration", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "ROCBUF       ",   "Reset On Configuration Buffer", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "ROM16X1      ",   "16-Deep by 1-Wide ROM", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "ROM32X1      ",   "32-Deep by 1-Wide ROM", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "ROM64X1      ",   "64-Deep by 1-Wide ROM", [No; Primitive; No; Primitive; No; No; No];
    "ROM128X1     ",   "128-Deep by 1-Wide ROM", [No; Primitive; No; Primitive; No; No; No];
    "ROM256X1     ",   "256-Deep by 1-Wide ROM", [No; Primitive; No; Primitive; No; No; No];
  ];

  "muxes", [
    "M2_1    ",    "2-to-1 Multiplexer", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "M2_1B1  ",    "2-to-1 Multiplexer with D0 Inverted", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "M2_1B2  ",    "2-to-1 Multiplexer with D0 and D1 Inverted", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "M2_1E   ",    "2-to-1 Multiplexer with Enable", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "M4_1E   ",    "4-to-1 Multiplexer with Enable", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "M8_1E   ",    "8-to-1 Multiplexer with Enable", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "M16_1E  ",    "16-to-1 Multiplexer with Enable", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "MUXCY   ",    "2-to-1 Multiplexer for Carry Logic with General Output", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "MUXCY_D ",    "2-to-1 Multiplexer for Carry Logic with Dual Output", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "MUXCY_L ",    "2-to-1 Multiplexer for Carry Logic with Local Output", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "MUXF5   ",    "2-to-1 Lookup Table Multiplexer with General Output", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "MUXF5_D ",    "2-to-1 Lookup Table Multiplexer with Dual Output", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "MUXF5_L ",    "2-to-1 Lookup Table Multiplexer with Local Output", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "MUXF6   ",    "2-to-1 Lookup Table Multiplexer with General Output", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "MUXF6_D ",    "2-to-1 Lookup Table Multiplexer with Dual Output", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "MUXF6_L ",    "2-to-1 Lookup Table Multiplexer with Local Output", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "MUXF7   ",    "2-to-1 Lookup Table Multiplexer with General Output", [No; Primitive; No; Primitive; No; No; No];
    "MUXF7_D ",    "2-to-1 Lookup Table Multiplexer with Dual Output", [No; Primitive; No; Primitive; No; No; No];
    "MUXF7_L ",    "2-to-1 Lookup Table Multiplexer with Local Output", [No; Primitive; No; Primitive; No; No; No];
    "MUXF8   ",    "2-to-1 Lookup Table Multiplexer with General Output", [No; Primitive; No; Primitive; No; No; No];
    "MUXF8_D ",    "2-to-1 Lookup Table Multiplexer with Dual Output", [No; Primitive; No; Primitive; No; No; No];
    "MUXF8_L ",    "2-to-1 Lookup Table Multiplexer with Local Output", [No; Primitive; No; Primitive; No; No; No];
  ];

  "shifters", [
    "BRLSHFT4",    "4-Bit Barrel Shifter", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "BRLSHFT8",    "8-Bit Barrel Shifter", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
  ];

  "shift registers", [
    "SR4CE     ",  "4-Bit Serial-In Parallel-Out Shift Register with Clock Enable and Asynchronous Clear", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "SR8CE     ",  "8-Bit Serial-In Parallel-Out Shift Register with Clock Enable and Asynchronous Clear", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "SR16CE    ",  "16-Bit Serial-In Parallel-Out Shift Register with Clock Enable and Asynchronous Clear", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "SR4CLE    ",  "4-Bit Loadable Serial/Parallel-In Parallel-Out Shift Register with Clock Enable and Asynchronous Clear", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "SR8CLE    ",  "8-Bit Loadable Serial/Parallel-In Parallel-Out Shift Register with Clock Enable and Asynchronous Clear", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "SR16CLE   ",  "16-Bit Loadable Serial/Parallel-In Parallel-Out Shift Register with Clock Enable and Asynchronous Clear", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "SR4CLED   ",  "4-Bit Shift Register with Clock Enable and Asynchronous Clear", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "SR8CLED   ",  "8-Bit Shift Register with Clock Enable and Asynchronous Clear", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "SR16CLED  ",  "16-Bit Shift Register with Clock Enable and Asynchronous Clear", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "SR4RE     ",  "4-Bit Serial-In Parallel-Out Shift Register with Clock Enable and Synchronous Reset", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "SR8RE     ",  "8-Bit Serial-In Parallel-Out Shift Register with Clock Enable and Synchronous Reset", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "SR16RE    ",  "16-Bit Serial-In Parallel-Out Shift Register with Clock Enable and Synchronous Reset", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "SR4RLE    ",  "4-Bit Loadable Serial/Parallel-In Parallel-Out Shift Register with Clock Enable and Synchronous Reset", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "SR8RLE    ",  "8-Bit Loadable Serial/Parallel-In Parallel-Out Shift Register with Clock Enable and Synchronous Reset", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "SR16RLE   ",  "16-Bit Loadable Serial/Parallel-In Parallel-Out Shift Register with Clock Enable and Synchronous Reset", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "SR4RLED   ",  "4-Bit Shift Register with Clock Enable and Synchronous Reset", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ]; 
    "SR8RLED   ",  "8-Bit Shift Register with Clock Enable and Synchronous Reset", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "SR16RLED  ",  "16-Bit Shift Register with Clock Enable and Synchronous Reset", [Macro; Macro; Macro; Macro; Primitive; Primitive; Primitive ];
    "SRD4CE    ",  "4-Bit Serial-In Parallel-Out Dual Edge Triggered Shift Register with Clock Enable and Asynchronous Clear", [No; No; No; No; No; No; Primitive];
    "SRD8CE    ",  "8-Bit Serial-In Parallel-Out Dual Edge Triggered Shift Register with Clock Enable and Asynchronous Clear", [No; No; No; No; No; No; Primitive];
    "SRD16CE   ",  "16-Bit Serial-In Parallel-Out Dual Edge Triggered Shift Register with Clock Enable and Asynchronous Clear", [No; No; No; No; No; No; Primitive];
    "SRD4CLE   ",  "4-Bit Loadable Serial/Parallel-In Parallel-Out Dual Edge Triggered Shift Register with Clock Enable and Asynchronous Clear", [No; No; No; No; No; No; Primitive];
    "SRD8CLE   ",  "8-Bit Loadable Serial/Parallel-In Parallel-Out Dual Edge Triggered Shift Register with Clock Enable and Asynchronous Clear", [No; No; No; No; No; No; Primitive];
    "SRD16CLE  ",  "16-Bit Loadable Serial/Parallel-In Parallel-Out Dual Edge Triggered Shift Register with Clock Enable and Asynchronous Clear", [No; No; No; No; No; No; Primitive];
    "SRD4CLED  ",  "4-Bit Dual Edge Triggered Shift Register with Clock Enable and Asynchronous Clear", [No; No; No; No; No; No; Primitive];
    "SRD8CLED  ",  "8-Bit Dual Edge Triggered Shift Register with Clock Enable and Asynchronous Clear", [No; No; No; No; No; No; Primitive];
    "SRD16CLED ",  "16-Bit Dual Edge Triggered Shift Register with Clock Enable and Asynchronous Clear", [No; No; No; No; No; No; Primitive];
    "SRD4RE    ",  "4-Bit Serial-In Parallel-Out Dual Edge Triggered Shift Register with Clock Enable and Synchronous Reset", [No; No; No; No; No; No; Primitive];
    "SRD8RE    ",  "8-Bit Serial-In Parallel-Out Dual Edge Triggered Shift Register with Clock Enable and Synchronous Reset", [No; No; No; No; No; No; Primitive];
    "SRD16RE   ",  "16-Bit Serial-In Parallel-Out Dual Edge Triggered Shift Register with Clock Enable and Synchronous Reset", [No; No; No; No; No; No; Primitive];
    "SRD4RLE   ",  "4-Bit Loadable Serial/Parallel-In Parallel-Out Dual Edge Triggered Shift Register with Clock Enable and Synchronous Reset", [No; No; No; No; No; No; Primitive];
    "SRD8RLE   ",  "8-Bit Loadable Serial/Parallel-In Parallel-Out Dual Edge Triggered Shift Register with Clock Enable and Synchronous Reset", [No; No; No; No; No; No; Primitive];
    "SRD16RLE  ",  "16-Bit Loadable Serial/Parallel-In Parallel-Out Dual Edge Triggered Shift Register with Clock Enable and Synchronous Reset", [No; No; No; No; No; No; Primitive];
    "SRD4RLED  ",  "4-Bit Dual Edge Triggered Shift Register with Clock Enable and Synchronous Reset", [No; No; No; No; No; No; Primitive];
    "SRD8RLED  ",  "8-Bit Dual Edge Triggered Shift Register with Clock Enable and Synchronous Reset", [No; No; No; No; No; No; Primitive];
    "SRD16RLED ",  "16-Bit Dual Edge Triggered Shift Register with Clock Enable and Synchronous Reset", [No; No; No; No; No; No; Primitive];
    "SRL16     ",  "16-Bit Shift Register Look-Up Table (LUT)", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "SRL16_1   ",  "16-Bit Shift Register Look-Up Table (LUT) with Negative-Edge Clock", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "SRL16E    ",  "16-Bit Shift Register Look-Up Table (LUT) with Clock Enable", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "SRL16E_1  ",  "16-Bit Shift Register Look-Up Table (LUT) with Negative-Edge Clock and Clock Enable", [Primitive; Primitive; Primitive; Primitive; No; No; No ];
    "SRLC16    ",  "16-Bit Shift Register Look-Up Table (LUT) with Carry", [No; Primitive; No; Primitive; No; No; No];
    "SRLC16_1  ",  "16-Bit Shift Register Look-Up Table (LUT) with Carry and Negative-Edge Clock", [No; Primitive; No; Primitive; No; No; No];
    "SRLC16E   ",  "16-Bit Shift Register Look-Up Table (LUT) with Carry and Clock Enable", [No; Primitive; No; Primitive; No; No; No];
    "SRLC16E_1 ",  "16-Bit Shift Register Look-Up Table (LUT) with Carry, Negative- Edge Clock, and Clock Enable", [No; Primitive; No; Primitive; No; No; No];
  ];
]

(*********************************************************************************************)
(*********************************************************************************************)

let strip_white_space s = 
  let rec to_chars s = 
    match s with
    | "" -> []
    | _ ->
      let hd = String.get s 0 in
      let tl = String.sub s 1 ((String.length s)-1) in
      hd :: (to_chars tl) in
  let chars = to_chars s in
  let rec strip c = 
    match c with
    | [] -> ""
    | ' ' :: tl -> strip tl
    | hd :: tl -> (String.of_char hd) ^ (strip tl) in
  strip (to_chars s)

let format_elements (name,elements) = (name, map (fun (n,e) -> strip_white_space n, e) elements)
let format_part part = map format_elements part

let classify i = 
  let p = map (fun (cl, l) -> (cl, 
    map (fun (e,d,c) -> (e,d)) (filter (fun (e,d,c) -> (List.nth c i) <> No) l)
  )) virtex_virtex2_spartan_cpld_p in
  filter (fun (n, l) -> l <> []) p

let fold_strings inner = fold_left (fun x a -> if x = "" then a else x ^ inner ^ a) "" 

let print_part part_name part = 
  let os = output_string stdout in
  os ("part: " ^ part_name ^ "\n");
  iter (fun (cat, els) ->
    os (" " ^ cat ^ ": ");
    os (fold_strings ", " (map (fun (n,d) -> n) els));
    os ("\n");
  ) part

(*
  Spartan-II,IIE 
  Spartan-3 
  Virtex, E 
  Virtex II, Pro, Pro X 
  XC9500/XV/XL 
  CR XPLA3 
  CR-II
*)
let xc9500      = format_part (classify 4)
let coolrunner  = format_part (classify 5)
let coolrunner2 = format_part (classify 6)
let spartan2    = format_part (classify 0)
let spartan3    = format_part (classify 1)
let virtex      = format_part (classify 2)
let virtex2     = format_part (classify 3)
let virtex4     = format_part virtex4_p 
let virtex5     = format_part virtex5_p 

let _ = print_part "xc9500" xc9500
let _ = print_part "coolrunner" coolrunner
let _ = print_part "coolrunner2" coolrunner2
let _ = print_part "spartan2" spartan2
let _ = print_part "spartan3" spartan3
let _ = print_part "virtex" virtex
let _ = print_part "virtex2" virtex2
let _ = print_part "virtex4" virtex4
let _ = print_part "virtex5" virtex5

