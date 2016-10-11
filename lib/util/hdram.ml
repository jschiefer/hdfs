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

(** Synthesizable rams built with the Signal_mem primitive *)
(* Note: switched rbw/wbr as they were backwards *)
module DigitalLogic.Circuits.Ram

open DigitalLogic
open Signal

(** Asynchronous read, synchronous write *)
let ram_async size clock we wa d ra = 
  memory size clock we wa d ra

(** Single port ram.  1 address line for both read and write *)
let ram_sp size clock we a d re = 
  memory size clock we a d (reg clock empty empty re a)

(** Dual port ram.  2 address lines - one read one write. *)
(** Read before write behaviour - if read/write addresses are *)
(** the same and a write occurs the value read will be the old *)
(** one *)
let ram_rbw size clock we wa d re ra = 
  reg clock empty empty re (memory size clock we wa d ra)

(** Dual port ram.  2 address lines - one read one write. *)
(** Write before read behaviour - if read/write addresses are *)
(** the same and a write occurs the value read will be the new *)
(** one *)
let ram_wbr size clock we wa d re ra =
  memory size clock we wa d (reg clock empty empty re ra)
