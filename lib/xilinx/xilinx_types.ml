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

type name_t = string
type type_t = 
  | T_std_logic
  | T_std_logic_vector of int * int
  | T_real
  | T_integer
  | T_boolean
  | T_string
  | T_bit
  | T_bit_vector of int * int
  | T_time
type default_t = string
type port_dir_t = In | Inout | Out
type port_t = Port of name_t * port_dir_t * type_t * default_t option 
type xilinx_comp_t = Component of name_t * port_t list * port_t list

