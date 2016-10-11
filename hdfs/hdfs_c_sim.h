/*
  HDFS Digital Logic Hardware Design 
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
*/

#ifndef _C_SIM_H
#define _C_SIM_H

#include <stdlib.h>
#include <string.h>

typedef unsigned int HDFS_Int;
typedef unsigned long long HDFS_Int64;

typedef enum _HDFS_PortType {
    Input, Output, Wire
} HDFS_PortType;

typedef struct _HDFS_Port {
    char *name;
    HDFS_Int data;
    HDFS_Int bits;
    HDFS_PortType type;
} HDFS_Port;

typedef struct _HDFS_Simulator {
    HDFS_Int *mem;
    HDFS_Int mem_size;
    HDFS_Port *ports;
    HDFS_Int num_ports;
} HDFS_Simulator;

/* 

API exported by each simulator object 

// Create a simulation object 
extern HDFS_Simulator*      HDFS_Create_<name>  ();

// Deallocate a simulation object 
extern void                 HDFS_Destroy_<name> ();

// Reset the simulation object
extern void                 HDFS_Reset_<name>   (HDFS_Simulator *sim);

// Find a named port
extern HDFS_Port *          HDFS_Port_<name>    (HDFS_Simulator *sim, char *name);

// Cycle the simulation
extern void                 HDFS_Cycle_<name>   (HDFS_Simulator *sim);
                               
*/

#endif// _C_SIM_H
