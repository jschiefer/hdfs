/*
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
*/
#include <stdio.h>

#include <hdfs_c_sim.h>

extern HDFS_Simulator*      HDFS_Create_test_csim  (); 
extern void                 HDFS_Destroy_test_csim (); 
extern void                 HDFS_Reset_test_csim   (HDFS_Simulator *sim); 
extern HDFS_Port *          HDFS_Port_test_csim    (HDFS_Simulator *sim, char *name); 
extern void                 HDFS_Cycle_test_csim   (HDFS_Simulator *sim);

int words(int bits) { return (bits+31)/32; }

void printhex(HDFS_Int *mem, HDFS_Port * port) {
    int w = words(port->bits);
    int i;
    for (i=w-1; i>=0; i--) {
        printf("%.8x", mem[port->data+i]);
    }
    printf("\n");
}

void printbin(HDFS_Int *mem, HDFS_Port * port) {
    int i;
    for (i=port->bits-1; i>=0; i--) {
        printf("%i", mem[port->data + (i/32)] & (1 << (i%32)) ? 1 : 0);
    }
    printf("\n");
}

int main(void) {
    int i;
    HDFS_Simulator *sim = HDFS_Create_test_csim();    
    HDFS_Port *out;
    HDFS_Port *ports = sim->ports;

    // Select a named port
    out = HDFS_Port_test_csim(sim, "o16");
    
    // Enumerate ports
    for (i=0; i<sim->num_ports; i++) {
        printf("%s: %i\n", ports[i].name, ports[i].bits);
    }
    
    // Sim reset
    HDFS_Reset_test_csim (sim);
    
    // Sim cycle
    HDFS_Cycle_test_csim (sim);

    printhex(sim->mem, out);
    printbin(sim->mem, out);
}
