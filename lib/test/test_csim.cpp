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

#include "../../output/test_csim.cpp"

int words(int bits) { return (bits+31)/32; }

void printhex(HDFS_Int *mem, HDFS_Port * port) {
    int w = words(port->bits);
    for (int i=w-1; i>=0; i--) {
        printf("%.8x", mem[port->data+i]);
    }
    printf("\n");
}

void printbin(HDFS_Int *mem, HDFS_Port * port) {
    for (int i=port->bits-1; i>=0; i--) {
        printf("%i", mem[port->data + (i/32)] & (1 << (i%32)) ? 1 : 0);
    }
    printf("\n");
}

int main(void) {
    HDFS_Simulator_test_csim sim;
    
    // Select a named port
    HDFS_Port *out = sim["o16"];    

    // Enumerate ports
    HDFS_Port *ports = sim.Ports();
    for (int i=0; i<sim.NumPorts(); i++) {
        printf("%s: %i\n", ports[i].name, ports[i].bits);
    }
    
    // Sim reset
    sim.Reset();
    
    // Sim cycle
    sim.Cycle();

    printhex(sim.sim->mem, out);
    printbin(sim.sim->mem, out);
}
