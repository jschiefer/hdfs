/*
  HDFS Digital Logic Hardware Design Utility Library (modelsim cosimulation module)
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

#pragma warning (disable : 4996)

#include "acc_user.h"
#include "vpi_user.h"
#include "veriuser.h"
#include <windows.h>

//////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////

HANDLE hPipe;
int debug = 0;      // By default no debug info
int stop = 1;       // By default stop on error / end of sim

void finish() {
    if (stop) 
        vpi_control(vpiFinish, 1);     // Is there a pli equivalent?  Does it matter?
}

void connect_pipe() {
    hPipe = CreateFile("\\\\.\\pipe\\hdfs_cosim", GENERIC_READ | GENERIC_WRITE, 0, NULL, OPEN_EXISTING, 0, NULL);
    if (hPipe == INVALID_HANDLE_VALUE) {
        DWORD err = GetLastError();
        if (debug) io_printf("pli: Failed to connect to pipe (errcode %i)\n", err);
        // wanna crash out here
        finish();
    } else {
        if (debug) io_printf("pli: Connected to simulation pipe\n");
    }
}

void close_pipe() {
    CloseHandle(hPipe);
}

void read(char *buf, int size) {
    DWORD cbRead;
    BOOL fOk;
    fOk = ReadFile(hPipe, buf, size, &cbRead, NULL);
    if (!fOk) {
        if (debug) {
            if (GetLastError() != ERROR_MORE_DATA) io_printf("pli: Read failed\n");
            else io_printf("pli: Read failed because more data was available than the buffer size\n");
        }
        finish();
    }
}

void write(char *buf, int size) {
    BOOL fOk;
    DWORD cbWritten;
    fOk = WriteFile(hPipe, buf, size, &cbWritten, NULL);
    if (!fOk) {
        if (debug) io_printf("pli: Write failed\n"); 
        finish();
    }
}

//////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////

int get_int(handle h) {
    char *s = acc_fetch_value(h, "%d", 0);
    return atoi(s);
}

char *get_name(handle h) {
    char *s = acc_fetch_name(h);
    return s;
}

//////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////

typedef struct _Monitor {
    int numData;
    int prevClock;
    handle hClock;
    handle *hData;
    HANDLE hPipe;
    char *writeBuf;
    int *dataBytes;
    int writeBytes;
} Monitor;

PLI_INT32 hdfs_monitor_misc(int data, int reason, int paramvc) {
    Monitor *monitor = (Monitor *) tf_igetworkarea(tf_getinstance());
    int clock, i, j;
    switch (reason) {
    case reason_paramvc:
    case reason_paramdrc:
        tf_synchronize( );
        break;
    case reason_synch:
        clock = get_int(monitor->hClock);
        if (!clock && monitor->prevClock) {         // Monitor values on the falling edge
            char *ptr = monitor->writeBuf;
            for (i=0; i<monitor->numData; i++) {
                char *val = acc_fetch_value(monitor->hData[i], "%x", 0);   // Are we leaking memory here???..
                for (j=0; j<monitor->dataBytes[i]; j++) ptr[j] = val[j];
                ptr += monitor->dataBytes[i];
                if (debug) io_printf("pli: %s = %s\n", get_name(monitor->hData[i]), val);
            }
            if (debug) io_printf("pli: All data = %s\n", monitor->writeBuf);
            write(monitor->writeBuf, monitor->writeBytes);
        }
        monitor->prevClock = clock;
        break;
    case reason_finish:
        if (debug) io_printf("pli: Simulation finished.  Closing pipe\n");
        close_pipe();
        break;
    }
    return 0;
}

PLI_INT32 hdfs_monitor_init(int data, int reason) {
    Monitor *monitor = (Monitor *) malloc (sizeof(Monitor));
    int i;
    
    monitor->numData = get_int(acc_handle_tfarg(1));
    
    monitor->hClock = acc_handle_tfarg(2);
    if (monitor->numData != 0) {
        int totalBytes = 0;
        monitor->hData = malloc(sizeof(handle) * monitor->numData);
        monitor->dataBytes = malloc(sizeof(int) * monitor->numData);
        for (i=0; i<monitor->numData; i++) {
            monitor->hData[i] = acc_handle_tfarg(i+3);
            monitor->dataBytes[i] = (acc_fetch_size(monitor->hData[i]) + 3) >> 2;
            if (debug) io_printf("pli: monitor %s = %i bytes\n", get_name(monitor->hData[i]), monitor->dataBytes[i]);
            totalBytes += monitor->dataBytes[i];
        }
        monitor->writeBuf = malloc(totalBytes+1);
        monitor->writeBuf[totalBytes] = 0;  // 0 terminate string
        monitor->writeBytes = totalBytes;
    } else {
        monitor->hData = 0;
    }
    
    monitor->prevClock = 0;
    
    // Enable callback
    tf_asynchon();
    
    tf_setworkarea((char *) monitor);
    
    return 0;
}

//////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////

typedef struct _Drive {
    int numData;
    int prevClock;
    handle hClock;
    handle *hData;
    HANDLE hPipe;
    char *readBuf;
    int *dataBytes;
    int readBytes;
    char *hexBuf;
} Drive;

PLI_INT32 drive_callback(p_vc_record r) {
    Drive *drive = (Drive*) r->user_data;
    int clock = get_int(drive->hClock), i, j;
    
    if (clock && !drive->prevClock) {       // Drive values on the rising edge
        char *ptr = drive->readBuf;
        s_setval_delay delay_s;
        s_setval_value value_s;
        value_s.format    = accHexStrVal;
        delay_s.model     = accNoDelay; 
        delay_s.time.type = accTime;
        delay_s.time.low  = 0;
        delay_s.time.high = 0;
        
        read(drive->readBuf, drive->readBytes);
        if (drive->readBuf[0] == 'q') {
            if (debug) io_printf("pli: Quit received from server: %c\n", drive->readBuf[0]);
            finish();
        } else {
            if (debug) {
                io_printf("pli: Read - ");
                for (i=0; i<drive->readBytes; i++) io_printf("%c", drive->readBuf[i]);
                io_printf("\n");
            }
            for (i=0; i<drive->numData; i++) {
                for (j=0; j<drive->dataBytes[i]; j++) drive->hexBuf[j] = ptr[j];
                drive->hexBuf[j] = 0;   // 0 terminate string
                ptr += drive->dataBytes[i];
                if (debug) io_printf("pli: %s = %s\n", get_name(drive->hData[i]), drive->hexBuf);
                value_s.value.str = drive->hexBuf; 
                acc_set_value(drive->hData[i], &value_s, &delay_s);
            }
        }
    }
    drive->prevClock = clock;
    return 0;
}

PLI_INT32 hdfs_drive_signals() {
    int i;
    Drive *drive = (Drive *) malloc(sizeof(Drive));
    
    drive->numData = get_int(acc_handle_tfarg(1));
    
    drive->hClock = acc_handle_tfarg(2);        // what if there's no clock?
    if (drive->numData != 0) {
        int totalBytes = 0;
        int max = 0;
        drive->hData = malloc(sizeof(handle) * drive->numData);
        drive->dataBytes = malloc(sizeof(int) * drive->numData);
        for (i=0; i<drive->numData; i++) {
            drive->hData[i] = acc_handle_tfarg(i+3);
            drive->dataBytes[i] = (acc_fetch_size(drive->hData[i]) + 3) >> 2;
            if (debug) io_printf("pli: drive %s = %i bytes\n", get_name(drive->hData[i]), drive->dataBytes[i]);
            totalBytes += drive->dataBytes[i];
            if (max < drive->dataBytes[i]) max = drive->dataBytes[i];
        }
        drive->readBuf = malloc(totalBytes+1);
        drive->readBuf[totalBytes] = 0;  // 0 terminate string
        drive->readBytes = totalBytes;
        drive->hexBuf = malloc(max+1);
        
    } else {
        drive->hData = 0;
    }
    
    for (i=0; i<drive->numData; i++) tf_putp(i, 0);
    
    // Save a copy of the present clk value.
    drive->prevClock = 0;
    
    // Add callback when ever clock changes
    acc_vcl_add(acc_handle_tfarg(2), drive_callback, (char*)drive, vcl_verilog_logic);
    
    acc_close();
    return 0;
}

//////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////

s_tfcell veriusertfs[] = {
    {usertask, 0, 0, 0, hdfs_monitor_init, hdfs_monitor_misc, "$hdfs_monitor_signals"},
    {usertask, 0, 0, 0, hdfs_drive_signals, 0, "$hdfs_drive_signals"},
    {0}  // last entry must be 0 
};

void init_usertfs() {
    char *arg;
    p_tfcell usertf;
    connect_pipe();
    if ((arg = mc_scan_plusargs("debug=")) != NULL) debug = atoi(arg);
    if ((arg = mc_scan_plusargs("stop=")) != NULL) stop = atoi(arg);
    for (usertf = veriusertfs; usertf; usertf++) {
        if (usertf->type == 0) return;
        mti_RegisterUserTF(usertf);
    }
}
