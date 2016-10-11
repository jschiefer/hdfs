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

//#include "acc_user.h"
#include "vpi_user.h"
//#include "veriuser.h"
#include <windows.h>

/*

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
        if (debug) vpi_printf("pli: Failed to connect to pipe (errcode %i)\n", err);
        // wanna crash out here
        finish();
    } else {
        if (debug) vpi_printf("pli: Connected to simulation pipe\n");
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
            if (GetLastError() != ERROR_MORE_DATA) vpi_printf("pli: Read failed\n");
            else vpi_printf("pli: Read failed because more data was available than the buffer size\n");
        }
        finish();
    }
}

void write(char *buf, int size) {
    BOOL fOk;
    DWORD cbWritten;
    fOk = WriteFile(hPipe, buf, size, &cbWritten, NULL);
    if (!fOk) {
        if (debug) vpi_printf("pli: Write failed\n"); 
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
                if (debug) vpi_printf("pli: %s = %s\n", get_name(monitor->hData[i]), val);
            }
            if (debug) vpi_printf("pli: All data = %s\n", monitor->writeBuf);
            write(monitor->writeBuf, monitor->writeBytes);
        }
        monitor->prevClock = clock;
        break;
    case reason_finish:
        if (debug) vpi_printf("pli: Simulation finished.  Closing pipe\n");
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
            if (debug) vpi_printf("pli: monitor %s = %i bytes\n", get_name(monitor->hData[i]), monitor->dataBytes[i]);
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
            if (debug) vpi_printf("pli: Quit received from server: %c\n", drive->readBuf[0]);
            finish();
        } else {
            if (debug) {
                vpi_printf("pli: Read - ");
                for (i=0; i<drive->readBytes; i++) vpi_printf("%c", drive->readBuf[i]);
                vpi_printf("\n");
            }
            for (i=0; i<drive->numData; i++) {
                for (j=0; j<drive->dataBytes[i]; j++) drive->hexBuf[j] = ptr[j];
                drive->hexBuf[j] = 0;   // 0 terminate string
                ptr += drive->dataBytes[i];
                if (debug) vpi_printf("pli: %s = %s\n", get_name(drive->hData[i]), drive->hexBuf);
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
            if (debug) vpi_printf("pli: drive %s = %i bytes\n", get_name(drive->hData[i]), drive->dataBytes[i]);
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
*/

HANDLE hPipe;
int debug = 2;      // By default no debug info
int stop = 0;       // By default stop on error / end of sim

/* These used to be allocated as user data in the PLI, but I cant be bothered to work out how to do it for VPI.  As it stands, it doesn't matter anyway. */

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

void finish() {
    if (stop) {
        MessageBox(NULL, "Killing simulator", "", MB_OK);
        vpi_control(vpiFinish, 1);     // Is there a pli equivalent?  Does it matter?
    }
}

void connect_pipe() {
    hPipe = CreateFile("\\\\.\\pipe\\hdfs_cosim", GENERIC_READ | GENERIC_WRITE, 0, NULL, OPEN_EXISTING, 0, NULL);
    if (hPipe == INVALID_HANDLE_VALUE) {
        //MessageBox(NULL, "Pipe not connected", "", MB_OK);
        DWORD err = GetLastError();
        if (debug) vpi_printf("pli: Failed to connect to pipe (errcode %i)\n", err);
        // wanna crash out here
        finish();
    } else {
        //MessageBox(NULL, "Pipe connected", "", MB_OK);
        if (debug) vpi_printf("pli: Connected to simulation pipe\n");
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
            if (GetLastError() != ERROR_MORE_DATA) vpi_printf("pli: Read failed\n");
            else vpi_printf("pli: Read failed because more data was available than the buffer size\n");
        }
        finish();
    }
}

void write(char *buf, int size) {
    BOOL fOk;
    DWORD cbWritten;
    fOk = WriteFile(hPipe, buf, size, &cbWritten, NULL);
    if (!fOk) {
        if (debug) vpi_printf("pli: Write failed\n"); 
        finish();
    }
}

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

typedef void (*iterfn)(int pos, vpiHandle net, void *arg);
void iter_args(vpiHandle hTf, iterfn fn, void *arg) {
    vpiHandle hIter, hNet;
    int pos = 0;
    hIter = vpi_iterate(vpiArgument, hTf);
    while ((hNet = vpi_scan(hIter)) != NULL) {
        fn(pos, hNet, arg);
        pos++;
    }
}

void print_net_val(int pos, vpiHandle net, void *arg) {
    s_vpi_value val;
    val.format = vpiHexStrVal;
    vpi_get_value(net, &val);
    vpi_printf(" name=%s, size=%d, val=%s\n", 
        vpi_get_str(vpiName, net), 
        vpi_get(vpiSize, net), 
        val.value.str); 
}

void print_net(int pos, vpiHandle net, void *arg) {
    vpi_printf(" name=%s, size=%d\n", 
        vpi_get_str(vpiName, net), 
        vpi_get(vpiSize, net)); 
}

void count_args(int pos, vpiHandle net, void *arg) {
    int *tot = (int *) arg;
    *tot = pos + 1;
}

void total_arg_bytes(int pos, vpiHandle net, void *arg) {
    int *tot = (int *) arg;
    int x = (vpi_get(vpiSize, net) + 7) / 8;
    *tot += x;
}

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

typedef struct _Monitor {
    int num;
    vpiHandle *hData;
    HANDLE hPipe;
    char *writeBuf;
    int *dataBytes;
    int *dataPos;
    int writeBytes;
} Monitor;
Monitor monitor = {0};

void monitor_init_size(int pos, vpiHandle net, void *arg) {
    Monitor *m = (Monitor *) arg;
    int bytes = (vpi_get(vpiSize, net) + 3) / 4;
    m->num = pos + 1;
    m->writeBytes += bytes;
}

void monitor_init(int pos, vpiHandle net, void *arg) {
    Monitor *m = (Monitor *) arg;
    m->hData[pos] = net;
    m->dataBytes[pos] = (vpi_get(vpiSize, net) + 3) / 4;
}

PLI_INT32 hdfs_monitor_signals_init(char *user_data) {
    int i;
    vpiHandle hTf = vpi_handle(vpiSysTfCall, NULL);
    if (debug) vpi_printf("Signals to monitor:\n");
    if (debug) iter_args(hTf, print_net, NULL);
    // get sizes of arrays
    iter_args(hTf, monitor_init_size, &monitor);
    // alloc arrays
    monitor.hData = malloc(monitor.num * sizeof(vpiHandle));
    monitor.writeBuf = malloc(monitor.writeBytes);
    monitor.dataBytes = malloc(monitor.num * sizeof(int));
    monitor.dataPos = malloc(monitor.num * sizeof(int));
    // fill arrays    
    iter_args(hTf, monitor_init, &monitor);
    monitor.dataPos[0] = 0;
    for (i=1; i<monitor.num; i++) monitor.dataPos[i] = monitor.dataPos[i-1] + monitor.dataBytes[i-1];
    if (debug) for (i=0; i<monitor.num; i++) vpi_printf(" pos = %d, bytes = %d\n", monitor.dataPos[i], monitor.dataBytes[i]);
    if (debug) vpi_printf(" tot args = %d, tot bytes = %d\n", monitor.num, monitor.writeBytes);
    return 0;
}

// Iterate through the signal and set their hex value into the string.
void monitor_set(int pos, vpiHandle net, void *arg) {
    Monitor *m = (Monitor *) arg;
    int j;
    s_vpi_value val;
    val.format = vpiHexStrVal;
    vpi_get_value(net, &val);
    if (debug) vpi_printf(" name=%s, size=%d, val=%s (pos=%d)\n", vpi_get_str(vpiName, net), vpi_get(vpiSize, net), val.value.str, m->dataPos[pos]); 
    for (j=0; j<m->dataBytes[pos]; j++) {
        if (debug > 1) vpi_printf(" put %c @ %d\n", val.value.str[j], m->dataPos[pos] + j); 
        m->writeBuf[m->dataPos[pos] + j] = val.value.str[j];
    }
}

PLI_INT32 hdfs_monitor_signals(char *user_data) {
    vpiHandle hTf = vpi_handle(vpiSysTfCall, NULL);    
    if (debug) vpi_printf("monitor:\n");
    iter_args(hTf, monitor_set, &monitor);
    write(monitor.writeBuf, monitor.writeBytes);
    //write(monitor.writeBuf, monitor.writeBytes);
    return 0;
}

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

typedef struct _Drive {
    int num;
    vpiHandle *hData;
    HANDLE hPipe;
    char *readBuf;
    int *dataBytes;
    int *dataPos;
    int readBytes;
    char *hexBuf;
} Drive;
Drive drive = {0};

void drive_init_size(int pos, vpiHandle net, void *arg) {
    Drive *d = (Drive*) arg;
    int bytes = (vpi_get(vpiSize, net) + 3) / 4;
    d->num = pos + 1;
    d->readBytes += bytes;
}

void drive_init(int pos, vpiHandle net, void *arg) {
    Drive *d = (Drive *) arg;
    d->hData[pos] = net;
    d->dataBytes[pos] = (vpi_get(vpiSize, net) + 3) / 4;
}

PLI_INT32 hdfs_drive_signals_init(char *user_data) {        
    int i, max=0;
    vpiHandle hTf = vpi_handle(vpiSysTfCall, NULL);
    if (debug) vpi_printf("Signals to drive:\n");
    if (debug) iter_args(hTf, print_net, NULL);
    // get sizes of arrays
    iter_args(hTf, drive_init_size, &drive);
    // alloc arrays
    drive.hData = malloc(drive.num * sizeof(vpiHandle));
    drive.readBuf = malloc(drive.readBytes);
    //for (i=0; i<drive.readBytes; i++) drive.readBuf[i] = '0'+i; // XXX
    drive.dataBytes = malloc(drive.num * sizeof(int));
    drive.dataPos = malloc(drive.num * sizeof(int));
    // fill arrays    
    iter_args(hTf, drive_init, &drive);
    drive.dataPos[0] = 0;
    for (i=1; i<drive.num; i++) drive.dataPos[i] = drive.dataPos[i-1] + drive.dataBytes[i-1];
    for (i=0; i<drive.num; i++) max = drive.dataBytes[i] > max ? drive.dataBytes[i] : max;
    drive.hexBuf = malloc(max+1);
    if (debug) for (i=0; i<drive.num; i++) vpi_printf(" pos = %d, bytes = %d\n", drive.dataPos[i], drive.dataBytes[i]);
    if (debug) vpi_printf(" tot args = %d, tot bytes = %d, max = %d\n", drive.num, drive.readBytes, max);
    return 0;
}

// Iterate through the signals and put their hex value into the simulator.
void drive_get(int pos, vpiHandle net, void *arg) {
    Drive *d = (Drive *) arg;
    int j;
    s_vpi_value val;
    val.format = vpiHexStrVal;
    for (j=0; j<d->dataBytes[pos]; j++) {
        d->hexBuf[j] = d->readBuf[d->dataPos[pos] + j];
        if (debug > 1) vpi_printf(" get %c @ %d\n", d->hexBuf[j], d->dataPos[pos] + j); 
    } 
    d->hexBuf[j]  = 0;
    val.value.str = d->hexBuf;
    if (debug) vpi_printf(" name=%s, size=%d, val=%s (pos=%d)\n", vpi_get_str(vpiName, net), vpi_get(vpiSize, net), d->hexBuf, d->dataPos[pos]); 
    vpi_put_value(net, &val, NULL, vpiNoDelay);
}

PLI_INT32 hdfs_drive_signals(char *user_data) {
    int i;
    static char temp[128] = {0};
    //read(drive.readBuf, drive.readBytes);
    vpiHandle hTf = vpi_handle(vpiSysTfCall, NULL);    
    read(drive.readBuf, drive.readBytes);
    if (drive.readBuf[0] == 'q') {
        //MessageBox(NULL, "Got quit message", "", MB_OK);
        finish();
    }
    if (debug) vpi_printf("drive:\n");
    iter_args(hTf, drive_get, &drive);
    return 0;
}

void registerFns() {
    s_vpi_systf_data task_data_s = {0};
    p_vpi_systf_data task_data_p = &task_data_s;
    
    // intialisation
    char *arg;
    if ((arg = vpi_scan_plusargs("debug=")) != NULL) debug = atoi(arg);
    //if ((arg = mc_scan_plusargs("stop=")) != NULL) stop = atoi(arg);
    //MessageBox(NULL, "Trying to connect to pipe..", "", MB_OK);
    connect_pipe();
    
    // register tasks
    task_data_p->type = vpiSysTask;
    task_data_p->tfname = "$hdfs_drive";
    task_data_p->calltf = hdfs_drive_signals;
    task_data_p->sizetf = NULL;
    task_data_p->compiletf = hdfs_drive_signals_init;

    vpi_register_systf(task_data_p);
    
    task_data_p->type = vpiSysTask;
    task_data_p->tfname = "$hdfs_monitor";
    task_data_p->calltf = hdfs_monitor_signals;
    task_data_p->sizetf = NULL;
    task_data_p->compiletf = hdfs_monitor_signals_init;
    
    vpi_register_systf(task_data_p);
}

// Register the new system task here
void (*vlog_startup_routines[ ] ) () = {
    registerFns,
    0  // last entry must be 0 
}; 
 
 
