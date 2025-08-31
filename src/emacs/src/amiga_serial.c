#include <exec/types.h>
#include <devices/serial.h>
#include <stdio.h>
#include <internal/devices.h>

#include <proto/exec.h>
#include <proto/dos.h>

#include "amiga.h"

static struct MsgPort  *SerReadPort;
static struct IOExtSer *SerReadRequest, *SerWriteRequest;
static char ser_inbuf[2];
char *far serial_device = "serial.device";
long far serial_unit;

void init_amiga_serial(void)
{
    if ((SerWriteRequest = (struct IOExtSer *)
	 _device_open(serial_device, serial_unit, 0L,
		      0L, 0, sizeof(struct IOExtSer))) &&
	(SerReadPort = CreateMsgPort()) &&
	(SerReadRequest  = (struct IOExtSer *)CreateIORequest(SerReadPort, sizeof (struct IOExtSer))))
    {
	SerReadRequest->IOSer.io_Device = SerWriteRequest->IOSer.io_Device;
	SerReadRequest->IOSer.io_Unit = SerWriteRequest->IOSer.io_Unit;
	ser_inbuf[1]=0;
	SerReadRequest->IOSer.io_Command = CMD_READ;
	SerWriteRequest->IOSer.io_Command = CMD_WRITE;
	SerReadRequest->IOSer.io_Length = 1;
	SerReadRequest->IOSer.io_Data = &ser_inbuf[0];
	SendIO(SerReadRequest);

	inputsig |= 1L << SerReadPort->mp_SigBit;
    }
    else _fail("No memory or serial.device missing");
}

void cleanup_amiga_serial(void)
{
    if (SerReadRequest)
    {
	AbortIO(SerReadRequest);
	WaitIO(SerReadRequest);
	DeleteIORequest(SerReadRequest);
    }
    if (SerReadPort) DeletePort(SerReadPort);
    _device_close(SerWriteRequest);
}

void check_serial(int force)
{
    while (CheckIO(SerReadRequest))
    {
	int c = ser_inbuf[0];
	SendIO(SerReadRequest);
	enque(c, FALSE);
    }
}

void serial_puts(char *str, int len)
{

    SerWriteRequest->IOSer.io_Length = len;
    SerWriteRequest->IOSer.io_Data = str;
    DoIO(SerWriteRequest);
}

unsigned long serial_baud_rate(void)
{
    return SerWriteRequest->io_Baud;
}
