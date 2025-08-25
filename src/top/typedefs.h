#ifndef GMD_TYPEDEFS
#define GMD_TYPEDEFS

/*
 *
 *  typedefs.h
 *
 *
 * Gary Duncan ;   24 Nov 95 
 */

/*
typedef struct 
*/

/*typedef BOOL 			BOOLEAN; */
/*typedef BPTR			LOCK; */


typedef struct AmigaGuideBase	AMIGAGUIDEBASE;
typedef struct AreaInfo 	AREAINFO;
typedef struct BitMap		BITMAP;
typedef struct BoolInfo 	BOOLINFO;
typedef struct BootBlock	BOOTBLOCK;
typedef struct Border		BORDER;
typedef struct ClipboardUnitPartial CLIPBOARDUNITPARTIAL;
typedef struct ClipRect 	CLIPRECT;
typedef struct ClockData	CLOCKDATA;
typedef struct ColorMap 	COLORMAP;
typedef struct CommandLineInterface COMMANDLINEINTERFACE;
typedef struct ConfigDev 	CONFIGDEV;
typedef struct ConUnit		CONUNIT;
typedef struct copinit		COPINIT;
typedef struct DateStamp	DATESTAMP;
typedef struct DateTime		DATETIME;
typedef struct Device		DEVICE;
typedef struct DeviceNode	DEVICENODE;
typedef struct DiskObject	DISKOBJECT;
typedef struct DisplayInfo	DISPLAYINFO; 
typedef struct DosEnvec		DOSENVEC;	
typedef struct DosLibrary	DOSLIBRARY;
typedef struct DosInfo		DOSINFO;
typedef struct DosList		DOSLIST;
typedef struct EClockVal	ECLOCKVAL;
typedef struct ExecBase 	EXECBASE;
typedef struct ExpansionBase	EXPANSIONBASE;
typedef struct FileInfoBlock	FILEINFOBLOCK;
typedef struct FileLock 	FILELOCK;
typedef struct FileRequester	FILEREQUESTER;
typedef struct Gadget		GADGET;
typedef struct GadgetInfo	GADGETINFO;
typedef struct GfxBase		GFXBASE;
typedef struct GListEnv 	GLISTENV;
typedef struct IBox		IBOX;
typedef struct IFFHandle	IFFHANDLE;
typedef struct Image		IMAGE;
typedef struct InputEvent	INPUTEVENT;
typedef struct Interrupt	INTERRUPT;
typedef struct IntuiMessage	INTUIMESSAGE;
typedef struct IntuiText	INTUITEXT;
typedef struct IntuitionBase	INTUITIONBASE;
typedef struct IOAudio		IOAUDIO;
typedef struct IOClipReq	IOCLIPREQ;
typedef struct IOExtSer 	IOEXTSER;
typedef struct IORequest 	IOREQUEST;
typedef struct IOStdReq 	IOSTDREQ;
typedef struct Layer		LAYER;
typedef struct Layer_Info	LAYER_INFO;
typedef struct LayersBase	LAYERSBASE;
typedef struct Library		LIBRARY;
typedef struct List		LIST;
typedef struct Manufacturer	MANUFACTURER; 
typedef struct MemChunk		MEMCHUNK;
typedef struct MemEntry 	MEMENTRY;
typedef struct MemHeader	MEMHEADER;
typedef struct MemList		MEMLIST;
typedef struct Menu		MENU;
typedef struct MenuItem 	MENUITEM;
typedef struct Message		MESSAGE;
typedef struct MinList		MINLIST;
typedef struct MinNode		MINNODE;
typedef struct MsgPort		MSGPORT;
typedef struct NameInfo		NAMEINFO; 
typedef struct NewGadget	NEWGADGET;
typedef struct NewMenu		NEWMENU;
typedef struct NewScreen	NEWSCREEN;
typedef struct NewWindow	NEWWINDOW;
typedef struct Node		NODE;
typedef struct PartitionBlock	PARTITIONBLOCK;
typedef struct PenPair		PENPAIR;
typedef struct Point		POINT;
typedef struct Preferences	PREFERENCES;
typedef struct PrinterData	PRINTERDATA;
typedef struct PrinterExtendedData PRINTEREXTENDEDDATA;
typedef struct Process		PROCESS;
typedef struct PropInfo 	PROPINFO;
typedef struct PubScreenNode	PUBSCREENNODE; 
typedef struct RastPort 	RASTPORT;
typedef struct Region		REGION;
typedef struct Remember 	REMEMBER;
typedef struct Requester	REQUESTER;
typedef struct RigidDiskBlock	RIGIDDISKBLOCK;
typedef struct RootNode		ROOTNODE;
typedef struct SatisfyMsg	SATISFYMSG;
typedef struct Screen		SCREEN;
typedef struct Semaphore	SEMAPHORE;
typedef struct SignalSemaphore	SIGNALSEMAPHORE;
typedef struct stat		STAT;
typedef struct StringInfo	STRINGINFO;
typedef struct TagItem		TAGITEM;
typedef struct Task		TASK;
typedef struct TextAttr 	TEXTATTR;
typedef struct TextFont 	TEXTFONT;
typedef struct timerequest	TIMEREQUEST;
typedef struct timeval		TIMEVAL;
typedef struct TimerBase	TIMERBASE;
typedef struct TmpRas		TMPRAS;
typedef struct tm		TM;
typedef struct Unit		UNIT;
typedef struct View		VIEW;
typedef struct ViewPort 	VIEWPORT;
typedef struct Window		WINDOW;
typedef struct WorkbenchBase	WORKBENCHBASE;


typedef struct
{
  char days;
  char *months;
} DM;

#endif
