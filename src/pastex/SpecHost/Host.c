/*
**	SpecialHost for PasTeX
**
**	Copyright © by Olaf Barthel & Georg Heßmann
*/

/*
** Host.c
**
** latest revision: 13 Jun 1995, by Giuseppe Ghibò.
*/

#include "Global.h"

#include "SpecialHost_rev.h"

#define TEMPLATE	"TRANSFER/K,RENDER/K,BASEDEPI/K/N,INVERT/S,USESCREEN/S,NOGUI/S"

enum	{	ARG_TRANSFER,ARG_RENDER,ARG_BASEDPI,ARG_INVERT,ARG_USESCREEN,ARG_NOGUI,
		ARGCOUNT
	};

STATIC VOID __saveds ChildFunc(VOID);
STATIC VOID __regargs Init_Transfer_Render(LONG *, LONG *, int draw_modus); /* (ghi) to fix problem with config file */

struct EnvVarPath *texconfig_var; /* for TEXCONFIG env var */
struct EnvVarPath *psheaders_var; /* for DVIPSHEADERS env var */

LONG __saveds
Main(VOID)
{
	LONG			 Result = RETURN_FAIL;
	BOOL			 NeedStack;
	struct WBStartup	*WBenchMsg;

	SysBase = *(struct ExecBase **)4;

	Father = (struct Process *)SysBase -> ThisTask;

	if(Father -> pr_CLI)
	{
		NeedStack = (((struct CommandLineInterface *)BADDR(Father -> pr_CLI)) -> cli_DefaultStack < 8192);

		WBenchMsg = NULL;
	}
	else
	{
		WaitPort(&Father -> pr_MsgPort);

		WBenchMsg = (struct WBStartup *)GetMsg(&Father -> pr_MsgPort);

		NeedStack = ((LONG)SysBase -> ThisTask -> tc_SPUpper - (LONG)SysBase -> ThisTask -> tc_SPLower < 32768);
	}

	if(DOSBase = (struct DosLibrary *)OpenLibrary("dos.library",37))
	{
		if(UtilityBase = OpenLibrary("utility.library",37))
		{
			BPTR	OldDir,
				Stream		= NULL,
				OldCOS		= NULL;
			APTR	OldConsoleTask	= NULL;

			if(WBenchMsg)
				OldDir = CurrentDir(WBenchMsg -> sm_ArgList[0] . wa_Lock);

			texconfig_var = Alloc_EnvVarPath("TEXCONFIG", EVP_BUFSIZE);
			psheaders_var = Alloc_EnvVarPath("DVIPSHEADERS", EVP_BUFSIZE);
			Init_EnvVarPath(texconfig_var, "TeX:config", ENVPATH_DEFSTR);
			Init_EnvVarPath(psheaders_var, "TeX:ps,POST:", ENVPATH_DEFSTR | ENVPATH_APPEND_PATH);

			ReadConfig(&Configuration);

			if(Father -> pr_CLI)
			{
				struct RDArgs	*ArgsPtr;
				STRPTR		 Arg[ARGCOUNT];
				LONG		 TransferMode,	// = CYID_Transfer_Memory,
						 RenderMode,	// = CYID_Render_None,
						 ID;

				Init_Transfer_Render(&TransferMode, &RenderMode, Configuration . draw_modus); /* (ghi) TransferMode and RenderMode are initialized to the value read from the Config file */

				memset(Arg,0,sizeof(Arg));

				if(ArgsPtr = ReadArgs(TEMPLATE,(LONG *)Arg,NULL))
				{
					if(Arg[ARG_TRANSFER])
					{
						if((ID = GetMapCode(TransferTable,Arg[ARG_TRANSFER])) != -1)
							TransferMode = ID;
					}

					if(Arg[ARG_RENDER])
					{
						if((ID = GetMapCode(RenderTable,Arg[ARG_RENDER])) != -1)
							RenderMode = ID;
					}

					if(Arg[ARG_BASEDPI])
					{
						if((ID = *(LONG *)Arg[ARG_BASEDPI]) > 0)
							Configuration . base_dpi = ID;
					}

					if(Arg[ARG_INVERT])
						Configuration . invert_bmap = TRUE;

					if(Arg[ARG_USESCREEN])
						Configuration . use_pubscr = TRUE;

					if(Arg[ARG_NOGUI])
						UseGUI = FALSE;

					ChangeDrawMode(TransferMode,RenderMode,FALSE);

					FreeArgs(ArgsPtr);
				}
			}
			else
			{
				if(IconBase = OpenLibrary("icon.library",37))
				{
					struct DiskObject *Icon;

					if(Icon = GetDiskObject(WBenchMsg -> sm_ArgList[0] . wa_Name))
					{
						STRPTR	Data;
						LONG	TransferMode,	// = CYID_Transfer_Memory,
							RenderMode,	// = CYID_Render_None,
							ID;

						Init_Transfer_Render(&TransferMode, &RenderMode, Configuration . draw_modus); /* (ghi) TransferMode and RenderMode are initialized to the value read from the Config file */

						if(Data = FindToolType(Icon -> do_ToolTypes,"TRANSFER"))
						{
							if((ID = GetMapCode(TransferTable,Data)) != -1)
								TransferMode = ID;
						}

						if(Data = FindToolType(Icon -> do_ToolTypes,"RENDER"))
						{
							if((ID = GetMapCode(RenderTable,Data)) != -1)
								RenderMode = ID;
						}

						if(Data = FindToolType(Icon -> do_ToolTypes,"BASEDPI"))
						{
							if(StrToLong(Data,&ID) > 0)
							{
								if(ID > 0)
									Configuration . base_dpi = ID;
							}
						}

						if(FindToolType(Icon -> do_ToolTypes,"INVERT"))
							Configuration . invert_bmap = TRUE;

						if(FindToolType(Icon -> do_ToolTypes,"USESCREEN"))
							Configuration . use_pubscr = TRUE;

						if(FindToolType(Icon -> do_ToolTypes,"NOGUI"))
							UseGUI = FALSE;

						ChangeDrawMode(TransferMode,RenderMode,FALSE);

						FreeDiskObject(Icon);
					}

					CloseLibrary(IconBase);
				}

				if(!UseGUI)
				{
					if(Configuration . use_pubscr)
						Stream = Open("CON:20/20/400/80/SpecialHost/SCREENShowDVI-PubScr",MODE_NEWFILE);
					else
						Stream = Open("CON:20/20/400/80/SpecialHost",MODE_NEWFILE);

					if(Stream)
					{
						struct FileHandle *Handle = (struct FileHandle *)BADDR(Stream);

						OldConsoleTask = Father -> pr_ConsoleTask;

						Father -> pr_ConsoleTask = (APTR)Handle -> fh_Type;

						OldCOS = Father -> pr_COS;

						if(!(Father -> pr_COS = Open("*",MODE_NEWFILE)))
						{
							Father -> pr_COS		= OldCOS;
							Father -> pr_ConsoleTask	= OldConsoleTask;

							Close(Stream);

							Stream = NULL;

							UseGUI = TRUE;
						}
					}
					else
						UseGUI = TRUE;
				}
			}

			if(NeedStack)
			{
				Forbid();

				SetSignal(0,SIGF_SINGLE);

				if(CreateNewProcTags(
					NP_CommandName,	"SpecialHost Child",
					NP_Name,	"SpecialHost Child Process",
					NP_Output,	Father -> pr_COS,
					NP_CloseOutput,	FALSE,
					NP_StackSize,	32768,
					NP_Entry,	ChildFunc,
					NP_Cli,		(WBenchMsg == NULL),
				TAG_DONE))
				{
					ULONG Signals;

					Result = RETURN_OK;

					Wait(SIGF_SINGLE);

					while(1)
					{
						Signals = Wait(SIGF_SINGLE | SIGBREAKF_CTRL_C | SIGBREAKF_CTRL_E);

						if(Signals & SIGF_SINGLE)
							break;
						else
							Signal(ThisProcess,Signals);
					}
				}

				Permit();
			}
			else
			{
				HandleInput();

				Result = RETURN_OK;
			}

			if(Stream)
			{
				BPTR NewCOS = Father -> pr_COS;

				Father -> pr_COS		= OldCOS;
				Father -> pr_ConsoleTask	= OldConsoleTask;

				Close(Stream);
				Close(NewCOS);
			}

			Free_EnvVarPath(texconfig_var);
			Free_EnvVarPath(psheaders_var);

			if(WBenchMsg)
				CurrentDir(OldDir);

			CloseLibrary(UtilityBase);
		}

		CloseLibrary(DOSBase);
	}

	if(WBenchMsg)
	{
		Forbid();

		ReplyMsg(&WBenchMsg -> sm_Message);
	}
	else
		return(Result);
}

int _FPERR;
void _CXFERR(int code) {}

STATIC VOID __saveds
ChildFunc()
{
	Signal(Father,SIGF_SINGLE);

	HandleInput();

	Forbid();

	Signal(Father,SIGF_SINGLE);
}

BOOL __regargs
BuildName(STRPTR LocalBuffer,BPTR Home,STRPTR Source,LONG ResX,LONG ResY,ULONG CRC,LONG *Error)
{
	UBYTE	NewDir[40];
	STRPTR	Index;
	BOOL	Success = FALSE;

	*Error = 0;

	strcpy(LocalBuffer,Source);

	if(FilePart(LocalBuffer) == LocalBuffer)
	{
		if(Home == (BPTR)~0)
			LocalBuffer[0] = 0;
		else
		{
			if(!NameFromLock(Home,LocalBuffer,MAX_FILENAME_LEN))
			{
				*Error = IoErr();

				return(FALSE);
			}
		}
	}
	else
	{
		Index = PathPart(LocalBuffer);

		*Index = 0;
	}

	sprintf(NewDir,"SpecialHost_%dx%d",ResX,ResY);

	if(AddPart(LocalBuffer,NewDir,MAX_FILENAME_LEN))
	{
		BPTR FileLock;

		if(FileLock = Lock(LocalBuffer,ACCESS_READ))
		{
			struct FileInfoBlock *FileInfo;

			if(FileInfo = (struct FileInfoBlock *)AllocDosObjectTags(DOS_FIB,TAG_DONE))
			{
				if(Examine(FileLock,FileInfo))
				{
					if(FileInfo -> fib_DirEntryType > 0)
						Success = TRUE;
					else
						*Error = ERROR_OBJECT_WRONG_TYPE;
				}
				else
					*Error = IoErr();

				FreeDosObject(DOS_FIB,FileInfo);

				UnLock(FileLock); /* (ghi) fixed */
			}
			else
				*Error = IoErr();
		}
		else
		{
			LONG LocalError = IoErr();

			if(LocalError == ERROR_OBJECT_NOT_FOUND)
			{
				if(FileLock = CreateDir(LocalBuffer))
				{
					Success = TRUE;

					UnLock(FileLock);
				}
				else
					*Error = IoErr();
			}
			else
				*Error = LocalError;
		}
	}
	else
		*Error = IoErr();

	if(Success)
	{		/* (ghi) save the CRC number within the filename */
		UBYTE Source2[32];

		sprintf(Source2,"%.20s_%1s%08lx",FilePart(Source),(Configuration . invert_bmap ? "I" : "N"), CRC);
		
		if(!AddPart(LocalBuffer,Source2,MAX_FILENAME_LEN))
		{
			*Error = IoErr();

			Success = FALSE;
		}
	}

	return(Success);
}

VOID __regargs
SetSleep(BOOL Mode)
{
	if(AP_Application)
	{
		if(Mode)
		{
			struct Window *Window;

			Forbid();

			if(get(WI_Main,MUIA_Window_Window,&Window))
				ThisProcess -> pr_WindowPtr = Window;

			Permit();

			set(AP_Application,MUIA_Application_Sleep,TRUE);
		}
		else
		{
			ThisProcess -> pr_WindowPtr = OldPtr;

			set(AP_Application,MUIA_Application_Sleep,FALSE);
		}
	}
}

/*
BPTR __regargs
OpenConfigFile(STRPTR Name,LONG Mode)
{
	UBYTE	Env[MAX_FILENAME_LEN],
		Path[MAX_FILENAME_LEN];
	STRPTR	Index;
	LONG	Len;
	BPTR	File;

	if(GetVar("TEXCONFIG",Env,MAX_FILENAME_LEN,NULL) < 1)
		strcpy(Env,"TeX:config");

	Index = strtok(Env,",;");

	do
	{
		strcpy(Path,Index);

		Len = strlen(Path);

		if(Path[Len - 1] != ':' && Path[Len - 1] != '/')
		{
			Path[Len]	= '/';
			Path[Len + 1]	= 0;
		}

		if(!strcmp(Path,"./"))
			Path[0] = 0;

		strcat(Path,Name);

		if(File = Open(Path,Mode))
			return(File);
	}
	while(Index = strtok(NULL,",;"));

	return(NULL);
}
*/

BOOL __regargs
ReadConfig(struct config_struct *ConfigPtr)
{
	BPTR File;
	BOOL Result = FALSE;

	if (File = EVP_Open("SpecialHost.config", texconfig_var, NULL, NULL, MODE_OLDFILE))
	{
		struct config_struct Config;

		if(Read(File,&Config,sizeof(struct config_struct)) == sizeof(struct config_struct))
		{
			if(Config . magic == CONFIG_FILE_MAGIC && Config . version <= CONFIG_FILE_VERSION)
			{
				if(Config . use_pubscr != ConfigPtr -> use_pubscr)
					Result = TRUE;

				CopyMem(&Config,ConfigPtr,sizeof(struct config_struct));
			}
			else
				PrintLine("\33bConfiguration file format is too old.\33n");
		}
		else
			PrintLine("\33bError reading configuration file.\33n");

		Close(File);
	}
	else
		PrintLine("\33bFailed to open configuration file.\33n");

	return(Result);
}

VOID __regargs
SaveConfig(struct config_struct *ConfigPtr)
{
	BPTR File;

	if(File = EVP_Open("SpecialHost.config",texconfig_var, NULL, NULL, MODE_NEWFILE))
	{
		if(Write(File,ConfigPtr,sizeof(struct config_struct)) != sizeof(struct config_struct))
			PrintLine("\33bError writing configuration file.\33n");

		Close(File);
	}
	else
		PrintLine("\33bFailed to open configuration file.\33n");
}

VOID
ChangeDrawMode(LONG Transfer,LONG Render,BOOL GetThem)
{
	if(GetThem)
	{
		if(!CY_Transfer || !CY_Render)
			return;
		else
		{
			get(CY_Transfer,MUIA_Cycle_Active,&Transfer);
			get(CY_Render,	MUIA_Cycle_Active,&Render);
		}
	}

	switch(Transfer)
	{
		case CYID_Transfer_Memory:

			switch(Render)
			{
				case CYID_Render_None:

					Configuration . draw_modus = DRAW_IN_MEM;
					break;

				case CYID_Render_Frame:

					Configuration . draw_modus = DRAW_IN_MEM_B;
					break;

				case CYID_Render_Clear:

					Configuration . draw_modus = DRAW_RECT;
					break;
			}

			break;

		case CYID_Transfer_Disk:

			switch(Render)
			{
				case CYID_Render_None:

					Configuration . draw_modus = DRAW_FILE;
					break;

				case CYID_Render_Frame:

					Configuration . draw_modus = DRAW_FILE_B;
					break;

				case CYID_Render_Clear:

					Configuration . draw_modus = DRAW_BORDER;
					break;
			}

			break;

		case CYID_Transfer_None:

			switch(Render)
			{
				case CYID_Render_None:

					Configuration . draw_modus = DRAW_RECT;
					break;

				case CYID_Render_Frame:

					Configuration . draw_modus = DRAW_BORDER;
					break;

				case CYID_Render_Clear:

					Configuration . draw_modus = DRAW_RECT;
					break;
			}

			break;
	}
}

VOID __inline
LibraryCleanup(struct Library **Library)
{
	if(*Library)
	{
		CloseLibrary(*Library);

		*Library = NULL;
	}
}

BOOL
OpenGUI()
{
	STRPTR Version = VERSTAG;

	AP_Application = ApplicationObject,
		MUIA_Application_Title,		"SpecialHost",
		MUIA_Application_Version,	Version + 1,
		MUIA_Application_Copyright,	"Copyright © 1993-1994 by Olaf `Olsen' Barthel & Georg Heßmann",
		MUIA_Application_Author,	"Olaf `Olsen' Barthel & Georg Heßmann",
		MUIA_Application_Description,	"PasTeX special extension processor",
		MUIA_Application_Base,		"SPECIALHOST",
		MUIA_Application_Menu,		MenuTemplate,

		SubWindow,
			WI_Main = WindowObject,
				MUIA_Window_Title,	"SpecialHost",
				MUIA_Window_ID,		MAKE_ID('M','A','I','N'),
				WindowContents,		VGroup,

					Child,	HGroup,
						Child,	ColGroup(2),GroupFrameT("Image"),
							Child,	KeyLabel1("Transfer",'t'),	Child,	CY_Transfer	= KeyCycle(CYA_Transfer,'t'),
							Child,	KeyLabel1("Rendering",'r'),	Child,	CY_Render	= KeyCycle(CYA_Render,'r'),
							Child,	KeyLabel1("Invert",'i'),	Child,	HGroup,
													Child,	CM_Invert	= KeyCheckMark(FALSE,'i'),
													Child,	HSpace(FALSE),
													End,
							Child,	KeyLabel2("Base DPI",'b'),	Child,	ST_BaseDPI = StringObject,
															StringFrame,
															MUIA_String_Integer,	100,
															MUIA_String_Accept,	"1234567890",
															MUIA_String_MaxLen,	5,
															MUIA_ControlChar,	'b',
													End,
							End,
						End,

					Child,	HGroup,
						GroupFrameT("Messages"),
						Child,	LV_Messages = ListviewObject,
							MUIA_Listview_Input,	FALSE,
							MUIA_Listview_List,	ListObject,
											ReadListFrame,
										End,
							End,
						End,

					Child,	VGroup,GroupFrame,
						Child,	GA_Gauge = GaugeObject,
							GaugeFrame,
							MUIA_Gauge_Horiz,	TRUE,
							MUIA_Weight,		0,
							End,
						Child,	RectangleObject,
							MUIA_Weight,		0,
							End,
						Child,	ScaleObject,
							End,
						End,
					Child,	HGroup,
						MUIA_Group_SameSize, TRUE,
						Child,	BT_Jump		= KeyButton("Jump to ShowDVI",'j'),
						Child,	BT_Show		= KeyButton("ShowDVI to front",'f'),
						Child,	BT_Clear	= KeyButton("Clear",'c'),
						End,
				End,
			End,
	End;

	if(AP_Application)
	{
		struct ListEntry	*Entry = (struct ListEntry *)MessageList . lh_Head;
		struct Window		*Window;

		DoMethod(WI_Main,	MUIM_Notify,MUIA_Window_CloseRequest,	TRUE,		AP_Application,2,MUIM_Application_ReturnID,MUIV_Application_ReturnID_Quit);
		DoMethod(CY_Transfer,	MUIM_Notify,MUIA_Cycle_Active,		MUIV_EveryTime,	AP_Application,2,MUIM_Application_ReturnID,GAD_TRANSFER);
		DoMethod(CY_Render,	MUIM_Notify,MUIA_Cycle_Active,		MUIV_EveryTime,	AP_Application,2,MUIM_Application_ReturnID,GAD_RENDER);
		DoMethod(CM_Invert,	MUIM_Notify,MUIA_Selected,		MUIV_EveryTime,	AP_Application,2,MUIM_Application_ReturnID,GAD_INVERT);
		DoMethod(ST_BaseDPI,	MUIM_Notify,MUIA_String_Contents,	MUIV_EveryTime,	AP_Application,2,MUIM_Application_ReturnID,GAD_BASEDPI);
		DoMethod(BT_Jump,	MUIM_Notify,MUIA_Pressed,FALSE,				AP_Application,2,MUIM_Application_ReturnID,GAD_JUMP);
		DoMethod(BT_Show,	MUIM_Notify,MUIA_Pressed,FALSE,				AP_Application,2,MUIM_Application_ReturnID,GAD_SHOW);
		DoMethod(BT_Clear,	MUIM_Notify,MUIA_Pressed,FALSE,				AP_Application,2,MUIM_Application_ReturnID,GAD_CLEAR);

		DoMethod(WI_Main,	MUIM_Window_SetCycleChain,CY_Transfer,CY_Render,CM_Invert,ST_BaseDPI,LV_Messages,BT_Jump,BT_Show,BT_Clear,NULL);

		while(Entry -> MinNode . mln_Succ)
		{
			DoMethod(LV_Messages,MUIM_List_Insert,&Entry -> Title,1,MUIV_List_Insert_Bottom);

			Entry = (struct ListEntry *)Entry -> MinNode . mln_Succ;
		}

		set(LV_Messages,MUIA_List_Active,MUIV_List_Active_Bottom);

		switch(Configuration . draw_modus)
		{
			case DRAW_IN_MEM:

				set(CY_Transfer,	MUIA_Cycle_Active,CYID_Transfer_Memory);
				set(CY_Render,		MUIA_Cycle_Active,CYID_Render_None);
				break;

			case DRAW_FILE:

				set(CY_Transfer,	MUIA_Cycle_Active,CYID_Transfer_Disk);
				set(CY_Render,		MUIA_Cycle_Active,CYID_Render_None);
				break;

			case DRAW_IN_MEM_B:

				set(CY_Transfer,	MUIA_Cycle_Active,CYID_Transfer_Memory);
				set(CY_Render,		MUIA_Cycle_Active,CYID_Render_Frame);
				break;

			case DRAW_FILE_B:

				set(CY_Transfer,	MUIA_Cycle_Active,CYID_Transfer_Disk);
				set(CY_Render,		MUIA_Cycle_Active,CYID_Render_Frame);
				break;

			case DRAW_BORDER:

				set(CY_Transfer,	MUIA_Cycle_Active,CYID_Transfer_None);
				set(CY_Render,		MUIA_Cycle_Active,CYID_Render_Frame);
				break;

			case DRAW_RECT:

				set(CY_Transfer,	MUIA_Cycle_Active,CYID_Transfer_None);
				set(CY_Render,		MUIA_Cycle_Active,CYID_Render_Clear);
				break;
		}

		set(CM_Invert,MUIA_Selected,Configuration . invert_bmap);

		InvertChanged = TRUE;

		set(ST_BaseDPI,MUIA_String_Integer,Configuration . base_dpi);

		if(PubScreen)
		{
			UnlockPubScreen(NULL,PubScreen);

			PubScreen = NULL;
		}

		if(Configuration . use_pubscr)
		{
			if(PubScreen = LockPubScreen("ShowDVI-PubScr"))
			{
				set(WI_Main,MUIA_Window_Screen,PubScreen);

				set(BT_Jump,MUIA_Disabled,TRUE);
			}
			else
				Configuration . use_pubscr = FALSE;
		}

		set(WI_Main,MUIA_Window_Open,TRUE);

		Forbid();

		if(get(WI_Main,MUIA_Window_Window,&Window))
		{
			if(Window)
			{
				struct MenuItem *Item;

				if(Item = ItemAddress(Window -> MenuStrip,FULLMENUNUM(0,3,NOSUB)))
				{
					if(Configuration . use_pubscr)
						Item -> Flags |= CHECKED;
					else
						Item -> Flags &= ~CHECKED;
				}

				Permit();

				return(TRUE);
			}
		}

		Permit();

		if(PubScreen)
		{
			UnlockPubScreen(NULL,PubScreen);

			PubScreen = NULL;
		}

		DisposeObject(AP_Application);

		AP_Application = NULL;
	}

	return(FALSE);
}

VOID
CloseAll()
{
	if(ThisProcess)
		ThisProcess -> pr_WindowPtr = OldPtr;

	if(MainPort)
	{
		struct special_msg *Message;

		RemPort(MainPort);

		while(Message = (struct special_msg *)GetMsg(MainPort))
		{
			Message -> action	= AC_REPLY_UNKNOWN;
			Message -> ret		= 0;

			ReplyMsg((struct Message *)Message);
		}

		DeleteMsgPort(MainPort);
	}

	if(Pool)
		LibDeletePool(Pool);

	LibraryCleanup(&MUIMasterBase);
	LibraryCleanup(&DataTypesBase);
/*
	LibraryCleanup(&MathBase);
	LibraryCleanup(&MathTransBase);
*/
	LibraryCleanup(&MathIeeeDoubBasBase);
	LibraryCleanup(&MathIeeeDoubTransBase);

	LibraryCleanup(&AslBase);
	LibraryCleanup(&IFFParseBase);
	LibraryCleanup(&GadToolsBase);
	LibraryCleanup(&GfxBase);
	LibraryCleanup(&IntuitionBase);
}

BOOL
OpenAll()
{
	ThisProcess = (struct Process *)SysBase -> ThisTask;

	if(!(IntuitionBase = (struct IntuitionBase *)OpenLibrary("intuition.library",37)))
	{
		ShowError(ERR_NO_INTUITION,NULL,FALSE);

		return(FALSE);
	}

	if(!(GfxBase = (struct GfxBase *)OpenLibrary("graphics.library",37)))
	{
		ShowError(ERR_NO_GRAPHICS,NULL,FALSE);

		return(FALSE);
	}

	if(!(GadToolsBase = OpenLibrary("gadtools.library",37)))
	{
		ShowError(ERR_NO_GADTOOLS,NULL,FALSE);

		return(FALSE);
	}

	if(!(IFFParseBase = OpenLibrary("iffparse.library",37)))
	{
		ShowError(ERR_NO_IFFPARSE,NULL,FALSE);

		return(FALSE);
	}

	if(!(AslBase = OpenLibrary("asl.library",37)))
	{
		ShowError(ERR_NO_ASL,NULL,FALSE);

		return(FALSE);
	}

/*	if(!(MathBase = OpenLibrary("mathffp.library",0)))*/

	if(!(MathIeeeDoubBasBase = OpenLibrary("mathieeedoubbas.library",0)))
	{
		ShowError(ERR_NO_MATHFFP,NULL,FALSE);

		return(FALSE);
	}

/*	if(!(MathTransBase = OpenLibrary("mathtrans.library",0)))*/

	if(!(MathIeeeDoubTransBase = OpenLibrary("mathieeedoubtrans.library",0)))
	{
		ShowError(ERR_NO_MATHTRANS,NULL,FALSE);

		return(FALSE);
	}

	if(UseGUI)
	{
		if(!(MUIMasterBase = OpenLibrary(MUIMASTER_NAME,MUIMASTER_VMIN)))
		{
			ShowError(ERR_NO_MUI,NULL,FALSE);

			return(FALSE);
		}
	}

	DataTypesBase = OpenLibrary("datatypes.library",39);

	if(!(Pool = LibCreatePool(MEMF_ANY | MEMF_PUBLIC,8192,8192)))
	{
		ShowError(ERR_NO_POOL,NULL,FALSE);

		return(FALSE);
	}

	Forbid();

	if(FindPort(SPECIAL_PORT))
	{
		ShowError(ERR_ALREADY_RUNNING,NULL,FALSE);

		Permit();

		return(FALSE);
	}

	if(!(MainPort = CreateMsgPort()))
	{
		ShowError(ERR_NO_PORT,NULL,FALSE);

		Permit();

		return(FALSE);
	}

	MainPort -> mp_Node . ln_Name = SPECIAL_PORT;

	AddPort(MainPort);

	Permit();

	if(GfxBase -> ChipRevBits0 & GFXF_BIG_BLITS)
	{
		WriteLine	= LargeWriteLine;
		ReadLine	= LargeReadLine;
	}
	else
	{
		WriteLine	= SmallWriteLine;
		ReadLine	= SmallReadLine;
	}

	NewList(&MessageList);

	OldPtr = ThisProcess -> pr_WindowPtr;

	return(TRUE);
}

VOID __stdargs
PrintLine(STRPTR Format,...)
{
	if(ThisProcess)
	{
		STATIC UBYTE __far	LineBuffer[512];
		va_list			VarArgs;

		va_start(VarArgs,Format);

		vsprintf(LineBuffer,Format,VarArgs);

		if(AP_Application)
		{
			struct ListEntry *Entry;

			if(Entry = (struct ListEntry *)AllocVecPooled(sizeof(struct ListEntry) + strlen(LineBuffer) + 1,MEMF_ANY))
			{
				Entry -> Title = (STRPTR)(Entry + 1);

				strcpy(Entry -> Title,LineBuffer);

				AddTail(&MessageList,(struct Node *)Entry);

				DoMethod(LV_Messages,MUIM_List_Insert,&Entry -> Title,1,MUIV_List_Insert_Bottom);

				set(LV_Messages,MUIA_List_Active,MUIV_List_Active_Bottom);
			}
		}
		else
		{
			if(ThisProcess -> pr_CLI)
			{
				STRPTR	From	= LineBuffer,
					To	= LineBuffer;
				LONG	i;

				for(i = 0 ; i < strlen(LineBuffer) ; i++)
				{
					if(LineBuffer[i] == '\33')
						To += 2;
					else
						*From++ = *To++;
				}

				*To = 0;

				FPrintf(ThisProcess -> pr_COS,"%s\n",LineBuffer);
			}
		}

		va_end(VarArgs);
	}
}


VOID __regargs
GetNewSize(struct special_map *SpecialMap, struct parse_result *Result, LONG *NewWidth, LONG *NewHeight, LONG DPI_X, LONG DPI_Y)
{
	float width, height, mag;

	width	= (float)SpecialMap -> width  * Result -> hres / DPI_X;
	height	= (float)SpecialMap -> height * Result -> vres / DPI_Y;
	mag	= (float)Result -> DVI_mag/1000.0;
 
	if(Result -> hsize != 0.0)
		width  = Result -> hsize * Result -> hres;

	if(Result -> vsize != 0.0)
		height = Result -> vsize * Result -> vres;

	if(Result -> scale > 0.0)
	{
		width  *= Result -> scale;
		height *= Result -> scale;
	}
	else
	{
		if(Result -> hscale > 0.0)
			width *= Result -> hscale;

		if(Result -> vscale > 0.0)
			height *= Result -> vscale;
	}

	*NewWidth  = (LONG) (width  * mag + 0.5); /* (ghi) we round only once */
	*NewHeight = (LONG) (height * mag + 0.5);

}

struct BitMap * __regargs
ProcessImage(struct special_msg *Message)
{
	STATIC struct special_map	SpecialMap;
	STATIC struct config_struct	ConfigBackup;

	UBYTE			LocalBuffer[256];
	struct parse_result	Result;
	struct BitMap		*BitMap = NULL;
	LONG			Width, Height, Sum1, Sum2, Error;
	STRPTR			String = Message -> special_string;
	BPTR			NewDir = NULL, OldDir = NULL, Home;

	LONG			DVI_mag;
	STATIC UBYTE		DVI_fname[256] = { '\0' };
	STATIC struct DateStamp DVI_date = {0L, 0L, 0L};
	STATIC LONG		DVI_page;

	BOOL			DVI_filechanged = FALSE,
				DVI_datechanged = FALSE,
				DVI_pagechanged = FALSE;

	if (Message -> msg . mn_Length >= 56)
		DVI_mag = Message -> DVImagnification; /* (ghi) support for global DVI magnification */
	else
		DVI_mag = 1000L; /* default magnification = no magnification */

	if (Message -> msg . mn_Length >= 60)
	{
		if (Message -> DVIfilename != NULL)
		{
			if (Stricmp(DVI_fname, Message -> DVIfilename))
			{
				strcpy(DVI_fname, Message -> DVIfilename);
				memcpy(&DVI_date, Message -> DVIfiledate, sizeof(struct DateStamp));
				DVI_filechanged = TRUE;
			}
		}

		if (!Message -> DVIfiledate)
		{
			if (!CompareDates(&DVI_date, Message -> DVIfiledate))
			{
				memcpy(&DVI_date, Message -> DVIfiledate, sizeof(struct DateStamp));
				DVI_datechanged = TRUE;
			}
		}

		if (DVI_page != Message -> DVIcurphypage)
		{
			DVI_page = Message -> DVIcurphypage;
			DVI_pagechanged = TRUE;
		}
	}

	if (DVI_pagechanged || DVI_filechanged || DVI_datechanged)
		Init_Extra_Transf();

	if (DVI_filechanged)
		Init_PSHeaders();

	if(Message -> msg . mn_Length >= 52 && TypeOfMem(BADDR(Message -> DVIdirLock)))
	{
		OldDir = CurrentDir(Message -> DVIdirLock);

		Home = Message -> DVIdirLock;
	}
	else
	{
		Home = (BPTR)~0;

		if(GetVar("TEXPICTPATH",LocalBuffer,256,NULL) > 0)
		{
			if(NewDir = Lock(LocalBuffer,ACCESS_READ))
			{
				OldDir = CurrentDir(NewDir);

				Home = NewDir;
			}
		}
	}

	PrintLine("Command string received (%d × %d DPI),",Message -> hresolution,Message -> vresolution);
	PrintLine("\"%s\"",Message -> special_string);

	memset(&Result,0,sizeof(struct parse_result));
	memset(&SpecialMap,0,sizeof(struct special_map));
	memcpy(&ConfigBackup,&Configuration,sizeof(struct config_struct));

	Result . hres		= Message -> hresolution;
	Result . vres		= Message -> vresolution;
	Result . mode		= BandW;
	Result . invert		= Configuration . invert_bmap;
	Result . base_dpi	= Configuration . base_dpi;
	Result . DVI_mag	= DVI_mag;

	if (Message -> dmap) /* (ghi) get current point */
	{
		Result . current_x   = Message -> dmap -> x;
		Result . current_y   = Message -> dmap -> y;
		Result . page_width  = Message -> dmap -> page_width;
		Result . page_height = Message -> dmap -> page_height;
	}
	else
	{
		Result . current_x   = 0L;
		Result . current_y   = 0L;
		Result . page_width  = 8 * Message -> hresolution;
		Result . page_height = 11 * Message -> vresolution;
	}

	strcpy(Result . psinit_file,"init.ps");

	get(CY_Transfer,MUIA_Cycle_Active,&Result . transfer);
	get(CY_Render,MUIA_Cycle_Active,&Result . rendering);

	Sum2 = 0;

	Sum1 = DoCRC(String,strlen(String));

	if(!ParseSpecial(Message -> special_string, &Result))
	{
		Message -> ret = 5;

		PrintLine("Done.");
		PrintLine("");

		ShowProgress(0);

		if(OldDir)
			CurrentDir(OldDir);

		if(NewDir)
			UnLock(NewDir);

		return(NULL);
	}

	if (psfig_status == PSFIG_END)
	{
		UBYTE TmpString[256];

		if (Extra_Transf)
		{
			sprintf(TmpString,"[%g %g %g %g] %ld %ld %ld %ld %ld %ld startTexFig %s%g rotate endTexFig",
				Extra_Transf -> CTM . a,
				Extra_Transf -> CTM . b,
				Extra_Transf -> CTM . c,
				Extra_Transf -> CTM . d,
				psfig_data . width, psfig_data . height,
				psfig_data . llx, psfig_data . lly, psfig_data . urx, psfig_data . ury,
				(psfig_data . clip ? "doclip " : ""),
				psfig_data . angle);
		}
		else
		{
			sprintf(TmpString,"%ld %ld %ld %ld %ld %ld startTexFig %s%g rotate endTexFig",
				psfig_data . width, psfig_data . height,
				psfig_data . llx, psfig_data . lly, psfig_data . urx, psfig_data . ury,
				(psfig_data . clip ? "doclip " : ""),
				psfig_data . angle);
		}

		Sum1 = DoCRC(TmpString,strlen(TmpString));
	}

	if (Extra_Transf && psfig_status == PSFIG_OFF)
	{
		UBYTE TmpString[256];

		sprintf(TmpString,"%s [%g %g %g %g]", String,
			Extra_Transf -> CTM . a,
			Extra_Transf -> CTM . b,
			Extra_Transf -> CTM . c,
			Extra_Transf -> CTM . d);

		Sum1 = DoCRC(TmpString, strlen(TmpString));
        }

	Configuration . invert_bmap	= Result . invert;
	Configuration . base_dpi	= Result . base_dpi;

	ChangeDrawMode(Result . transfer,Result . rendering,FALSE);

	SpecialMap . hoffset	= (LONG)((float)Message -> hresolution * Result . hoffset * Result . DVI_mag / 1000.0 + 0.5);
	SpecialMap . voffset	= (LONG)((float)Message -> vresolution * Result . voffset * Result . DVI_mag / 1000.0 + 0.5);

	Message -> ret	= 0;
	Message -> bmap	= &SpecialMap;

	SpecialMap . loc . map	= NULL;
	SpecialMap . where_is	= LOC_NONE;

	if(Result . iffile[0])
	{
		if(Configuration . draw_modus == DRAW_BORDER || Configuration . draw_modus == DRAW_RECT)
		{
			if(GetImageSize(Result . iffile,&Width,&Height,&Error))
			{
				LONG NewWidth,NewHeight,DPI_X = Configuration . base_dpi,DPI_Y = Configuration . base_dpi;

				SpecialMap . width	= Width;
				SpecialMap . height	= Height;

				if(Configuration . draw_modus == DRAW_BORDER)
					SpecialMap . where_is = LOC_BORDER;
				else
					SpecialMap . where_is = LOC_RECTANGLE;

				GetImageDPI(Result . iffile,&DPI_X,&DPI_Y,NULL);

				GetNewSize(&SpecialMap, &Result, &NewWidth, &NewHeight, DPI_X, DPI_Y);

				SpecialMap . width	= NewWidth;
				SpecialMap . height	= NewHeight;
			}
			else
			{
				STRPTR Result = ShowError(Error,NULL,TRUE);

				if(Result)
					PrintLine("\33bCannot determine picture size: %s.\33n",Result);
				else
					PrintLine("\33bCannot determine picture size.\33n");

				Message -> ret = 5;
			}
		}
		else
		{
			UBYTE			 FileName[MAX_FILENAME_LEN];
			struct GreyImage	*Image;
			LONG			 i;

			if(Configuration . draw_modus == DRAW_FILE || Configuration . draw_modus == DRAW_FILE_B)
			{
				LONG	DateResult;
				BPTR	File;
				LONG	Error;

				if(!BuildName(FileName,Home,Result . iffile,Result . hres,Result . vres,Sum1,&Error))
				{
					STRPTR Result = ShowError(Error,NULL,TRUE);

					if(Result)
						PrintLine("\33bCannot store converted image: %s.\33n",Result);
					else
						PrintLine("\33bCannot store converted image.\33n");

					Message -> ret = 5;

					memcpy(&Configuration,&ConfigBackup,sizeof(struct config_struct));

					if(OldDir)
						CurrentDir(OldDir);

					if(NewDir)
						UnLock(NewDir);

					return(NULL);
				}

				Sum2 = Sum1 + 1;

				if(File = Open(FileName,MODE_OLDFILE))
				{
					LONG Magic[2];

					if(Read(File,Magic,sizeof(Magic)))
					{
						if(Magic[0] == MAGIC_WORD)
							Sum2 = Magic[1];
					}

					Close(File);
				}

				DateResult = FileDateCheck(Result . iffile,FileName,&Error);

				if(Error || DateResult < 0) /* (ghi) fixed */
					Sum2 = Sum1 + 1;

				if(Sum1 == Sum2)
				{
					STATIC UBYTE BitMapFileName[MAX_FILENAME_LEN];

					if(Configuration . draw_modus == DRAW_FILE)
						SpecialMap . where_is = LOC_FILE;
					else
						SpecialMap . where_is = LOC_FILE_BORDER;

					strcpy(BitMapFileName,FileName);

					SpecialMap . loc . filename = BitMapFileName;

					PrintLine("Done.");
					PrintLine("");

					memcpy(&Configuration,&ConfigBackup,sizeof(struct config_struct));

					if(OldDir)
						CurrentDir(OldDir);

					if(NewDir)
						UnLock(NewDir);

					return(NULL);
				}
			}

			InvertChanged = FALSE;

			for(i = 0 ; i < 256 ; i++)
				Filter[i] = i;

			BrightnessFilter(Filter,Result . bright);
			ContrastFilter(Filter,Result . contrast);
			GammaFilter(Filter,Result . gamma);

			if(Result . red < 1 && Result . green < 1 && Result . blue < 1)
			{
				LONG R,G,B,Delta1,Delta2;

				for(i = 0 ; i < 256 ; i++)
				{
					R = (i * 65536 * 299) / 1000;
					G = (i * 65536 * 587) / 1000;
					B = (i * 65536 * 114) / 1000;

					Delta1 = i * 65536 - (R + G + B);
					Delta2 = Delta1 / 2;

					LumR[i] = R + Delta2;
					LumG[i] = G + Delta1 - 2 * Delta2;
					LumB[i] = B + Delta2;
				}
			}
			else
			{
				LONG R,G,B;

				R = Result . red;
				G = Result . green;
				B = Result . blue;

				while(R + G + B > 100)
				{
					if(G > 33 && R + G + B > 100)
						G--;

					if(R > 33 && R + G + B > 100)
						R--;

					if(B > 33 && R + G + B > 100)
						B--;
				}

				for(i = 0 ; i < 256 ; i++)
				{
					LumR[i] = ((i * 65536) * R) / 100;
					LumG[i] = ((i * 65536) * G) / 100;
					LumB[i] = ((i * 65536) * B) / 100;
				}
			}

			PrintLine("Loading picture from \"%s\"...",Result . iffile);

			if(Image = ReadImage(Result . iffile,Result . patch_colours,&Error))
			{
				LONG NewWidth,NewHeight,DPI_X = Configuration . base_dpi,DPI_Y = Configuration . base_dpi;

				Error = 0;

				SpecialMap . width	= Image -> Width;
				SpecialMap . height	= Image -> Height;

				GetImageDPI(Result . iffile,&DPI_X,&DPI_Y,NULL);

				GetNewSize(&SpecialMap, &Result, &NewWidth, &NewHeight, DPI_X, DPI_Y);

				if(NewWidth > 32768 || NewHeight > 32768)
					Error = ERR_TOO_LARGE;
				else
				{
					PrintLine("Processing picture (size %d × %d)...",NewWidth,NewHeight);

					SetMaxProgress(NewHeight - 1);

					switch(Result . mode)
					{
						case BandW:

							Error = ScaleImage(&BitMap,Image,NewWidth,NewHeight);
							break;

						case FS:

							Error = DitherImage_FS(&BitMap,Image,NewWidth,NewHeight);
							break;

						case Burkes:

							Error = DitherImage_Burkes(&BitMap,Image,NewWidth,NewHeight);
							break;

						case Sierra:

							Error = DitherImage_Sierra(&BitMap,Image,NewWidth,NewHeight);
							break;

						case JJN:

							Error = DitherImage_JJN(&BitMap,Image,NewWidth,NewHeight);
							break;

						case StevensonArce:

							Error = DitherImage_Stevenson_Arce(&BitMap,Image,NewWidth,NewHeight);
							break;

						case Stucki:

							Error = DitherImage_Stucki(&BitMap,Image,NewWidth,NewHeight);
							break;

						case BlueNoise:

							Error = DitherImage_BlueNoise(&BitMap,Image,NewWidth,NewHeight,Result . threshold);
							break;

						case Ordered:

							Error = DitherImage_Matrix(&BitMap,Image,NewWidth,NewHeight,OrderedVanilla);
							break;

						case Halftone:

							switch(Result . dither_opt)
							{
								case 4:	Error = DitherImage_Matrix(&BitMap,Image,NewWidth,NewHeight,OrderedHalftone4);
									break;

								case 8:	Error = DitherImage_Matrix(&BitMap,Image,NewWidth,NewHeight,OrderedHalftone8);
									break;

								default:Error = DitherImage_Matrix(&BitMap,Image,NewWidth,NewHeight,OrderedHalftone);
									break;
							}

							break;

						case RandomNoise:

							Error = DitherImage_Matrix(&BitMap,Image,NewWidth,NewHeight,NULL);
							break;

						case BckBrick:

							switch(Result . dither_opt)
							{
								case 4:	Error = DitherImage_Matrix(&BitMap,Image,NewWidth,NewHeight,OrderedBckBrick4);
									break;

								case 8:	Error = DitherImage_Matrix(&BitMap,Image,NewWidth,NewHeight,OrderedBckBrick8);
									break;

								default:Error = DitherImage_Matrix(&BitMap,Image,NewWidth,NewHeight,OrderedBckBrick);
									break;
							}

							break;

						case FwdBrick:

							switch(Result . dither_opt)
							{
								case 4:	Error = DitherImage_Matrix(&BitMap,Image,NewWidth,NewHeight,OrderedFwdBrick4);
									break;

								case 8:	Error = DitherImage_Matrix(&BitMap,Image,NewWidth,NewHeight,OrderedFwdBrick8);
									break;

								default:Error = DitherImage_Matrix(&BitMap,Image,NewWidth,NewHeight,OrderedFwdBrick);
									break;
							}

							break;

						case Hexagon:

							Error = DitherImage_Matrix(&BitMap,Image,NewWidth,NewHeight,OrderedHexagonalCluster);
							break;

						case SpiralDot:

							switch(Result . dither_opt)
							{
								case 4:	Error = DitherImage_Matrix(&BitMap,Image,NewWidth,NewHeight,OrderedSpiralDot4);
									break;

								case 8:	Error = DitherImage_Matrix(&BitMap,Image,NewWidth,NewHeight,OrderedSpiralDot8);
									break;

								default:Error = DitherImage_Matrix(&BitMap,Image,NewWidth,NewHeight,OrderedSpiralDot);
									break;
							}

							break;

						case Horizontal:

							Error = DitherImage_Matrix(&BitMap,Image,NewWidth,NewHeight,OrderedHorizontal);
							break;
					}

					if(!BitMap)
						Error = ERR_NO_MEM;
				}

				if(BitMap)
				{
					if(Configuration . draw_modus == DRAW_IN_MEM_B || Configuration . draw_modus == DRAW_FILE_B)
						SpecialMap . where_is = LOC_BITMAP_BORDER;
					else
						SpecialMap . where_is = LOC_BITMAP;

					SpecialMap . width	= NewWidth;
					SpecialMap . height	= NewHeight;
					SpecialMap . loc . map	= (ULONG *)BitMap -> Planes[0];

					if(Configuration . invert_bmap)
						BltBitMap(BitMap,0,0,BitMap,0,0,NewWidth,NewHeight,0x50,1,NULL);

					if(Configuration . draw_modus == DRAW_FILE || Configuration . draw_modus == DRAW_FILE_B)
					{
						struct AsyncFile *File;

						PrintLine("Saving picture...");

						Error = 0;

						if(File = OpenAsync(FileName,MODE_WRITE,2 * 16384))
						{
							ULONG Magic[4];

							Magic[0] = MAGIC_WORD;
							Magic[1] = Sum1;
							Magic[2] = SpecialMap . width;
							Magic[3] = SpecialMap . height;

							if(WriteAsync(File,Magic,sizeof(Magic)) == sizeof(Magic))
							{
								PLANEPTR	Plane = BitMap -> Planes[0];
								LONG		i;

								SetMaxProgress(BitMap -> Rows - 1);

								for(i = 0 ; i < BitMap -> Rows ; i++)
								{
									if(WriteAsync(File,Plane,BitMap -> BytesPerRow) != BitMap -> BytesPerRow)
									{
										Error = IoErr();

										break;
									}
									else
									{
										Plane += BitMap -> BytesPerRow;

										ShowProgress(i);
									}
								}
							}
							else
								Error = IoErr();

							CloseAsync(File);
						}
						else
							Error = IoErr();

						if(Error)
						{
							STRPTR Result = ShowError(Error,NULL,TRUE); /* (ghi) fixed */

							if(Result)
								PrintLine("\33bError saving picture: %s.\33n",Result);
							else
								PrintLine("\33bError saving picture.\33n");

							DeleteFile(FileName);
						}
						else
							AddProtection(FileName,FIBF_EXECUTE);
					}

				}

				if(!BitMap)
				{
					if(Error)
					{
						STRPTR Result = ShowError(Error,NULL,TRUE);

						if(Result)
							PrintLine("\33b%s.\33n",Result);
					}

					Message -> ret = 5;
				}

				DeleteImage(Image);
			}
			else
			{
				STRPTR Result = ShowError(Error,NULL,TRUE);

				if(Result)
					PrintLine("\33bError loading picture: %s.\33n",Result);
				else
					PrintLine("\33bError loading picture.\33n");

				Message -> ret = 5;
			}
		}
	}

	if(Result . psfile[0]) /* (ghi) fixed; `psfile' instead of `iffile' */
	{
		LONG PS_hoff, PS_voff;
		PSGetSizeDot(&Result, &SpecialMap . width, &SpecialMap . height, &PS_hoff, &PS_voff, &SpecialMap . hoffset, &SpecialMap . voffset); /* (ghi) */

		if(Configuration . draw_modus == DRAW_BORDER || Configuration . draw_modus == DRAW_RECT)
		{
			if(Configuration . draw_modus == DRAW_BORDER)
				SpecialMap . where_is = LOC_BORDER;
			else
				SpecialMap . where_is = LOC_RECTANGLE;
		}
		else
		{
			UBYTE FileName[MAX_FILENAME_LEN];

			if(Configuration . draw_modus == DRAW_FILE || Configuration . draw_modus == DRAW_FILE_B)
			{
				LONG	DateResult;
				BPTR	File;
				LONG	Error;

				if(!BuildName(FileName,Home,Result . psfile,Result . hres,Result . vres,Sum1,&Error))
				{
					STRPTR Result = ShowError(Error,NULL,TRUE);

					if(Result)
						PrintLine("\33bCannot store converted image: %s.\33n",Result);
					else
						PrintLine("\33bCannot store converted image.\33n");

					Message -> ret = 5;

					memcpy(&Configuration,&ConfigBackup,sizeof(struct config_struct));

					if(OldDir)
						CurrentDir(OldDir);

					if(NewDir)
						UnLock(NewDir);

					return(NULL);
				}

				Sum2 = Sum1 + 1;

				if(File = Open(FileName,MODE_OLDFILE))
				{
					LONG Magic[2];

					if(Read(File,Magic,sizeof(Magic)))
					{
						if(Magic[0] == MAGIC_WORD)
							Sum2 = Magic[1];
					}

					Close(File);
				}

				DateResult = FileDateCheck(Result . psfile,FileName,&Error);

				if(Error || DateResult < 0) /* (ghi) fixed */
					Sum2 = Sum1 + 1;

				if(Sum1 == Sum2)
				{
					STATIC UBYTE BitMapFileName[MAX_FILENAME_LEN];

					if(Configuration . draw_modus == DRAW_FILE)
						SpecialMap . where_is = LOC_FILE;
					else
						SpecialMap . where_is = LOC_FILE_BORDER;

					strcpy(BitMapFileName,FileName);

					SpecialMap . loc . filename = BitMapFileName;

					if (psfig_status == PSFIG_END)
						psfig_status = PSFIG_OFF; /* re-init psfig_status */

					PrintLine("Done.");
					PrintLine("");

					memcpy(&Configuration,&ConfigBackup,sizeof(struct config_struct));

					if(OldDir)
						CurrentDir(OldDir);

					if(NewDir)
						UnLock(NewDir);

					return(NULL);
				}
			}

			InvertChanged = FALSE;

			if(BitMap = ProcessPostscript(Message,&SpecialMap,&Result,&Error))
			{
				if(Configuration . draw_modus == DRAW_IN_MEM_B || Configuration . draw_modus == DRAW_FILE_B)
					SpecialMap . where_is = LOC_BITMAP_BORDER;
				else
					SpecialMap . where_is = LOC_BITMAP;

				SpecialMap . loc . map = (ULONG *)BitMap -> Planes[0];

				if(Configuration . invert_bmap)
					InvertBitMap(BitMap,SpecialMap . width,SpecialMap . height);

				if(Configuration . draw_modus == DRAW_FILE || Configuration . draw_modus == DRAW_FILE_B)
				{
					struct AsyncFile *File;

					PrintLine("Saving picture...");

					Error = 0;

					if(File = OpenAsync(FileName,MODE_WRITE,2 * 16384))
					{
						ULONG Magic[4];

						Magic[0] = MAGIC_WORD;
						Magic[1] = Sum1;
						Magic[2] = SpecialMap . width;
						Magic[3] = SpecialMap . height;

						if(WriteAsync(File,Magic,sizeof(Magic)) == sizeof(Magic))
						{
							PLANEPTR	Plane = BitMap -> Planes[0];
							LONG		i;

							SetMaxProgress(BitMap -> Rows - 1);

							for(i = 0 ; i < BitMap -> Rows ; i++)
							{
								if(WriteAsync(File,Plane,BitMap -> BytesPerRow) != BitMap -> BytesPerRow)
								{
									Error = IoErr();

									break;
								}
								else
								{
									Plane += BitMap -> BytesPerRow;

									ShowProgress(i);
								}
							}
						}
						else
							Error = IoErr();

						CloseAsync(File);
					}
					else
						Error = IoErr();

					if(Error)
					{
						STRPTR Result = ShowError(Error,NULL,TRUE); /* (ghi) fixed */

						if(Result)
							PrintLine("\33bError saving picture: %s.\33n",Result);
						else
							PrintLine("\33bError saving picture.\33n");

						DeleteFile(FileName);
					}
					else
						AddProtection(FileName,FIBF_EXECUTE);
				}

			}
			else
			{
				if(Error)
				{
					STRPTR Result = ShowError(Error,NULL,TRUE);

					if(Result)
						PrintLine("\33b%s.\33n",Result);
				}

				Message -> ret = 5;
			}
		}
	}

	memcpy(&Configuration,&ConfigBackup,sizeof(struct config_struct));

	PrintLine("Done.");
	PrintLine("");

	ShowProgress(0);

	if(OldDir)
		CurrentDir(OldDir);

	if(NewDir)
		UnLock(NewDir);

	return(BitMap);
}

VOID __saveds
HandleInput()
{
	if(OpenAll())
	{
		if(UseGUI)
		{
			if(OpenGUI())
			{
				BOOL	Done = FALSE;
				ULONG	SignalMask,Signals;
				LONG	Value;

				SignalMask = Signals = NULL;

				do
				{
					if(Signals & SIGBREAKF_CTRL_C)
						Done = TRUE;

					switch(DoMethod(AP_Application,MUIM_Application_Input,&SignalMask))
					{
						case MEN_QUIT:
						case MUIV_Application_ReturnID_Quit:

							Done = TRUE;
							break;

						case GAD_TRANSFER:

							ChangeDrawMode(0,0,TRUE);
							break;

						case GAD_RENDER:

							ChangeDrawMode(0,0,TRUE);
							break;

						case GAD_INVERT:

							get(CM_Invert,MUIA_Selected,&Value);

							Configuration . invert_bmap = Value;

							InvertChanged = TRUE;

							break;

						case GAD_BASEDPI:

							get(ST_BaseDPI,MUIA_String_Integer,&Value);

							if(!Value)
								set(ST_BaseDPI,MUIA_String_Integer,Configuration . base_dpi);
							else
								Configuration . base_dpi = Value;

							break;

						case GAD_JUMP:

							Configuration . use_pubscr = TRUE;

							SignalMask = NULL;

							ThisProcess -> pr_WindowPtr = OldPtr;

							MUI_DisposeObject(AP_Application);

							AP_Application = NULL;

							if(PubScreen)
							{
								UnlockPubScreen(NULL,PubScreen);

								PubScreen = NULL;
							}

							if(!OpenGUI())
							{
								ShowError(ERR_NO_GUI,NULL,FALSE);

								Done = TRUE;
							}
							else
								DoMethod(WI_Main,MUIM_Window_ScreenToFront,NULL);

							break;

						case GAD_SHOW:

							{
								struct Process *ShowDVI;

								Forbid();

								if(ShowDVI = (struct Process *)FindTask("ShowDVI-Task"))
								{
									Signal(ShowDVI,SIGBREAKF_CTRL_E);

									Permit();
								}
								else
								{
									Permit();

									PrintLine("ShowDVI is not currently running.");
								}
							}

							break;

						case GAD_CLEAR:

							DoMethod(LV_Messages,MUIM_List_Clear,0);

							ClearList(&MessageList);

							break;

						case MEN_ABOUT:

							MUI_Request(AP_Application,WI_Main,0,NULL,"Ok",
								"\33c\33\70"
								VERS
								"\33\62\n\n"
								"Last compiled "
								DATE
								"\n"
								"Copyright © 1993-1994 by Olaf `Olsen' Barthel & Georg Heßmann\n"
								"Improvements to PostScript support by Giuseppe Ghibò\n\n"
								"This is a MUI-Application.\n"
								"MUI is copyrighted by Stefan Stuntz.");

							break;

						case MEN_OPEN:

							SetSleep(TRUE);

							if(ReadConfig(&Configuration))
							{
								SignalMask = NULL;

								ThisProcess -> pr_WindowPtr = OldPtr;

								MUI_DisposeObject(AP_Application);

								AP_Application = NULL;

								if(PubScreen)
								{
									UnlockPubScreen(NULL,PubScreen);

									PubScreen = NULL;
								}

								if(!OpenGUI())
								{
									ShowError(ERR_NO_GUI,NULL,FALSE);

									Done = TRUE;
								}
								else
									DoMethod(WI_Main,MUIM_Window_ScreenToFront,NULL);
							}
							else
							{
								SetSleep(FALSE);

								switch(Configuration . draw_modus)
								{
									case DRAW_IN_MEM:

										set(CY_Transfer,	MUIA_Cycle_Active,CYID_Transfer_Memory);
										set(CY_Render,		MUIA_Cycle_Active,CYID_Render_None);
										break;

									case DRAW_FILE:

										set(CY_Transfer,	MUIA_Cycle_Active,CYID_Transfer_Disk);
										set(CY_Render,		MUIA_Cycle_Active,CYID_Render_None);
										break;

									case DRAW_IN_MEM_B:

										set(CY_Transfer,	MUIA_Cycle_Active,CYID_Transfer_Memory);
										set(CY_Render,		MUIA_Cycle_Active,CYID_Render_Frame);
										break;

									case DRAW_FILE_B:

										set(CY_Transfer,	MUIA_Cycle_Active,CYID_Transfer_Disk);
										set(CY_Render,		MUIA_Cycle_Active,CYID_Render_Frame);
										break;

									case DRAW_BORDER:

										set(CY_Transfer,	MUIA_Cycle_Active,CYID_Transfer_None);
										set(CY_Render,		MUIA_Cycle_Active,CYID_Render_Frame);
										break;

									case DRAW_RECT:

										set(CY_Transfer,	MUIA_Cycle_Active,CYID_Transfer_None);
										set(CY_Render,		MUIA_Cycle_Active,CYID_Render_Clear);
										break;
								}

								set(CM_Invert,MUIA_Selected,Configuration . invert_bmap);

								InvertChanged = TRUE;

								set(ST_BaseDPI,MUIA_String_Integer,Configuration . base_dpi);
							}

							break;

						case MEN_SAVE:

							SetSleep(TRUE);

							SaveConfig(&Configuration);

							SetSleep(FALSE);

							break;

						case MEN_PUBSCREEN:

							{
								struct Window	*Window;
								BOOL		 Reopen = FALSE;

								if(get(WI_Main,MUIA_Window_Window,&Window))
								{
									if(Window)
									{
										struct MenuItem *Item;

										if(Item = ItemAddress(Window -> MenuStrip,FULLMENUNUM(0,3,NOSUB)))
										{
											if(Item -> Flags & CHECKED)
											{
												if(!Configuration . use_pubscr)
													Reopen = Configuration . use_pubscr = TRUE;
											}
											else
											{
												if(Configuration . use_pubscr)
												{
													Reopen				= TRUE;
													Configuration . use_pubscr	= FALSE;
												}
											}
										}
									}
								}

								if(Reopen)
								{
									SignalMask = NULL;

									ThisProcess -> pr_WindowPtr = OldPtr;

									MUI_DisposeObject(AP_Application);

									AP_Application = NULL;

									if(PubScreen)
									{
										UnlockPubScreen(NULL,PubScreen);

										PubScreen = NULL;
									}

									if(!OpenGUI())
									{
										ShowError(ERR_NO_GUI,NULL,FALSE);

										Done = TRUE;
									}
									else
										DoMethod(WI_Main,MUIM_Window_ScreenToFront,NULL);
								}
							}

							break;
					}

					if(!Done)
					{
						if(Signals & SIGBREAKF_CTRL_E)
						{
							Signals = NULL;

							Configuration . use_pubscr = FALSE;

							SignalMask = NULL;

							ThisProcess -> pr_WindowPtr = OldPtr;

							MUI_DisposeObject(AP_Application);

							AP_Application = NULL;

							if(PubScreen)
							{
								UnlockPubScreen(NULL,PubScreen);

								PubScreen = NULL;
							}

							if(!OpenGUI())
							{
								ShowError(ERR_NO_GUI,NULL,FALSE);

								Done = TRUE;
							}
							else
								DoMethod(WI_Main,MUIM_Window_ScreenToFront,NULL);
						}

						if(Signals & PORTMASK(MainPort))
						{
							struct special_msg	*Message;
							struct BitMap		*BitMap;

							while(Message = (struct special_msg *)GetMsg(MainPort))
							{
								switch(Message -> action)
								{
									case AC_SEND_SPECIAL:

										SetSleep(TRUE);

										BitMap = ProcessImage(Message);

										Message -> action = AC_REPLY_SPECIAL;

										ReplyMsg((struct Message *)Message);

										WaitPort(MainPort);

										Message = (struct special_msg *)GetMsg(MainPort);

										if(Message -> action != AC_OK_BITMAP)
											PrintLine("\33bExpected AC_OK_BITMAP but found %d!\33n",Message -> action);

										Message -> action	= AC_REPLY_BITMAP;
										Message -> ret		= 0;

										ReplyMsg((struct Message *)Message);

										if(BitMap)
											DeleteBitMap(BitMap);

										SetSleep(FALSE);

										break;

									case AC_SEND_TPIC:

										SetSleep(TRUE);

										work_with_tpic(Message -> tpic,Message -> dmap,Message -> hresolution,Message -> vresolution);

										Message -> action	= AC_REPLY_TPIC;
										Message -> ret		= 0;

										ReplyMsg((struct Message *)Message);

										SetSleep(FALSE);

										break;

									default:

										PrintLine("\33bUnknown action Nº%d.\33n",Message -> action);

										Message -> action	= AC_REPLY_UNKNOWN;
										Message -> ret		= 0;

										ReplyMsg((struct Message *)Message);

										break;
								}
							}
						}

						if(!Done)
						{
							if(SignalMask)
								Signals = Wait(SignalMask | PORTMASK(MainPort) | SIGBREAKF_CTRL_C | SIGBREAKF_CTRL_E);
							else
								Signals = CheckSignal(PORTMASK(MainPort) | SIGBREAKF_CTRL_C | SIGBREAKF_CTRL_E);
						}
					}
				}
				while(!Done);

				ThisProcess -> pr_WindowPtr = OldPtr;

				MUI_DisposeObject(AP_Application);

				AP_Application = NULL;

				if(PubScreen)
				{
					UnlockPubScreen(NULL,PubScreen);

					PubScreen = NULL;
				}
			}
			else
				ShowError(ERR_NO_GUI,NULL,FALSE);
		}
		else
		{
			ULONG Signals;

			FOREVER
			{
				Signals = Wait(PORTMASK(MainPort) | SIGBREAKF_CTRL_C);

				if(Signals & SIGBREAKF_CTRL_C)
					break;
				else
				{
					if(Signals & PORTMASK(MainPort))
					{
						struct special_msg	*Message;
						struct BitMap		*BitMap;

						while(Message = (struct special_msg *)GetMsg(MainPort))
						{
							switch(Message -> action)
							{
								case AC_SEND_SPECIAL:

									BitMap = ProcessImage(Message);

									Message -> action = AC_REPLY_SPECIAL;

									ReplyMsg((struct Message *)Message);

									WaitPort(MainPort);

									Message = (struct special_msg *)GetMsg(MainPort);

									if(Message -> action != AC_OK_BITMAP)
										PrintLine("\33bExpected AC_OK_BITMAP but found %d!\33n",Message -> action);

									Message -> action	= AC_REPLY_BITMAP;
									Message -> ret		= 0;

									ReplyMsg((struct Message *)Message);

									if(BitMap)
										DeleteBitMap(BitMap);

									break;

								case AC_SEND_TPIC:

									work_with_tpic(Message -> tpic,Message -> dmap,Message -> hresolution,Message -> vresolution);

									Message -> action	= AC_REPLY_TPIC;
									Message -> ret		= 0;

									ReplyMsg((struct Message *)Message);

									break;

								default:

									PrintLine("\33bUnknown action Nº%d.\33n",Message -> action);

									Message -> action	= AC_REPLY_UNKNOWN;
									Message -> ret		= 0;

									ReplyMsg((struct Message *)Message);

									break;
							}
						}
					}
				}
			}
		}
	}

	CloseAll();
}

#pragma libcall GfxBase WritePixelLine8 306 9A210806
#pragma libcall GfxBase ReadPixelLine8 300 9a210806

VOID __regargs
SmallWriteLine(struct RastPort *RPort,LONG Line,LONG Width,UBYTE *Buffer,struct RastPort *Temp)
{
	if(Width > 1008)
	{
		LONG	Size	= 1008,
			Offset	= 0;

		do
		{
			WritePixelLine8(RPort,Offset,Line,Size,Buffer,Temp);

			Buffer	+= Size;
			Offset	+= Size;
			Width	-= Size;

			if(Width > 1008)
				Size = 1008;
			else
				Size = Width;
		}
		while(Width);
	}
	else
		WritePixelLine8(RPort,0,Line,Width,Buffer,Temp);
}

VOID __regargs
SmallReadLine(struct RastPort *RPort,LONG Line,LONG Width,UBYTE *Buffer,struct RastPort *Temp)
{
	if(Width > 1008)
	{
		LONG	Size	= 1008,
			Offset	= 0;

		do
		{
			ReadPixelLine8(RPort,Offset,Line,Size,Buffer,Temp);

			Buffer	+= Size;
			Offset	+= Size;
			Width	-= Size;

			if(Width > 1008)
				Size = 1008;
			else
				Size = Width;
		}
		while(Width);
	}
	else
		ReadPixelLine8(RPort,0,Line,Width,Buffer,Temp);
}

VOID __regargs
LargeWriteLine(struct RastPort *RPort,LONG Line,LONG Width,UBYTE *Buffer,struct RastPort *Temp)
{
	if(Width > 32752)
	{
		LONG	Size	= 32752,
			Offset	= 0;

		do
		{
			WritePixelLine8(RPort,Offset,Line,Size,Buffer,Temp);

			Buffer	+= Size;
			Offset	+= Size;
			Width	-= Size;

			if(Width > 32752)
				Size = 32752;
			else
				Size = Width;
		}
		while(Width);
	}
	else
		WritePixelLine8(RPort,0,Line,Width,Buffer,Temp);
}

VOID __regargs
LargeReadLine(struct RastPort *RPort,LONG Line,LONG Width,UBYTE *Buffer,struct RastPort *Temp)
{
	if(Width > 32752)
	{
		LONG	Size	= 32752,
			Offset	= 0;

		do
		{
			ReadPixelLine8(RPort,Offset,Line,Size,Buffer,Temp);

			Buffer	+= Size;
			Offset	+= Size;
			Width	-= Size;

			if(Width > 32752)
				Size = 32752;
			else
				Size = Width;
		}
		while(Width);
	}
	else
		ReadPixelLine8(RPort,0,Line,Width,Buffer,Temp);
}

/* (ghi) Init_Transfer_Render(): given the draw_modus returns Transfer and
   Render, i.e. performs the reverse operation of the function
   ChangeDrawMode() */

STATIC VOID __regargs
Init_Transfer_Render(LONG *Transfer, LONG *Render, int draw_modus)
{
	switch (draw_modus)
	{

		case DRAW_IN_MEM:

			*Transfer = CYID_Transfer_Memory;
			*Render = CYID_Render_None;
			break;

		case DRAW_IN_MEM_B:

			*Transfer = CYID_Transfer_Memory;
			*Render = CYID_Render_Frame;
			break;

		case DRAW_FILE:

			*Transfer = CYID_Transfer_Disk;
			*Render = CYID_Render_None;
			break;

		case DRAW_FILE_B:

			*Transfer = CYID_Transfer_Disk;
			*Render = CYID_Render_Frame;
			break;

		case DRAW_BORDER:

			*Transfer = CYID_Transfer_None;
			*Render = CYID_Render_Frame;
			break;

		case DRAW_RECT:

			*Transfer = CYID_Transfer_None;
			*Render = CYID_Render_Clear;
			break;

		default:

			*Transfer = CYID_Transfer_Memory;
			*Render = CYID_Render_None;
			break;

	}

}
