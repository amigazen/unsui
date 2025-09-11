/*
Auto:		sc <file>
*/

/* $Revision Header built automatically *************** (do not edit) ************
**
** © Copyright by GuntherSoft
**
** File             : SnakeSYS:CPrgs/Utils/Man.c
** Created on       : Friday, 16.07.93 18:00:23
** Created by       : Kai Iske
** Current revision : V1.11
**
**
** Purpose
** -------
**   - This is a man program, which may scan different directories
**     for man-pages. These directories are set within an ENV-VAR
**     called MANPATHS. Additionally a VIEWER may be set using
**     ENV-VAR MANVIEW (for plain ASCII) or MANVIEWAG (for
**     AMIGAGUIDE). THIS ONE`S PURE AND IN THE PUBLIC DOMAIN
**
** Revision V1.11
** --------------
** created on Monday, 28.02.94 19:33:29  by  Kai Iske.   LogMessage :
**  -*-  changed on Saturday, 05.03.94 02:30:54  by  Kai Iske.   LogMessage :
**   - Man produced an enforcer hit when no manpage was supplied
**     (Reported by : Michael van Elst)
**  -*-  changed on Monday, 28.02.94 20:03:34  by  Kai Iske.   LogMessage :
**   - Man will no longer issue an error if you a) didn`t specify a
**     manpage and b) set the MANGETFILE attribute for the File
**     Requester. This way the Requester will popup if you
**     call MAN without any arguments
**     (Requested by : Bill Hogsett)
**  -*-  changed on Monday, 28.02.94 20:02:39  by  Kai Iske.   LogMessage :
**   - The FileRequester will pop up on the default PubScreen now
**     (Requested by : Bill Hogsett)
**   - The default path for the FileRequester will be set to the
**     first Dir of the MANPATHS now
**     (Requested by : Bill Hogsett)
**  -*-  created on Monday, 28.02.94 19:33:29  by  Kai Iske.   LogMessage :
**   - Support for DVI files added
**   - MAN will set the Viewer`s current dir of that where the
**     man page resides in
**
** Revision V1.10
** --------------
** created on Tuesday, 25.01.94 14:41:58  by  Kai Iske.   LogMessage :
**   - Mike Barsoom added MANBASENAME attribute for stripping
**     off any path names from the manpage name. Useful when
**     launching MAN from within (ie) ToolsDaemon
**     (Submitted by : Mike Barsoom)
**
** Revision V1.9
** --------------
** created on Sunday, 23.01.94 21:36:26  by  Kai Iske.   LogMessage :
**   - Recompiled using SAS/C 6.51
**   - Added MANGETFILE parameter
**     (Requested by : Bill Hogsett)
**
** Revision V1.8
** --------------
** created on Saturday, 15.01.94 01:14:17  by  Kai Iske.   LogMessage :
**   - Man now correctly handles multi-assigns, since ExAll
**     doesn`t....;)
**     (Reported by : Jan Hoeydahl)
**
** Revision V1.7
** --------------
** created on Friday, 31.12.93 14:00:28  by  Kai Iske.   LogMessage :
**   - All config variables have been moved to a single one,
**     which will be parsed just like a CommandLine
**     (Suggested by : Michael 'Mick' Hohmann)
**
** Revision V1.6
** --------------
** created on Thursday, 16.12.93 16:49:51  by  Kai Iske.   LogMessage :
**   - Added MANRMEXT, MANAGEXT and MANNOVIEW options,
**     which may be used for extensibility of MAN
**     (Somehow suggested by : Michael 'Mick' Hohmann)
**
** Revision V1.5
** --------------
** created on Thursday, 09.12.93 00:58:47  by  Kai Iske.   LogMessage :
**   - Referenced free memory area
**
** Revision V1.4
** --------------
** created on Wednesday, 08.12.93 22:07:47  by  Kai Iske.   LogMessage :
**  -*-  created on Wednesday, 08.12.93 22:07:47  by  Kai Iske.   LogMessage :
**   - DOS-Library wasn`t closed
**
** Revision V1.3
** --------------
** created on Wednesday, 08.12.93 19:26:17  by  Kai Iske.   LogMessage :
**  -*-  changed on Wednesday, 08.12.93 19:31:23  by  Kai Iske.   LogMessage :
**   - Added CTRL-C checking
**  -*-  created on Wednesday, 08.12.93 19:26:17  by  Kai Iske.   LogMessage :
**   - Recompiled using SAS 6.50
**   - Reduced stack usage
**   - Reduced executable size
**
** Revision V1.2
** --------------
** created on Tuesday, 27.07.93 15:20:51  by  Kai Iske.   LogMessage :
**   - Used MAN as a keyword for the template which prevented MAN
**     to accept inputs like "man man".
**
** Revision V1.1
** --------------
** created on Monday, 26.07.93 17:42:32  by  Kai Iske.   LogMessage :
**   - Accidentially called Exit() instead of exit(), which
**     prevented the program to pass the cleanup code of SAS.
**     So a lock to the directory was kept and the shell could
**     never been left.............
**
** Revision V1.0
** --------------
** created on Friday, 16.07.93 18:00:23  by  Kai Iske.   LogMessage :
**  -*-  changed on Saturday, 17.07.93 16:30:41  by  Kai Iske.   LogMessage :
**   - Man now searches for files that end up with .doc/.man/.guide
**     If a .guide file is found, the second Viewer (MANVIEWAG)
**     will be used to display the AmigaGuide viewer. Otherwise
**     the normal viewer (MANVIEW) will be used
**  -*-  created on Friday, 16.07.93 18:00:23  by  Kai Iske.   LogMessage :
**     --- Initial release ---
**
*********************************************************************************/
#define REVISION "1.11a"
#define REVDATE  "05.03.94"
#define REVTIME  "02:30:54"
#define AUTHOR   "Kai Iske"
#define VERNUM   1
#define REVNUM   11



#define		_USE_SYSBASE



/**********************************************************************/
/*         This is, so that the code references our Libraries         */
/*                      stored in our structure                       */
/**********************************************************************/
#define		DOSBase		MC->mc_DOSBase
#define		UtilityBase	MC->mc_UtilityBase


#include	<string.h>
#include	<stdlib.h>
#include	<exec/types.h>
#include	<exec/memory.h>
#include	<exec/execbase.h>
#include	<libraries/asl.h>
#include	<dos/dos.h>
#include	<dos/exall.h>
#include	<dos/dostags.h>
#include	<proto/exec.h>
#include	<proto/asl.h>
#include	<clib/dos_protos.h>
#include	<clib/utility_protos.h>
#include	<pragmas/dos_pragmas.h>
#include	<pragmas/utility_pragmas.h>




/**********************************************************************/
/*             This is the private structure for all vars             */
/**********************************************************************/
struct ManControl
{
	struct	DOSLibrary	*mc_DOSBase;
	struct	Library		*mc_UtilityBase;
	struct	ExAllControl	*mc_EAC;
	char			mc_ManOpt[1536],
				mc_ManPaths[1024],
				mc_ViewCmd[1024],
				mc_ViewCmdAG[1024],
				mc_ViewCmdDVI[1024],
				mc_CheckDir[1024],
				mc_SearchName[1536],
				mc_FileName[1024],
				mc_Pattern[3074],
				mc_NormExt[1024],
				mc_AGExt[1024],
				mc_DVIExt[1024],
				mc_NameBuffer[1024],
				mc_NonExt[1024];
	APTR			*mc_EAB;
	BPTR			mc_OutHandle;
	BOOL			mc_Found,
				mc_Breaked;
};






/**********************************************************************/
/*                             Prototypes                             */
/**********************************************************************/
static char	*GetDir(char *NewName, char *OldName);
static BOOL	DoCheckDir(struct ManControl *MC);
static BOOL	IsAssign(struct ManControl *MC);
static void	DetermineFileType(struct ManControl *MC);





/**********************************************************************/
/*                           Version-String                           */
/**********************************************************************/
static const char *Version	= "$VER: Man "REVISION" ("REVDATE")\0";




/**********************************************************************/
/*                 Template for Command-Line parsing                  */
/**********************************************************************/
static const char *Template	= "MANPAGE";
enum {MAN_ARG, TMP_LAST_ARG};




/**********************************************************************/
/*                    Template for MANOPT settings                    */
/**********************************************************************/
static const char *ManTemplate	= "MANPATHS/K/A,MANVIEW/K/A,MANVIEWAG/K/A,MANVIEWDVI/K/A,MANNRMEXT/K,MANAGEXT/K,MANDVIEXT/K,MANNOVIEW/K,MANGETFILE/S,MANBASENAME/S";
enum {MANPATHS_ARG, MANVIEW_ARG, MANVIEWAG_ARG, MANVIEWDVI_ARG, MANNRMEXT_ARG, MANAGEXT_ARG, MANDVIEXT_ARG, MANNOVIEW_ARG, MANGETFILE_ARG, MANBASENAME_ARG, MAN_LAST_ARG};






/**********************************************************************/
/*                       This is the main part                        */
/**********************************************************************/
ULONG __saveds main(void)
{
	struct	ManControl	*MC;
	struct	Process		*MyProc;
	struct	RDArgs		*RDArgs,
				*ManRDArgs;
	APTR			*Args,
				*ManArgs;
	ULONG			MySig;
	char			*DirOffset;
	BOOL			StartedSearch	= FALSE,
				UseAsl		= FALSE,
				GotManPage;



		// Allocate control structure

	if(!(MC = AllocVec(sizeof(struct ManControl), MEMF_CLEAR)))
		return(20);

	MC->mc_Found		= FALSE;
	MC->mc_Breaked		= FALSE;




		// Don`t start from WB

	MyProc	= (struct Process *)FindTask(NULL);

	if(!MyProc->pr_CLI)
	{
		struct	WBStartup	*Msg;

		WaitPort(&MyProc->pr_MsgPort);
		Msg	= (struct WBStartup *)GetMsg(&MyProc->pr_MsgPort);

		Disable();
		ReplyMsg((struct Message *)Msg);
		FreeVec(MC);

		return(0);
	}



		// Try to open DOSBase

	if(!(DOSBase	= (struct DOSLibrary *)OpenLibrary("dos.library", 0)))
	{
		FreeVec(MC);
		return(20);
	}


		// Get Out Handle

	MC->mc_OutHandle = Output();



		// Check for System we`re running on

	if(((struct Library *)*((ULONG **)0x4L))->lib_Version < 37)
	{
		Write(MC->mc_OutHandle, "You must use KickStart 2.04 (37.175) or higher for MAN\n", 55);

			// Close library

		CloseLibrary((struct Library *)DOSBase);
			// Free buffer

		FreeVec(MC);
		return(20);
	}


		// Try to open UtilityBase

	if(!(UtilityBase = OpenLibrary("utility.library", 0)))
	{
		CloseLibrary((struct Library *)DOSBase);
		FreeVec(MC);
		return(20);
	}


		// Get buffer for Commandline parsing

	if((Args = AllocVec(TMP_LAST_ARG * sizeof(ULONG), MEMF_CLEAR)))
	{
			// Get buffer for MANOPT parsing

		if((ManArgs = AllocVec(MAN_LAST_ARG * sizeof(ULONG), MEMF_CLEAR)))
		{
				// Get RDArgs structure for MANOPT parsing

			if((ManRDArgs = AllocDosObject(DOS_RDARGS, NULL)))
			{
					// Get structure for ExAll()

				if((MC->mc_EAC = AllocDosObject(DOS_EXALLCONTROL, NULL)))
				{
						// Get buffer for ExAll()

					if((MC->mc_EAB = AllocVec(sizeof(struct ExAllData)*20, MEMF_CLEAR)))
					{
							// Try to parse commandline

						if((RDArgs = ReadArgs((char *)Template, (LONG *)Args, NULL)))
						{
								// Rearrange pointer to paths

							DirOffset	= MC->mc_ManPaths;

								// Did we get a manpage ?!?

							GotManPage	= (BOOL)(Args[MAN_ARG] != NULL);


								// Try to get MANOPT Env-Variable

							if(GetVar("MANOPT", MC->mc_ManOpt, 2048, GVF_GLOBAL_ONLY) >= 1)
							{
									// Try to parse strings

								strcat(MC->mc_ManOpt, "\n");
								ManRDArgs->RDA_Source.CS_Buffer	= MC->mc_ManOpt;
								ManRDArgs->RDA_Source.CS_Length	= strlen(MC->mc_ManOpt);
								ManRDArgs->RDA_Source.CS_CurChr	= 0;
								ManRDArgs->RDA_DAList		= NULL;
								ManRDArgs->RDA_Buffer		= NULL;
								ManRDArgs->RDA_BufSiz		= 0;
								ManRDArgs->RDA_ExtHelp		= NULL;
								ManRDArgs->RDA_Flags		= 0;

									// Try to parse MANOPT variable

								if(ReadArgs((char *)ManTemplate, (LONG *)ManArgs, ManRDArgs))
								{
										// Get Paths etc.pp

									strcpy(MC->mc_ManPaths, (char *)ManArgs[MANPATHS_ARG]);
									strcpy(MC->mc_ViewCmd, (char *)ManArgs[MANVIEW_ARG]);
									strcpy(MC->mc_ViewCmdAG, (char *)ManArgs[MANVIEWAG_ARG]);
									strcpy(MC->mc_ViewCmdDVI, (char *)ManArgs[MANVIEWDVI_ARG]);

										// Per default use the ASCII viewer for "non-extension" files

									strcpy(MC->mc_NonExt, MC->mc_ViewCmd);

										// Get additional patterns for normal texts

									if(ManArgs[MANNRMEXT_ARG])
										strcpy(MC->mc_NormExt, (char *)ManArgs[MANNRMEXT_ARG]);

										// Get additional patterns for AmigaGuide texts

									if(ManArgs[MANAGEXT_ARG])
										strcpy(MC->mc_AGExt, (char *)ManArgs[MANAGEXT_ARG]);

										// Get additional patterns for DVI files

									if(ManArgs[MANDVIEXT_ARG])
										strcpy(MC->mc_DVIExt, (char *)ManArgs[MANDVIEXT_ARG]);

										// Get name of viewer to use when no extension was found

									if(ManArgs[MANNOVIEW_ARG])
										strcpy(MC->mc_NonExt, (char *)ManArgs[MANNOVIEW_ARG]);

										// Check for GetFile attribute

									if(ManArgs[MANGETFILE_ARG])
										UseAsl		= TRUE;

										// Do we have a manpage ???

									if(GotManPage)
									{
											// Set pattern for ExAll() search

										if(ManArgs[MANBASENAME_ARG])
											strcpy(MC->mc_SearchName, (char *)FilePart(Args[MAN_ARG]));
										else
											strcpy(MC->mc_SearchName, Args[MAN_ARG]);

											// Append patterns

										strcat(MC->mc_SearchName, "(.doc|.man|.guide|.dvi|");
										strcat(MC->mc_SearchName, MC->mc_NormExt);
										strcat(MC->mc_SearchName, "|");
										strcat(MC->mc_SearchName, MC->mc_AGExt);
										strcat(MC->mc_SearchName, "|");
										strcat(MC->mc_SearchName, MC->mc_DVIExt);
										strcat(MC->mc_SearchName, "|)");

											// Parse the pattern

										if(ParsePatternNoCase(MC->mc_SearchName, MC->mc_Pattern, 1024) != -1)
										{
												// Ok, reached this point

											StartedSearch	= TRUE;

												// Loop for all dirs and wait until file has been found

											while(!MC->mc_Found && !MC->mc_Breaked && DirOffset)
											{
													// Check for CTRL-C

												MySig	= CheckSignal(SIGBREAKF_CTRL_C);

												if(!(MySig & SIGBREAKF_CTRL_C))
												{
														// Extract next directory from list

													DirOffset = GetDir(MC->mc_CheckDir, DirOffset);

														// This dir an assign ???

													if(IsAssign(MC))
													{
														struct	MsgPort	*OldSysTask;
														struct	DevProc	*DirProc	= NULL;
														BOOL	ErrLoop			= FALSE;

															// Get old FileSystemTask

														OldSysTask	= GetFileSysTask();

															// Loop for dirs assigned

														do
														{
																// get the deviceproc for this Assign

															if((DirProc = GetDeviceProc(MC->mc_CheckDir, DirProc)))
															{
																	// Set FileSystem task for locking

																SetFileSysTask(DirProc->dvp_Port);

																	// Get full device/path name and try to scan dir

																if(NameFromLock(DirProc->dvp_Lock, MC->mc_NameBuffer, 1024) == DOSTRUE)
																	DoCheckDir(MC);
																else
																{
																	PrintFault(IoErr(), "Man ");
																	ErrLoop	= TRUE;
																}
															}
															else
															{
																ULONG	Err = IoErr();

																if(Err != ERROR_NO_MORE_ENTRIES)
																{
																	PrintFault(IoErr(), "Man ");
																	ErrLoop	= TRUE;
																}
															}

															// Loop for additional assignments

														} while(!ErrLoop && !MC->mc_Found && !MC->mc_Breaked && DirProc && (DirProc->dvp_Flags & DVPF_ASSIGN));

															// Restore old FileSystemTask

														SetFileSysTask(OldSysTask);

															// Still having a DeviceProc ??? -> Release it

														if(DirProc)
															FreeDeviceProc(DirProc);

															// Error occured -> End whole loop

														if(ErrLoop)
															DirOffset = NULL;
													}
													else
													{
															// For "normal" dirs simply check

														strcpy(MC->mc_NameBuffer, MC->mc_CheckDir);

														DoCheckDir(MC);
													}
												}
												else
													MC->mc_Breaked = TRUE;
											}

											if(!MC->mc_Found)
												strcpy(MC->mc_FileName, Args[MAN_ARG]);
										}
										else
											PrintFault(IoErr(), "Man ");
									}
									else
									{
											// if no popup wanted, issue error

										if(!UseAsl)
											PrintFault(ERROR_REQUIRED_ARG_MISSING, "Man ");
									}

									FreeArgs(ManRDArgs);
								}
								else
									PrintFault(IoErr(), "Man (MANOPT) ");
							}
							else
								PrintFault(IoErr(), "Man (MANOPT) ");

							FreeArgs(RDArgs);
						}
						else
							PrintFault(IoErr(), "Man ");

						FreeVec(MC->mc_EAB);
					}
					else
						FPuts(MC->mc_OutHandle, "Man : Could not allocate buffer for ExAll()\n");

					FreeDosObject(DOS_EXALLCONTROL, (void *)MC->mc_EAC);
				}
				else
					FPuts(MC->mc_OutHandle, "Man : Could not allocate structure for ExAll()\n");

				FreeDosObject(DOS_RDARGS, (void *)ManRDArgs);
			}
			else
				FPuts(MC->mc_OutHandle, "Man : Could not allocate structure for MANOPT parsing\n");

			FreeVec(ManArgs);
		}
		else
			FPuts(MC->mc_OutHandle, "Man : Could not allocate buffer for MANOPT parsing\n");

		FreeVec(Args);
	}
	else
		FPuts(MC->mc_OutHandle, "Could not allocate buffer for CommandLine Parsing\n");


		// Found and not breaked ???

	if((MC->mc_Found || UseAsl) && !MC->mc_Breaked)
	{
		if(!MC->mc_Found && UseAsl)
		{
			struct	Library		*AslBase;
			struct	FileRequester	*FileReq;

				// Open ASL library

			if((AslBase = OpenLibrary("asl.library", 37)))
			{
					// Extract first path of MANPATH

				GetDir(MC->mc_CheckDir, MC->mc_ManPaths);

					// Create FileRequester

				if((FileReq = AllocAslRequestTags(ASL_FileRequest,
					ASLFR_PubScreenName,	NULL,
					ASLFR_PositiveText,	"Show",
					ASLFR_Flags2,		FRF_REJECTICONS,
					ASLFR_RejectIcons,	TRUE,
					ASLFR_TitleText,	"Man page not found; please select",
					ASLFR_InitialFile,	MC->mc_FileName,
					ASLFR_InitialDrawer,	MC->mc_CheckDir,
				TAG_DONE)))
				{
						// Let the user select

					if(AslRequest(FileReq, TAG_DONE))
					{
							// Copy Drawer part

						strcpy(MC->mc_CheckDir, FileReq->fr_Drawer);

							// ...and file part

						strcpy(MC->mc_FileName, FileReq->fr_File);

							// Check for type of file to be displayed

						DetermineFileType(MC);
					}

						// Free FileRequester

					FreeAslRequest(FileReq);
				}
				else
					PrintFault(ERROR_NO_FREE_STORE, "Man (FileRequester) ");

					// Close library again

				CloseLibrary(AslBase);
			}
			else
				PrintFault(ERROR_NO_FREE_STORE, "Man (FileRequester) ");
		}

			// Really found a man page ???

		if(MC->mc_Found)
		{
			char	*Offset	= PathPart(MC->mc_CheckDir);
			BPTR	CurDir;

				// Get name of path doc file resides in

			memset(MC->mc_FileName, 0, 1024);
			strncpy(MC->mc_FileName, MC->mc_CheckDir, (ULONG)(Offset - MC->mc_CheckDir));

				// And try to lock directory

			CurDir	= Lock(MC->mc_FileName, ACCESS_READ);

				// Call viewer

			if(SystemTags(MC->mc_SearchName, NP_CurrentDir, CurDir, TAG_DONE) == -1)
			{
					// Free lock on current dir, if command could not be launched

				if(CurDir)
					UnLock(CurDir);
			}
		}


				// Close libs

		CloseLibrary((struct Library *)DOSBase);
		CloseLibrary(UtilityBase);

			// Free structure

		FreeVec(MC);
		return(0);
	}
		// Got buffers ???

	else if(Args && RDArgs)
	{
			// Started search ???

		if(StartedSearch)
		{
				// Not breaked ???

			if(!MC->mc_Breaked)
			{
					// No pages found

				FPuts(MC->mc_OutHandle, "Man-Pages not found for : ");
				FPuts(MC->mc_OutHandle, MC->mc_FileName);
				FPuts(MC->mc_OutHandle, "\n");
			}
			else
				FPuts(MC->mc_OutHandle, "Man : ^C...\n");
		}

			// Close libs

		CloseLibrary((struct Library *)DOSBase);
		CloseLibrary(UtilityBase);

			// Free structure

		FreeVec(MC);
		return(10);
	}

		// Close libs

	CloseLibrary((struct Library *)DOSBase);
	CloseLibrary(UtilityBase);

		// Free structure

	FreeVec(MC);
	return(20);
}





/**********************************************************************/
/*          Get portions from the Env-Var -> Next directory           */
/**********************************************************************/
static char *GetDir(char *NewDir, char *OldDir)
{
	while(*OldDir == ' ')
		OldDir++;

	while((*OldDir != '\n') && (*OldDir != '\0') && (*OldDir != '|') && (*OldDir != ','))
		*NewDir++ = *OldDir++;

	*NewDir		= '\0';

	if(*OldDir == '\n' || *OldDir == '\0')
		OldDir = NULL;
	else
		OldDir++;

	return(OldDir);
}








/**********************************************************************/
/*                        Check this directory                        */
/**********************************************************************/
static BOOL DoCheckDir(struct ManControl *MC)
{
	BPTR	TestLock;
	ULONG	MySig;
	BOOL	GoOn;

		// Try to lock directory

	if((TestLock = Lock(MC->mc_NameBuffer, SHARED_LOCK)))
	{
			// Fill in ExAll structure

		MC->mc_EAC->eac_LastKey		= 0;
		MC->mc_EAC->eac_MatchString	= MC->mc_Pattern;
		MC->mc_EAC->eac_MatchFunc	= NULL;

		do
		{
				// Check for CTRL-C

			MySig = CheckSignal(SIGBREAKF_CTRL_C);
			if((MySig & SIGBREAKF_CTRL_C))
				MC->mc_Breaked = TRUE;

				// Do the scanning

			GoOn = ExAll(TestLock, (struct ExAllData *)MC->mc_EAB, (20*sizeof(struct ExAllData)), ED_NAME, MC->mc_EAC);

				// Error occured ???

			if((!GoOn) && (IoErr() != ERROR_NO_MORE_ENTRIES))
				PrintFault(IoErr(), "Man ");

				// End of dir reached ;

			if(MC->mc_EAC->eac_Entries == 0)
				GoOn = FALSE;
			else if(!MC->mc_Breaked)
			{
					// Copy real name of directory to our CheckDir buffer

				strcpy(MC->mc_CheckDir, MC->mc_NameBuffer);

					// Get first name matching

				strcpy(MC->mc_FileName, ((struct ExAllData *)MC->mc_EAB)->ed_Name);


					// Check for type of file and set command

				DetermineFileType(MC);
			}
		} while(GoOn);

		GoOn = TRUE;

			// Unlock Directory

		UnLock(TestLock);
	}
	else
	{
			// Display message

		FPuts(MC->mc_OutHandle, "Man : Skipping directory; not existent : ");
		FPuts(MC->mc_OutHandle, MC->mc_CheckDir);
		FPuts(MC->mc_OutHandle, "\n");

		GoOn = FALSE;
	}

	return(GoOn);
}



/**********************************************************************/
/*                 Check if a given name is an assign                 */
/**********************************************************************/
static BOOL IsAssign(struct ManControl *MC)
{
	struct	DosList	*DList;
	UBYTE		*AssignName;
	UCOUNT		AssignLength;
	LONG		Position;
	BOOL		RetVal	= FALSE;

	Position = SplitName(MC->mc_CheckDir, ':', MC->mc_NameBuffer, 0, 1024);

	if(Position != -1)
	{
		if(MC->mc_CheckDir[Position] == '\0')
		{
			if((DList = AttemptLockDosList(LDF_ASSIGNS | LDF_READ)) > (struct DosList *)1)
			{
				while(DList = NextDosEntry(DList, LDF_ASSIGNS))
				{
					AssignName	= (UBYTE *)BADDR(DList->dol_Name);
					AssignLength	= AssignName[0];

					if(!Strnicmp(AssignName + 1, MC->mc_NameBuffer, AssignLength))
					{
						RetVal = TRUE;
						break;
					}
				}

				UnLockDosList(LDF_ASSIGNS | LDF_READ);
			}
		}
	}

	return(RetVal);
}







/**********************************************************************/
/*       Check for the file given and set command to be called        */
/**********************************************************************/
static void DetermineFileType(struct ManControl *MC)
{
		// Check extension to decide whether it`s an AmigaGuide file

	if(strchr(MC->mc_FileName, '.'))
	{
		char	*ExtPtr,
			*DelPtr;
		int	ExtLen;
		BOOL	FoundAG		= FALSE,
			FoundDVI	= FALSE;

			// Append default .guide extension and additional
			// seperator for ease of calculating the length

		ExtPtr	= MC->mc_AGExt;
		strcat(ExtPtr, "|.guide|");

			// Loop for all extensions

		while(!FoundAG && (ExtPtr = strchr(ExtPtr, '.')))
		{
				// Calc len of extension

			DelPtr	= strchr(ExtPtr, '|');
			ExtLen	= DelPtr - ExtPtr;

			if(!strnicmp(&MC->mc_FileName[strlen(MC->mc_FileName) - ExtLen], ExtPtr, ExtLen))
				FoundAG	= TRUE;

			ExtPtr++;
		}

			// Now set view command accordingly

		if(FoundAG)
			strcpy(MC->mc_SearchName, MC->mc_ViewCmdAG);
		else
		{
				// No AmigaGuide file found, check for DVI

				// Append default .guide extension and additional
				// seperator for ease of calculating the length

			ExtPtr	= MC->mc_DVIExt;
			strcat(ExtPtr, "|.dvi|");

				// Loop for all extensions

			while(!FoundDVI && (ExtPtr = strchr(ExtPtr, '.')))
			{
					// Calc len of extension

				DelPtr	= strchr(ExtPtr, '|');
				ExtLen	= DelPtr - ExtPtr;

				if(!strnicmp(&MC->mc_FileName[strlen(MC->mc_FileName) - ExtLen], ExtPtr, ExtLen))
					FoundDVI	= TRUE;

				ExtPtr++;
			}

			if(FoundDVI)
				strcpy(MC->mc_SearchName, MC->mc_ViewCmdDVI);

		}


			// No AmigaGuide, nor DVI file found, use default viewer

		if(!FoundAG && !FoundDVI)
			strcpy(MC->mc_SearchName, MC->mc_ViewCmd);
	}
	else
	{
			// Only use default viewer for non-extension files, when not set to "none"

		if(Stricmp(MC->mc_NonExt, "none"))
		{
				// No extension use default viewer

			strcpy(MC->mc_SearchName, MC->mc_NonExt);

		}
	}

	MC->mc_Found = TRUE;

		// Append directory and FileName

	AddPart(MC->mc_CheckDir, MC->mc_FileName, 1024);

	strcat(MC->mc_SearchName, " ");
	strcat(MC->mc_SearchName, MC->mc_CheckDir);
}
