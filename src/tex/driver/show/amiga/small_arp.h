/* ** small_arp.h ** used arp-functions */


#ifndef EXEC_LIBRARIES_H
#include <exec/libraries.h>
#endif
#ifndef DOS_DOS_H
#include <dos/dos.h>
#endif

#define	ARGs(x)	x
#define C_Args	__stdargs


/*
 ************************************************************************
 *	Standard definitions for arp library information		*
 ************************************************************************
 */
#define	ArpName		"arp.library"	/* Name of library... */
#define	ArpVersion	39L		/* Current version... */

/*
 ************************************************************************
 *	The current ARP library node...					*
 ************************************************************************
 */
struct	ArpBase	{
	struct	Library			LibNode;	 /* Standard library node		*/
		APTR			DosRootNode;	 /* Copy of dl_Root			*/
		UBYTE			Flags;		 /* See bitdefs below			*/
		UBYTE			ESCChar; 	 /* Character to be used for escaping	*/
		LONG			ArpReserved1;	 /* ArpLib's use only!!			*/
	struct	Library			*EnvBase; 	 /* Dummy library for MANX compatibility*/
	struct	Library			*DosBase; 	 /* Cached DosBase			*/
	struct	Library			*GfxBase; 	 /* Cached GfxBase			*/
	struct	Library			*IntuiBase;	 /* Cached IntuitionBase		*/
	struct	MinList			ResLists;	 /* Resource trackers			*/
	struct	ResidentProgramNode	*ResidentPrgList;/* Resident Programs.			*/
	struct	SignalSemaphore		ResPrgProtection;/* protection for above		*/
		BPTR			SegList; 	 /* Pointer to loaded libcode (a BPTR).	*/
		};

/*
 ************************************************************************
 *	The ARP file requester data structure...			*
 ************************************************************************
 */
struct ARPFileRequester	{
			BYTE	*fr_Hail;		/* Hailing text			*/
			BYTE	*fr_File;		/* Filename array (FCHARS + 1)	*/
			BYTE	*fr_Dir;		/* Directory array (DSIZE + 1)	*/
		struct	Window	*fr_Window;		/* Window requesting or NULL	*/
			UBYTE	fr_FuncFlags;		/* Set bitdef's below		*/
			UBYTE	fr_Flags2;		/* New flags...			*/
			VOID	(*fr_Function)();	/* Your function, see bitdef's	*/
			WORD	fr_LeftEdge;		/* To be used later...		*/
			WORD	fr_TopEdge;
			};

/*
 ************************************************************************
 * The following are the defines for fr_FuncFlags.  These bits tell	*
 * FileRequest() what your fr_UserFunc is expecting, and what		*
 * FileRequest() should call it for.					*
 *									*
 * You are called like so:						*
 * fr_Function(Mask, Object)						*
 * ULONG	Mask;							*
 * CPTR		*Object;						*
 *									*
 * The Mask is a copy of the flag value that caused FileRequest() to	*
 * call your function. You can use this to determine what action you	*
 * need to perform, and exactly what Object is, so you know what to do	*
 * and what to return.							*
 ************************************************************************
 */
#define	FRB_DoWildFunc	7L /* Call me with a FIB and a name, ZERO return accepts.	*/
#define	FRB_DoMsgFunc	6L /* You get all IDCMP messages not for FileRequest()		*/
#define	FRB_DoColor	5L /* Set this bit for that new and different look		*/
#define	FRB_NewIDCMP	4L /* Force a new IDCMP (only if fr_Window != NULL)		*/
#define	FRB_NewWindFunc	3L /* You get to modify the newwindow structure.		*/
#define	FRB_AddGadFunc	2L /* You get to add gadgets.					*/
#define	FRB_GEventFunc	1L /* Function to call if one of your gadgets is selected.	*/
#define	FRB_ListFunc	0L /* Not implemented yet.					*/

#define	FRF_DoWildFunc	(1L << FRB_DoWildFunc)
#define	FRF_DoMsgFunc	(1L << FRB_DoMsgFunc)
#define	FRF_DoColor	(1L << FRB_DoColor)
#define	FRF_NewIDCMP	(1L << FRB_NewIDCMP)
#define	FRF_NewWindFunc	(1L << FRB_NewWindFunc)
#define	FRF_AddGadFunc	(1L << FRB_AddGadFunc)
#define	FRF_GEventFunc	(1L << FRB_GEventFunc)
#define	FRF_ListFunc	(1L << FRB_ListFunc)

/*
 ************************************************************************
 * The FR2B_ bits are for fr_Flags2 in the file requester structure	*
 ************************************************************************
 */
#define	FR2B_LongPath	0L /* Specify the fr_Dir buffer is 256 bytes long */

#define	FR2F_LongPath	(1L << FR2B_LongPath)

/*
 ************************************************************************
 *	The sizes of the different buffers...				*
 ************************************************************************
 */
#define	FCHARS		32L	/* Filename size				*/
#define	DSIZE		33L	/* Directory name size if not FR2B_LongPath	*/

#define	LONG_DSIZE	254L	/* If FR2B_LongPath is set, use LONG_DSIZE	*/
#define	LONG_FSIZE	126L	/* For compatibility with ARPbase.i		*/

#define	FR_FIRST_GADGET	0x7680L	/* User gadgetID's must be less than this value	*/




/*
 ************************************************************************
 * Note that C_Args is a #define that, in LATTICE does __stdargs	*
 ************************************************************************
 */

/*
 ************************************************************************
 * This prototype is here to prevent the possible error in defining	*
 * IoErr() as LONG and thus causing LastTracker to give you trash...	*
 *									*
 * N O T E !  You MUST! have IoErr() defined as LONG to use LastTracker *
 *	      If your compiler has other defines for this, you may wish *
 *	      to move the prototype for IoErr() into the DO_ARP_COPIES	*
 ************************************************************************
 */

/*
 ************************************************************************
 *	Now for the stuff that only exists in arp.library...		*
 ************************************************************************
 */
	BYTE			*Getenv			ARGs(	(char *, char *, LONG)					);
	BYTE			*FileRequest 		ARGs(	(struct ARPFileRequester *)				);

/*
 ************************************************************************
 *	Added V32 of arp.library					*
 ************************************************************************
 */

/*
 ************************************************************************
 *	Added V33 of arp.library					*
 ************************************************************************
 */
	VOID			TackOn			ARGs(	(char *, char *)					);
	BYTE			*BaseName		ARGs(	(char *)						);

/*
 ************************************************************************
 *	Added V36 of arp.library					*
 ************************************************************************
 */
struct	ARPFileRequester	C_Args	*ArpAllocFreq		ARGs(	(VOID)							);







#ifdef	PROTO_ARP_H

#error	Nein nicht proto/arp.h   DIESES FILE MUSS INCLUDET WERDEN!!
#else

#define	PROTO_ARP_H	1

#pragma	libcall	ArpBase	Getenv			011A	09803
#pragma	libcall	ArpBase	FileRequest		0126	801
#pragma	libcall	ArpBase	ArpAllocFreq		0294	00
#pragma	libcall	ArpBase	TackOn			0270	9802
#pragma	libcall	ArpBase	BaseName		0276	801

#endif
