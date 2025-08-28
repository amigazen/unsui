#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <libraries/dos.h>
#include <exec/memory.h>
#include <proto/dos.h>
#include <proto/exec.h>

BOOL found;
char GlobalFileName[512];
char SearchChar[108];
char PotentialChar[108];

BOOL DoSizeCheck(BPTR);

int CXBRK() {return(0);}
int chkabort() {return(0);}

void main(argc,argv)
int argc;
char *argv[];
{
   BPTR mylock;

   found=FALSE;

   printf("WhereIs - file searcher by Jean-François Stenuit\n");
   if (argc!=3)
   {
      printf("Usage : sc rootdir filename\n");
      exit(0);
   };
   if (argv[1][strlen(argv[1])-1]=='/')
      argv[1][strlen(argv[1])-1]=0;
   if ((mylock=Lock(argv[1],ACCESS_READ))==0)
   {
      printf("Unable to lock %s, DOS error code %d.\n",argv[1],IoErr());
      exit(0);
   };
   strcpy(GlobalFileName,argv[1]);
   strncpy(SearchChar,argv[2],108);
   strlwr(SearchChar);
   if (DoSizeCheck(mylock)==FALSE)
      printf("Recursive routine failed\n");
   else
      if (!(found))
         printf("%s not found.\n",argv[2]);
   UnLock(mylock);
   exit(0);
}

BOOL DoSizeCheck(ALock)
BPTR ALock;
{
   register struct FileInfoBlock *myFIB;
   register BPTR NewLock;
   register short int i;
   register char tmp;

   if ((myFIB=(struct FileInfoBlock *)
        AllocMem(sizeof(struct FileInfoBlock),MEMF_PUBLIC))==0)
      return(FALSE);
   if (Examine(ALock,myFIB)==FALSE)
   {
      FreeMem((APTR)myFIB,sizeof(struct FileInfoBlock));
      return(FALSE);
   };
   if (myFIB->fib_DirEntryType<0)
   /* A file : just add its size to total */
   {
      FreeMem((APTR)myFIB,sizeof(struct FileInfoBlock));
      return(TRUE);
   }
   else
   /* A subdir : check the whole contents */
   {
      for(;;)
      {
         if (ExNext(ALock,myFIB)==FALSE)
         {
            FreeMem((APTR)myFIB,sizeof(struct FileInfoBlock));
            if (IoErr()==ERROR_NO_MORE_ENTRIES)
               return(TRUE);
            else
               return(FALSE);
         }
         else
         {
            if (myFIB->fib_DirEntryType<0)
            {
            /* A file of the subdir : compare with searchdir */
               strcpy(PotentialChar,myFIB->fib_FileName);
               strlwr(PotentialChar);
               if (strcmp(PotentialChar,SearchChar)==0)
               {
                  printf("Found in %s\n",GlobalFileName);
                  found=TRUE;
               };
            }
            else
            {
               if (GlobalFileName[strlen(GlobalFileName)-1]!=':')
                  strcat(GlobalFileName,"/");
               strcat(GlobalFileName,myFIB->fib_FileName);
               NewLock=Lock(GlobalFileName,ACCESS_READ);
               if (DoSizeCheck(NewLock)==FALSE)
               {
                  UnLock(NewLock);
                  FreeMem((APTR)myFIB,sizeof(struct FileInfoBlock));
                  return(FALSE);
               };
               for (i=strlen(GlobalFileName);
                            ((tmp=GlobalFileName[i])!='/')&&(tmp!=':');
                             i--)
                  GlobalFileName[i]=0;
               if (GlobalFileName[i]!=':')
                  GlobalFileName[i]=0;
               UnLock(NewLock);
            };
         };
      };
   };
}
