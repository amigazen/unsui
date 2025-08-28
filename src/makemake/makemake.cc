// MakeMake (C)1994 by Jeff Shepherd
// a makefile compiler for use in C and C++ programs

// Originally Compiled with g++ version 2.5.8 on an Amiga 4000/030/6/120
// Also tested with g++ version 2.3 on SunOS V4.1.3 revision 2
// compile me by typing g++ -O2 makemake.cc -o makemake

// to see the command line just type "makemake"

// COMMAND LINE: makemake [-f makefilename] [-s] [-I directory] 
//               [-C|-C++] files.[c,cc] ...
// -f output results to makefilename. 'Makefile' is the default name
// -I specifies other directories to search for dependency files
//    default is the current directory.
// -s shortcut. Don't compile .o files. Used when you have only one file. 
//    Default false
// -C|C++ tells makemake you are compiling C or C++ files. Default -C
// files.[c,cc] a list of the .c or .cc files.
// EG. makemake *.cc will supply all the C++ files in this directory.

// NOTE: makemake assumes that 'compilername' can invoke the compiler and the 
//       linker
//	 ( this is the case for g++ and u++?? )
//	 if this is not the case, just add lines to get the linker name
//	 and change the line '$(CC) -o $(OUTPUT) $(OBJ)' to something like
//	 '$(LINK) -o $(OUTPUT) $(OBJ)'
//	 ALSO YOU CANNOT USE THE -s OPTION IF YOU MAKE THIS CHANGE

// TODO: Allow multiple executable files to be compiled

#include <iostream.h>
#include <iomanip.h>
#include <fstream.h>
#include <string.h>
#include <stdlib.h>
#include <curses.h>

// change MAX_LENGTH if the program line length is bigger or the length of
// options is bigger
// total length of all .c files is 3*MAX_LENGTH
#define MAX_LENGTH 256

// Column width
// when each line of OBJ goes past COL_WIDTH, '\' is appended
// and the list is continued on the next line
#define COL_WIDTH 75


// amiga version string
// to find out the version of makemake just type verson makemake
char *VERSION = "$VER: makemake version 1.02 (06/03/94)";

// Linked list class
// NOTE: you have to initialize the list to NULL
// eg. LList *test = NULL;
// This list could be generalized using templates but I don't know how to use 
// them
class LList {
  char *Listoption;
  LList *NextList;
public:
  LList *Add(char *toption);          // Add an item to the linked list
  void PrintList();                   // Print the list
  LList *Next() {return NextList;}    // Get the Next pointer
  char *option() {return Listoption;} // Get the linked list item
  ~LList();                           // Destructor
  long Len();                         // Length of list
};

void LList :: PrintList() {
  LList *list = this;
  while(list->NextList) {
    cout << list->Listoption << endl;
    list=list->NextList;
  }
}

LList * LList::Add(char *toption) {
    LList *result = new LList;
    result->Listoption=strdup(toption);
    result->NextList = this;
    return result;
}

LList ::~LList() {
    LList *temp, *temp2 = this;
    while(temp2) {
      temp = temp2;
      temp2 = temp2->NextList;
      delete(temp);
   }
}

long LList :: Len() {
 long result = 0;
 LList *temp = this;
    while(temp->NextList) {
	result++;
	temp=temp->NextList;
    }
    return result;
}

// program prototypes
void basename(char **,char *);
void checkfile(ofstream &makefile, char *, LList *, char *);

main(int argc, char **argv) {

  char compiler[MAX_LENGTH];	       // compiler name
  char base[35], *baseptr = base;      // used when calling basename
  char compileroptions[MAX_LENGTH];    // options invoked when the compiler is 
				       //called
  char outname[MAX_LENGTH];	       // executable name
  char linkeroptions[MAX_LENGTH];      // options passed to the linker
  char makefilename[MAX_LENGTH] = "Makefile";
				       // name of makefile 'Makefile' is default
  char objects[3 * MAX_LENGTH] = {0,}; // list of .o files
  LList *source=NULL;		       // list of source files
  LList *includedir=NULL;	       // list of other include directories to 
				       // search
  short shortcut = FALSE;	       // if TRUE then don't compile .o files
  short CFiles = TRUE;		       // TRUE means C files, FALSE- C++ files
  char suffix[4]=".c";		       // .c or .cc
  // print command line
  if (argc == 1) {
    cout << "makemake: a Makefile compiler. Version 1.02" << endl;
    cout << "(C)1994 by Jeff Shepherd" << endl;
    cout << "COMMAND LINE: " << endl;
    cout << "makemake [-f makefilename] [-s] [-I directory]"
	 << "[-C|-C++] files.[c,cc] ..." << endl;
    cout << "-f output results to makefilename. 'Makefile' is the default name"
	 << endl;
    cout << "-s shortcut. Don't compile .o files. Used when you have only one "
	 << "file." << endl <<"Default FALSE" << endl;
    cout << "   If you use this option, put the compiler options in the linker"
	 << " options" << endl;

    cout << "-I specifies other directories to search for dependency files" 
	 << endl;
    cout << "   the default directory is the current directory." << endl;
    cout << "[-C|-C++] specifies compiling of C++ or C files. Default -C.";
    cout << "files.[c,cc] a list of .c or .cc files." << endl;
    cout << "eg. makemake *.cc will supply all the C++ files in this directory."
         << endl << endl;
    exit(0);
  }

  // get source files and parse arguments
  for (int i=1; i < argc;i++) {

    // do command line options
    // structure allows for expansion
    // commands could be written with or without spaces
    // eg -ftestfile -f testfile
    if (argv[i][0] == '-') {
	switch (argv[i][1]) {
	    case 'f' :
		if (argv[i][2]) {
		    strcpy(makefilename,&argv[i][2]);
		}
		else {
		  i++;
		  strcpy(makefilename,argv[i]);
		}
	    break;

	    case 's' :
		shortcut = TRUE;
	    break;

	    case 'I' :
		if (argv[i][2]) {
		   includedir=includedir->Add(&argv[i][2]);
		}
		else {
		   i++;
		   includedir=includedir->Add(argv[i]);
		}
	    break;

	    case 'C' :
		if (!argv[i][2]) {
		   CFiles = TRUE;
                   strcpy(suffix,".c");
                }
                else if (!strcmp(&argv[i][2],"++")) {
		   CFiles = FALSE;
                   strcpy(suffix,".cc");
                }
            break;

	    // other cases go here
	    default:
		cout << "illegal option: " << argv[i] << endl;
		exit(3);
	     break;
	} //switch
    }// if
    else {
       source=source->Add(argv[i]);
       basename(&baseptr,argv[i]);

       // put a '\' on the end of the line if the new line length > COL_WIDTH
       if ( (strlen(objects) / COL_WIDTH) <
	    ((strlen(objects)+strlen(baseptr)+2) / COL_WIDTH) ) {
	   strcat(objects,"\\\n      ");
       } // if
       strcat(objects,baseptr);
       strcat(objects,".o ");
    } // else
  } // for

  cout << "Compiler Name: ";
  cin >> compiler;

  cout <<  "Compiler options: ";
  cin.getline(compileroptions,MAX_LENGTH);
  // ?? I need to do this twice WHY??
  cin.getline(compileroptions,MAX_LENGTH);

  cout	<< "Linker options: ";
  cin.getline(linkeroptions,MAX_LENGTH);

  cout << "Output filename: ";
  cin >> outname;

  // start writing out the makefile
  ofstream makefile(makefilename);

  makefile << "#Makefile generated by Makemake (C)1994 Jeff Shepherd" << endl;
  makefile << "CC = " << compiler << endl;
  makefile << "CFLAGS = " << compileroptions << endl;
  makefile << "LFLAGS = " << linkeroptions << endl;

  // list .o files only if shortcut is false
  // else list the .c or .cc file
  // NOTE: I assume there is only 1 .c[c] file
  if (!shortcut) {
     makefile << "OBJ = " << objects << endl ;
  }
  else {
     makefile << "OBJ = " << source->option() << endl;
  }

  makefile << "OUTPUT = " << outname << endl << endl;

  // compile the .c or .cc files
  if (!shortcut) {
     makefile <<".SUFFIXES: " << suffix << " .h .o" << endl;
     makefile << suffix << ".o:" << endl;
     makefile << "\t$(CC) $(CFLAGS) -c $< -o $@" << endl << endl;
  }
  makefile << "all : $(OUTPUT)\n" << endl;
  makefile << "$(OUTPUT) : $(OBJ)" << endl;
  makefile << "\t$(CC) -o $(OUTPUT) $(OBJ) $(LFLAGS)" << endl << endl;

  // check and print out any file dependencies
  for (LList *sourcelist=source; sourcelist; sourcelist=sourcelist->Next()) {

    // UPDATE 06/03/94: Don't check .o files
    char *dotpos;
    if ( !( (dotpos=strrchr(sourcelist->option(),'.')) && 
            (*(++dotpos) == 'o') ) ) {

       // UPDATE 05/??/94: Should list .o file instead of .cc or .c file
       char objectname[35]; char *object = objectname;
       basename(&object,sourcelist->option());
       strcat(object,".o");
       checkfile(makefile,object,includedir,sourcelist->option());
    }
  }
  makefile.close();
}

// strips the suffix from a filename
// eg tempfile.c -> tempfile
// result is returned in base
// very similar to the unix 'basename' command
void basename(char **base, char *name) {
   char *pos = strchr(name,'.');
   int length;

   if (pos) {
       length = strlen(name)-strlen(pos);
       strncpy(*base,name,length);
       (*base)[length]='\0';
   }
   else {
       strcpy(*base,name);
   }
}

// search through .c or .cc file and find dependencies
// The method I will use is #include "file.h" where the file.h is enclosed
// in quotes instead of '<' and '>'.
// Usually compiler includes are enclosed in '<' and '>'.
// In the makefile you will see:
// <infile.c[c]> : file.h
// checkfile also recurses into file.h to find any other dependencies
void checkfile(ofstream &makefile, char *cfile, LList *dir, char *includefile) {
    ifstream file(includefile);   // open file for reading
    char line[MAX_LENGTH];	  // line inputted from file
    char dependname[MAX_LENGTH];  // name of dependent include file
    char completepath[MAX_LENGTH]; // path and filename
    char *firstquote, *secondquote; // position of "'s in the line
    int length;     	   	    // length of completepath
    LList *includedir = dir;

    // check include directories for include file
    while (!file && includedir) {

       // tack the directory name held in directory L-List onto the filename 
       // and check again
       strcpy(completepath,includedir->option());
       length = strlen(completepath);
       if ( !(completepath[--length] == ':' || completepath[length] == '/') ) {
	  strcat(completepath,"/");
       }
       strcat(completepath,includefile);
       includedir=includedir->Next();
       file.open(completepath);
    }

    // file is not found
    if (!file) {
       cerr << includefile << " not found." << endl;
       exit(5);
    }

    file.getline(line,MAX_LENGTH);
    // search the whole file
    while (file.eof() == FALSE) {
       // search for '"#include "' or '#    include "'
       if ( (line[0] == '#') && (strstr(line,"include")) && 
	    (firstquote=strchr(line,'\"')) ) {
	   firstquote++;
	   if (secondquote=strchr(firstquote,'\"')) {

	      // blank out dependname first
	      // This needs to be done for some reason
	      for (int i=0;i < MAX_LENGTH; i++) {
		  dependname[i] =0;
	      }

	      // copy the dependency file name
	      strncpy(dependname,firstquote,strlen(firstquote)-
		      strlen(secondquote));

	      // write out the dependency
	      makefile << cfile << " : " << dependname << endl;

	      // recurse into the dependent file
	      checkfile(makefile,cfile,dir,dependname);

	   } // if
       } // if

       // do line-by-line parsing
       file.getline(line,MAX_LENGTH);
    } // while
    file.close();
}
 
