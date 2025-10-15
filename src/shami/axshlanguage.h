#ifndef AXSHLANGUAGE_H
#define AXSHLANGUAGE_H


/****************************************************************************/


/* This file was created automatically by CatComp.
 * Do NOT edit by hand!
 */


#ifndef EXEC_TYPES_H
#include <exec/types.h>
#endif

#ifdef CATCOMP_ARRAY
#undef CATCOMP_NUMBERS
#undef CATCOMP_STRINGS
#define CATCOMP_NUMBERS
#define CATCOMP_STRINGS
#endif

#ifdef CATCOMP_BLOCK
#undef CATCOMP_STRINGS
#define CATCOMP_STRINGS
#endif


/****************************************************************************/


#ifdef CATCOMP_NUMBERS

#define MSG_SHUTDOWN_FORCED 0
#define MSG_SHUTDOWN_MSG 1
#define MSG_SHUTDOWN_PLEASE 2
#define MSG_MEM_ALLOC_ERROR 3
#define MSG_HOME_NOT_FOUND 4
#define MSG_NO_CONFIG 5
#define MSG_NO_NORMAL_CONFIG 6
#define MSG_DIR_NOT_FOUND 7
#define MSG_ILLEGAL_DISK_OR_ASSIGN 8
#define MSG_NO_SUCH_DIR 9
#define MSG_OUT_OF_PATH_SPACE 10
#define MSG_TOO_LONG_FILE_NAME 11
#define MSG_NEW_MAIL_ARRIVED 12
#define MSG_GURU_DUMPPED 13
#define MSG_GURU_4 14
#define MSG_GURU_5 15
#define MSG_GURU_6 16
#define MSG_GURU_7 17
#define MSG_GURU_8 18
#define MSG_GURU_9 19
#define MSG_GURU_A 20
#define MSG_GURU_B 21
#define MSG_GURU_DEF 22
#define MSG_SYS_CRASH 23
#define MSG_TOO_LONG_ARG 24
#define MSG_TOO_MANY_ARGS 25
#define MSG_COMMAND_NOT_FOUND 26
#define MSG_UNKNOWN_COMMAND 27
#define MSG_CMD_ALLOWED_HOME 28
#define MSG_NO_MULTIPLE_REDIRECTIONS 29
#define MSG_ILLEGAL_REDIRECTION 30
#define MSG_CANNOT_CREATE_REDIRECTION 31
#define MSG_CANNOT_OPEN 32
#define MSG_NO_PATH_ALLOWED 33
#define MSG_TOO_BIG_ARG_MESS 34
#define MSG_WARN_ARGS_IGNORED 35
#define MSG_TOO_LONG_ARG_LINE 36
#define MSG_RETURNCODE 37
#define MSG_QUOTA_EXCEEDED 38
#define MSG_CANNOT_MAKE_ALIAS 39
#define MSG_NO_SUCH_ALIAS 40
#define MSG_TOO_LONG_ALIAS 41
#define MSG_CANNOT_REMOVE_ALIAS 42
#define MSG_FATAL_GURU_DUMPPED 43
#define MSG_GURU_0 44
#define MSG_GURU_1 45
#define MSG_GURU_2 46
#define MSG_GURU_3 47
#define MSG_FATAL_GURU_DEF 48
#define MSG_FATAL_SYS_CRASH 49
#define MSG_CLOSING_SYS 50
#define MSG_TRAPCODE_RESUMED 51
#define MSG_REBOOTING 52
#define MSG_NO_SUCH_FILE_OR_DIR 53
#define MSG_NO_SUCH_LABEL 54
#define MSG_ENDIF_MISSING 55
#define MSG_RECURSION_DEPTH_REACHED 56
#define MSG_ERROR_IN_LINE 57
#define MSG_NOT_FOUND 58
#define MSG_EXECUTE_USAGE 59
#define MSG_FINGER_USAGE 60
#define MSG_CANNOT_OPEN_DEST_FILE 61
#define MSG_NO_SUCH_FILE 62
#define MSG_FTP_USAGE 63
#define MSG_USER_CANNOT_SET_VAR 64
#define MSG_VAR_ALREADY_UNSET 65
#define MSG_FILE_ALREADY_EXISTS 66
#define MSG_UPLOAD_USAGE 67
#define MSG_WAIT_USAGE 68
#define MSG_COULD_NOT_ACCESS_TTY 69
#define MSG_USER_NOT_LOGGED_IN 70
#define MSG_WRITE_USAGE 71
#define MSG_YOU_HAVE_MAIL 72
#define MSG_YOU_HAVE_NEW_MAIL 73
#define MSG_CONNECTION_CLOSED 74
#define MSG_NEWS_FEED_EVENT 75
#define MSG_QUOTA_LIST 76
#define MSG_DISK_QUOTAS_FOR 77
#define MSG_ACCESS_DENIED 78
#define MSG_PROTECT_USAGE 79
#define MSG_CANNOT_LOCK_OR_EXAMINE 80
#define MSG_AXSH_ALIAS 81
#define MSG_AXSH_INTERNAL 82
#define MSG_INTERNAL_LIST 83

#endif /* CATCOMP_NUMBERS */


/****************************************************************************/


#ifdef CATCOMP_STRINGS

#define MSG_SHUTDOWN_FORCED_STR "\aSystem going down - you are forced to log out\n\n"
#define MSG_SHUTDOWN_MSG_STR "\a*** Shutdown message for %s from %s\n\n"
#define MSG_SHUTDOWN_PLEASE_STR "\aSystem going down -- PLEASE logout\n\n"
#define MSG_MEM_ALLOC_ERROR_STR "\aMemory allocation error!\n"
#define MSG_HOME_NOT_FOUND_STR "Home %s not found!\nUsing demo's home directory...\n"
#define MSG_NO_CONFIG_STR "No configuration for %s - Let's try Normal...\n"
#define MSG_NO_NORMAL_CONFIG_STR "\aNo config for Normal either! C'mon root, do you work?\n"
#define MSG_DIR_NOT_FOUND_STR "Directory %s not found...\n"
#define MSG_ILLEGAL_DISK_OR_ASSIGN_STR "Illegal disk or assign : %s\n"
#define MSG_NO_SUCH_DIR_STR "No such directory : %s\n"
#define MSG_OUT_OF_PATH_SPACE_STR "Out of string space on pathname (%ld chars)\n"
#define MSG_TOO_LONG_FILE_NAME_STR "Too long file or directory name (40 chars max)\n"
#define MSG_NEW_MAIL_ARRIVED_STR "\a\nNew mail has arrived!\n"
#define MSG_GURU_DUMPPED_STR "\n\a*** guru dumped:\n----------------\n"
#define MSG_GURU_4_STR "error:  illegal instruction\ntrap:   0x04\ncaused: illegal opcode"
#define MSG_GURU_5_STR "error:  zero divide\ntrap:   0x05\ncaused: processor division by zero"
#define MSG_GURU_6_STR "error:  CHK instruction\ntrap:   0x06\nregister bounds error trap by CHK"
#define MSG_GURU_7_STR "error:  TRAPV instruction\ntrap:   0x07\noverflow error trap by TRAPV"
#define MSG_GURU_8_STR "error:  privilege violation\ntrap:   0x08\nuser execution of supervisor opcode"
#define MSG_GURU_9_STR "error:  trace\ntrap:   0x09\nstatus register TRACE bit map"
#define MSG_GURU_A_STR "error:  line 1010 emulator\ntrap:   0x0a\nexecution of opcode beginning with $A"
#define MSG_GURU_B_STR "error:  line 1111 emulator\ntrap:   0x0b\nexecution of opcode beginning with $F"
#define MSG_GURU_DEF_STR "error:  trap instruction\ntrap:   0x%02x\nTRAP 0x%02x instruction"
#define MSG_SYS_CRASH_STR "\n\n*** system crash - you are forced to log out\n\n"
#define MSG_TOO_LONG_ARG_STR "Too long argument (only %s chars allowed)!\n"
#define MSG_TOO_MANY_ARGS_STR "Too many arguments, use \"'s to make several args into one\n"
#define MSG_COMMAND_NOT_FOUND_STR "Command not found: %s\n"
#define MSG_UNKNOWN_COMMAND_STR "Unknown command: %s\n"
#define MSG_CMD_ALLOWED_HOME_STR "Command allowed only in home directory\n"
#define MSG_NO_MULTIPLE_REDIRECTIONS_STR "No multiple redirections allowed\n"
#define MSG_ILLEGAL_REDIRECTION_STR "Illegal redirection: %s\n"
#define MSG_CANNOT_CREATE_REDIRECTION_STR "Cannot create redirection file '%s'\n"
#define MSG_CANNOT_OPEN_STR "Can't open '%s'\n"
#define MSG_NO_PATH_ALLOWED_STR "No path parameters allowed\n"
#define MSG_TOO_BIG_ARG_MESS_STR "Too big argument mess (only %d chars allowed)!\n"
#define MSG_WARN_ARGS_IGNORED_STR "Warning! Arguments ignored\n"
#define MSG_TOO_LONG_ARG_LINE_STR "Too long argument line\n"
#define MSG_RETURNCODE_STR "Returncode %d in %s %s\n"
#define MSG_QUOTA_EXCEEDED_STR "\aQuota exceeded! - Please remove %dkB immediately!\n"
#define MSG_CANNOT_MAKE_ALIAS_STR "You can not make an alias for 'alias' or 'unalias'\n"
#define MSG_NO_SUCH_ALIAS_STR "No such alias\n"
#define MSG_TOO_LONG_ALIAS_STR "Too long alias name\n"
#define MSG_CANNOT_REMOVE_ALIAS_STR "Cannot remove! Alias not found.\n"
#define MSG_FATAL_GURU_DUMPPED_STR "\n\a\n\a\n\a*** fatal guru dumpped:\n-----------------------\n"
#define MSG_GURU_0_STR "error:  reset\ntrap:   0x00\ncaused: stack pointer of the cold boot"
#define MSG_GURU_1_STR "error:  reset\ntrap:   0x01\ncaused: cold boot starting address"
#define MSG_GURU_2_STR "error:  bus error\ntrap:   0x02\ncaused: access of nonexistent memory"
#define MSG_GURU_3_STR "error:  address error\ntrap:   0x03\ncaused: long/word access of odd address (68000)"
#define MSG_FATAL_GURU_DEF_STR "Unknown error: %s\n"
#define MSG_FATAL_SYS_CRASH_STR "\n\n*** fatal system crash - you are forced to log out\n"
#define MSG_CLOSING_SYS_STR "*** preparing to close system\n"
#define MSG_TRAPCODE_RESUMED_STR "*** old trapcode pointer resumed\n"
#define MSG_REBOOTING_STR "\a*** please hang up - rebooting in 20 seconds"
#define MSG_NO_SUCH_FILE_OR_DIR_STR "No such file or directory: %s\n"
#define MSG_NO_SUCH_LABEL_STR "No such label: %s\n"
#define MSG_ENDIF_MISSING_STR "Endif missing.\n"
#define MSG_RECURSION_DEPTH_REACHED_STR "Maximum recursion depth reached!\n"
#define MSG_ERROR_IN_LINE_STR "Error in line %d of %s\n"
#define MSG_NOT_FOUND_STR "%s not found!\n"
#define MSG_EXECUTE_USAGE_STR "Usage: execute filename\n"
#define MSG_FINGER_USAGE_STR "Usage: finger username\n"
#define MSG_CANNOT_OPEN_DEST_FILE_STR "Cannot open destination file: %s\n"
#define MSG_NO_SUCH_FILE_STR "No such file: %s\n"
#define MSG_FTP_USAGE_STR "Usage: ftp file1 [file2 [file3] [..] ]\n"
#define MSG_USER_CANNOT_SET_VAR_STR "Variable \"%s\" cannot be set by user\n"
#define MSG_VAR_ALREADY_UNSET_STR "\"%s\" is already unset\n"
#define MSG_FILE_ALREADY_EXISTS_STR "File already exists: %s\n"
#define MSG_UPLOAD_USAGE_STR "Usage: upload file1 [file2 [file3] [..] ]\n"
#define MSG_WAIT_USAGE_STR "Usage:  Wait <seconds>\n"
#define MSG_COULD_NOT_ACCESS_TTY_STR "Could not access tty.\n"
#define MSG_USER_NOT_LOGGED_IN_STR "User is not logged in.\n"
#define MSG_WRITE_USAGE_STR "Usage: write username [text] [..]\n"
#define MSG_YOU_HAVE_MAIL_STR "You have mail.\n"
#define MSG_YOU_HAVE_NEW_MAIL_STR "\aYou have new mail.\n"
#define MSG_CONNECTION_CLOSED_STR "Connection closed.\n"
#define MSG_NEWS_FEED_EVENT_STR "\n\aA news or mail feed event occured - please log out as soon as possible!\n"
#define MSG_QUOTA_LIST_STR "Home Directory      Files  Used(kB)  Limit(kB)\n%-20s%-5d  %-8d  %d\n"
#define MSG_DISK_QUOTAS_FOR_STR "Disk quotas for %s (%s)\n"
#define MSG_ACCESS_DENIED_STR "Access denied!\n"
#define MSG_PROTECT_USAGE_STR "Usage: protect [ugo][+-=][hsparwed] file [file2]\n"
#define MSG_CANNOT_LOCK_OR_EXAMINE_STR "Cannot lock or examine file %s\n"
#define MSG_AXSH_ALIAS_STR "AXsh ALIAS\n"
#define MSG_AXSH_INTERNAL_STR "AXsh INTERNAL\n"
#define MSG_INTERNAL_LIST_STR "AXsh's internal commands:\n"

#endif /* CATCOMP_STRINGS */


/****************************************************************************/


#ifdef CATCOMP_ARRAY

struct CatCompArrayType
{
    LONG   cca_ID;
    STRPTR cca_Str;
};

static const struct CatCompArrayType CatCompArray[] =
{
    {MSG_SHUTDOWN_FORCED,(STRPTR)MSG_SHUTDOWN_FORCED_STR},
    {MSG_SHUTDOWN_MSG,(STRPTR)MSG_SHUTDOWN_MSG_STR},
    {MSG_SHUTDOWN_PLEASE,(STRPTR)MSG_SHUTDOWN_PLEASE_STR},
    {MSG_MEM_ALLOC_ERROR,(STRPTR)MSG_MEM_ALLOC_ERROR_STR},
    {MSG_HOME_NOT_FOUND,(STRPTR)MSG_HOME_NOT_FOUND_STR},
    {MSG_NO_CONFIG,(STRPTR)MSG_NO_CONFIG_STR},
    {MSG_NO_NORMAL_CONFIG,(STRPTR)MSG_NO_NORMAL_CONFIG_STR},
    {MSG_DIR_NOT_FOUND,(STRPTR)MSG_DIR_NOT_FOUND_STR},
    {MSG_ILLEGAL_DISK_OR_ASSIGN,(STRPTR)MSG_ILLEGAL_DISK_OR_ASSIGN_STR},
    {MSG_NO_SUCH_DIR,(STRPTR)MSG_NO_SUCH_DIR_STR},
    {MSG_OUT_OF_PATH_SPACE,(STRPTR)MSG_OUT_OF_PATH_SPACE_STR},
    {MSG_TOO_LONG_FILE_NAME,(STRPTR)MSG_TOO_LONG_FILE_NAME_STR},
    {MSG_NEW_MAIL_ARRIVED,(STRPTR)MSG_NEW_MAIL_ARRIVED_STR},
    {MSG_GURU_DUMPPED,(STRPTR)MSG_GURU_DUMPPED_STR},
    {MSG_GURU_4,(STRPTR)MSG_GURU_4_STR},
    {MSG_GURU_5,(STRPTR)MSG_GURU_5_STR},
    {MSG_GURU_6,(STRPTR)MSG_GURU_6_STR},
    {MSG_GURU_7,(STRPTR)MSG_GURU_7_STR},
    {MSG_GURU_8,(STRPTR)MSG_GURU_8_STR},
    {MSG_GURU_9,(STRPTR)MSG_GURU_9_STR},
    {MSG_GURU_A,(STRPTR)MSG_GURU_A_STR},
    {MSG_GURU_B,(STRPTR)MSG_GURU_B_STR},
    {MSG_GURU_DEF,(STRPTR)MSG_GURU_DEF_STR},
    {MSG_SYS_CRASH,(STRPTR)MSG_SYS_CRASH_STR},
    {MSG_TOO_LONG_ARG,(STRPTR)MSG_TOO_LONG_ARG_STR},
    {MSG_TOO_MANY_ARGS,(STRPTR)MSG_TOO_MANY_ARGS_STR},
    {MSG_COMMAND_NOT_FOUND,(STRPTR)MSG_COMMAND_NOT_FOUND_STR},
    {MSG_UNKNOWN_COMMAND,(STRPTR)MSG_UNKNOWN_COMMAND_STR},
    {MSG_CMD_ALLOWED_HOME,(STRPTR)MSG_CMD_ALLOWED_HOME_STR},
    {MSG_NO_MULTIPLE_REDIRECTIONS,(STRPTR)MSG_NO_MULTIPLE_REDIRECTIONS_STR},
    {MSG_ILLEGAL_REDIRECTION,(STRPTR)MSG_ILLEGAL_REDIRECTION_STR},
    {MSG_CANNOT_CREATE_REDIRECTION,(STRPTR)MSG_CANNOT_CREATE_REDIRECTION_STR},
    {MSG_CANNOT_OPEN,(STRPTR)MSG_CANNOT_OPEN_STR},
    {MSG_NO_PATH_ALLOWED,(STRPTR)MSG_NO_PATH_ALLOWED_STR},
    {MSG_TOO_BIG_ARG_MESS,(STRPTR)MSG_TOO_BIG_ARG_MESS_STR},
    {MSG_WARN_ARGS_IGNORED,(STRPTR)MSG_WARN_ARGS_IGNORED_STR},
    {MSG_TOO_LONG_ARG_LINE,(STRPTR)MSG_TOO_LONG_ARG_LINE_STR},
    {MSG_RETURNCODE,(STRPTR)MSG_RETURNCODE_STR},
    {MSG_QUOTA_EXCEEDED,(STRPTR)MSG_QUOTA_EXCEEDED_STR},
    {MSG_CANNOT_MAKE_ALIAS,(STRPTR)MSG_CANNOT_MAKE_ALIAS_STR},
    {MSG_NO_SUCH_ALIAS,(STRPTR)MSG_NO_SUCH_ALIAS_STR},
    {MSG_TOO_LONG_ALIAS,(STRPTR)MSG_TOO_LONG_ALIAS_STR},
    {MSG_CANNOT_REMOVE_ALIAS,(STRPTR)MSG_CANNOT_REMOVE_ALIAS_STR},
    {MSG_FATAL_GURU_DUMPPED,(STRPTR)MSG_FATAL_GURU_DUMPPED_STR},
    {MSG_GURU_0,(STRPTR)MSG_GURU_0_STR},
    {MSG_GURU_1,(STRPTR)MSG_GURU_1_STR},
    {MSG_GURU_2,(STRPTR)MSG_GURU_2_STR},
    {MSG_GURU_3,(STRPTR)MSG_GURU_3_STR},
    {MSG_FATAL_GURU_DEF,(STRPTR)MSG_FATAL_GURU_DEF_STR},
    {MSG_FATAL_SYS_CRASH,(STRPTR)MSG_FATAL_SYS_CRASH_STR},
    {MSG_CLOSING_SYS,(STRPTR)MSG_CLOSING_SYS_STR},
    {MSG_TRAPCODE_RESUMED,(STRPTR)MSG_TRAPCODE_RESUMED_STR},
    {MSG_REBOOTING,(STRPTR)MSG_REBOOTING_STR},
    {MSG_NO_SUCH_FILE_OR_DIR,(STRPTR)MSG_NO_SUCH_FILE_OR_DIR_STR},
    {MSG_NO_SUCH_LABEL,(STRPTR)MSG_NO_SUCH_LABEL_STR},
    {MSG_ENDIF_MISSING,(STRPTR)MSG_ENDIF_MISSING_STR},
    {MSG_RECURSION_DEPTH_REACHED,(STRPTR)MSG_RECURSION_DEPTH_REACHED_STR},
    {MSG_ERROR_IN_LINE,(STRPTR)MSG_ERROR_IN_LINE_STR},
    {MSG_NOT_FOUND,(STRPTR)MSG_NOT_FOUND_STR},
    {MSG_EXECUTE_USAGE,(STRPTR)MSG_EXECUTE_USAGE_STR},
    {MSG_FINGER_USAGE,(STRPTR)MSG_FINGER_USAGE_STR},
    {MSG_CANNOT_OPEN_DEST_FILE,(STRPTR)MSG_CANNOT_OPEN_DEST_FILE_STR},
    {MSG_NO_SUCH_FILE,(STRPTR)MSG_NO_SUCH_FILE_STR},
    {MSG_FTP_USAGE,(STRPTR)MSG_FTP_USAGE_STR},
    {MSG_USER_CANNOT_SET_VAR,(STRPTR)MSG_USER_CANNOT_SET_VAR_STR},
    {MSG_VAR_ALREADY_UNSET,(STRPTR)MSG_VAR_ALREADY_UNSET_STR},
    {MSG_FILE_ALREADY_EXISTS,(STRPTR)MSG_FILE_ALREADY_EXISTS_STR},
    {MSG_UPLOAD_USAGE,(STRPTR)MSG_UPLOAD_USAGE_STR},
    {MSG_WAIT_USAGE,(STRPTR)MSG_WAIT_USAGE_STR},
    {MSG_COULD_NOT_ACCESS_TTY,(STRPTR)MSG_COULD_NOT_ACCESS_TTY_STR},
    {MSG_USER_NOT_LOGGED_IN,(STRPTR)MSG_USER_NOT_LOGGED_IN_STR},
    {MSG_WRITE_USAGE,(STRPTR)MSG_WRITE_USAGE_STR},
    {MSG_YOU_HAVE_MAIL,(STRPTR)MSG_YOU_HAVE_MAIL_STR},
    {MSG_YOU_HAVE_NEW_MAIL,(STRPTR)MSG_YOU_HAVE_NEW_MAIL_STR},
    {MSG_CONNECTION_CLOSED,(STRPTR)MSG_CONNECTION_CLOSED_STR},
    {MSG_NEWS_FEED_EVENT,(STRPTR)MSG_NEWS_FEED_EVENT_STR},
    {MSG_QUOTA_LIST,(STRPTR)MSG_QUOTA_LIST_STR},
    {MSG_DISK_QUOTAS_FOR,(STRPTR)MSG_DISK_QUOTAS_FOR_STR},
    {MSG_ACCESS_DENIED,(STRPTR)MSG_ACCESS_DENIED_STR},
    {MSG_PROTECT_USAGE,(STRPTR)MSG_PROTECT_USAGE_STR},
    {MSG_CANNOT_LOCK_OR_EXAMINE,(STRPTR)MSG_CANNOT_LOCK_OR_EXAMINE_STR},
    {MSG_AXSH_ALIAS,(STRPTR)MSG_AXSH_ALIAS_STR},
    {MSG_AXSH_INTERNAL,(STRPTR)MSG_AXSH_INTERNAL_STR},
    {MSG_INTERNAL_LIST,(STRPTR)MSG_INTERNAL_LIST_STR},
};

#endif /* CATCOMP_ARRAY */


/****************************************************************************/


#ifdef CATCOMP_BLOCK

static const char CatCompBlock[] =
{
    "\x00\x00\x00\x00\x00\x32"
    MSG_SHUTDOWN_FORCED_STR "\x00\x00"
    "\x00\x00\x00\x01\x00\x28"
    MSG_SHUTDOWN_MSG_STR "\x00\x00"
    "\x00\x00\x00\x02\x00\x26"
    MSG_SHUTDOWN_PLEASE_STR "\x00"
    "\x00\x00\x00\x03\x00\x1C"
    MSG_MEM_ALLOC_ERROR_STR "\x00\x00"
    "\x00\x00\x00\x04\x00\x34"
    MSG_HOME_NOT_FOUND_STR "\x00\x00"
    "\x00\x00\x00\x05\x00\x30"
    MSG_NO_CONFIG_STR "\x00\x00"
    "\x00\x00\x00\x06\x00\x38"
    MSG_NO_NORMAL_CONFIG_STR "\x00"
    "\x00\x00\x00\x07\x00\x1C"
    MSG_DIR_NOT_FOUND_STR "\x00\x00"
    "\x00\x00\x00\x08\x00\x1E"
    MSG_ILLEGAL_DISK_OR_ASSIGN_STR "\x00\x00"
    "\x00\x00\x00\x09\x00\x18"
    MSG_NO_SUCH_DIR_STR "\x00"
    "\x00\x00\x00\x0A\x00\x2E"
    MSG_OUT_OF_PATH_SPACE_STR "\x00\x00"
    "\x00\x00\x00\x0B\x00\x30"
    MSG_TOO_LONG_FILE_NAME_STR "\x00"
    "\x00\x00\x00\x0C\x00\x1A"
    MSG_NEW_MAIL_ARRIVED_STR "\x00\x00"
    "\x00\x00\x00\x0D\x00\x26"
    MSG_GURU_DUMPPED_STR "\x00\x00"
    "\x00\x00\x00\x0E\x00\x40"
    MSG_GURU_4_STR "\x00"
    "\x00\x00\x00\x0F\x00\x44"
    MSG_GURU_5_STR "\x00"
    "\x00\x00\x00\x10\x00\x48"
    MSG_GURU_6_STR "\x00\x00"
    "\x00\x00\x00\x11\x00\x44"
    MSG_GURU_7_STR "\x00"
    "\x00\x00\x00\x12\x00\x4E"
    MSG_GURU_8_STR "\x00\x00"
    "\x00\x00\x00\x13\x00\x3A"
    MSG_GURU_9_STR "\x00\x00"
    "\x00\x00\x00\x14\x00\x4E"
    MSG_GURU_A_STR "\x00"
    "\x00\x00\x00\x15\x00\x4E"
    MSG_GURU_B_STR "\x00"
    "\x00\x00\x00\x16\x00\x40"
    MSG_GURU_DEF_STR "\x00"
    "\x00\x00\x00\x17\x00\x32"
    MSG_SYS_CRASH_STR "\x00\x00"
    "\x00\x00\x00\x18\x00\x2C"
    MSG_TOO_LONG_ARG_STR "\x00"
    "\x00\x00\x00\x19\x00\x3C"
    MSG_TOO_MANY_ARGS_STR "\x00\x00"
    "\x00\x00\x00\x1A\x00\x18"
    MSG_COMMAND_NOT_FOUND_STR "\x00\x00"
    "\x00\x00\x00\x1B\x00\x16"
    MSG_UNKNOWN_COMMAND_STR "\x00\x00"
    "\x00\x00\x00\x1C\x00\x28"
    MSG_CMD_ALLOWED_HOME_STR "\x00"
    "\x00\x00\x00\x1D\x00\x22"
    MSG_NO_MULTIPLE_REDIRECTIONS_STR "\x00"
    "\x00\x00\x00\x1E\x00\x1A"
    MSG_ILLEGAL_REDIRECTION_STR "\x00\x00"
    "\x00\x00\x00\x1F\x00\x26"
    MSG_CANNOT_CREATE_REDIRECTION_STR "\x00\x00"
    "\x00\x00\x00\x20\x00\x12"
    MSG_CANNOT_OPEN_STR "\x00\x00"
    "\x00\x00\x00\x21\x00\x1C"
    MSG_NO_PATH_ALLOWED_STR "\x00"
    "\x00\x00\x00\x22\x00\x30"
    MSG_TOO_BIG_ARG_MESS_STR "\x00"
    "\x00\x00\x00\x23\x00\x1C"
    MSG_WARN_ARGS_IGNORED_STR "\x00"
    "\x00\x00\x00\x24\x00\x18"
    MSG_TOO_LONG_ARG_LINE_STR "\x00"
    "\x00\x00\x00\x25\x00\x18"
    MSG_RETURNCODE_STR "\x00"
    "\x00\x00\x00\x26\x00\x34"
    MSG_QUOTA_EXCEEDED_STR "\x00"
    "\x00\x00\x00\x27\x00\x34"
    MSG_CANNOT_MAKE_ALIAS_STR "\x00"
    "\x00\x00\x00\x28\x00\x10"
    MSG_NO_SUCH_ALIAS_STR "\x00\x00"
    "\x00\x00\x00\x29\x00\x16"
    MSG_TOO_LONG_ALIAS_STR "\x00\x00"
    "\x00\x00\x00\x2A\x00\x22"
    MSG_CANNOT_REMOVE_ALIAS_STR "\x00\x00"
    "\x00\x00\x00\x2B\x00\x38"
    MSG_FATAL_GURU_DUMPPED_STR "\x00\x00"
    "\x00\x00\x00\x2C\x00\x42"
    MSG_GURU_0_STR "\x00"
    "\x00\x00\x00\x2D\x00\x3E"
    MSG_GURU_1_STR "\x00"
    "\x00\x00\x00\x2E\x00\x44"
    MSG_GURU_2_STR "\x00"
    "\x00\x00\x00\x2F\x00\x54"
    MSG_GURU_3_STR "\x00\x00"
    "\x00\x00\x00\x30\x00\x14"
    MSG_FATAL_GURU_DEF_STR "\x00\x00"
    "\x00\x00\x00\x31\x00\x36"
    MSG_FATAL_SYS_CRASH_STR "\x00"
    "\x00\x00\x00\x32\x00\x20"
    MSG_CLOSING_SYS_STR "\x00\x00"
    "\x00\x00\x00\x33\x00\x22"
    MSG_TRAPCODE_RESUMED_STR "\x00"
    "\x00\x00\x00\x34\x00\x2E"
    MSG_REBOOTING_STR "\x00"
    "\x00\x00\x00\x35\x00\x20"
    MSG_NO_SUCH_FILE_OR_DIR_STR "\x00\x00"
    "\x00\x00\x00\x36\x00\x14"
    MSG_NO_SUCH_LABEL_STR "\x00\x00"
    "\x00\x00\x00\x37\x00\x10"
    MSG_ENDIF_MISSING_STR "\x00"
    "\x00\x00\x00\x38\x00\x22"
    MSG_RECURSION_DEPTH_REACHED_STR "\x00"
    "\x00\x00\x00\x39\x00\x18"
    MSG_ERROR_IN_LINE_STR "\x00"
    "\x00\x00\x00\x3A\x00\x10"
    MSG_NOT_FOUND_STR "\x00\x00"
    "\x00\x00\x00\x3B\x00\x1A"
    MSG_EXECUTE_USAGE_STR "\x00\x00"
    "\x00\x00\x00\x3C\x00\x18"
    MSG_FINGER_USAGE_STR "\x00"
    "\x00\x00\x00\x3D\x00\x22"
    MSG_CANNOT_OPEN_DEST_FILE_STR "\x00"
    "\x00\x00\x00\x3E\x00\x12"
    MSG_NO_SUCH_FILE_STR "\x00"
    "\x00\x00\x00\x3F\x00\x28"
    MSG_FTP_USAGE_STR "\x00"
    "\x00\x00\x00\x40\x00\x26"
    MSG_USER_CANNOT_SET_VAR_STR "\x00\x00"
    "\x00\x00\x00\x41\x00\x18"
    MSG_VAR_ALREADY_UNSET_STR "\x00\x00"
    "\x00\x00\x00\x42\x00\x1A"
    MSG_FILE_ALREADY_EXISTS_STR "\x00\x00"
    "\x00\x00\x00\x43\x00\x2C"
    MSG_UPLOAD_USAGE_STR "\x00\x00"
    "\x00\x00\x00\x44\x00\x18"
    MSG_WAIT_USAGE_STR "\x00"
    "\x00\x00\x00\x45\x00\x18"
    MSG_COULD_NOT_ACCESS_TTY_STR "\x00\x00"
    "\x00\x00\x00\x46\x00\x18"
    MSG_USER_NOT_LOGGED_IN_STR "\x00"
    "\x00\x00\x00\x47\x00\x24"
    MSG_WRITE_USAGE_STR "\x00\x00"
    "\x00\x00\x00\x48\x00\x10"
    MSG_YOU_HAVE_MAIL_STR "\x00"
    "\x00\x00\x00\x49\x00\x16"
    MSG_YOU_HAVE_NEW_MAIL_STR "\x00\x00"
    "\x00\x00\x00\x4A\x00\x14"
    MSG_CONNECTION_CLOSED_STR "\x00"
    "\x00\x00\x00\x4B\x00\x4C"
    MSG_NEWS_FEED_EVENT_STR "\x00\x00"
    "\x00\x00\x00\x4C\x00\x44"
    MSG_QUOTA_LIST_STR "\x00"
    "\x00\x00\x00\x4D\x00\x1A"
    MSG_DISK_QUOTAS_FOR_STR "\x00\x00"
    "\x00\x00\x00\x4E\x00\x10"
    MSG_ACCESS_DENIED_STR "\x00"
    "\x00\x00\x00\x4F\x00\x32"
    MSG_PROTECT_USAGE_STR "\x00"
    "\x00\x00\x00\x50\x00\x20"
    MSG_CANNOT_LOCK_OR_EXAMINE_STR "\x00"
    "\x00\x00\x00\x51\x00\x0C"
    MSG_AXSH_ALIAS_STR "\x00"
    "\x00\x00\x00\x52\x00\x10"
    MSG_AXSH_INTERNAL_STR "\x00\x00"
    "\x00\x00\x00\x53\x00\x1C"
    MSG_INTERNAL_LIST_STR "\x00\x00"
};

#endif /* CATCOMP_BLOCK */


/****************************************************************************/


struct LocaleInfo
{
    APTR li_LocaleBase;
    APTR li_Catalog;
};


#ifdef CATCOMP_CODE

STRPTR GetString(struct LocaleInfo *li, LONG stringNum)
{
LONG   *l;
UWORD  *w;
STRPTR  builtIn;

    l = (LONG *)CatCompBlock;

    while (*l != stringNum)
    {
        w = (UWORD *)((ULONG)l + 4);
        l = (LONG *)((ULONG)l + (ULONG)*w + 6);
    }
    builtIn = (STRPTR)((ULONG)l + 6);

#define XLocaleBase LocaleBase
#define LocaleBase li->li_LocaleBase
    
    if (LocaleBase)
        return(GetCatalogStr(li->li_Catalog,stringNum,builtIn));
#define LocaleBase XLocaleBase
#undef XLocaleBase

    return(builtIn);
}


#endif /* CATCOMP_CODE */


/****************************************************************************/


#endif /* AXSHLANGUAGE_H */
