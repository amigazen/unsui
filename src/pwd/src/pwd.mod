(***************************************************************************

    MODUL
      pwd.mod

    DESCRIPTION
      Pwd prints out the current working directory. No more, no less. ;-)

    NOTE
      compiled with Oberon-A, OC version 5.37
      ( A lot of thanks to Frank for his great compiler ! )

    BUGS
      What ? Hhhm if there is at least one, report it to:
         roland.jesse@student.uni-magdeburg.de

    HISTORY
      23-12-95 Roland (rj,-) Jesse  created

***************************************************************************)

<* STANDARD- *>             (* necessary for assignable cleanup procedure *)

MODULE pwd;

IMPORT
   pwdRev,
   Errors, Kernel,
   Dos,
   S := SYSTEM,
   Strings;


VAR
   success : BOOLEAN;
   len, i : INTEGER;
   buf : ARRAY 256 OF CHAR;


(* Remove all the allocated stuff *)
PROCEDURE* Cleanup (VAR rc : LONGINT);
BEGIN

END Cleanup;


BEGIN (* pwd *)
   Errors.Init;
   Kernel.SetCleanup (Cleanup);

   (* Sorry, but OS2+ is needed. *)
   ASSERT (Dos.base.lib.version >= 36, Dos.fail);

   buf[0] := 0AX;
   len := 256; (* number of bytes for the buffer *)
   success := Dos.GetCurrentDirName (buf, len);
   Strings.Append ("\n", buf);
   success := Dos.FPuts (Dos.Output (), buf);

END pwd.

