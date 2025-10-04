* -- Machine generated FORTRAN 77
* -- code created by RATFOR-77.
* -- Not intended for human consumption.
      CHARACTERFUNCTIONTOLOW(C)
      CHARACTERC
      IF (C.GE.'A'.AND.C.LE.'Z') THEN
      TOLOW=(CHAR(ICHAR(C)+32))
      RETURN
      ELSE
      TOLOW=(C)
      RETURN
      END IF
      END
      LOGICALFUNCTIONISDIG(C)
      CHARACTERC
      ISDIG=(C.GE.'0'.AND.C.LE.'9')
      RETURN
      END
      LOGICALFUNCTIONISLOW(C)
      CHARACTERC
      ISLOW=(C.GE.'a'.AND.C.LE.'z')
      RETURN
      END
