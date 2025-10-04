* -- Machine generated FORTRAN 77
* -- code created by RATFOR-77.
* -- Not intended for human consumption.
      INTEGERFUNCTIONOPENF(FUNIT,NAME,MODE)
      INTEGERFUNIT,MODE,IOS
      CHARACTERNAME*80,ST*7
      IF (MODE.EQ.2) THEN
      OPEN(FUNIT,IOSTAT=IOS,STATUS='scratch',FORM='formatted')
      ELSE
      I23000=(MODE)
      IF (I23000.EQ.(0)) THEN
      ST='old'
      ELSE IF (I23000.EQ.(1)) THEN
      ST='new'
      ELSE
      ST='unknown'
      END IF
      OPEN(FUNIT,FILE=NAME,IOSTAT=IOS,STATUS=ST,FORM='formatted')
      END IF
      IF (IOS.NE.0) THEN
      OPENF=(0)
      RETURN
      ELSE
      OPENF=(1)
      RETURN
      END IF
      END
      SUBROUTINECLOSEF(FUNIT)
      INTEGERFUNIT
      CLOSE(FUNIT)
      END
      INTEGERFUNCTIONGETLIN(FUNIT,BUF,LENGTH)
      INTEGERFUNIT,LENGTH,IOS
      CHARACTER*80BUF
      READ(FUNIT,'(A)',IOSTAT=IOS)BUF
      IF (IOS.LT.0) THEN
      GETLIN=(-1)
      RETURN
      ELSE
      IF (IOS.GT.0) THEN
      GETLIN=(0)
      RETURN
      ELSE
      GETLIN=(1)
      RETURN
      END IF
      END IF
      END
      SUBROUTINEPUTLIN(FUNIT,LINE,LENGTH)
      INTEGERFUNIT,LENGTH
      CHARACTERLINE*80
      INTEGERL
      L=LENGTH
23001 IF (.NOT.(L.GT.1)) GO TO 23002
      GO TO 23003
23004 L=L-1
      GO TO 23001
23003 CONTINUE
      IF (LINE(L:L).NE.' ') THEN
      GO TO 23002
      END IF
      GO TO 23004
23002 CONTINUE
      WRITE(FUNIT,'(A)')LINE(:L)
      END
