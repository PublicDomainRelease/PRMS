C
C
C
      SUBROUTINE   RDDSN 
     O                  ( RETC )
C
C     + + + PURPOSE + + +
C     Read in data set numbers for the basin averaged time series
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   RETC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     RETC   - return code
C               0 - successfully read DSNs 
C              -1 - DSNs not read
C
C     + + + COMMONS + + +
      INCLUDE 'csvwdm.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I, INDSN, PRT
      CHARACTER*5 ID
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  INDSN, PRT
     >     /   71,  72 /
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT ( A5, 5X, 10I5 )
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (//, 2X, 'Basin averaged time series written to wdm file',
     $        //, 2X, '    dsn*  time series              ',
     $         /, 2X, '   -----  -------------------------',
     $         /, 2X,     I8, '  simulated flow           ',
     $         /, 2X,     I8, '  precipitation            ',
     $         /, 2X,     I8, '  potential evaporation    ',
     $         /, 2X,     I8, '  actual evapotranspiration',
     $         /, 2X,     I8, '  available soil moisture  ',
     $         /, 2X,     I8, '  ground water contribution',
     $         /, 2X,     I8, '  subsurface contribution  ',
     $         /, 2X,     I8, '  surface contribution     ',
     $         /, 2X, '  * - time series not saved if dsn = 0', / )
 2005 FORMAT (/, 5X, '***** Problem with group 1 input record DSNDV',
     $        /, 5X, '      found record ', A5, ' instead' )
 2010 FORMAT (/, 5X, '***** Problem with group 1 input record DSNDV',
     $        /, 5X, '      unexpected end of file or a read error' )
C
C     + + + END SPECIFICATIONS + + +
C
      RETC = 0
C     read dsns for basin
      READ (INDSN,1000,END=198,ERR=198) ID, ( DSNDV(I), I = 1, 8 )
C       successfully read the record
        IF (ID .EQ. 'DSNDV'  .OR.  ID .EQ. 'dsndv') THEN
C         expected record
          WRITE (PRT,2000) ( DSNDV(I), I = 1, 8 )
        ELSE
C         unexpected record
          RETC = -1
          WRITE (PRT,2005) ID
        END IF
        GO TO 200
 198  CONTINUE
C       end of file or read error
        RETC = -1
        WRITE (PRT,2010)
 200  CONTINUE
C
      RETURN
      END
