C
C
C
      SUBROUTINE   SBGET
     I                  ( NHRU, NIRS, NIGW, DARU,
     O                    RETC )
C
C     + + + PURPOSE + + +
C     Read user input specifications defining sub-basins to be saved
C     to the wdm file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NHRU, NIRS, NIGW, RETC
      REAL      DARU(NHRU)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NHRU   - number of hru in this run
C     NIRS   - number of subsurface reservoirs
C     NIGW   - number of groundwater reservoirs
C     DARU   - array containing drainage area for each hru
C     RETC   - return code
C              0 - input successfully read
C              1 - problem reading input
C
C     + + + COMMONS + + +
      INCLUDE 'csbasn.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     IN, OT, N, K
      REAL        DARUSB
      CHARACTER*5 ID
C
C     + + + DATA INITIALIZATIONS + + +
      DATA   IN, OT
     $     / 71, 72 /
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT ( A5, 5X, I5 )
 1010 FORMAT ( A5, 5X, 2I5, 12I5, (/, 20X, 12I5) )
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/, I4, ' sub-basins will be saved in the wdm file',
     $       //,'   dsn    area    HRUs HRUs contained in sub-basin',
     $        /,' ----- ---------- ---- ---------------------------')
 2001 FORMAT (/, 5X, '***** Invalid number of sub-basins',
     $        /, 5X, '          1 is the minimum allowed',
     $        /, 5X, '        ', I3, ' is the maximum allowed',
     $        /, 5X, '        ', I3, ' was entered.' )
 2010 FORMAT (  I6, F11.2, I4, 2X, 12I3, ( /, 23X, 12I3 ) )
 2011 FORMAT (/, 5X, '***** Invalid number of HRUs in sub-basin',I3,
     $        /, 5X, '          1 is the minimum allowed',
     $        /, 5X, '        ', I3, ' is the total number of HRU',
     $        /, 5X, '        ', I3, ' was entered.' )
 2012 FORMAT (/, 5X, '***** Invalid HRU index in sub-basin', I3,
     $        /, 5X, '          1 is the minimum allowed',
     $        /, 5X, '        ', I3, ' is the total number of HRU',
     $        /, 5X, '        ', I3, ' was entered' )
 2018 FORMAT (/, 5X, '***** Trouble reading the sub-basin input')
 2030 FORMAT (/, 5X, '***** Problem with group 1 input record SBSNS',
     $        /, 5X, '      found record ', A5, ' instead' )
 2031 FORMAT (/, 5X, '***** Problem with group 1 input record DSNSB',
     $        /, 5X, '      found record ', A5, ' instead' )
 2100 FORMAT (/// )
C
C     + + + END SPECIFICATIONS + + +
C
      RETC = 0
C     how many basins
      READ (IN,1000) ID, NSB
      IF (ID .EQ. 'SBSNS'  .OR.  ID .EQ. 'sbsns') THEN
C       expected record
        IF (0 .LT. NSB  .AND.  NSB .LE. 50) THEN
C         valid number of sub-basin
          WRITE (OT,2000) NSB
        ELSE
C         invalid number of sub-basins
          WRITE (OT,2001) NSB, NHRU
          RETC = 1
        END IF
      ELSE
C       unexpected record
        RETC = 1
        WRITE (OT,2030) ID
      END IF
C
      IF (RETC .EQ. 0) THEN
C       get description for each sub-basin
        N = 0
 100    CONTINUE
          N = N + 1
C         for this sub-basin: dsn, # of hru, hru indexes
          READ (IN,1010,ERR=118,END=118) ID, DSNSB(N), NHRUSB(N),
     $                  (KHRUSB(K,N), K = 1, NHRUSB(N))
            IF (ID .EQ. 'DSNSB'  .OR. ID .EQ. 'dsnsb') THEN
C             expected record
              IF (0 .LT. NHRUSB(N)  .AND.  NHRUSB(N) .LE. NHRU) THEN
C               calculate sub-basin drainage area
                DARUSB = 0.0
                DO 110 K = 1, NHRUSB(N)
                  IF (          0 .LT. KHRUSB(K,N) .AND.
     $                KHRUSB(K,N) .LE. NHRU) THEN
                    DARUSB = DARUSB + DARU(KHRUSB(K,N))
                  ELSE
C                   invalid hru index
                    WRITE (OT,2012) N, NHRU, KHRUSB(K,N)
                    RETC = 1
                  END IF
 110            CONTINUE
                WRITE (OT,2010) DSNSB(N), DARUSB, NHRUSB(N),
     $                        (KHRUSB(K,N), K = 1, NHRUSB(N))
              ELSE
C               invalid number of sub-basins
                WRITE (OT,2011) N, NHRU,  NHRUSB(N)
                RETC = 0
              END IF
            ELSE
C             unexpected record
              WRITE (OT,2031) ID
              RETC = 1
            END IF
            GO TO 119
 118      CONTINUE
C           problem reading the input file
            WRITE (OT,2018) N
            RETC = -1
 119      CONTINUE
        IF (RETC .EQ. 0  .AND.  N .LT. NSB) GO TO 100
        WRITE (OT,2100)
      END IF
C
C     initialize pointer
      IPT = 0
C
      RETURN
      END
C
C
C
      SUBROUTINE   SBSAV
     I                  ( NHRU, SRO, RASQ, BASQ, KRES, KGW, DARU )
C
C     + + + PURPOSE + + +
C     Save sub-basin outflow in temporary buffer.
C
C     + + + DUMMY ARUGMENTS + + +
      INTEGER   NHRU
      INTEGER   KRES(NHRU), KGW(NHRU)
      REAL      SRO(NHRU), RASQ(NHRU), BASQ(NHRU), DARU(NHRU)
C
C     + + + ARUGMENT DEFINITIONS + + +
C     NHRU   - number of HRU in this run
C     SRO    - Surface runoff from each HRU
C     RASQ   - Daily subsurface flow from each reservoir
C     BASQ   - Daily flow from each groundwater reservoir
C     KRES   - Subsurface index for each HRU
C     KGW    - Groundwater index for each HRU
C
C     + + + COMMONS + + +
      INCLUDE 'csbasn.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   N, K, IRU, IRS, IGW
      REAL      SUM
c
 3001 format ( ' --> sbsav:  ipt  nsb    n nhrusb(n) ',
     $                      'ru rs gw      sro     rasq     basq  daru',
     $         '           ', 2i5 )
 3002 format ( '           ', 10x, i5, i5 )
 3003 format ( '           ', 20x, 5x, 3i3, 4f9.5 )
 3004 format ( '              sum =', f9.5 )
C
C     + + + END SPECIFICATIONS + + +
C
C     increment pointer
      IPT = IPT + 1
C
C     caluculate runoff for each sub-basin
      DO 150 N = 1, NSB
        SUM = 0.0
        DO 100 K = 1, NHRUSB(N)
          IRU = KHRUSB(K,N)
          IRS = KRES(IRU)
          IGW = KGW(IRU)
          SUM = SUM + (SRO(IRU) + RASQ(IRS) + BASQ(IGW)) * DARU(IRU)
 100    CONTINUE
C       convert acre inches of runoff to cfs
        WHRU(IPT,N) = SUM / 23.76
 150  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   SBWRT
     I                  ( WDM, OT, DATE )
C
C     + + + PURPOSE + + +
C     Write temporary hru buffer to wdm file
C
C     + + + DUMMY ARUGMENTS + + +
      INTEGER   WDM, OT, DATE(6)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDM    - Fortran unit number of wdm file
C     OT     - Fortran unit for error messages
C     DATE   - date time array (year, month, day, hour, minute, second)
C
C     + + + COMMONS + + +
      INCLUDE 'csbasn.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   N, TSSTEP, TCODE, DTOVWR, QFLG, RETC
      integer   i
C
C     + + + DATA INITIALIZATIONS + + +
      DATA   TSSTEP, TCODE, DTOVWR, QFLG
     $     /     1,     4,      1,    0 /
C
C     + + + OUTPUT FORMATS + + +
 2090 FORMAT (/, 5X, '***** Error  writing sub-basin time series to wdm',
     $        /, 5X, '*****    file =', I5,
     $        /, 5X, '*****     dsn =', I5,
     $        /, 5X, '*****   basin =', I5,
     $        /, 5X, '*****    nval =', I5,
     $        /, 5X, '*****    retc =', I5,
     $        /, 5X, '*****    date =', I5,5I3 )
C
C     + + + END SPECIFICATIONS
C
C     write each sub-basin
      DO 100 N = 1, NSB
        CALL WDTPUT ( WDM, DSNSB(N), TSSTEP, DATE, IPT, DTOVWR, QFLG,
     I                TCODE, WHRU(1,N), 
     O                RETC )
        IF (RETC .NE. 0) THEN
C         error writing to wdm file
          WRITE (OT,2090) WDM, DSNSB(N), NSB, IPT, RETC, DATE
        END IF
 100  CONTINUE
C
C     reset pointer and date
      IPT = 0
C
      RETURN
      END
