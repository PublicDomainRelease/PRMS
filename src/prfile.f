C
C
C
      SUBROUTINE   PRFILE
     O                   ( RETC )
C
C     + + + PURPOSE + + +
C     This routine makes sure all of the necessary files are open.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   RETC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     RETC   - return code
C              0 - all necessary files opened successfully
C              1 - problem opening a file
C
C     + + + PARAMETERS + + +
      INCLUDE 'pinout.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   MAX, MAXT, KNT, FUNT(18), FTYP(18), FOPN(18), TYP
      CHARACTER*4 FCOD(18)
      CHARACTER*64 SCRATCH, WDNAME
C
C     + + + EXTERNALS + + +
      EXTERNAL   GETFLS, OPNFIL
C
C     + + + DATA INITIALIZATIONS + + +
      DATA FCOD /'CG1 ', 'CG2 ', 'CG3 ', 'CG4 ', 'CG5 ', 'CG6 ', 'CG7 ',
     >           'WDM ', 'OUT ', 'QDY ', 'QUN ', 'PLT ', 'HRU ',
     >           'PG1 ', 'PG2 ', 'PG3 ', 'PG4 ', 'PG5 ' /
      DATA FUNT / 71,      32,     33,     34,     35,     36,     37,
     >            10,      72,     20,     19,     42,     43,
     >            40,      50,     51,     52,     53 /
      DATA FTYP /  2,       2,      2,      2,      2,      2,      2,
     >             1,       3,      3,      3,      3,      3,
     >             3,       3,      3,      3,      3 /
C
C     + + + OUTPUT FORMATS + + +
 2004 FORMAT (    '     opened      ', 'temp', I6, 3X, A64 )
 2005 FORMAT (    ' ** not opened   ', 'temp', I6, 3X, A64 )
 2007 FORMAT (    ' -------------   ----------   ', 50('-') )
 2008 FORMAT (    '     opened      ', 'message', I3, 3X, A64 )
 2009 FORMAT (    ' ** not opened   ', 'message', I3, 3X, A64 )
C
C     + + + END SPECIFICATIONS + + +
C
      SCRATCH = '    '
      MAX = 18
      MAXT = 18
C
      DO 50 KNT = 1, MAXT
C       set file open status to no action
        FOPN(KNT) = 0
 50   CONTINUE
C
C     open user specified files
      CALL GETFLS ( MAX, FCOD, FUNT, FTYP,
     O              FOPN, RETC )
      IF (RETC .EQ. 0  .AND.  MAXT .GT. MAX) THEN
C       open temporary files
        KNT = MAX
 100    CONTINUE
          IF (FOPN(KNT) .EQ. 0) THEN
            CALL OPNFIL ( FUNT(KNT), FTYP(KNT), SCRATCH, RETC )
            IF (RETC .EQ. 0) THEN
C             successful file open
              FOPN(KNT) = 1
              WRITE (IOOT,2004) FUNT(KNT)
            ELSE
C             file open not successful
              FOPN(KNT) = 2
              WRITE (IOOT,2005) FUNT(KNT)
            END IF
          END IF
          KNT = KNT + 1
        IF (KNT .LE. MAXT  .AND.  RETC .EQ. 0) GO TO 100
      END IF
C
      WRITE (IOOT,2007)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GETFLS
     I                   ( MAX, FCOD, FUNT, FTYP,
     O                     FOPN, RETC )
C
C     + + + PURPOSE + + +
C     Reads a list of files from a master file and opens these files
C     for input or output.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MAX, RETC
      INTEGER   FUNT(MAX), FTYP(MAX), FOPN(MAX)
      CHARACTER*4 FCOD(MAX)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MAX    - number of files defined
C     FCOD   - array of defined file type codes
C     FUNT   - array of fortran unit numbers corresponding to FCODs
C     FTYP   - array of file types
C              1 - WDM file
C              2 - input file
C              3 - output file
C     FOPN   - flag indicating final status of file
C              0 - no open attempted
C              1 - file open was successful
C              2 - file open was unsuccessful
C     RETC   - return code
C              0 - all files opened successfully
C              1 - problem opening one or more files
C
C     + + + PARAMETERS + + +
      INCLUDE 'pinout.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   KNT, KNTMX, IUNIT, ITYP, DONE
      CHARACTER*10 CODES
      CHARACTER*64 NAME
C
C     + + + EXTERNALS + + +
      EXTERNAL   OPNFIL
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT ( A64 )
 1001 FORMAT ( A10, A64 )
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (//, ' Enter name of master file:' )                       KD1291
 2001 FORMAT (//, ' Unable to open master file ', A64,                  KD1291
     #         /, ' try again.' )                                       KD1291
 2002 FORMAT (//, ' Unable to open master file ', A64,                  KD1291
     #         /, ' Check your directory for valid file name.'          KD1291
     #         / )
 2003 FORMAT (//, '     status         codes     file name'
     #         /, ' -------------   ----------   ', 50('-') )
 2004 FORMAT (    '     opened      ', A10, 3X, A64 )
 2005 FORMAT (    ' ** not opened   ', A10, 3X, A64 )
 2006 FORMAT (    ' ** invalid      ', A10, 3X, A64 )
 2007 FORMAT (    ' -------------   ----------   ', 50('-') )
 2008 FORMAT (    '                 ', A10, 3X, A64 )
C
C     + + + END SPECIFICATIONS + + +
C
      IUNIT = 60
      ITYP = 2
      KNT = 0
      KNTMX = 3
 100  CONTINUE
C       get name of master file and open it
        WRITE (IOOT,2000)
        READ (IOIN,1000) NAME
        CALL OPNFIL ( IUNIT, ITYP, NAME, RETC )
        IF (RETC .NE. 0  .AND.  KNT+1 .LT. KNTMX) THEN
C         problem opening master file, try again
          KNT = KNT + 1
          WRITE (IOOT,2001) NAME
        ELSE IF (RETC .NE. 0) THEN
C         unable to open master file
          WRITE (IOOT,2002) NAME
        END IF
      IF (RETC .NE. 0  .AND.  KNT .LT. KNTMX) GO TO 100
C
      IF (RETC .EQ. 0) THEN
C       read master file
        DONE = 0
        WRITE (IOOT,2003)
 200    CONTINUE
          READ (IUNIT,1001,END=290) CODES, NAME
C           successful read, check for match
            KNT = 1
 250        CONTINUE
              IF (CODES(1:3) .EQ. '***') THEN
C               comment record
                WRITE (IOOT,2008) CODES, NAME
                KNT = 0
              ELSE IF (CODES(1:4) .EQ. FCOD(KNT)) THEN
C               matches file type, open it
                CALL OPNFIL ( FUNT(KNT), FTYP(KNT), NAME, RETC )
                IF (RETC .EQ. 0) THEN
C                 file opened successfully
                  FOPN(KNT) = 1
                  KNT = 0
                  WRITE (IOOT,2004) CODES, NAME
                ELSE
C                 problem opening file
                  FOPN(KNT) = 2
                  KNT = -1
                  WRITE (IOOT,2005) CODES, NAME
                END IF
              ELSE IF (KNT .LT. MAX) THEN
C               no match, keep looking
                KNT = KNT + 1
              ELSE
C               no match, end of file types
                KNT = 0
                WRITE (IOOT,2006) CODES, NAME
              END IF
            IF (KNT .GT. 0) GO TO 250
            GO TO 299
 290      CONTINUE
C           end of file
            CLOSE (IUNIT)
            DONE = 1
            WRITE (IOOT,2007)
 299      CONTINUE
        IF (RETC .EQ. 0  .AND.  DONE .EQ. 0) GO TO 200
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   OPNFIL
     I                   ( IUNIT, ITYP, NAME,
     O                     RETC )
C
C     + + + PURPOSE + + +
C     Opens a file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IUNIT, ITYP, RETC
      CHARACTER*64 NAME
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IUNIT  - Fortran unit number to be used for open
C     ITYP   - type of file to be opened
C              1 - WDM data file
C              2 - input file
C              3 - output file
C              4 - direct access, unformatted
C              5 - temporary, unformatted, sequential access
C              6 - WDM message file
C     NAME   - name of file to be opened
C              for ITYP = 4, if blank, will be opened as scratch
C                  ITYP = 5, leave blank
C     RETC   - return code
C              0 - file opened successfully
C              1 - file not opened
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   IOS, IOERR, RLNGTH(5), RONWFG
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDBOPN
C
C     + + + DATA INITIALIZATIONS + + +
      INCLUDE 'xrecl.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      IF (ITYP .LE. 0  .OR.  ITYP .GT. 6) THEN
C       invalid file type
        RETC = 1
      ELSE
C       open file using appropriate open statement
C               wdm   in  out   da  temp mess
        GO TO ( 100, 200, 300, 400, 500, 100 ), ITYP
 100      CONTINUE
C           wdm file:  1-users, 6-message
            IF (ITYP .EQ. 1) THEN
C             users wdm, open read and write
              RONWFG = 0
            ELSE
C             message wdm, open read only
              RONWFG = 1
            END IF
            CALL WDBOPN ( IUNIT, NAME, RONWFG,
     O                    RETC )
            IF (RETC .NE. 0) RETC = 1
            GO TO 900
 200      CONTINUE
C           input file
            OPEN ( UNIT = IUNIT,
     #             FILE = NAME,
     #           STATUS = 'OLD',
     #           IOSTAT = IOS,
     #              ERR = 210 )
              RETC = 0
              GO TO 900
 210        CONTINUE
C             problem with open
              RETC = 1
              GO TO 900
 300      CONTINUE
C           output file
            OPEN ( UNIT = IUNIT,
     #             FILE = NAME,
     #           STATUS = 'NEW',
     #           IOSTAT = IOS,
     #              ERR = 310 )
              RETC = 0
              GO TO 900
 310        CONTINUE
C             problem with open
              IF (IOS .EQ. IOERR) THEN
C               file exists, try to delete it
                OPEN ( UNIT=IUNIT, FILE=NAME, IOSTAT=IOS )
                IF (IOS .EQ. 0)
     #            CLOSE ( UNIT=IUNIT, STATUS='DELETE', IOSTAT=IOS )
                IF (IOS .EQ. 0) THEN
C                 old file deleted, open new one
                  OPEN ( UNIT = IUNIT,
     #                   FILE = NAME,
     #                 STATUS = 'NEW',
     #                 IOSTAT = IOS )
                  IF (IOS .EQ. 0) THEN
C                   successful open
                    RETC = 0
                  ELSE
C                   no success, give it up
                    RETC = 1
                  END IF
                ELSE
C                 could not delete old file
                  RETC = 1
                END IF
              ELSE
C               unknown problem with file
                RETC = 1
              END IF
              GO TO 900
 400      CONTINUE
C           direct access file, unformatted
            IF (NAME(1:4) .EQ. '    ') THEN
C             open scratch file
              OPEN ( UNIT = IUNIT, STATUS = 'SCRATCH',
     #                               FORM = 'UNFORMATTED',
     #                             ACCESS = 'DIRECT',
     #                               RECL = RLNGTH(4) )
            ELSE
C             open file by name
              OPEN ( UNIT = IUNIT, FILE = NAME,
     #                             FORM = 'UNFORMATTED',
     #                           ACCESS = 'DIRECT',
     #                             RECL = RLNGTH(4) )
              RETC = 0
            END IF
            GO TO 900
 500      CONTINUE
C           temporary, unformatted, sequential access file
            OPEN (UNIT = IUNIT, STATUS = 'SCRATCH',
     #                            FORM = 'UNFORMATTED',
     #                          ACCESS = 'SEQUENTIAL' )
            RETC = 0
            GO TO 900
C
 900    CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   OPTEMP
C
C     + + + PURPOSE + + +
C     Opens the temporary files required for all prms runs.
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   FACTOR, RECL
C
C     + + + LOCAL DEFINITIONS + + +
C     FACTOR - used to adjust the record length for unformatted, 
C              direct access files as appropriate for the computer
C              type and compiler.
C              factor  computer/compiler  expects
C              ------  -----------------  ----------
C                1          DG vms        word
C                4          DG ansi       bytes
C                4          PC            bytes
C                2          Prime         half words
C                2          Vax           half words
C
C     + + + END SPECIFICATIONS + + +
C
C     using DG ansi **** Computer and compiler specific ****
      FACTOR = 4
C
C     optimization
      OPEN (UNIT=38,ACCESS='SEQUENTIAL',
     $                FORM='UNFORMATTED',
     $              STATUS='SCRATCH')
C     initial probablility conditions in probgn
      OPEN (UNIT=60,ACCESS='SEQUENTIAL',
     $                FORM='UNFORMATTED',
     $              STATUS='SCRATCH')
C     probability analysis in sumallt and probary
C     ***** Change this file to formatted for testing purposes
      OPEN (UNIT=65,ACCESS='SEQUENTIAL',
     $                FORM='UNFORMATTED',
     $              STATUS='SCRATCH')
C     sensitivity analysis
      OPEN (UNIT=8,ACCESS='SEQUENTIAL',
     $               FORM='UNFORMATTED',
     $             STATUS='SCRATCH')
      OPEN (UNIT=45,ACCESS='SEQUENTIAL',
     $                FORM='UNFORMATTED',
     $              STATUS='SCRATCH')
C     optimization and sensitivity
      RECL = 256 * FACTOR
      OPEN (UNIT=9,ACCESS='DIRECT',
     $               FORM='UNFORMATTED',
     $             STATUS='SCRATCH',
     $               RECL=RECL)
C     sensitivity analysis
      OPEN (UNIT=70,ACCESS='SEQUENTIAL',
     $                FORM='FORMATTED')
C
C     lateral inflows
      RECL = 2880 * FACTOR
      OPEN (UNIT=17,ACCESS='DIRECT',
     $                FORM='UNFORMATTED',
     $              STATUS='SCRATCH',
     $                RECL=RECL)
C     channel segments storm hydrograph data
      OPEN (UNIT=18,ACCESS='SEQUENTIAL',
     $                FORM='UNFORMATTED',
     $              STATUS='SCRATCH')
C     predicted and observed flows
      RECL = 367 * FACTOR
      OPEN (UNIT=30,ACCESS='SEQUENTIAL',
     $                FORM='UNFORMATTED',
     $              STATUS='SCRATCH',
     $                RECL=RECL)
C     inventory of unit rainfall
      OPEN (UNIT=11,ACCESS='SEQUENTIAL',
     $                FORM='UNFORMATTED',
     $              STATUS='SCRATCH')
C     hydrograph seperation information
      OPEN (UNIT=12,ACCESS='SEQUENTIAL',
     $                FORM='UNFORMATTED',
     $              STATUS='SCRATCH')
C     hydrograph seperation information
      OPEN (UNIT=13,ACCESS='SEQUENTIAL',
     $                FORM='UNFORMATTED',
     $              STATUS='SCRATCH')
C     unit-values discharge
      OPEN (UNIT=14,ACCESS='SEQUENTIAL',
     $                FORM='UNFORMATTED',
     $              STATUS='SCRATCH')
C     unit-values precipitation
      OPEN (UNIT=15,ACCESS='SEQUENTIAL',
     $                FORM='UNFORMATTED',
     $              STATUS='SCRATCH')
C
      RETURN
      END
