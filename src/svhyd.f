C
C
C
      SUBROUTINE   SVHYD
     I                  ( ISAVE, NOFSEG, NCRSEG )
C
C     + + + PURPOSE + + +
C     Saves predicted unit discharge and sediment to wdm data sets.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   ISAVE, NOFSEG, NCRSEG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ISAVE  - number of different segments saved in file 18
C     NOFSEG - number of overland flow segments
C     NCRSEG - number of channel segments
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   IFLG(200), DSN(200), TSSTEP(200),
     >          QSWY, QSMO, QSDY, QSIO, QSQS, QSRPD, QSHR,
     >          TSF, DTOVWR, QFLG, TCODE, RETC,
     >          DATE(6), JDY, JDY1, JDYN, IPT, MORE, I, J, K,
     >          NH, NSS, NHS, NE, NEF, FHA(80), NSEGS
      REAL      QC, FHAF(10), VAL(1440)
      CHARACTER*4 QSID, IO(2), QS(2)
C
C     + + + EXTERNALS + + +
      EXTERNAL   SVDSN, WDTPUT
C
C     + + + DATA INITIALIZATIONS + + +
      DATA   TSF, DTOVWR, QFLG, TCODE
     >     / 10,      1,    0,     2 /
      DATA  IO / ' in ', ' out' /
      DATA  QS / '  Q ', ' sed' /
C
C     + + + OUTPUT FORMATS + + +
 2100 FORMAT ( 5X, A4, 2X, A4, 2X, A4, I6 )
 2101 FORMAT ( 5X, A4, 2X, A4, 2X, A4, 6X, '<-- missing' )
 2200 FORMAT ( 5X, 'Next storm' )
 2209 FORMAT ( 5X, 'Finished saving time series to wdm file' )
 2309 FORMAT ( 5X, '*** Error writing to wdm file:',
     >       /,5X, '       File =', I6,
     >       /,5X, '        dsn =', I6,
     >       /,5X, '    segment =', 2X,A4,
     >       /,5X, '       date =', I6,5I3,
     >       /,5X, '       retc =', I6,
     >       /,5X, '    Will not continue saving this segment.' )
C
C     + + + END SPECIFICATIONS + + +
C
C     rewind hydrograph seperation and segment discharge files
      REWIND 12
      REWIND 18
C
C     total number of segments
      NSEGS = NOFSEG + NCRSEG
C
C     figure out which time series should be saved to wdm file
      DO 100 I = 1, ISAVE
C       get descriptive record and skip time series record
        READ (18) QSWY, QSMO, QSDY, QSIO, QSQS, QSID, QSRPD
        READ (18)
        IF (QSQS .EQ. 5  .OR.  QSQS .EQ. 6) THEN
C         discharge or sediment flagged to be saved, is dsn defined?
          CALL SVDSN ( QSID, QSQS, NSEGS, DSN(I) )
          IF (DSN(I) .NE. 0) THEN
C           dsn specified, save this time series
            IFLG(I) = 1
            TSSTEP(I) = 1440 / QSRPD
            WRITE (72,2100) QSID, IO(QSIO), QS(QSQS-4), DSN(I)
          ELSE
C           no dsn specified, so can not be saved
            IFLG(I) = 0
            TSSTEP(I) = 0
            WRITE (72,2101) QSID, IO(QSIO), QS(QSQS-4)
          END IF
        ELSE
C         segment flagged for other purposes, will not be saved
          IFLG(I) = 0
          TSSTEP(I) = 0
          DSN(I) = 0
        END IF
 100  CONTINUE
C
C     rewind hydrograph seperation and segment discharge files
      REWIND 12
      REWIND 18
C
C     process each storm event
      MORE = 1
 200  CONTINUE
C       get information on next storm
        READ (12,END=209) NSS, QC, NHS, NE, NEF,
     >                    (FHA(J), J = 1, NE), (FHAF(K), K = 1, NEF)
C         another storm to be processed
          WRITE (72,2200)
          GO TO 210
 209    CONTINUE
C         finished processing all storms
          WRITE (72,2209)
          MORE = 0
 210    CONTINUE
        IF (MORE .EQ. 1) THEN
C         save times series for this storm
          IPT = 0
          DO 500 NH = 1, NHS
C           process each hydrograph segment for this storm
            QSHR = FHA(IPT+4)
            DATE(4) = QSHR / 100
            DATE(5) = QSHR - DATE(4) * 100
            DATE(6) = 0
            JDY1 = FHA(IPT+14)
            JDYN = FHA(IPT+16)
            DO 400 JDY = JDY1, JDYN
C             process each day in storm segment
              DO 300 I = 1, ISAVE
C               process each flow plane and channel segment
                IF (IFLG(I) .GT. 0) THEN
C                 save this record
                  READ (18) QSWY, QSMO, QSDY, QSIO, QSQS, QSID, QSRPD
                  READ (18) (VAL(K), K = 1, QSRPD)
C                 fill date array
                  IF (QSMO .LE. 9) THEN
C                   water year is same as calendar year
                    DATE(1) = QSWY
                  ELSE
C                   adjust water year to calendar year
                    DATE(1) = QSWY - 1
                  END IF
                  DATE(2) = QSMO
                  DATE(3) = QSDY
C                 output the record to the wdm data set
                  CALL WDTPUT ( TSF, DSN(I), TSSTEP(I), DATE, QSRPD,
     I                          DTOVWR, QFLG, TCODE, VAL,
     O                          RETC )
                  IF (RETC .NE. 0) THEN
C                   error writing to wdm file
                    WRITE (72,2309) TSF, DSN(I), QSID, DATE, RETC
C                   don't write to this dsn again
                    IFLG(I) = 0
                  END IF
                ELSE
C                 skip this record
                  READ (18)
                  READ (18)
                END IF
 300          CONTINUE
 400        CONTINUE
 500      CONTINUE
        END IF
C       go back for next storm if more
      IF (MORE .EQ. 1) GO TO 200
C
      RETURN
      END
C
C
C
      SUBROUTINE   SVDSN
     I                  ( QSID, QSQS, NSEGS,
     O                    DSN )
C
C     + + + PORPOSE + + +
C     Determine appropriate data set number for segment
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   QSQS, DSN, NSEGS
      CHARACTER*4 QSID
C
C     + + + ARGUMENT DEFINTIONS + + +
C     QSID   - character id for segment
C     QSQS   - type indicator
C              5 - discharge
C              6 - sediment
C     NSEGS  - number of segments
C     DSN    - data-set number to be used
C
C     + + + INCLUDES + + +
      INCLUDE 'cpcrch.inc'
C
C     + + + SAVES + + +
      INTEGER   FIRST, DSNQS(100,2)
      SAVE      FIRST, DSNQS
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   INDSN, PRT, MORE, IPT, I
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  FIRST, INDSN, PRT
     >     /    1,    34,   72 /
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT ( 10X, 10I5 )
C
C     + + + OUTPUT FORMATS + + +
 2005 FORMAT (//, '   Segment data written to wdm file',
     $        //, '   segment  Q-dsn*  sed-dsn*'
     $        /,  '   -------  ------  --------' )
 2006 FORMAT ( 5X, A4, I7, I9 )
 2007 FORMAT (    '  * time series not saved if dsn = 0', // )
C
C     + + + END SPECIFICATIONS + + +
C
      IF (FIRST .EQ. 1) THEN
C       first time subroutine called, read in data set numbers
        FIRST = 0
        READ (INDSN,1000) (DSNQS(I,1), I = 1, NSEGS)
        READ (INDSN,1000) (DSNQS(I,2), I = 1, NSEGS)
        WRITE (PRT,2005) 
        DO 50 I = 1, NSEGS
          IF (DSNQS(I,1) .GT. 0  .OR.  DSNQS(I,2) .GT. 0) THEN
C           segment to be saved
            WRITE (PRT,2006) PCRIDA(I), DSNQS(I,1), DSNQS(I,2)
          END IF
 50     CONTINUE
        WRITE (PRT,2007)
      END IF
C
C     match segment id
      IPT = 1
      DSN = 0
      MORE = 1
 100  CONTINUE
C       check for match
        IF (QSID .EQ. PCRIDA(IPT)) THEN
C         found a match
          IF (QSQS .EQ. 5) THEN
C           discharge time series
            DSN = DSNQS(IPT,1)
            MORE = 0
          ELSE IF (QSQS .EQ. 6) THEN
C           sediment time series
            DSN = DSNQS(IPT,2)
            MORE = 0
          ELSE
C           invalid type
            MORE = 0
          END IF
        ELSE
C         no match, keep looking
          IPT = IPT + 1
        END IF
      IF (IPT .LE. NSEGS  .AND.  MORE .EQ. 1) GO TO 100
C
      RETURN
      END
