C
C
C
      SUBROUTINE   GTDSNS
C
C     + + + PURPOSE + + +
C     This routine is used to get the data set numbers and related
C     information about the input and output time series used by
C     the model.
C
C     + + + COMMONS + + +
C
C     + + + LOCAL VARIABLES + + +
C
C     + + + DATA INITIALIZATIONS + + +
C
C     + + + INPUT FORMATS + + +
C
C     + + + OUTPUT FORMATS + + +
C
C     + + + END SPECIFICATIONS + + +
C
      READ (IN,1000,ERR=110) IC, NDTY, NDTYO
C       successful read
        FLAG = 0
        IF (IC .NE. JC(1)) THEN
C         found wrong record
          FLAG = 1
          WRITE (OT,2000) JC(1), IC
        END IF
        GO TO 115
 110  CONTINUE
C       read error
        FLAG = 1
        WRITE (OT,2001) JC(1)
 115  CONTINUE
C
      IF (FLAG .EQ. 0) THEN
C       initialize arrays
        CALL ZIPI ( LN10, LN0, IDUS )
        CALL ZIPI ( LN10, LN0, PARMCA )
        CALL ZIPI ( LN10, LN0, STATCA )
C       get information on input data sets
        N = 0
 200    CONTINUE
          N = N + 1
          READ (IN,1001) CARD
          IF (CARD(1:2) .EQ. JC(2)) THEN
C           correct record, process information
            READ (CARD,1002,ERR=210) TYPE, K, DSN, PARM, STAT, STAID
C             find data type, check for daily or unit
              I = STRFND ( LNT, TYPES, LN4, TYPE )
              J = STRFND ( LN2, DORU, LN1, CARD(__:__) )
              IF (J .EQ. 3) THEN
C               default missing data time to daily
                J = 1
                WRITE (OT,2002) JC(2), CARD
              END IF
              IF (I .EQ. 0  .OR.  J .EQ. 0) THEN
C               unknown data type or time
                FLAG = 1
                WRITE (OT,2003) JC(2), CARD
              END IF
              GO TO 215
 210        CONTINUE
C             read error
              FLAG = 1
              WRITE (OT,2004) JC(2), CARD
 215        CONTINUE
          ELSE
C           not expected record
            FLAG = 1
            WRITE (OT,200_) JC(2), CARD(1:2), CARD
          END IF
C
          IF (FLAG .EQ. 0) THEN
C           store data in appropriate arrays
            IF (I .GT. 0  .AND.  I .LT. 8) THEN
C             check for unit discharge
              IF (I .EQ. 1  .AND.  J .EQ. 2) I = 8
              IDUS(I)   = 1
              PARMCA(I) = PARM
              STATCA(I) = STAT
              STAIDC(I) = STAID
              DSNC(I)   = DSN
            ELSE IF (I .EQ. 8) THEN
C             precipitaiton
              IF (K .GE. 1  .AND.  K .LE. 5) THEN
                I = 9
                IF (J .EQ. 2) I = 10
                IDUC(I)   = 1
                PARMCA(I) = PARM
                STATCA(I) = STAT
                STAIDP(I) = STAID
                DSNP(J,K) = DSN
              ELSE
C               outside array limits
                WRITE (OT,2005) JC(2), K, CARD
                FLAG = 1
              END IF
            ELSE IF (I .EQ. 9) THEN
C             snow data
              IF (K .GE. 1  .AND.  K .LE. 5) THEN
                DSNT(J,K) = DSN
              ELSE
C               outside array limits
                WRITE (OT,2005) JC(2), K, CARD
                FLAG = 1
              END IF
            END IF
          END IF
        IF (FLAG .EQ.  .AND.  N .LT. NDTY) GO TO 200
      END IF
C
      IF (FLAG .EQ. 0  .AND.  NDTYO .GT. 0) THEN
C       get information on output data sets
        CALL ZIPI ( LNOT, LN0, DSNO )
        N = 0
 300    CONTINUE
          N = N + 1
          READ (IN,1001) CARD
          IF (CARD(1:2) .EQ. JC(3)) THEN
C           correct record, process information
            READ (CARD,1005,ERR=310) TYPE, K, DSN, STAID
C             find data type, check for daily or unit
              I = STRFND ( LNTO, TYPESO, LN4, TYPE )
              J = STRFND ( LN2, DORU, LN1, CARD(__:__) )
              IF (I .EQ. 0  .OR.  J .EQ. 0) THEN
C               unknown data type or time
                FLAG = 1
                WRITE (OT,2003) JC(3), CARD
              END IF
            ELSE
C             not expected record
              FLAG = 1
              WRITE (OT, 20__) JC(3), CARD(1:2), CARD
            END IF
C
            IF (FLAG .EQ. 0) THEN
C             store data in appropriate arrays
              IF (I .GT. 0  .AND.  I .LE. 1) THEN
                IF (K .EQ. 1) THEN
                  DSNO(I,K) = DSN
                  STAIDO(I) = STAID
                ELSE
C                 outside array limits
                  FLAG = 1
                  WRITE (OT,2005) JC(3), K, CARD
                END IF
              END IF
            END IF
          IF (FLAG .EQ. 0  .AND. N .LT. NDTYO) GO TO 300
        END IF
C
      IF (FLAG .NE. 0) THEN
C       typically ungraceful exit
        STOP
      END IF
C
      RETURN
      END
