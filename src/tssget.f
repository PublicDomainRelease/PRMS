C
C
C
      SUBROUTINE   DVRETR
C
C     + + + PURPOSE + + +
C     ?????
C
C     + + + COMMONS + + +
      INCLUDE 'cwdsn.inc'
      INCLUDE 'cdates.inc'
      INCLUDE 'cdvdt.inc'
      INCLUDE 'cuvrt.inc'
      INCLUDE 'copt.inc'
      INCLUDE 'csize.inc'
      INCLUDE 'cscs.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      TSF,DSN,DELT,DFLG,DTOVWR,DTRAN,QFLG,TUNITS,          LS0287
     #             NVAL,    SDATE(6),RETCOD,CHK(6),EDATE(6),
     #             I, J, IRC, NST, N, K
      REAL         BUFF(366)
C
C     + + + EXTERNALS + + +
      EXTERNAL   CKDATE, NUMPTS, WDTGET, TSSRCD
C
C     + + + OUTPUT FORMATS + + +
C
 100  FORMAT(' IN DVRETR, ZERO VALUE FOR DSN ENCOUNTERED!'/
     1       5X,'JDS=',I6,5X,'IWY=',I6/
     2       5X,'DSNP=',5I5,5X,5I5/
     3       5X,'DSNC=',8I5)
  101 FORMAT(   1X, 'ERROR IN RETRIEVAL OF DAILY VALUES :',              KF0684
     *       /, 5X, I5, ' = TIME SERIES FILE',                           KF0684
     *       /, 5X, I5, ' = DATA SET INDEX NUMBER',                      KF0684
     *       /, 5X, I5, ' = TIME STEP ',                                 KF0684
     *       /, 5X, I5, ' = NUMBER OF VALUES BEING RETRIEVED',           KF0684
     *       /, 5X, I5, ' = NUMBER OF DAYS SKIPPED' )                    KF0684
  102 FORMAT(   1X, 'THE TIME PERIOD BEING RETRIEVED : ',                KF0684
     *              'START -', I5, 4I3, '   END -', I5, 4I3 )            KF0684
C
C     + + + END SPECIFICATIONS + + +
C
      SDATE(6)=0                                                        0686KF
      EDATE(6)=0                                                        0686KF
      CHK(6)=0                                                          0686KF
C                                                                        KF0684
C     ZERO OUT TIME SERIES ARRAYS                                        KF0684
C                                                                        KF0684
      DO 120 I = 1, JDS                                                  KF0684
         DO 110 J = 1, 366                                               KF0684
            DVP(J,I) = 0.0                                               KF0684
  110    CONTINUE                                                        KF0684
  120 CONTINUE                                                           KF0684
      DO 140 I = 1, 7                                                    KF0684
         DO 130 J = 1, 366                                               KF0684
            DS(J,I) = 0.0                                                KF0684
  130    CONTINUE                                                        KF0684
  140 CONTINUE                                                           KF0684
C
C     WRITE(72,*) ' IN DVRETR, BYR,BMO,BDY=',BYR,BMO,BDY
C     WRITE(72,*) ' IN DVRETR, EYR,EMO,EDY=',EYR,EMO,EDY
C     WRITE(72,*) ' IN DVRETR, IWY=',IWY
      TSF = 10
      DTOVWR = 0
      DTRAN=0                                                           LS0287
      QFLG=31                                                           LS0287
      TUNITS=2                                                          LS0287
      DELT = 1440
      IRC = 0                                                            KF0684
C
C     DETERMINE START DATE WITHIN THE WATER YEAR
C
      SDATE(1) = IWY -1
      SDATE(2) = 10
      SDATE(3) = 1
      SDATE(4) = 0                                                      LS0287
      SDATE(5) = 0
      SDATE(6) = 0
      CHK(1) = BYR
      CHK(2) = BMO
      CHK(3) = BDY
      CHK(4) = 0                                                        LS0287
      CHK(5) = 0
      CHK(6) = 0
      CALL CKDATE (CHK,SDATE,DFLG)                                      0686KF
C     WRITE(72,*) ' DFLG=',DFLG
      IF (DFLG.EQ.1) THEN                                               0686KF
C       CHK DATE FOLLOWS SDATE
        SDATE(1) = CHK(1)
        SDATE(2) = CHK(2)
        SDATE(3) = CHK(3)
      END IF                                                            0686KF
C
C     DETERMINE END DATE WITHIN THE WATER YEAR
C
      EDATE(1) = IWY
      EDATE(2) = 9
      EDATE(3) = 30
      EDATE(4) = 24
      EDATE(5) = 0
      EDATE(6) = 0
      CHK(1) = EYR
      CHK(2) = EMO
      CHK(3) = EDY
      CHK(4) = 24                                                       LS0287
      CHK(5) = 0
      CHK(6) = 0
      CALL CKDATE (EDATE,CHK,DFLG)                                      0686KF
C     WRITE(72,*) ' DFLG=',DFLG
      IF (DFLG.EQ.1) THEN                                               0686KF
C       EDATE FOLLOWS CHK
        EDATE(1) = CHK(1)
        EDATE(2) = CHK(2)
        EDATE(3) = CHK(3)
      END IF                                                            0686KF
C
C     WRITE(72,*) ' SDATE=',SDATE
C     WRITE(72,*) ' EDATE=',EDATE
      CALL NUMPTS (SDATE,EDATE,DELT,NVAL)
C     NUMBER OF DAYS FROM OCT 1 TO START
      CHK(1) = IWY - 1
      CHK(2) = 10
      CHK(3) = 1
      CHK(4) = 0                                                        LS0287
      CALL NUMPTS (CHK,SDATE,DELT,NST)
      IF (NST.GT.0) NST = NST - 1
C
      IF (NST+NVAL.GT.366) THEN                                         0686KF
        WRITE(72,*) ' BAD NEWS! IN DVRETR'
C       WRITE(72,*) ' NVAL,NST=',NVAL,NST
        STOP 'DVRETR'
       END IF                                                           0686KF
C
C     GET PRECIPITATION DATA
C
      DO 30 I = 1,JDS
        DSN = DSNP(1,I)
        IF (DSN.LE.0) THEN                                              0686KF
         WRITE(72,100) JDS,IWY,((DSNP(N,J),J=1,5),N=1,2),(DSNC(N),N=1,8)
         STOP
        END IF                                                          0686KF
C       WRITE(72,*) ' CALL TSDGET 1, TSF,DSN,DELT=',TSF,DSN,DELT
C       WRITE(72,*) ' SDATE=',SDATE
        CALL WDTGET (TSF,DSN,DELT,SDATE,NVAL,DTRAN,QFLG,TUNITS,         LS0287
     *               BUFF,RETCOD)                                       LS0287
        IF (RETCOD.NE.0) THEN                                           0686KF
          CALL TSSRCD (RETCOD,DTOVWR)
          WRITE(72,101) TSF, DSN, DELT, NVAL, NST                         KF0684
          IRC = IRC + 1                                                  KF0684
        END IF                                                          0686KF
C       DO 10 J=1,366                                                    KF0684
C         DVP(J,I) = 0.0                                                 KF0684
C10     CONTINUE                                                         KF0684
        DO 20 K = 1,NVAL
          J = K + NST
          DVP(J,I) = BUFF(K)
 20     CONTINUE
C       WRITE(72,*)' DVP=',(DVP(N,I),N=1,366)
 30   CONTINUE
C
C     GET STREAMFLOW(1) OR PAN EVAP(2) OR RADIATION(5)                  LS0287
C
      DO 70 I = 1,7
        IF(I.EQ.3.OR.I.EQ.4.OR.I.EQ.6) GO TO 70                         GL081584
        IF (IDUS(I).EQ.1) THEN                                          0686KF
          DSN = DSNC(I)                                                 0686KF
C         WRITE(72,*)' DSN,YEAR=',DSN,YEAR                               0686KF
C         WRITE(72,*) ' CALL TSDGET 2, TSF,DSN,DELT=',TSF,DSN,DELT       0686KF
C         WRITE(72,*) ' SDATE=',SDATE                                    0686KF
          CALL WDTGET (TSF,DSN,DELT,SDATE,NVAL,DTRAN,QFLG,TUNITS,       LS0287
     *                 BUFF,RETCOD)                                     LS0287
          IF (RETCOD.NE.0) THEN                                         0686KF
            CALL TSSRCD (RETCOD,DTOVWR)
            WRITE(72,101) TSF, DSN, DELT, NVAL, NST                       KF0684
            IRC = IRC + 1                                                KF0684
          END IF                                                        0686KF
C         DO 38 J = 1,366                                                KF0684
C           DS(J,I) = 0.0                                                KF0684
C38       CONTINUE                                                       F0684
          DO 40 K = 1,NVAL
            J = K + NST
            DS(J,I) = BUFF(K)
   40     CONTINUE
        END IF                                                          0686KF
C       WRITE(72,*)' DS=',(DS(N,I),N=1,366)
 70   CONTINUE
C
C     GET MAX TEMPERATURE DATA
C
      DO 230 I = 1,NTS                                                  GL081584
        DSN = DSNT(1,I)
        IF (DSN.EQ.0) THEN                                              LS0287
         WRITE(72,100) JDS,IWY,((DSNT(N,J),J=1,5),N=1,2),(DSNC(N),N=1,8)
         STOP
        END IF                                                          LS0287
C       WRITE(72,*) ' CALL TSDGET 1, TSF,DSN,DELT=',TSF,DSN,DELT
C       WRITE(72,*) ' SDATE=',SDATE
        CALL WDTGET (TSF,DSN,DELT,SDATE,NVAL,DTRAN,QFLG,TUNITS,         LS0287
     *               BUFF,RETCOD)                                       LS0287
        IF (RETCOD.NE.0) THEN                                           LS0287
          CALL TSSRCD (RETCOD,DTOVWR)
          WRITE(72,101) TSF, DSN, DELT, NVAL, NST                         KF0684
          IRC = IRC + 1                                                  KF0684
        END IF                                                          LS0287
C       DO 10 J=1,366                                                    KF0684
C         DVP(J,I) = 0.0                                                 KF0684
C10     CONTINUE                                                         KF0684
        DO 220 K = 1,NVAL
          J = K + NST
          DVTX(J,I) = BUFF(K)
 220    CONTINUE
C       WRITE(72,*)' DVP=',(DVP(N,I),N=1,366)
 230  CONTINUE
C
C     GET MIN TEMPERATURE DATA
C
      DO 330 I = 1,NTS                                                  GL081584
        DSN = DSNT(2,I)
        IF (DSN.EQ.0) THEN                                              LS0287
         WRITE(72,100) JDS,IWY,((DSNT(N,J),J=1,5),N=1,2),(DSNC(N),N=1,8)
         STOP
        END IF                                                          LS0287
C       WRITE(72,*) ' CALL TSDGET 1, TSF,DSN,DELT=',TSF,DSN,DELT
C       WRITE(72,*) ' SDATE=',SDATE
        CALL WDTGET (TSF,DSN,DELT,SDATE,NVAL,DTRAN,QFLG,TUNITS,         LS0287
     *               BUFF,RETCOD)                                       LS0287
        IF (RETCOD.NE.0) THEN                                           LS0287
          CALL TSSRCD (RETCOD,DTOVWR)
          WRITE(72,101) TSF, DSN, DELT, NVAL, NST                         KF0684
          IRC = IRC + 1                                                  KF0684
        END IF                                                          LS0287
C       DO 10 J=1,366                                                    KF0684
C         DVP(J,I) = 0.0                                                 KF0684
C10     CONTINUE                                                         KF0684
        DO 320 K = 1,NVAL
          J = K + NST
          DVTN(J,I) = BUFF(K)
 320    CONTINUE
C       WRITE(72,*)' DVP=',(DVP(N,I),N=1,366)
 330  CONTINUE
C
C     GET SNOW PILLOW DATA
C
      IF(NPLW.NE.0) THEN                                                GL081584
        DO 430 I = 1,NPLW                                               GL081584
          DSN = DSNS(I)
          IF (DSN.EQ.0) THEN                                            LS0287
            WRITE(72,100) JDS,IWY,((DSNP(N,J),J=1,5),N=1,2),             LS0287
     *      (DSNC(N),N=1,8)                                             LS0287
            STOP
          END IF                                                        LS0287
C         WRITE(72,*) ' CALL TSDGET 1, TSF,DSN,DELT=',TSF,DSN,DELT
C         WRITE(72,*) ' SDATE=',SDATE
          CALL WDTGET (TSF,DSN,DELT,SDATE,NVAL,DTRAN,QFLG,TUNITS,       LS0287
     *                 BUFF,RETCOD)                                     LS0287
          IF (RETCOD.NE.0) THEN                                         LS0287
            CALL TSSRCD (RETCOD,DTOVWR)
            WRITE(72,101) TSF, DSN, DELT, NVAL, NST                       KF0684
            IRC = IRC + 1                                                KF0684
          END IF                                                        LS0287
C         DO 10 J=1,366                                                  KF0684
C           DVP(J,I) = 0.0                                               KF0684
C10       CONTINUE                                                       KF0684
          DO 420 K = 1,NVAL
            J = K + NST
            DVSP(J,I) = BUFF(K)
  420     CONTINUE
C         WRITE(72,*)' DVP=',(DVP(N,I),N=1,366)
  430   CONTINUE
      END IF                                                            LS0287
      IF(IRC .NE. 0) THEN                                                KF0684
        WRITE(72,102)(SDATE(J),J=1,5),(EDATE(J),J=1,5)                   LS0287
        STOP                                                             ** STOP
      END IF                                                             KF0684
C
C     IF SIMULATING UNIT VALUES, THEN GET UVWY ARRAY FROM FILE 11
C
      IF (ISIM.GT.0) THEN                                               LS0790  P
  450   READ(11) UVWY                                                   LS0790
        IF(IWY.GT.UVWY(367)) GO TO 450                                   LS0790
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   UVRET
     I                  ( OWY )
C
C     + + + PURPOSE + + +
C     Routine gets unit data from wdm file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     OWY                                                   LS0287
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OWY    - ?????
C
C     + + + COMMONS + + +
      INCLUDE 'cwdsn.inc'
      INCLUDE 'csize.inc'
      INCLUDE 'call.inc'
      INCLUDE 'cuvrt.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     TSF,DSN(6),YEAR,DELT(6),                              0485KF
     1                              IBUFF(512),IR,I,ID,                 0485KF
     1            BEGREC,TDFREC,TDLREC,INTRVL,NDAYS,                    0485KF
     3            NVAL(6),    DATE(6),DTOVWR,RETCOD,DATEO(6)            LS0888
      INTEGER   TCODE, TSSTEP, RETC, STAI, DTRAN, QFLG,TUNITS           LS0287
      INTEGER     FO, UVRDG1, UVNRDG, UVPARM(6), UVSTAT(6),
     #            IRC, JDS, J, NVL, L, K
      REAL        UVDATA(1440),UVSTA(4,6),UVNVAL                        0485KF
      REAL        BUFF(512)                                             0485KF
      CHARACTER*16 CODE                                                 0985KF
      CHARACTER*1 STAC(16)                                              LS0287
      LOGICAL     UVDEL
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (BUFF(1),IBUFF(1))                                    0485KF
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDBSGI, WDBSGC, TIMADD, WDTGET, TSSRCD
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  DTOVWR,UVDEL,TSF,UVNVAL,UVRDG1,DTRAN,QFLG,TUNITS            LS0287
     *     /    0,.TRUE.,10, 999.0,  1,     0,    31,   2/              LS0287
C
C     + + + OUTPUT FORMATS + + +
  101 FORMAT(  1X, 'ERROR IN RETRIEVAL OF UNIT VALUES :',
     *       /,5X, I5, 4I3, ' - DATE',
     *       /,5X, I5, ' = TIME SERIES FILE',
     *       /,5X, I5, ' = DATA SET NUMBER',
     *       /,5X, I5, ' = TIME STEP',
     *       /,5X, I5, ' = NUMBER OF VALUES BEING RETRIEVED' )
  102 FORMAT(  1X, 'SIMULATION WILL NOT BE RUN.' )
  103 FORMAT( I16 )                                                     0985KF
  104 FORMAT(16A1 )                                                     LS0287
  105 FORMAT( 'NO STATION ID   ' )                                      0985KF
  106 FORMAT( 4A4 )                                                     0985KF
  107 FORMAT( ' !!!!ERROR!!!! BAD DATA SET -', I7 )                     0985KF
  108 FORMAT( ' !!!! ERROR !!!!  INVALID TIME SERIES FILE ! ', /,       1189KF
     #              '            RETCOD =', I6 )                        1189KF
C
C     + + + END SPECIFICATIONS + + +
C
      IF (OWY.EQ.0) RETURN
C
      DATE(6)=0                                                           0686KF
      IRC = 0
      JDS = NDS                                                         0485KF
C     DETERMINE DATA SETS TO BE RETRIEVED                               0485KF
      DO 10 IDS = 1, JDS                                                0485KF
        DSN(IDS) = DSNP(2,IDS)                                          0485KF
        UVSTAT(IDS) = 6                                                 0485KF
        UVPARM(IDS) = 45                                                0485KF
 10     CONTINUE                                                        0485KF
      IF(IDUS(8) .NE. 0) THEN                                           0485KF
C       UNIT DISCHARGE DATA AVAILABLE                                   0485KF
        JDS = JDS + 1                                                   0485KF
        DSN(JDS) = DSNC(8)                                              0485KF
        UVSTAT(IDS) = 3                                                 0485KF
        UVPARM(IDS) = 60                                                0485KF
      ENDIF                                                             0485KF
C                                                                       0485KF
C     READ LABEL FOR EACH DATA SET                                      0485KF
C       WDS FILE                                                        0985KF
        DO 30 IDS = 1, JDS                                              0985KF
          CALL WDBSGI( TSF, DSN(IDS), 17, 1, TCODE, RETC )              0985KF
          IF ((TCODE .EQ. 2 .OR.  TCODE .EQ. 3) .AND.  RETC .EQ. 0) THEN0985KF
C           GOOD TIME CODE                                              0985KF
            CALL WDBSGI( TSF, DSN(IDS), 33, 1, TSSTEP, RETC )           0985KF
            IF (RETC .EQ. 0) THEN                                       0985KF
C             GOOD TIME STEP                                            0985KF
              IF (TCODE .EQ. 2) THEN                                    0985KF
C               TIME STEP IS MINUTES                                    0985KF
                DELT(IDS) = TSSTEP                                      0985KF
              ELSE IF (TCODE .EQ. 3) THEN                               0985KF
C               TIME STEP IS HOURS                                      0985KF
                DELT(IDS) = TSSTEP * 60                                 0985KF
              END IF                                                    0985KF
              NVAL(IDS) = 1440 / DELT(IDS)                              0985KF
C                                                                       0985KF
C             GET STATION NUMBER                                        0985KF
              CALL WDBSGI( TSF, DSN(IDS), 51, 1, STAI, RETC )           0985KF
              IF (RETC .EQ. 0) THEN                                     0985KF
C               INTEGER STATION ID                                      0985KF
                WRITE(CODE,103) STAI                                    0985KF
              ELSE                                                      0985KF
C               NOT INTEGER, TRY CHARACTER                              0985KF
                CALL WDBSGC( TSF, DSN(IDS), 2,16, STAC, RETC )          LS0287
                IF (RETC .EQ. 0) THEN                                   0985KF
C                 CHARACTER STATION ID                                  0985KF
                  WRITE(CODE,104) STAC                                  0985KF
                ELSE                                                    0985KF
C                 NO STATION ID                                         0985KF
                  WRITE(CODE,105)                                       0985KF
                END IF                                                  0985KF
              END IF                                                    0985KF
              READ(CODE,106) ( UVSTA(J,IDS), J = 1, 4 )                 0985KF
            ELSE                                                        0985KF
C             BAD TSSTEP                                                0985KF
              WRITE(72,107) DSN(IDS)                                     0985KF
              IRC = IRC + 1                                             0985KF
            END IF                                                      0985KF
          ELSE                                                          0985KF
C           BAD TCODE                                                   0985KF
            WRITE(72,107) DSN(IDS)                                       0985KF
            IRC = IRC + 1                                               0985KF
          END IF                                                        0985KF
 30     CONTINUE                                                        0985KF
C                                                                       0485KF
C     RETRIEVE THE TIME SERIES DATA                                     0485KF
      YEAR = UVWY(367)                                                  0485KF
      NDAYS = 365                                                       0485KF
      IF(MOD(YEAR,4) .EQ. 0) NDAYS = 366                                0485KF
          DATE(1) = YEAR - 1                                            LS0888
          DATE(2) = 10                                                  LS0888
          DATE(3) = 1                                                   LS0888
          DATE(4) =  0                                                  LS0888
          DATE(5) = 0                                                   LS0888
          INTRVL = 1440 * ( I - 1 )                                     LS0888
      DO 60 I = 1, NDAYS                                                0485KF
        IF(UVWY(I) .GT. 0) THEN                                         0485KF
C         UNIT DAY                                                      0485KF
          TUNITS=4                                                      LS0888
          TSSTEP=1                                                      LS0888
          NVL=I-1                                                       LS0888
          CALL TIMADD(DATE,TUNITS,TSSTEP,NVL ,DATEO)                    LS0888
C         DATE(1)=DATEO(1)                                              LS0888
C         DATE(2)=DATEO(2)                                              LS0888
C         DATE(3)=DATEO(3)                                              LS0888
C         DATE(4)=DATEO(4)                                              LS0888
C         DATE(5)=DATEO(5)                                              LS0888
          FO = 15                                                       0485KF
          DO 50 IDS = 1, JDS                                            0485KF
            IF(IDS .GT. NDS  .AND.  UVWY(I) .LT. 2) GO TO 50            0485KF
            IF(IDS .GT. NDS) FO = 14                                    0485KF
C             DATE(4) = 0                                               LS0287
C             DATE(5) = DELT(IDS)                                       LS0287
C             IF(DELT(IDS) .EQ. 60) THEN                                LS0287
C               DATE(4) = 1                                             LS0287
C               DATE(5) = 0                                             LS0287
C             ENDIF                                                     LS0287
              UVNRDG = NVAL(IDS)                                        0485KF
              TUNITS=2                                                  LS0888
              CALL WDTGET( TSF, DSN(IDS),      DELT(IDS), DATEO,         LS0287
     *                     NVAL(IDS),DTRAN,QFLG,TUNITS,UVDATA,RETCOD)   LS0287
              IF(RETCOD .NE. 0) THEN                                    0485KF
C               ERROR IN RETRIEVING DATA                                0485KF
                CALL TSSRCD( RETCOD, DTOVWR )                           0485KF
                WRITE(72,101)(DATEO(L),L=1,5),TSF,DSN(IDS),DELT(IDS),    0485KF
     *          NVAL(IDS)
                IRC = IRC + 1                                           0485KF
              ENDIF                                                     0485KF
              WRITE(FO) UVDEL, ( UVSTA(J,IDS), J = 1, 4 ), UVPARM(IDS), 0485KF
     *                  UVSTAT(IDS), (DATEO(L), L = 1, 3 ), NVAL(IDS),  0485KF
     *                  UVRDG1, UVNRDG, UVNVAL,                         0485KF
     *                  ( UVDATA(K), K = 1, UVNRDG )                    0485KF
 50       CONTINUE                                                      0485KF
        ENDIF                                                           0485KF
 60   CONTINUE                                                          0485KF
C                                                                       0485KF
      IF(IRC .GT. 0) THEN                                               0485KF
C       PROBLEM WITH RETRIEVAL OF DATA                                  0485KF
        WRITE(72,102)                                                    0485KF
        STOP 'UVRET'                                                    0485KF
      ENDIF                                                             0485KF
      RETURN                                                            0485KF
      END                                                               0485KF
C
C
C
      SUBROUTINE   TSSRCD
     M                   ( RETCOD, DTOVWR )
C
C     + + + PURPOSE + + +
C     Write error messages for TSDGET and TSDPUT.
C
C     + + +   DUMMY ARGUMENTS   + + +
      INTEGER RETCOD,DTOVWR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     RETCOD - ?????
C     DTOVWR - ?????
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   OT
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  OT / 72 /
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/, 5X, '***** Unknown error code =', I5 )
 2008 FORMAT (/, 5X, '***** invalid date' )
 2014 FORMAT (/, 5X, '***** Date not within valid range for data set')
 2020 FORMAT (/, 5X, '***** Problem with retrieve parameters')
 2021 FORMAT (/, 5X, '***** Date from wdm does not match expected date')
 2081 FORMAT (/, 5X, '***** Data set does not exist')
 2082 FORMAT (/, 5X, '***** Data set exists but is wrong data set type')
 2084 FORMAT (/, 5X, '***** Data set number outside valid range')
C
C     + + + END SPECIFICATIONS + + +
C
C     return codes from wdtget
      IF (RETCOD .EQ. -8) THEN
C       invalid date
        WRITE (OT,2008)
      ELSE IF (RETCOD .EQ. -14) THEN
C       requested date not within valid range for data set
        WRITE (OT,2014)
      ELSE IF (RETCOD .EQ. -20) THEN
C       problem with [GPFLG, DXX, NVAL, QUALVL, LTSTEP, LTUNIT]
        WRITE (OT,2020)
      ELSE IF (RETCOD .EQ. -21) THEN
C       date from wdm doesn't match expected date
        WRITE (OT,2021)
      ELSE IF (RETCOD .EQ. -81) THEN
C       data set does not exist
        WRITE (OT,2081)
      ELSE IF (RETCOD .EQ. -82) THEN
C       data set exists but wrong type
        WRITE (OT,2082)
      ELSE IF (RETCOD .EQ. -84) THEN
C       data set number out of range
        WRITE (OT,2084)
      ELSE
C       unknown problem
        WRITE (OT,2000) RETCOD
      END IF
C
      RETURN
      END
