C
C
C
      SUBROUTINE   DVPLOT
C
C     + + + PURPOSE + + +
C     Generate line printer plot of daily hydrographs.
C
C     + + + COMMONS + + +
      INCLUDE 'csize.inc'
      INCLUDE 'cstach.inc'
      INCLUDE 'cplot.inc'
      INCLUDE 'cjm.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   P, MTH(12), J, LEAP, JWY, IBPL, IEPL, IDP, IMP, LTH,
     #          JJ, I, K, LP, IPR, IOB
      REAL      MOCHAR(12), DAY(31), C3, PRPROS(366), PROROS(366),
     #          BUF1(120),BUF5(120),Y(11),BUFLN1(100),BUFLN5(100),
     #          BUFLG1(120),BUFLG5(120), ZP, ZO, ZR, ZSTAR, CON, RNG,
     #          RNGF, YINC, CMIN, PCHR, OCHR, SAVP, SAVO
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD, ALOG
C
C     + + + DATA INITIALIZATIONS + + +
      DATA MTH/92,123,151,182,212,243,273,304,335,0,31,61/
      DATA BUFLN1/9*1H ,1H!,9*1H ,1H!,9*1H ,1H!,9*1H ,1H!,9*1H ,1H!,    XXXXXXXX
     19*1H ,1H!,9*1H ,1H!,9*1H ,1H!,9*1H ,1H!,9*1H ,1HI/                XXXXXXXX
      DATA BUFLN5/9*1H-,1H+,9*1H-,1H+,9*1H-,1H+,9*1H-,1H+,9*1H-,1H+,
     19*1H-,1H+,9*1H-,1H+,9*1H-,1H+,9*1H-,1H+,9*1H-,1HI/                XXXXXXXX
      DATA BUFLG1/11*1H ,1H!,15*1H ,1H!,11*1H ,1HI,11*1H ,1H!,15*1H ,   XXXXXXXX
     11H!,11*1H ,1HI,11*1H ,1H!,15*1H ,1H!,11*1H ,1HI/                  XXXXXXXX
      DATA BUFLG5/11*1H-,1H+,15*1H-,1H+,11*1H-,1H+,11*1H-,1H+,15*1H-,
     11H+,11*1H-,1H+,11*1H-,1H+,15*1H-,1H+,11*1H-,1HI/                  XXXXXXXX
      DATA ZP,ZO,ZR,ZSTAR/1HP,1HO,1HR,1H*/
      DATA CON/17.3716668/
      DATA MOCHAR/3HJAN,3HFEB,3HMAR,3HAPR,3HMAY,3HJUN,3HJUL,3HAUG,3HSEP,XXXXXXXX
     1            3HOCT,3HNOV,3HDEC/                                    XXXXXXXX
      DATA DAY/3H  1,3H  2,3H  3,3H  4,3H  5,3H  6,3H  7,3H  8,3H  9,   XXXXXXXX
     1   3H 10,3H 11,3H 12,3H 13,3H 14,3H 15,3H 16,3H 17,3H 18,3H 19,   XXXXXXXX
     2   3H 20,3H 21,3H 22,3H 23,3H 24,3H 25,3H 26,3H 27,3H 28,3H 29,   XXXXXXXX
     3   3H 30,3H 31/                                                   XXXXXXXX
C
C     + + + OUTPUT FORMATS + + +
 9000 FORMAT('1',A70 ,5X,'WATER YEAR=',I5,//,1X,                        KD1291
     1'O=OBSERVED  P=PREDICTED  *=O AND P  R=OUT OF RANGE',//)          KD1291
 9005 FORMAT(40X,'DAILY MEAN DISCHARGE, IN CFS',/)                      KD1291
C9005 FORMAT(40X,'DAILY MEAN DISCHARGE, IN CFS',/)                      KD1291
 9010 FORMAT(4X,F5.0,10F10.0,' RAIN'/,8X,'I',100A1)                     KD1291
 9015 FORMAT(1X,F5.0,F12.0,4X,3F12.0,4X,3F12.0,4X,2F12.0,' RAIN')       XXXXXXXX
 9020 FORMAT(1X,A3,1X,'I',120A1,F6.2)                                   KD1291
 9025 FORMAT(8X,'I',100A1)                                              KD1291
 9026 FORMAT(5X,'I',120A1)                                              KD1291
 9030 FORMAT(1X,A3,I3,1X,'I',100A1,F6.2)                                KD1291
C
C     + + + END SPECIFICATIONS + + +
C
C     FILE NUMBER FOR PLOT FILE                                         XXXXXXXX
      P = 6                                                             XXXXXXXX
C                                                                       XXXXXXXX
      DO 8000 J=1,366
      PRPROS(J)=PRPRO(J)
 8000 PROROS(J)=PRORO(J)
      LEAP=0
      JWY=IWY
      IF(MOD(JWY,4).EQ.0) LEAP=1
      IBPL=MTH(IMPLB)+IDPLB
      IF(IMPLB.GT.1.AND.IMPLB.LT.10) IBPL=IBPL+LEAP
      IEPL=MTH(IMPLE)+IDPLE
      IF(IMPLE.GT.1.AND.IMPLE.LT.10) IEPL=IEPL+LEAP
      WRITE(P,9000) TITL,IWY                                            LS0287
      WRITE(P,9005)
      IDP=IDPLB
      IMP=IMPLB
      IF(IPLTYP.EQ.1) GO TO 30
C     ARITHMETIC AXIS                                                   XXXXXXXX
      LTH=100
C     IPMX=PLMX+.05                                                     LS0287
C     IPMN=PLMN+.05                                                     LS0287
      RNG=PLMX-PLMN
      RNGF=RNG/100.
      Y(1)=PLMN
      Y(11)=PLMX
      YINC=RNG/10.
      DO 20 J=2,10
   20 Y(J)=Y(J-1)+YINC
      WRITE(P,9010) (Y(J),J=1,11),(BUFLN5(JJ),JJ=1,100)
      DO 22 J=1,LTH
      BUF1(J)=BUFLN1(J)
   22 BUF5(J)=BUFLN5(J)
      DO 25 J=IBPL,IEPL
      PRPRO(J)=PRPRO(J)/RNGF
   25 PRORO(J)=PRORO(J)/RNGF
      GO TO 50
C     LOGARITHMIC AXIS                                                  XXXXXXXX
   30 LTH=120
      CMIN=ALOG(PLMN)
      DO 32 J=1,LTH
      BUF1(J)=BUFLG1(J)
   32 BUF5(J)=BUFLG5(J)
      Y(1)=PLMN
      I=0
      K=1
      DO 34 J=1,3
      Y(I+2)=2.*Y(K)
      Y(I+3)=5.*Y(K)
      Y(I+4)=10.*Y(K)
      I=I+3
      K=K+3
   34 CONTINUE
      WRITE(P,9015) (Y(J),J=1,10)                                       XXXXXXXX
      WRITE(P,9026) (BUFLG5(JJ),JJ=1,120)                               XXXXXXXX
      DO 35 J=IBPL,IEPL
      IF(PRPRO(J).LT..01) PRPRO(J)=.01
      IF(PRORO(J).LT..01) PRORO(J)=.01
      PRPRO(J)=(ALOG(PRPRO(J))-CMIN)*CON
      PRORO(J)=(ALOG(PRORO(J))-CMIN)*CON
      IF(PRPRO(J).LT.1.) PRPRO(J)=1.
      IF(PRORO(J).LT.1.) PRORO(J)=1.
   35 CONTINUE
   50 DO 200 LP=IBPL,IEPL
      IPR=PRPRO(LP)+0.5
      IOB=PRORO(LP)+0.5
      PCHR=ZP
      OCHR=ZO
      IF(IPR.GT.1) GO TO 40
      IPR=1
      PCHR=ZR
      GO TO 60
   40 IF(IPR.LE.LTH) GO TO 60
      IPR=LTH
      PCHR=ZR
   60 IF(IOB.GT.1) GO TO 70
      IOB=1
      OCHR=ZR
      GO TO 80
   70 IF(IOB.LE.LTH) GO TO 80
      IOB=LTH
      OCHR=ZR
   80 IF(IDP.EQ.30.OR.LP.EQ.IBPL) GO TO 90                              XXXXXXXX
      IF(IDP.EQ.1) GO TO 100                                            XXXXXXXX
   90 SAVP=BUF1(IPR)
      SAVO=BUF1(IOB)
      BUF1(IPR)=PCHR
      BUF1(IOB)=OCHR
      IF(PCHR.EQ.ZR) GO TO 92
      IF(IPR.EQ.IOB) BUF1(IOB)=ZSTAR
   92 CONTINUE                                                          XXXXXXXX
      IF (IPLTYP.NE.0) GO TO 96                                         XXXXXXXX
        IF(PRECX(LP).LT.0.005) WRITE(P,9030) MOCHAR(IMP),IDP,(BUF1(J),  XXXXXXXX
     1                                       J=1,LTH)                   XXXXXXXX
        IF(PRECX(LP).GE.0.005) WRITE(P,9030) MOCHAR(IMP),IDP,(BUF1(J),  XXXXXXXX
     1                                       J=1,LTH),PRECX(LP)         XXXXXXXX
   96 CONTINUE                                                          XXXXXXXX
      IF (IPLTYP.NE.1) GO TO 98                                         XXXXXXXX
        IF (IDP.EQ.1) C3 = MOCHAR(IMP)                                  XXXXXXXX
        IF (IDP.GT.1) C3 = DAY(IDP)                                     XXXXXXXX
        IF (PRECX(LP).LT.0.005) WRITE(P,9020) C3,(BUF1(J),J=1,LTH)      XXXXXXXX
        IF (PRECX(LP).GE.0.005) WRITE(P,9020) C3,(BUF1(J),J=1,LTH),     XXXXXXXX
     1                                                    PRECX(LP)     XXXXXXXX
   98 CONTINUE                                                          XXXXXXXX
      BUF1(IPR)=SAVP
      BUF1(IOB)=SAVO
      GO TO 110
  100 SAVP=BUF5(IPR)
      SAVO=BUF5(IOB)
      BUF5(IPR)=PCHR
      BUF5(IOB)=OCHR
      IF(PCHR.EQ.ZR) GO TO 112
      IF(IPR.EQ.IOB) BUF5(IOB)=ZSTAR
  112 CONTINUE                                                          XXXXXXXX
      IF (IPLTYP.NE.0) GO TO 116                                        XXXXXXXX
        IF(PRECX(LP).LT.0.005) WRITE(P,9030) MOCHAR(IMP),IDP,(BUF5(J),  XXXXXXXX
     1                                       J=1,LTH)                   XXXXXXXX
        IF(PRECX(LP).GE.0.005) WRITE(P,9030) MOCHAR(IMP),IDP,(BUF5(J),  XXXXXXXX
     1                                       J=1,LTH),PRECX(LP)         XXXXXXXX
  116 CONTINUE                                                          XXXXXXXX
      IF (IPLTYP.NE.1) GO TO 118                                        XXXXXXXX
        IF (IDP.EQ.1) C3 = MOCHAR(IMP)                                  XXXXXXXX
        IF (IDP.GT.1) C3 = DAY(IDP)                                     XXXXXXXX
        IF (PRECX(LP).LT.0.005) WRITE(P,9020) C3,(BUF5(J),J=1,LTH)      XXXXXXXX
        IF (PRECX(LP).GE.0.005) WRITE(P,9020) C3,(BUF5(J),J=1,LTH),     XXXXXXXX
     1                                                    PRECX(LP)     XXXXXXXX
  118 CONTINUE                                                          XXXXXXXX
      BUF5(IPR)=SAVP
      BUF5(IOB)=SAVO
  110 IDP=IDP+1
      IF(IDP.LE.NDY(IMP)) GO TO 200
      IDP=1
      IMP=IMP+1
      IF(IMP.GT.12) IMP=1
  200 CONTINUE
      IF (IPLTYP.EQ.0) GO TO 210                                        XXXXXXXX
C       LOG PLOT - RIGHT HAND AXIS                                      XXXXXXXX
        WRITE(P,9026) (BUFLG5(J),J=1,120)                               XXXXXXXX
        WRITE(P,9015) (Y(J),J=1,10)                                     XXXXXXXX
      GO TO 220                                                         XXXXXXXX
 210  CONTINUE                                                          XXXXXXXX
C       ARITHMETIC PLOT - RIGHT HAND AXIS                               XXXXXXXX
        WRITE(P,9025) (BUFLN5(J),J=1,100)                               XXXXXXXX
        WRITE(P,9010) (Y(J),J=1,11)                                     XXXXXXXX
 220  CONTINUE                                                          XXXXXXXX
      DO 8100 J=1,366
      PRPRO(J)=PRPROS(J)
 8100 PRORO(J)=PROROS(J)
      RETURN
      END
C
C
C
      SUBROUTINE SUMUNT
C
C     + + + PURPOSE + + +
C     Summarize storms
C
C     + + + COMMONS + + +
      INCLUDE 'call.inc'
      INCLUDE 'copt.inc'
      INCLUDE 'csed.inc'
      INCLUDE 'cvol.inc'
      INCLUDE 'csent3.inc'
      INCLUDE 'cjm.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   ISIM1, J, NOBF, I
      REAL      MEAN(15), MEANOF(4,2), SUM(15), RUN(15), DIF(4),
     *          OF(4,2), ZNOBF, PER1, PER2, PER3, PER4
C
C     + + + INTRINSICS + + +
      INTRINSIC   ALOG, ABS, FLOAT, SQRT
C
C     + + + EXTERNALS + + +
      EXTERNAL   STATS
C
C     + + + OUTPUT FORMATS + + +
   11 FORMAT('1',' STORM PREDICTED  OBSERVED  OBSERVED',/,10X,          KD1291
     1'RUNOFF',4X,'RUNOFF',5X,'PEAK',/,9X,'(INCHES)',2X,                KD1291
     2'(INCHES)',3X,'(CFS)')                                            KD1291
   12 FORMAT(3X,I3,3F10.2)
   22 FORMAT('1STORM PREDICTED  ROUTED   OBSERVED   PREDICTED OBSERVED',KD1291
     1,/,'        VOLUME    OUTFLOW   OUTFLOW     PEAK     PEAK'/,      KD1291
     2'       (INCHES)   (INCHES)  (INCHES)     (CFS)     (CFS)'/,      KD1291
     3' ----- ----------------------------   ------------------')       KD1291
   23 FORMAT(3X,I3,5F10.2)
   32 FORMAT('1STORM PREDICTED  ROUTED   OBSERVED   PREDICTED OBSERVED',KD1291
     1'   PREDICTED',/,'        VOLUME    OUTFLOW   OUTFLOW     PEAK',  KD1291
     2'  PEAK     SEDIMENT',/,'       (INCHES)   (INCHES) (INCHES)',    KD1291
     3'     (CFS)     (CFS)     (TONS)',/,  ' ----- -----------------', KD1291
     4'---------   ------------------   ---------')                     KD1291
   33 FORMAT(3X,I3,6F10.2)
  260 FORMAT(/,2X,'MEAN',2F10.2,/,2X,'LOGS',2F10.2)                     KD1291
  262 FORMAT(/,2X,'MEAN',5F10.2,/,2X,'LOGS',5F10.2)                     KD1291
  271 FORMAT(///,35X,' STORM VOLUME ERROR SUMMARY')                     KD1291
  280 FORMAT(28X,'ABS VALUE OBF FNC',19X,'SUM OF SQUARES OBF FNC',/,    KD1291
     122X,'NO LOG',17X,'LOG',14X,                                       KD1291
     2'NO LOG',17X,'LOG',/,'     SUM',4F20.2,/,'    MEAN',4F20.2,/,     KD1291
     3' PERCENT',F20.2,20X,F20.2)                                       KD1291
  272 FORMAT(///,36X,' STORM PEAK ERROR SUMMARY')                       KD1291
C
C     + + + END SPECIFICATIONS + + +
C
      ISIM1=ISIM+1
      REWIND 30
      IF(ISTAT.EQ.1) CALL STATS
      GO TO (5,10,20,30),ISIM1
    5 RETURN
   10 WRITE(72,11)
      WRITE(72,12)(J,BRO(J),OBRO(J),OBPK(J),J=1,NST)
      NOBF=NST
      GO TO 60
   20 DO 21 J=1,NST
   21 PQ(J)=(PQ(J)/DAT)*.00027548
      WRITE(72,22)
      WRITE(72,23)(J,BRO(J),PQ(J),OBRO(J),BPK(J),OBPK(J),J=1,NST)
      NOBF=NST
      GO TO 60
   30 DO 31 J=1,NST
   31 PQ(J)=(PQ(J)/DAT)*.00027548
      WRITE(72,32)
      WRITE(72,33)(J,BRO(J),PQ(J),OBRO(J),BPK(J),OBPK(J),PSED(J),
     1J=1,NST)
      NOBF=NST
   60 IF(IOBS.EQ.0) RETURN                                              LS0884
      DO 100 I=1,12                                                     LS0884
      SUM(I)=0.
      RUN(I)=0.
  100 CONTINUE
      DO 101 I=1,4
      DO 101 J=1,2
      OF(I,J)=0.
  101 CONTINUE
      IF(IOBS.EQ.0) RETURN                                              LS110483
      DO 200 I=1,NOBF
      RUN(1)=OBRO(I)
      RUN(2)=ALOG(OBRO(I)+1.E-10)
      RUN(3)=BRO(I)
      RUN(4)=ALOG(BRO(I)+1.E-10)
      IF(ISIM.EQ.1)GO TO 202
      RUN(5)=OBPK(I)
      RUN(6)=ALOG(OBPK(I)+1.E-10)
      RUN(7)=BPK(I)
      RUN(8)=ALOG(BPK(I)+1.E-10)
      RUN(9)=PQ(I)
      RUN(10)=ALOG(PQ(I)+1.E-10)
      IF(ISIM.EQ.2)GO TO 202
      RUN(11)=PSED(I)
      RUN(12)=ALOG(PSED(I)+1.E-10)
  202 DO 210 J=1,12
  210 SUM(J)=SUM(J)+RUN(J)
      DIF(1)=RUN(1)-RUN(3)
      DIF(2)=RUN(2)-RUN(4)
      DIF(3)=RUN(5)-RUN(7)
      DIF(4)=RUN(6)-RUN(8)
      DO 220 J=1,4
      OF(J,1)=OF(J,1)+ABS(DIF(J))
  220 OF(J,2)=OF(J,2)+DIF(J)*DIF(J)
  200 CONTINUE
      ZNOBF=FLOAT(NOBF)
      DO 240 I=1,12
  240 MEAN(I)=SUM(I)/ZNOBF
      DO 250 I=1,4
      DO 250 J=1,2
      MEANOF(I,J)=OF(I,J)/ZNOBF
  250 CONTINUE
C
C      SKIP WRITES IF NO OBSERVED PEAKS, IE MEAN(1) =0.0                AL112383
C                                                                       AL112383
      IF (MEAN(1).LT.1.0E-8) GO TO 299                                  AL112383
      PER1=100.*MEANOF(1,1)/MEAN(1)
      PER2=100.*SQRT(MEANOF(1,2))/MEAN(1)
      IF(ISIM.EQ.1)WRITE(72,260)MEAN(3),MEAN(1),MEAN(4),MEAN(2)
      IF(ISIM.EQ.2)WRITE(72,262)MEAN(3),MEAN(9),MEAN(1),MEAN(7),
     1MEAN(5),MEAN(4),MEAN(10),MEAN(2),MEAN(8),MEAN(6)
      WRITE(72,271)
      WRITE(72,280)((OF(I,J),I=1,2),J=1,2),((MEANOF(I,J),I=1,2),J=1,2)
     1,PER1,PER2
      IF(ISIM.EQ.1)RETURN
      PER3=100.*MEANOF(3,1)/MEAN(5)
      PER4=100.*SQRT(MEANOF(3,2))/MEAN(5)
      WRITE(72,272)
      WRITE(72,280)((OF(I,J),I=3,4),J=1,2),((MEANOF(I,J),I=3,4),J=1,2)
     1,PER3,PER4
C                                                                       AL112383
  299 CONTINUE                                                          AL112383
C                                                                       AL112383
      DO 300 J = 1,NST                                                  II071183
        PQ(J) = 0.0                                                     II071183
C       BPK(J) = 0.0                                                    LS021684
 300  CONTINUE                                                          II071183
      RETURN
      END
C
C
C
      SUBROUTINE UVPLOT
C
C     + + + PURPOSE + + +
C     Generate line printer plot of unit hydrographs.
C
C     + + + COMMONS + + +
      INCLUDE 'cvol.inc'
      INCLUDE 'cuntrf.inc'
      INCLUDE 'copt.inc'
      INCLUDE 'cpcrch.inc'
      INCLUDE 'cstach.inc'
      INCLUDE 'cjm.inc'
C
C     + + + SAVES + + +
      INTEGER   LASTDY(8), LASTK(8), LASTMO(8), LASTYR(8)
      REAL      OBLAST, QXLAST(8), TBLAST(8)
      SAVE LASTDY,LASTK,LASTKX,LASTMO,LASTYR,OBLAST,QXLAST,TBLAST       LS0888
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   UVPARM, UVSTAT, QSWY, QSMO, QSDY, QSIO, QSQS, QSRPD,
     #          WY, WYD, RFL, SFL, UVNRDG, UVYR, UVMO, UVDY, UVRPD,
     #          UVRDG1, I, NS, IS, KS, ISW2, J, K, IEY, IPL, IHS,
     #          ISW, KP, IPY, IPM, IPD, IPWY, M, IRB, IRE, ICT, KX,
     #          LASTKX, KK, IY, IM, ID, JIN, JOUT, IN, IRBT, IRBO,
     #          IREO, II, NPTS, IX, IPO, NHRS, NMIN, ITIME, JJ,
     #          IPOS, IE, ICI, ICO, JPOS, J2, N1, N2
      REAL*4 UVSTA(4)                                                   LS0888
      REAL      DATA(8, 1440), UVDATA(1440), DFH(20), SMQA(1440),
     #          DATAOB(1440), QSDATA(1440), PRTLN2(120), DOT, PRED,
     #          OBS, BOTH, BLNK, RNGE, FL, OF, QMX, TB, TE, PINT,
     #          XTB, QMXO, XOLAST, UVNVAL, TIN, TOUT, TD, FAC, TU,
     #          F2, F1, OINT, OTE, QSI, SX, X5, X4, X3, X2, X1,
     #          VALUE, PT, PTB
      CHARACTER*4 QSID                                                  LS0287
      LOGICAL UVDEL
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (HA(1),DATA(1,1))                                     LS0287
C
C     + + + INTRINSICS + + +
      INTRINSIC   AMOD, ALOG10, FLOAT, MOD
C
C     + + + DATA INITIALIZATIONS + + +
      DATA DOT/1H./,PRED/1HP/,OBS/1HO/,BOTH/1H*/,BLNK/1H /,RNGE/1HR/
      DATA FL/4H IN /,OF/4H OUT/
C
C     + + + OUTPUT FORMAT + + +
   15 FORMAT(' MAXIMUM OF 8 PLOTS ALLOWED PER HYDROGRAPH SEGMENT')      KD1291
 5103 FORMAT('1',A70    ,', SEDIMENT IN G/L')                           KD1291
 5105 FORMAT('1',A70    ,', DISCHARGE IN CFS')                          KD1291
 5110 FORMAT(' ',30X,'SEGMENT ',A4,A4,2X,I2,'/',I2,'/',I4,' - ',I2,'/', KD1291
     1I2,'/',I4)                                                        KD1291
 5120 FORMAT(' PREDICTED (P), OBSERVED (O), BOTH (*), OUT OF PLOT RANG',KD1291
     1'E (R)')                                                          KD1291
 5125 FORMAT(' VALUE = PREDICTED DIVIDED BY 10 TO THE APPROPRIATE POWE',KD1291
     1'R')                                                              KD1291
 5130 FORMAT(/,10X,F8.4,22X,F8.4,20X,F9.3,19X,F9.2,18X,F9.2)
 5140 FORMAT(' TIME VALUE  ',4('|        2    |   4  | 6 | 8 |'))       KD1291
 5450 FORMAT(1X,I4,1X,F7.4,120A1)
C
C     + + + END SPECIFICATIONS + + +
C
      REWIND 13
      REWIND 14
      REWIND 18
      DO 5 I=1,1440
      QSDATA(I)=0.0
    5 UVDATA(I)=0.0
C***
C*** READ HYDROGRAPH SEPARATION FILE
C***
    1 READ(UNIT=13,END=9000)WY,WYD,NS,IS,RFL,SFL,DFH
      KS=0
      ISW2=0
      DO 5000 J=1,ISAVE
C***
C*** READ PREDICTED DATA FILE
C***
      READ(18,END=9000)QSWY,QSMO,QSDY,QSIO,QSQS,QSID,QSRPD
      READ(18)(QSDATA(K),K=1,QSRPD)
      IF(QSQS.LT.3) GO TO 5000
      KS=KS+1
      IF(KS.LE.8) GO TO 20
      WRITE(72,15)
      GO TO 5000
   20 IEY=WY
      IF(QSMO.GT.9) IEY=IEY-1
      IPL=0
      DO 1000 IHS=1,NS
      ISW=0
      IF(IHS.GT.1) GO TO 100
      IF(RFL.EQ.0) GO TO 100
C***
C***HYDROGRAPH CONTINUED FROM PREVIOUS DAY
C***
      ISW=1
      KP=LASTK(KS)
      QMX=QXLAST(KS)
      IPY=LASTYR(KS)
      IPM=LASTMO(KS)
      IPD=LASTDY(KS)
      PTB=TBLAST(KS)
      GO TO 200
C***
C*** BEGINNING OF HYDROGRAPH SEGMENT
C***
  100 KP=0
      IPWY=WY
      IPM=QSMO
      IPD=QSDY
      IPY=IPWY
      IF(IPM.GT.9) IPY=IPWY-1
      QMX=0.
C***
C*** DETERMINE BEGIN AND END TIMES AND INDEX OF HYDROGRAPH SEGMENTS
C***
  200 M=2*(IHS-1)
      TB=DFH(M+1)
      TE=DFH(M+2)
      PINT=1440./QSRPD
      IRB=TB/PINT
      IF(AMOD(TB,PINT).GT.0.) IRB=IRB+1
      XTB=IRB*PINT
      IF(IRB.EQ.0) IRB=1
      IF(ISW.NE.1) PTB=XTB
      IRE=TE/PINT
C***
C*** ASSIGN PREDICTED VALUES TO PLOT ARRAY
C***
      DO 300 I=IRB,IRE
      KP=KP+1
      DATA(KS,KP)=QSDATA(I)
      IF(QSDATA(I).GT.QMX) QMX=QSDATA(I)
  300 CONTINUE
C***
C*** IF LAST CHANNEL SEGMENT - PLOT OBSERVED DISCHARGE ALSO
C***
      ICT=NOFSEG+NCRSEG
      IF(IOBS.EQ.0.OR.QSID.NE.PCRIDA(ICT).OR.QSIO.NE.2) GO TO 400
      IF(QSQS.EQ.4) GO TO 400
      IPL=1
      KX=0
      IF(ISW.EQ.1) KX=LASTKX
      QMXO=0.
      IF(ISW.EQ.1) QMXO=XOLAST
      IF(ISW2.EQ.1) GO TO 345
C***
C*** READ DAILY FILE OF OBSERVED DISCHARGE
C***
 310  READ(14)UVDEL,(UVSTA(KK),KK=1,4),UVPARM,UVSTAT,UVYR,UVMO,UVDY,
     1UVRPD,UVRDG1,UVNRDG,UVNVAL,(UVDATA(I),I=1,UVNRDG)
      IY=UVYR
      IM=UVMO
      ID=UVDY
      IF(IY.EQ.IEY.AND.IM.EQ.QSMO.AND.ID.EQ.QSDY) GO TO 330
      GO TO 310
C***
C*** IF TIME INTERVAL OF OBSERVED DISCHARGE SHORTER THAN PLOT TIME,
C*** RECOMPUTE OBSERVED DISCHARGE AT LONGER INTERVAL
C***
  330 JIN=UVRPD
      IF(JIN.LE.QSRPD) GO TO 344
      JOUT=QSRPD
      DO 2000 I=1,1440
 2000 SMQA(I)=0.0
      SMQA(1)=UVDATA(1)
      DO 2100 I=2,IN
 2100 SMQA(I)=UVDATA(I)+SMQA(I-1)
      TIN=1440./JIN                                                     LS0486
      TOUT=1440./JOUT                                                   LS0486
      TD=0.0
      FAC=TIN/TOUT
      DO 2350 I=1,JOUT
      TD=TD+TOUT
      KK=TD/TIN
      TU=TIN*KK
      F2=(TD-TU)/TIN
      IF(F2.GE..01) GO TO 2300
      SMQA(I)=SMQA(KK)                                                  LS0888
      GO TO 2350
 2300 F1=1.0-F2
      SMQA(I)=SMQA(KK)*F1+SMQA(KK+1)*F2
 2350 CONTINUE
      DO 2400 I=2,JOUT
      J2=JOUT-I+2
 2400 SMQA(J2)=(SMQA(J2)-SMQA(J2-1))*FAC
      SMQA(1)=SMQA(1)*FAC
      DO 2500 I=1,JOUT
 2500 UVDATA(I)=SMQA(I)
      UVRPD=QSRPD
  344 CONTINUE
      ISW2=1
      OINT=1440./UVRPD
C***
C*** DETERMINE TIMING FOR OBSERVED DISCHARGE
C***
  345 IRBT=TB/OINT
      IF(IRBT.EQ.0) IRBT=1
      N1=IRBT*OINT
      N2=IRB*PINT
  346 IF(N1.GE.N2) GO TO 350
      N1=N1+OINT
      GO TO 346
C     OTB=N1                                                            LS0287
  350 IRBO=N1/OINT                                                      LS0287
      IRBT=TE/OINT
      N1=IRBT*OINT
      N2=IRE*PINT
  355 IF(N1.LE.N2) GO TO 360
      N1=N1-OINT
      GO TO 355
  360 OTE=N1
      IREO=N1/OINT
      DO 380 I=IRBO,IREO
      II=I-UVRDG1+1
      KX=KX+1
      DATAOB(KX)=UVDATA(II)
      IF(DATAOB(KX).GT.QMXO) QMXO=DATAOB(KX)
  380 CONTINUE
  400 IF(IHS.EQ.NS.AND.SFL.EQ.1) GO TO 500
C***
C***   PLOT
C***
      NPTS=KP
      IF(IPL.EQ.0) GO TO 5100
      IF(QMXO.GT.QMX) QMX=QMXO
 5100 QSI=OF
      IF(QSIO.EQ.1) QSI=FL
      IF(QSQS.EQ.3) GO TO 5104
      WRITE(72,5103) TITL
      GO TO 5109
 5104 WRITE(72,5105) TITL
 5109 WRITE(72,5110)QSID,QSI,IPM,IPD,IPY,QSMO,QSDY,IEY
      WRITE(72,5120)
      WRITE(72,5125)
      IX=ALOG10(QMX)
      IF(QMX.GT..1) IX=IX+1
      SX=IX-4
      X5=10.**IX
      X4=10.**(IX-1)
      X3=10.**(IX-2)
      X2=10.**(IX-3)
      X1=10.**(IX-4)
      WRITE(72,5130) X1,X2,X3,X4,X5
      WRITE(72,5140)
      IPO=1
      DO 5500 I=1,NPTS
      PT=PT+PINT
      IF(PTB.EQ.0.) PTB=PTB+PINT
      IF(I.EQ.1) PT=PTB
      NHRS=PT/60.
      NMIN=AMOD(PT,60.)
      IF(PT.LE.1440.) GO TO 5200
      NHRS=NHRS-24
      PT=FLOAT(NHRS*60)+NMIN                                            LS0486
 5200 ITIME=100*NHRS+NMIN
      DO 5210 K=1,120
 5210 PRTLN2(K)=BLNK
      IF(MOD(I,10).NE.1) GO TO 5250
      JJ=1
      DO 5230 K=1,4
      PRTLN2(JJ)=DOT
      PRTLN2(JJ+9)=DOT
      PRTLN2(JJ+14)=DOT
      PRTLN2(JJ+18)=DOT
      PRTLN2(JJ+21)=DOT
      PRTLN2(JJ+23)=DOT
      PRTLN2(JJ+25)=DOT
      PRTLN2(JJ+27)=DOT
      PRTLN2(JJ+29)=DOT
      JJ=JJ+30
 5230 CONTINUE
 5250 IF(DATA(KS,I).GT.X1) GO TO 5260
      PRTLN2(1)=RNGE
      VALUE=DATA(KS,I)/X1
      GO TO 5270
 5260 IPOS=(30.*(ALOG10(DATA(KS,I))-SX))+1.5
      IF(IPOS.EQ.0) IPOS=1
      IF(IPOS.GT.120) IPOS = 120
      PRTLN2(IPOS)=PRED
      IE=IPOS/30+SX
      VALUE=DATA(KS,I)/10.**IE
 5270 IF(IPL.NE.1) GO TO 5400
      ICI=(I-1)*PINT+PTB
 5275 ICO=(IPO-1)*OINT+PTB
      IF(ICI.EQ.ICO) GO TO 5300
      IF(ICI.LT.ICO) GO TO 5400
      IPO=IPO+1
      GO TO 5275
 5300 IF(DATAOB(IPO).GT.X1) GO TO 5360
      PRTLN2(1)=RNGE
      GO TO 5400
 5360 JPOS=(30.*(ALOG10(DATAOB(IPO))-SX))+1.5
      IF(JPOS.EQ.0) JPOS=1
      IF(JPOS.GT.120) JPOS=120
      PRTLN2(JPOS)=OBS
      IF(JPOS.EQ.IPOS) PRTLN2(JPOS)=BOTH
      IPO=IPO+1
 5400 WRITE(72,5450) ITIME,VALUE,PRTLN2
 5500 CONTINUE
      GO TO 1000
C***
C*** IF HYDROGRAPH SEGMENT CONTINUED TO NEXT DAY, SAVE DATA
C***
  500 LASTK(KS)=KP
      LASTKX=KX
      QXLAST(KS)=QMX
      XOLAST=QMXO
      LASTYR(KS)=IPY
      LASTMO(KS)=IPM
      LASTDY(KS)=IPD
      TBLAST(KS)=PTB
      OBLAST=OTE
 1000 CONTINUE
 5000 CONTINUE
      GO TO 1
 9000 RETURN
      END
C
C
C
      SUBROUTINE   STATS
C
C     + + + PURPOSE + + +
C     Summary statistics
C
C     + + + COMMONS + + +
      INCLUDE 'copt.inc'
      INCLUDE 'csize.inc'
      INCLUDE 'cstach.inc'
      INCLUDE 'cdates.inc'
      INCLUDE 'cjm.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER BEGYR,ENDYR,                                               LS0287
     *          NDAYS(12), MONTHS(12), KJYS(6), KJY(6), KIRNY(6),
     *          KIRPY(6), KIRNYS(6), KIRPYS(6), KJM(6,12),
     *          KIRNM(6,12), KIRPM(6,12), IENDS(6), IBEGS(6),
     #          I, IDUMD, IDUMM, IBMO, IEMO, J, II, LAST, IWYR, INDB,
     #          INDE, IOBST, MYR, NOP, NOPS, IRNOP, IRPOP, IRNOPS,
     #          IRPOPS,INOP, JWYR, NY, INY, NYS, IRNY, IRPY, IRNYS,
     #          IRPYS, IMON, IRNM, IRPM, MONTH, ILAST, IDAY, JM, JY,
     #          JYS, JOPS, JOP, M, LASTY, NMYR, NMYEAR
      REAL MOO,MPO,MOOS,MPOS,NT,NTS,NTM,NTSM,MUR,MURS,NS,NSS
      REAL MOOLG,MOOSLG,NTL,NTSL
      REAL      MOM(6,12),MPM(6,12),MOY(6),MPY(6),MOYS(6),MPYS(6),
     *          SPM(6,12), SOM(6,12), SOMLG(6,12), SOYLG(6), SOYSLG(6),
     *          SFSUM(6), SFMEAN(6), SFPER(6), OBS(6,366), PRE(6,366),
     *          SOY(6), SPY(6), YOD(365), YODS(365), SOYS(6), SPYS(6),
     #          SOO, SOOLG, SPO, SOOS, SOOSLG, SPOS, RESID, S1SQ, S2SQ,
     #          S1SQL, S2SQL, S2, S2L, S2ABS, S2ABL, SS1SQ,
     #          SS1SQL, SS2SQ, SS2SQL, SS2, SS2L, SS2ABS, SS2ABL,
     #          S1SQM, S2SQM, SS1SQM, SS2SQM, SCL, SSCL, SPREL,
     #          SSPREL, SPRSQ, SSPRSQ, RESDL, RESDLG, COR, CORS,
     #          S, SS, R, RS, A, AS, XIRP, XIRN, SIGR0, SIGR, CP,
     #          XIRPS, XIRNS, SIGRS, CPS, S3SQ, S4SQ, SS3SQ, SS4SQ
      LOGICAL POSM,POSY,POSOP,POSYS,POSOPS
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD, ALOG, ABS, FLOAT, SQRT
C
C     + + + DATA INITIALIZATIONS + + +
      DATA NDAYS /31,28,31,30,31,30,31,31,30,31,30,31/
      DATA MONTHS /3HJAN,3HFEB,3HMAR,3HAPR,3HMAY,3HJUN,3HJUL,3HAUG,3HSEP
     *,3HOCT,3HNOV,3HDEC/
C
C     + + + OUTPUT FORMATS + + +
  600 FORMAT('1',//,' ',A70    ,/,'  SUMMARY STATISTICS FOR WATER YEAR',KD1291
     *' ',I4,///)                                                        KD1291
  605 FORMAT (T18,'MEAN RUNOFF',T37,'TOTAL RUNOFF',T58,'# OF     # OF') KD1291
  606 FORMAT (T20,'(CFS)',T38,'(CFS DAYS)',T56,'RESIDUALS  RUNS')       KD1291
  607 FORMAT (/,T17,'OBSV.    PRED.',6X,'OBSV.',6X,'PRED.',5X,'+ -,//') KD1291
  608 FORMAT ('0',7X,A3,4X,2(F6.3,2X),1X,2(F9.3,2X),1X,I3,2X,I3,4X,I3)  KD1291
  609 FORMAT ('0',//7X,'YEAR',4X,2(F6.3,2X),1X,2(F9.3,2X),1X,2(I3,2X),2 KD1291
     *X,I3)
  610 FORMAT ('0',2X,' MFS-MFN',2X,2(2X,F6.3),1X,2(2X,F9.3),3X,I3,2X,I3 KD1291
     *,4X,I3)
  611 FORMAT (5X,'SEASON',///,'0  RESIDUAL = OBSERVED - PREDICTED')     KD1291
  612   FORMAT (3X,' MFS-MFN SEASON IS ',A3,' TO ',A3)                  KD1291
  615 FORMAT ('1',//,' ',A70    ,/,' SUMMARY STATISTICS FOR OPTIMIZATI',KD1291
     *'ON PERIOD ',I4,' TO ',I4,///)                                    KD1291
  616 FORMAT ('0',5X,'TOTAL',2X,2(2X,F6.3),1X,2(2X,F9.3),3X,I4,1X,I4,2X KD1291
     *,I4)
  617 FORMAT ('0',2X,' MFS-MFN',2X,2(2X,F6.3),1X,2(2X,F9.3),4X,I3,2X,I3 KD1291
     *,3X,I3)
  618 FORMAT (5X,'SEASON',///,'0  RESIDUAL = OBSERVED - PREDICTED')     KD1291
  630 FORMAT (/////,' VERIFICATION CRITERIA')                           KD1291
  631 FORMAT (T44,'DAILY',T66,'MONTHLY',/,T39,2('TOTAL    MFS-MFN',6X   KD1291
     *  ), // )
  635 FORMAT (' COEFFICIENT OF DETERMINATION',T37,F7.3,3X,F7.3,5X,F7.3, KD1291
     *3X,F7.3,/,31X,'(LOGS)',F7.3,3X,F7.3)                              KD1291
  641 FORMAT ('0COEFFICIENT OF PERSISTENCE',T37,F7.3,3X,F7.3)           KD1291
  645 FORMAT ('0COEFFICIENT OF GAIN',T37,F7.3,3X,F7.3)                  KD1291
  646 FORMAT ('      FROM DAILY AVERAGES')                              KD1291
  647 FORMAT('0RESIDUAL-PREDICTED CORRELATION',T37,F7.3,3X,F7.3)        KD1291
  776 FORMAT(///,30X,'ERROR SUMMARY (MFS-MFN PERIOD)')                  KD1291
  780 FORMAT(24X,'ERRORS',17X,'ABSOLUTE ERRORS',15X,                    KD1291
     1'SQUARED ERRORS',/,17X,'NO LOG',7X,'LOG',14X,'NO LOG',7X,         KD1291
     2'LOG',14X,'NO LOG',7X,'LOG',/,'     SUM',5X,3(2F10.2,10X),/,      KD1291
     3'    MEAN',5X,3(2F10.2,10X),/,' PERCENT',5X,F10.2,20X,F10.2,      KD1291
     420X,F10.2)
  782 FORMAT(///,30X,'ERROR SUMMARY (TOTAL PERIOD)')                    KD1291
 6100 FORMAT ('0',2X,'FORECAST',2X,2(2X,F6.3),1X,2(2X,F9.3),3X,I3,2X,I3 KD1291
     *,4X,I3)
 6120 FORMAT (3X,'FORECAST SEASON IS ',A3,' TO ',A3)                    KD1291
 6170 FORMAT ('0',2X,'FORECAST',2X,2(2X,F6.3),1X,2(2X,F9.3),4X,I3,2X,I3 KD1291
     *,3X,I3)
 6310 FORMAT (T44,'DAILY',T66,'MONTHLY',/,T39,2('TOTAL   FORECAST',6X   KD1291
     *  ), // )                                                         LS0888
 7760 FORMAT(///,30X,'ERROR SUMMARY (FORECAST PERIOD)')                 KD1291
C
C     + + + END SPECIFICATIONS + + +
C
C     ASSUMES MELT SEASON BEGINS ON FIRST DAY                           LS061484
C       OF THE MONTH                                                    LS061484
C                                                                       LS061484
      DO 7 I=1,2                                                        LS061484
      IDUMD=0                                                           LS061484
      IDUMM=BMO                                                         LS061484
      IF(I.EQ.2)IDUMM=EMO                                               LS061484
      IDUMM=IDUMM-9                                                     LS061484
      IF(IDUMM.LT.1)IDUMM=IDUMM+12                                      LS061484
      IF(I.EQ.1)IBMO=IDUMM                                              LS061484
      IF(I.EQ.2)IEMO=IDUMM                                              LA061484
      DO 3 J=1,12                                                       LS061484
      IF(J.GE.IDUMM) GO TO 3                                            LS061484
      II=J+9                                                            LS061484
      IF(II.GT.12) II=II-12                                             LS061484
      LAST=NDAYS(II)                                                    LS061484
      IF(MOD(IWYR,4).EQ.0.AND.II.EQ.2) LAST=29                          LS061484
      IDUMD=IDUMD+LAST                                                  LS061484
    3 CONTINUE                                                          LS061484
      IF(I.EQ.1) INDB=IDUMD+BDY                                         LS061484
      IF(I.EQ.2) INDE=IDUMD+EDY                                         LS061484
    7 CONTINUE                                                          LS061484
      DO 5 I=1,6
      SPY(I)=0.
      SOY(I)=0.
      SOYLG(I)=0.
      SPYS(I)=0.
      SOYS(I)=0.
      SOYSLG(I)=0.
      DO 6 J=1,12
      SOM(I,J)=0.
      SOMLG(I,J)=0.
    6 SPM(I,J)=0.
    5 CONTINUE
      DO 8 I=1,365
      YOD(I)=0.
    8 YODS(I)=0.
      IOBST=0
      BEGYR=BWY
      ENDYR=EWY
      REWIND 30
C     FIND BEGINNING YEAR ON FILE
 10   READ (30,END=15) MYR                                              AL0884
      IF (MYR .EQ. BEGYR) GO TO 15
      GO TO 10
 15   BACKSPACE 30
C     INITIALIZE SUMS FOR OPTIM. PERIOD
      NOP=0
      NOPS=0
      IRNOP=0
      IRPOP=0
      IRNOPS=0
      IRPOPS=0
      INOP=0                                                            LS0287
      SOO=0.0
      SOOLG=0.
      SPO=0.0
      SOOS=0.0
      SOOSLG=0.
      SPOS=0.0
      DO 300 IWYR=BEGYR,ENDYR
      JWYR=IWYR-BEGYR+1
      NY=0
      INY=0                                                             LS061484
      NYS=0
      IRNY=0
      IRPY=0
      IRNYS=0
      IRPYS=0
C     INPUT PRED. & OBSV. FROM TAPE 30
      READ (30,END=300) MYR,(OBS(JWYR,J),J=1,366)                       AL0884
      READ (30,END=300) MYR,(PRE(JWYR,J),J=1,366)                       AL0884
      DO 200 IMON=1,12
      IRNM=0
      IRPM=0
      MONTH=IMON+9
      IF (MONTH.GT.12) MONTH=MONTH-12
      LAST=NDAYS(MONTH)
      IF (MOD(IWYR,4).EQ.0.AND.MONTH.EQ.2) LAST=29
      IF(IOBST.EQ.0.AND.MONTH.EQ.MFS)IOBST=1                            LS061484
      IF(IWYR.EQ.BEGYR.AND.IMON.LT.IBMO) GO TO 21                       LS061484
      GO TO 23                                                          LS061484
   21 NY=NY+LAST                                                        LS061484
      NOP=NOP+LAST                                                      LS061484
      GO TO 200                                                         LS061484
   23 IF(IWYR.EQ.ENDYR.AND.IMON.GT.IEMO) GO TO 200                      LS061484
      ILAST=0                                                           LS061484
      DO 100 IDAY=1,LAST
      NY=NY+1
      NOP=NOP+1
      IF(IWYR.EQ.BEGYR.AND.NY.LT.INDB) GO TO 100                        LS061484
      IF(IWYR.EQ.ENDYR.AND.NY.GT.INDE) GO TO 100                        LS061484
      INY=INY+1                                                         LS061484
      INOP=INOP+1                                                       LS061484
      ILAST=ILAST+1                                                     LS061484
      RESID=OBS(JWYR,NY)-PRE(JWYR,NY)
C     ACCUMULATE + AND - RESIDUALS, AND # OF DAYS IN MELT SEASON
      IF (RESID.GT.0.0) GO TO 20
      IRNM=IRNM+1
      IRNY=IRNY+1
      IRNOP=IRNOP+1
      IF (IOBST.EQ.0) GO TO 25
      IRNYS=IRNYS+1
      IRNOPS=IRNOPS+1
      NYS=NYS+1
      NOPS=NOPS+1
      GO TO 22
 20   IRPM=IRPM+1
      IRPY=IRPY+1
      IRPOP=IRPOP+1
      IF(IOBST.EQ.0)GO TO 25
      IRPYS=IRPYS+1
      IRPOPS=IRPOPS+1
      NYS=NYS+1
      NOPS=NOPS+1
 22   IENDS(JWYR)=NY
C     ACCUMULATE # OF RUNS
C     BY MONTH
 25   IF (IDAY.GT.1) GO TO 30
      JM=1
      POSM=(RESID.GT.0.0)
      GO TO 40
 30   IF (RESID.LT.0.0) GO TO 35
      IF (POSM) GO TO 40
      POSM=.TRUE.
      JM=JM+1
      GO TO 40
 35   IF (.NOT.POSM) GO TO 40
      POSM=.FALSE.
      JM=JM+1
C     BY YEAR
 40   IF (NY.GT.1) GO TO 42
      JY=1
      POSY=(RESID.GT.0.0)
      GO TO 50
 42   IF (RESID.LT.0.0) GO TO 44
      IF (POSY) GO TO 50
      POSY=.TRUE.
      JY=JY+1
      GO TO 50
 44   IF (.NOT.POSY) GO TO 50
      POSY=.FALSE.
      JY=JY+1
 50   IF(IOBST.EQ.0)GO TO 70
C     MFS TO MFN SEASON FOR YEAR
      IF (NYS.GT.1) GO TO 52
      IBEGS(JWYR)=NY
      JYS=1
      POSYS=(RESID.GT.0.0)
      GO TO 60
 52   IF (RESID.LT.0.0) GO TO 54
      IF (POSYS) GO TO 60
      POSYS=.TRUE.
      JYS=JYS+1
      GO TO 60
 54   IF (.NOT.POSYS) GO TO 60
      POSYS=.FALSE.
      JYS=JYS+1
C     MFS TO MFN SEASON FOR OPT. PERIOD
 60   IF (NOPS.GT.1) GO TO 62
      JOPS=1
      POSOPS=(RESID.GT.0.0)
      GO TO 70
 62   IF (RESID.LT.0.0) GO TO 64
      IF (POSOPS) GO TO 70
      POSOPS=.TRUE.
      JOPS=JOPS+1
      GO TO 70
 64   IF (.NOT.POSOPS) GO TO 70
      POSOPS=.FALSE.
      JOPS=JOPS+1
C     BY OPTIMIZATION PERIOD
 70   IF (NOP.GT.1) GO TO 72
      JOP=1
      POSOP=(RESID.GT.0.0)
      GO TO 80
 72   IF (RESID.LT.0.0) GO TO 74
      IF (POSOP) GO TO 80
      POSOP=.TRUE.
      JOP=JOP+1
      GO TO 80
 74   IF (.NOT.POSOP) GO TO 80
      POSOP=.FALSE.
      JOP=JOP+1
C     SUMS FOR MONTH (CFS DAYS)
 80   SOM(JWYR,MONTH)=SOM(JWYR,MONTH)+OBS(JWYR,NY)
      SOMLG(JWYR,MONTH)=SOMLG(JWYR,MONTH)+ALOG(OBS(JWYR,NY)+1.)
      SPM(JWYR,MONTH)=SPM(JWYR,MONTH)+PRE(JWYR,NY)
100   CONTINUE
C     END OF MONTH . . . COMPUTE MONTHLY MEANS, STORE RUNS, ETC.
      MOM(JWYR,MONTH)=SOM(JWYR,MONTH)/ILAST                             LS061484
      MPM(JWYR,MONTH)=SPM(JWYR,MONTH)/ILAST                             LS061484
      KJM(JWYR,MONTH)=JM
      KIRNM(JWYR,MONTH)=IRNM
      KIRPM(JWYR,MONTH)=IRPM
C     SUMS FOR YEAR
      SOY(JWYR)=SOY(JWYR)+SOM(JWYR,MONTH)
      SOYLG(JWYR)=SOYLG(JWYR)+SOMLG(JWYR,MONTH)
      SPY(JWYR)=SPY(JWYR)+SPM(JWYR,MONTH)
      IF(IOBST.EQ.0)GO TO 199
      SOYS(JWYR)=SOYS(JWYR)+SOM(JWYR,MONTH)
      SOYSLG(JWYR)=SOYSLG(JWYR)+SOMLG(JWYR,MONTH)
      SPYS(JWYR)=SPYS(JWYR)+SPM(JWYR,MONTH)
199   IF(IOBST.EQ.1.AND.MONTH.EQ.MFN)IOBST=0
200   CONTINUE
C     END OF YEAR . . . COMPUTE MEANS, STORE RUNS, ETC.
      MOY(JWYR)=SOY(JWYR)/INY                                           LS061484
      MPY(JWYR)=SPY(JWYR)/INY                                           LS061484
      IF(NYS.EQ.0) GO TO 210                                            LS061484
      MOYS(JWYR)=SOYS(JWYR)/NYS
      MPYS(JWYR)=SPYS(JWYR)/NYS
  210 KJY(JWYR)=JY                                                      LS061484
      KIRNY(JWYR)=IRNY
      KIRPY(JWYR)=IRPY
      KIRNYS(JWYR)=IRNYS
      KIRPYS(JWYR)=IRPYS
      KJYS(JWYR)=JYS
      SOO=SOO+SOY(JWYR)
      SOOLG=SOOLG+SOYLG(JWYR)
      SPO=SPO+SPY(JWYR)
      SOOS=SOOS+SOYS(JWYR)
      SOOSLG=SOOSLG+SOYSLG(JWYR)
      SPOS=SPOS+SPYS(JWYR)
300   CONTINUE
C     END OF OPTIMIZATION PERIOD
      MOO=SOO/INOP                                                      LS061484
      MOOLG=SOOLG/INOP                                                  LS061484
      MPO=SPO/INOP                                                      LS061484
      MOOS=SOOS/NOPS
      MOOSLG=SOOSLG/NOPS
      MPOS=SPOS/NOPS
C     PRINT SUMMARY STATISTICS
      DO 350 IWYR=BEGYR,ENDYR
      JWYR=IWYR-BEGYR+1
      WRITE (72,600) TITL,IWYR
      WRITE (72,605)
      WRITE (72,606)
      WRITE (72,607)
      DO 320 IMON=1,12
      MONTH=IMON+9
      IF (MONTH.GT.12) MONTH=MONTH-12
      J=JWYR
      M=MONTH
      WRITE(72,608) MONTHS(M),MOM(J,M),MPM(J,M),SOM(J,M),SPM(J,M),KIRPM(
     *J,M),KIRNM(J,M),KJM(J,M)
320   CONTINUE
      WRITE(72,609) MOY(J),MPY(J),SOY(J),SPY(J),KIRPY(J),KIRNY(J),KJY(J)
      IF(PROB.EQ.1) THEN                                                LS0888
      WRITE(72,6100)MOYS(J),MPYS(J),SOYS(J),SPYS(J),KIRPYS(J),KIRNYS(J),LS0888
     *KJYS(J)
      ELSE                                                              LS0888
      WRITE(72,610) MOYS(J),MPYS(J),SOYS(J),SPYS(J),KIRPYS(J),KIRNYS(J),
     *KJYS(J)
      END IF                                                            LS0888
      WRITE (72,611)
      IF(PROB.EQ.1)THEN                                                 LS0888
        WRITE (72,6120)MONTHS(MFS),MONTHS(MFN)                           LS0888
      ELSE                                                              LS0888
        WRITE (72,612) MONTHS(MFS),MONTHS(MFN)
      END IF                                                              LS0888
350   CONTINUE
      IF(PROB.EQ.1.OR.IPROB.EQ.1) RETURN                                LS0888
C     END OF YEARLY PRINTOUT
      WRITE (72,615) TITL,BEGYR,ENDYR
      WRITE (72,605)
      WRITE (72,606)
      WRITE (72,607)
      WRITE (72,616) MOO,MPO,SOO,SPO,IRPOP,IRNOP,JOP
      IF(PROB.EQ.1) THEN                                                LS0888
      WRITE (72,6170)MOOS,MPOS,SOOS,SPOS,IRPOPS,IRNOPS,JOPS              LS0888
      ELSE                                                              LS0888
      WRITE (72,617) MOOS,MPOS,SOOS,SPOS,IRPOPS,IRNOPS,JOPS
      END IF                                                            LS0888
      WRITE (72,618)
      WRITE (72,612) MONTHS(MFS),MONTHS(MFN)
C
      S1SQ=0.0
      S1SQL=0.0
      S2SQ=0.0
      S2SQL=0.0
      S2=0.0
      S2L=0.0
      S2ABS=0.0
      S2ABL=0.0
      SS1SQ=0.0
      SS1SQL=0.0
      SS2SQ=0.0
      SS2SQL=0.0
      SS2=0.0
      SS2L=0.0
      SS2ABS=0.0
      SS2ABL=0.0
      S1SQM=0.0
      S2SQM=0.0
      SS1SQM=0.0
      SS2SQM=0.0
      SCL=0.
      SSCL=0.0
      SPREL=0.0
      SSPREL=0.0
      SPRSQ=0.0
      SSPRSQ=0.0
      DO 450 IWYR=BEGYR,ENDYR
      J=IWYR-BEGYR+1
      LASTY=366
      IF (MOD(IWYR,4).EQ.0) GO TO 370
      LASTY=365
370   DO 450 I=1,LASTY
      IF(IWYR.EQ.BEGYR.AND.I.LT.INDB)GO TO 450                          LS061484
      IF(IWYR.EQ.ENDYR.AND.I.GT.INDE)GO TO 450                          LS061484
      RESDL=OBS(J,I)-PRE(J,I)
      RESDLG=ALOG(OBS(J,I)+1.)-ALOG(PRE(J,I)+1.)
      S1SQ=S1SQ+(OBS(J,I)-MOO)**2
      S1SQL=S1SQL+(ALOG(OBS(J,I)+1.)-MOOLG)**2
      S2=S2+RESDL
      S2L=S2L+RESDLG
      S2ABS=S2ABS+ABS(RESDL)
      S2ABL=S2ABL+ABS(RESDLG)
      S2SQ=S2SQ+RESDL*RESDL
      S2SQL=S2SQL+(RESDLG)**2
      SPREL=SPREL+ALOG(PRE(J,I)+1.)
      SPRSQ=SPRSQ+ALOG(PRE(J,I)+1.)*ALOG(PRE(J,I)+1.)
      SCL=SCL+RESDLG*ALOG(PRE(J,I)+1.)
      IF (I.LT.IBEGS(J).OR.I.GT.IENDS(J)) GO TO 450
      SS1SQ=SS1SQ+(OBS(J,I)-MOOS)**2
      SS1SQL=SS1SQL+(ALOG(OBS(J,I)+1.)-MOOSLG)**2
      SS2=SS2+RESDL
      SS2L=SS2L+RESDLG
      SS2ABS=SS2ABS+ABS(RESDL)
      SS2ABL=SS2ABL+ABS(RESDLG)
      SS2SQ=SS2SQ+(RESDL)**2
      SS2SQL=SS2SQL+(RESDLG)**2
      SSPREL=SSPREL+ALOG(PRE(J,I)+1.)
      SSPRSQ=SSPRSQ+ALOG(PRE(J,I)+1.)*ALOG(PRE(J,I)+1.)
      SSCL=SSCL+RESDLG*ALOG(PRE(J,I)+1.)
450   CONTINUE
C     COMPUTE AND PRINT VERIFICATION CRITERIA
C     CORRELATION BETWEEN RESIDUALS AND PREDICTED VALUES
      COR=(SCL-S2L*SPREL/FLOAT(INOP))/SQRT((S2SQL-S2L*S2L/FLOAT(INOP))* LS061484
     1(SPRSQ-SPREL*SPREL/FLOAT(INOP)))                                  LS061484
      CORS=(SSCL-SS2L*SSPREL/FLOAT(NOPS))/SQRT((SS2SQL-SS2L*SS2L/
     1FLOAT(NOPS))*(SSPRSQ-SSPREL*SSPREL/FLOAT(NOPS)))
C     COEFFICIENT OF DETERMINATION
      NT=(S1SQ-S2SQ)/S1SQ
      NTL=(S1SQL-S2SQL)/S1SQL
      NTS=(SS1SQ-SS2SQ)/SS1SQ
      NTSL=(SS1SQL-SS2SQL)/SS1SQL
      IOBST=0
      DO 470 IWYR=BEGYR,ENDYR
      J=IWYR-BEGYR+1
      DO 470 IMON=1,12
      M=IMON+9
      IF (M.GT.12) M=M-12
      IF(IOBST.EQ.0.AND.M.EQ.MFS)IOBST=1
      S1SQM=S1SQM+(MOM(J,M)-MOO)**2
      S2SQM=S2SQM+(MPM(J,M)-MOM(J,M))**2
      IF(IOBST.EQ.0)GO TO 469
      SS1SQM=SS1SQM+(MOM(J,M)-MOOS)**2
      SS2SQM=SS2SQM+(MPM(J,M)-MOM(J,M))**2
469   IF(IOBST.EQ.1.AND.M.EQ.MFN)IOBST=0
470   CONTINUE
      NTM=(S1SQM-S2SQM)/S1SQM
      NTSM=(SS1SQM-SS2SQM)/SS1SQM
      WRITE (72,630)
      IF(PROB.EQ.1) THEN                                                LS0888
      WRITE (72,6310)                                                    LS0888
      ELSE                                                              LS0888
      WRITE (72,631)
      END IF                                                            LS0888
      WRITE (72,635) NT,NTS,NTM,NTSM,NTL,NTSL
C     RATIO OF STANDARD ERROR TO MEAN
      S=(S2SQ/INOP)**.5/MOO                                             LS061484
      SS=(SS2SQ/NOPS)**.5/MOOS
C     RATIO OF RELATIVE ERROR TO MEAN
      R=S2/INOP/MOO                                                     LS061484
      RS=SS2/NOPS/MOOS
C     RATIO OF ABSOLUTE ERROR TO MEAN
      A=S2ABS/INOP/MOO                                                  LS061484
      AS=SS2ABS/NOPS/MOOS
C     COEFFICIENT OF PERSISTENCE
      XIRP=FLOAT(IRPOP)
      XIRN=FLOAT(IRNOP)
      MUR=1.+2.*XIRP*XIRN/(XIRP+XIRN)
      SIGR0=2.*XIRP*XIRN*(2.*XIRP*XIRN-XIRP-XIRN)
      SIGR=(SIGR0/(XIRP+XIRN)**2./(XIRP+XIRN-1.))**.5
      CP=FLOAT(JOP-MUR)/SIGR                                            LS0486
      XIRPS=FLOAT(IRPOPS)
      XIRNS=FLOAT(IRNOPS)
      MURS=1.+2.*XIRPS*XIRNS/(XIRPS+XIRNS)
      SIGRS=2.*XIRPS*XIRNS*(2.*XIRPS*XIRNS-XIRPS-XIRNS)
      SIGRS=(SIGRS/(XIRPS+XIRNS)**2./(XIRPS+XIRNS-1.))**.5
      CPS=FLOAT(JOPS-MURS)/SIGRS                                        LS0486
      IF(CP.GT.99.999)CP=99.999
      IF(CP.LT.-99.999)CP=-99.999
      IF(CPS.GT.99.999)CPS=99.999
      IF(CPS.LT.-99.999)CPS=-99.999
      WRITE (72,641) CP,CPS
C     COEFFICIENT OF GAIN FROM DAILY AVERAGES
      NMYEAR =ENDYR-BEGYR+1
      S3SQ=0.0
      S4SQ=0.0
      SS3SQ=0.0
      SS4SQ=0.0
      DO 500 IDAY=1,365
      NMYR=NMYEAR                                                       LS061484
      IF(IDAY.LT.INDB) NMYR=NMYR-1                                      LS061484
      IF(IDAY.GT.INDE) NMYR=NMYR-1                                      LS0287
      I=IDAY
      DO 500 IWYR=BEGYR,ENDYR
      J=IWYR-BEGYR+1
      IF (MOD(IWYR,4).EQ.0.AND.IDAY.GE.60) I=IDAY+1
      YOD(IDAY)=YOD(IDAY)+OBS(J,I)/NMYR                                 LS061484
      IF (IDAY.LT.IBEGS(J).OR.IDAY.GT.IENDS(J)) GO TO 500
      YODS(IDAY)=YODS(IDAY)+OBS(J,I)/NMYR                               LS061484
500   CONTINUE
      DO 540 IWYR=BEGYR,ENDYR
      J=IWYR-BEGYR+1
      DO 540 IDAY=1,365
      I=IDAY
      IF (MOD(IWYR,4).EQ.0.AND.IDAY.GE.60) I=IDAY+1
      IF(IWYR.EQ.BEGYR.AND.I.LT.INDB) GO TO 540                         LS061484
      IF(IWYR.EQ.ENDYR.AND.I.GT.INDE) GO TO 540                         LS061484
      S3SQ=S3SQ+(OBS(J,I)-YOD(IDAY))**2
      S4SQ=S4SQ+(OBS(J,I)-PRE(J,I))**2
      IF (IDAY.LT.IBEGS(J).OR.IDAY.GT.IENDS(J)) GO TO 540
      SS3SQ=SS3SQ+(OBS(J,I)-YODS(IDAY))**2
      SS4SQ=SS4SQ+(OBS(J,I)-PRE(J,I))**2
540   CONTINUE
      IF(NMYEAR.EQ.1)GO TO 541
      NS=(S3SQ-S4SQ)/S3SQ
      NSS=(SS3SQ-SS4SQ)/SS3SQ
      WRITE (72,645) NS,NSS
      WRITE (72,646)
  541 WRITE(72,647)COR,CORS
C
      IF(PROB.EQ.1) THEN                                                LS0888
      WRITE(72,7760)                                                     LS0888
      ELSE                                                              LS0888
      WRITE(72,776)
      END IF                                                            LS0888
      SFSUM(1)=SS2
      SFSUM(2)=SS2L
      SFSUM(3)=SS2ABS
      SFSUM(4)=SS2ABL
      SFSUM(5)=SS2SQ
      SFSUM(6)=SS2SQL
      DO 777 I=1,6
  777 SFMEAN(I)=SFSUM(I)/NOPS
      SFPER(1)=RS*100.
      SFPER(2)=AS*100.
      SFPER(3)=SS*100.
      WRITE(72,780)(SFSUM(I),I=1,6),(SFMEAN(I),I=1,6),(SFPER(I),I=1,3)
      WRITE(72,782)
      SFSUM(1)=S2
      SFSUM(2)=S2L
      SFSUM(3)=S2ABS
      SFSUM(4)=S2ABL
      SFSUM(5)=S2SQ
      SFSUM(6)=S2SQL
      DO 786 I=1,6
  786 SFMEAN(I)=SFSUM(I)/INOP                                           LS061484
      SFPER(1)=R*100.
      SFPER(2)=A*100.
      SFPER(3)=S*100.
      WRITE(72,780)(SFSUM(I),I=1,6),(SFMEAN(I),I=1,6),(SFPER(I),I=1,3)
      RETURN
      END
