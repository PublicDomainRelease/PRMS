C
C
C
      SUBROUTINE   PROBGN
C
C     + + + PURPOSE + + +
C     ?????
C
C     + + + COMMONS + + +
      INCLUDE 'cplot.inc'
      INCLUDE 'cdates.inc'
      INCLUDE 'cprobn.inc'
      INCLUDE 'call.inc'
      INCLUDE 'cbs.inc'
      INCLUDE 'cdpr.inc'
      INCLUDE 'cdvdt.inc'
      INCLUDE 'cet.inc'
      INCLUDE 'copt.inc'
      INCLUDE 'csize.inc'
      INCLUDE 'cjm.inc'
      INCLUDE 'csm.inc'
      INCLUDE 'csno.inc'
      INCLUDE 'copsno.inc'
      INCLUDE 'csent.inc'
      INCLUDE 'csent1.inc'
      INCLUDE 'csed.inc'
      INCLUDE 'cvol.inc'
      INCLUDE 'cswtch.inc'
      INCLUDE 'copsn2.inc'
      INCLUDE 'csent2.inc'
      INCLUDE 'csent3.inc'
      INCLUDE 'csncv.inc'
      INCLUDE 'cscs.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   ITER, IISTAT, I, IM, MOPP, J, MDIFF, JJ, NSWIT
      REAL      TSMIASW, VSUM, UMIN, PHIOPT(10)
      REAL      GU(20),HU(20),OBFU(10),SDX(20),U(2),STMX(20),COR(20)
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL   SETYR
C
C     + + + DATA INITITALIZATIONS + + +
      DATA NSWIT/1/,ITER/1/
C
C     + + + OUTPUT FORMATS + + +
   46 FORMAT('1')                                                       KD1291
  989 FORMAT('1')                                                       KD1291
C
C     + + + END SPECIFICATIONS + + +
C
      IF (DEBUG.EQ.0) GO TO 5000                                        JKM10/85
      PRINT *,'NSWIT = ',NSWIT
      PRINT *,' YEAR JUST RUN ',BYR,BMO,BDY,EYR,EMO,EDY
      PRINT *,' WATER YEAR ',BWY,EWY
 5000 GO TO (27,26,29,169,35),NSWIT
   27 IF(INIT.EQ.1)GO TO 35
      IF(INIT.EQ.1)GO TO 35
      INIT=1
      NPRNT=0
      IWSW=0
      NYR=0
      IPTEMP=IPLOT
      IISTAT=ISTAT
      ISTAT=0
      IF(BYRFC.EQ.0) GO TO 26
      BYR=BYRFC
      BMO=BMOFC
      BDY=BDYFC
      EYR=EYRFC
      EMO=EMOFC
      EDY=EDYFC
      MYR=BYR
      MO=BMO
      MDY=BDY
      BWY=BYR
      IF(BMO.GE.10)BWY=BWY+1
      EWY=EYR
      IF(EMO.GE.10)EWY=EWY+1
      NYR=EWY-BWY+1
      NSWIT=2
      IOBSW=0
      DO 11 I=1,12
        IM=MOD(MFS+I-2,12)+1
        IF(IM.EQ.BMO)IOBSW=1
        IF(IM.EQ.MFN)RETURN
   11 CONTINUE
      RETURN
C               WRITE INITIAL CONDITIONS FOR START OF EACH
C                         CONDITIONAL SIMULATION
   26 WRITE(60) XIN,INSW,SMAV,RECHR,GW,RES,PWEQV,PICE,PACT,PKDEF,
     1FREWT,TCAL,LST,SNSV,SALB,SLST,JLDY,ITSW,JSOL,MXDY,
     2INTSW,JWDY,MOPP,DPT,PSS,ISO,LSO,MSO,MPC1,ISNO,IT,TSM
     3IASW,PST,ASC,ASCSV,PKSV,SCRV
      IF(IPSW.EQ.1) WRITE(60)IPSR
      DO 9 J=1,10
        OFSPR(J)=0.
    9 OBF(J)=0.
      NOBF=0
      NMOBF=0
      ISTAT=IISTAT
      NPRNT=1
      IWSW=1
      BYR=BYRPR
      BWY=BYR
      BMO=BMOPR
      BDY=BDYPR
      IF(BMO.GE.10)BWY=BWY+1
      EYR=BYRPR
      IF(BMO.GE.10) EYR=EYR+1                                           JKM10/85
      EMO=EMOPR
      EDY=EDYPR
      MYR=BYR
      MO=BMO
      EWY=EYR
      IF(EMO.GE.10)EWY=EWY+1
      NYRI=NYR
      IF(BYRFC.EQ.0.OR.NYRI.GT.1)GO TO 36
      MDIFF=ME-MB+1
      IF(MDIFF.LT.12)NYRI=0
   36 NYR=EWY-BWY+1
      IF((BMO.NE.10.OR.BDY.NE.1).AND.BYRFC.NE.0)IOSW=2
      IF (DEBUG.EQ.0) GO TO 3000                                        JKM10/85
      PRINT *,' YEAR OF NSWIT=2 ',BYR,BMO,BDY,EYR,EMO,EDY
      PRINT *,' WATER YEARS ',BWY,EWY
 3000 ISSRO=1
      ISX1=1
      ISTMP=1
      ISSOL=1
      ISNOYR=0
      IF(IOSW.EQ.2)ISNOYR=BWY
      NSWIT=3
C     IF(IOPT.NE.2)GO TO 37
C     IWSW2=2
C     DEFINE FILE 9(1000,128,L,NREC)                                    XXXXXXXX
C     ABOVE LINE RELOCATED TO MAIN                                      XXXXXXXX
C     NREC=2
   37 IWSW=1
      IOBSW=0
      DO 12 I=1,12
        IM=MOD(MFS+I-2,12)+1
        IF(IM.EQ.BMO)IOBSW=1
        IF(IM.EQ.MFN)GO TO 13
   12 CONTINUE
   13 IOBSWK=IOBSW
      RETURN
   29 ENDFILE 60
   35 NDOP=0
      VSUM=0.
      DO 42 JJ=1,IR
   42 PHIOPT(JJ)=0.
      UMIN=1.E12
      WRITE(72,46)
      NSWIT=4
  169 CALL SETYR(NDOP)
  220 NOBF=0
      NMOBF=0
      REWIND 60
      IF(NDOP.EQ.1)GO TO 250
  240 READ(60) XIN,INSW,SMAV,RECHR,GW,RES,PWEQV,PICE,PACT,PKDEF,
     1FREWT,TCAL,LST,SNSV,SALB,SLST,JLDY,ITSW,JSOL,MXDY,
     2INTSW,JWDY,MOPP,DPT,PSS,ISO,LSO,MSO,MPC1,ISNO,IT,TSM
     3IASW,PST,ASC,ASCSV,PKSV,SCRV
      IF(IPSW.EQ.1)READ(60)IPSR
      IF(BYRFC.EQ.0) IPPT=1
      IF(BYRFC.EQ.0) ISNOSW=1
      ISSRO=1
      ISX1=1
      ISTMP=1
      ISSOL=1
      ISBAS=1
      ISNOYR=0
      IF((BMO.NE.10.OR.BDY.NE.1).AND.BYRFC.NE.0)ISNOYR=BWY
      IOBSW=IOBSWK
      DO 202 J=1,10
        OFSPR(J)=0.
  202 OBF(J)=0.
C     IF(IABS(IOPT).LE.2.AND.ISEN.LE.2) RETURN                          LS110483
C     IF(IABS(IOPT).LE.2)RETURN                                         LS061984
      IF(NST.EQ.0) RETURN                                               LS061984
      DO 245 J=1,NST
        PQ(J)=0.
        PSED(J)=0.
  245 BPK(J)=0.
      RETURN
  250 WRITE(72,989)
      IPLOT=IPTEMP
      NPRNT=1
      IWSW=1
      NSWIT=5
      IPROB=PROB
      PROB=0
      REWIND 30
      GO TO 240
      END
