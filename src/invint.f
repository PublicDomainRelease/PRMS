C
C
C
      SUBROUTINE   INVIN
     O                  ( IERR )
C
C     + + + PURPOSE + + +
C     Reads storm input records.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IERR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IERR   - ?????
C
C     + + + COMMONS + + +
      INCLUDE 'cdtmn1.inc'
      INCLUDE 'call.inc'
      INCLUDE 'cdates.inc'
      INCLUDE 'copt.inc'
      INCLUDE 'csize.inc'
      INCLUDE 'cuvrt.inc'
      INCLUDE 'cvol.inc'
      INCLUDE 'cpcrch.inc'
      INCLUDE 'cstor.inc'
      INCLUDE 'csed.inc'
      INCLUDE 'cunss.inc'
      INCLUDE 'cswtch.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   OWY,FWY,FDY,RODYS,EM,BM,EY,BY,ED,CTSW,TYPE,UVPARM,
     1          WY,WYD,RFL,SFL,CD,QC,
     2          RITE,UPSW,PRTIN,PRTOUT,ODELS,RB
      INTEGER   UVNRDG,UVSTAT,UVYR,UVMO,UVDY,UVRPD,UVRDG1
      INTEGER   FHA(80), MTH(12)
      INTEGER   J, NSP, LSTORM, NERR, ICHK, I, ISS, IHS, NY, NM, ND,
     #          JY, JM, JD, KD, KT, LD, LT, IC2, NSTORM, JSTORM,
     #          IJK, NEF, NE, L, NSS, NHS, II, JJ,KK, JWYD, LWY, LDY,
     #          IC1, JK, N, IY, KRTN, K, IBS, IS, IKJ, NEL, NS, IES, M,
     #          IB, IC, IR, NS2, NS1, INS, KCK, KDAT, KH, KM, LH, LM,
     #          IWY4, IDT, IOF, NDX, IRU, NXS, NDELS, IRIT, III, JJJ,
     #          ISQ, NCR, JC, NUMDA
      REAL      KR,KF,MM,IDA(50),IMPERV,IMPV                            LS0287
      REAL      OFAR(50,50), UVSTA(4), FHAF(10), DFH(20),
     #          UVDATA(1440), UINFX(4800), OFLT(50)
      REAL      UVNVAL, RPD, DT, DTD, CRO, RT, TB, TE, QMAX, STRO,
     #          D, RKM, RLM, DUR, FLGTH, SLOPE, FRN, PARM1, PARM2,
     #          THRES, DTM, DTOS, FRNI, OIP1, OIP2, DX, ALPR, CMP,
     #          ALPR1, CMP1, ALPI, CMI, ALPI1, CMI1, DTS, DTDX,
     #          DTDXM, DXDT, DA, O1, PERDIF, RC1, RC2
      CHARACTER*1 BLNK                                                  LS0287
      CHARACTER*4 PCRID, UP1, UP2, UP3, RBC, LBC, BLNK4,
     #            CUPA(50,3), OFKEYS(100)
      LOGICAL   UVDEL
C
C     + + + EQUIVALENCES + + +
       EQUIVALENCE (BSED(1),OFLT(1))                                    LS0586
       EQUIVALENCE (UINF(1,1),UINFX(1))                                 LS0586
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD, ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL    WYDYS, UVRET, AM
C
C     + + + DATA INITIALIZATIONS + + +
      DATA MTH/92,123,151,182,212,243,273,304,335,0,31,61/
      DATA BLNK4,BLNK/4H    ,1H /
      DATA DFH/20*0./
C
C     + + + INPUT FORMATS + + +
   13 FORMAT(10X,4F5.0)
   26 FORMAT(10X,4F5.0,5F10.0)
 8000 FORMAT(10X,I5)                                                    LS103183
 8010 FORMAT(10X,I5,I1,I4,2(I5,1X,2I2),4I5,4X,I1,5X,2F5.0)              LS103183
 8014 FORMAT(40X,4I5,4X,I1,5X,2F5.0)                                    LS103183
 8040 FORMAT(10X,I5)
 8065 FORMAT(48X,3F5.0)                                                  0884KF
 8070 FORMAT(10X,A4,13X,F5.2,I2,2I1,I2,5F5.0,I2,3F5.0)                  LS103183
 8080 FORMAT(10X,I5)
 8090 FORMAT(6X,6A4,2X,I2,2I1,I2,5F5.0,2X,3F5.0)                        LS103183
C
C     + + + OUTPUT FORMATS + + +
   14 FORMAT(//,  ' STORMFLOW HYDROGRAPH PARAMETERS FOR EACH HYDROLOGI',KD1291
     1'C RESPONSE UNIT(IRU)')                                           KD1291
   15 FORMAT(/,  ' IRU   KSAT    PSP    RGF    DRN           KR    HC', KD1291
     1'  KF      MM      EN')                                           KD1291
   18 FORMAT(1X,I3,4F7.3,5X,F9.2,F8.1,F8.5,2F8.2)
   21 FORMAT(1X,I3,4F7.3)
   22 FORMAT(' IRU   KSAT   PSP    RGF    DRN')                         KD1291
  521 FORMAT(1X,6I5,20F5.0)
 1066 FORMAT(5X,'*****ERROR - DRAINAGE AREA FOR ROUTING IS DIFFERENT F',KD1291
     +'ROM TOTAL BY',F5.2,' PERCENT')                                   KD1291
 1200 FORMAT(//,  '    WY  WYD  #HS  ST#  RFL  SFL  BEGIN AND END TIME',KD1291
     1'S FOR #HS')
 1212 FORMAT(/,5X,'***** ERROR IN EROSION PARAMETERS FOR UNIT',I5,      LS070783
     +/,10X,'VARIABLE    KRA     HCA      KFA      MMA      ENA',       LS070783
     +/,10X,'EXPECTED   0-100   0-100    0-100     0-100   0-100',      LS070783
     +/,10X,'FOUND   ',5F9.5)                                           LS070783
 1213 FORMAT(/,5X,'***** ERROR IN INFILTRATION PARAMETERS FOR UNIT',I5, LS070783
     +/,10X,'VARIABLE   KSAT     PSP      RGF      DRN',                LS070783
     +/,10X,'EXPECTED   0-20    0-100    0-100     0-2      ',          LS070783
     +/,10X,'FOUND   ',4F9.5)                                           LS070783
 3000 FORMAT(5X,'*****ERROR - ONLY POSSIBLE VALUES FOR TYPE ARE 4,5,6 ',KD1291
     +'AND 99, FOUND ',I5)                                              KD1291
 3005 FORMAT(5X,'*****ERROR - IMPV MUST BE BETWEEN 0 AND 1, FOUND',F6.3)LS070783
 3015 FORMAT(5X,'*****ERROR - IRU IS GREATER THAN # OF NRU-S, FOUND',
     +          I3,'>',I3)
 3023   FORMAT(5X,'*****WARNING - NDX FOR FLOW PLANE TYPE 99 MUST BE 1',KD1291
     +'.   NDX HAS BEEN SET TO 1')                                      KD1291
 3025 FORMAT(5X,'*****ERROR - MAXIMUM VALUE FOR NDX IS 10, FOUND',I4)   LS070783
 3035 FORMAT(5X,'*****ERROR - SHORTEST ROUTING INTERVAL ALLOWED IS 1 M',KD1291
     +'INUTE, FOUND ',F5.1)                                             KD1291
 3050 FORMAT(5X,'*****WARNING - VALUE OUT OF SUGGESTED RANGE',          LS070783
     +/,10X,'VARIABLE    PRTIN   PRTOUT      FRN    THRES    SLOPE',    AL072883
     +/,10X,'EXPECTED     0-3     0-3     0.001-1.   0-6   0.0001-0.5',  LS KF
     +/,10X,'FOUND   ',I7,I9,2X,3F9.4)                                  AL072883
 3060 FORMAT(5X,'*****WARNING - PARM1 OR PARM2 OUT OF RANGE FOR TYPE=4',LS070783
     */,10X,'RANGE FOR PARM1 IS 0.001 TO 100.',                         AL072883
     */,10X,'RANGE FOR PARM2 IS 0.5 TO 3.0')                            LS070783
 3210 FORMAT(5X,'*****ERROR- WRONG TYPE SPECIFIED')                     LS070783
 3400 FORMAT(5X,'*****WARNING - PARM1 VALUE OUT OF SUGGESTED RANGE')    LS070783
 3410 FORMAT(5X,'*****WARNING - VALUES OUT OF SUGGESTED RANGE',         LS070783
     +/,10X,'RANGE FOR PARM1 IS 0.0001 TO 100.0',                       AL072883
     +/,10X,'RANGE FOR PARM2 IS 0.5 TO 3.0')                            LS070783
 8078 FORMAT(5X,'*****ERROR - NUMBER OF CHANNEL, JUNCTION AND RESERVOI',KD1291
     +'R SEGMENTS IS LIMITED TO A TOTAL OF 50')                         KD1291
 9000 FORMAT('1','BYR=',I5,' BMO=',I5,' BDY=',I5)                       KD1291
 9004   FORMAT(5X,'***** ERROR - NUMBER OF STORM PERIODS LIMITED TO 50')AL081083
 9010 FORMAT(1X,'NSP=',I5)
 9012 FORMAT(/,5X,'*****ERROR - CHECK STORM DATES FOR STORM #',I5)      LS070783
 9014 FORMAT(/,5X,'*****ERROR - STORM PERIOD ',I5,' IS OUT OF SEQUENCE',LS070783
     1       ' LAST STORM WAS ',I10)
 9017 FORMAT(/,5X,'*****WARNING - IF ISUN SWITCH = 1, STORM SHOULD ',/, LS070783
     +10X,'BEGIN AT TIME 0000 AND END AT 2400')                         LS070783
 9020 FORMAT(1X,2I3,2(I4,2I2),2X,'NE=',I3)                              KD1291
 9021   FORMAT (/5X,'***** ERROR - END-OF-FILE REACHED ON FILE FOR',    AL072183
     1         ' GROUP 2 CARDS.',/,                                     KD1291
     2         10X,I4,' STORM PERIOD CARDS FOUND',/,                    KD1291
     3         10X,I4,' STORM PERIODS REQUESTED',/)                     KD1291
 9030 FORMAT(1X,'INV=',I4)                                              KD1291
 9056 FORMAT(5X,'*****ERROR - OVERLAND FLOW SEGMENTS LIMITED TO 50')
 9060 FORMAT(1X,//,1X,'NUMBER OF OVERLAND FLOW PLANE SEGMENTS IS',      KD1291
     1I5,'.  THEIR CHARACTERISTICS ARE AS FOLLOWS:',//,                 KD1291
     2' SEGMENT  IDS         IRU',29X,'THRES TYPE  PRINT  NDX LENGTH',  KD1291
     3' SLOPE ROUGH- PARM1 PARM2 ALPHA EXPM  ROUTE PRINT',/,' # NAME',  KD1291
     447X,'DEPTH',7X,'IN OUT',19X,'NESS',27X,'INT.  INT.')              KD1291
 9070 FORMAT(1X,I2,1X,A4,I5,7X,I6,28X,F6.4,I4,I5,I3,I5,F7.1,1X,F5.4,2X,
     1F4.3,F7.2,2F6.2,F5.2,2F6.1)
 9080 FORMAT(/1X,'NUMBER OF CHANNEL AND RESERVOIR SEGMENTS IS',I4//)    KD1291
 9081 FORMAT(' SEGMENT     UPSTREAM       ADJACENT     INC.   CUM.  T', KD1291
     1'HRES TYPE  PRINT  NDX LENGTH SLOPE ROUGH- PARM1 PARM2 ALPHA EXP',
     2'M  ROUTE PRINT',/,' # NAME      SEGMENTS       SEGMENTS     ARE',KD1291
     3'A   AREA  DISC.       IN OUT',19X,'NESS',26X,'INT.  INT.')       KD1291
 9090 FORMAT(1X,I2,1X,A4,5(2X,A4),F9.1,F7.1,F6.2,I3,I5,I3,I5,F7.1,1X,
     1F5.4,2X,F4.3,F7.2,2F6.2,F5.2,2F6.1)
C
C     + + + END SPECIFICATIONS + + +
C
      SIMOPT(1)=1
      JPL=0
      IERR=0                                                            LS070783
      JPR=0
      IPRT=0
      JPG = 0                                                            1083KF
      DO 10 J=1,367
   10 UVWY(J)=0
      WRITE(72,9000) BYR,BMO,BDY
      OWY=BWY
      READ(32,8000) NSP
      IF (NSP.LE.50) GO TO 9005                                         AL081883
        IERR = 1                                                        AL081083
        WRITE(72,9004)                                                   AL081083
 9005 CONTINUE                                                          AL081083
C                                                                       AL081083
      WRITE(72,9010) NSP
      NST=0
      LSTORM=BYR*10000 + BMO*100 + BDY                                  LS070783
      NERR=0                                                            LS070783
C          ** DELINEATE ALL STORM PERIODS **
      ICHK = 0                                                          AL072183
      DO 250 I=1,NSP
      READ(32,8010,END=251) ISS,CD,IHS,NY,NM,ND,JY,JM,JD,KD,KT,LD,LT,   AL072183
     1         IC2,RC1,RC2                                              LS103183
      ICHK = ICHK + 1                                                   AL072183
      NSTORM=NY*10000+NM*100+ND                                         LS070783
      JSTORM=JY*10000+JM*100+JD                                         LS070783
      IF(JSTORM.LT.NSTORM) NERR=1                                       LS070783
      IF(NSTORM.GT.LSTORM) GO TO 9013                                   LS070783
      WRITE(72,9014) ISS, LSTORM                                         LS070783
      IERR=1                                                            LS070783
 9013 LSTORM=NSTORM                                                     LS070783
      IF(KD.NE.ND) NERR=1                                               LS070783
      IF(IHS.EQ.1.AND.LD.NE.JD) NERR=1                                  LS070783
      IF(IHS.GT.1.AND.LD.GT.JD) NERR=1                                  LS070783
      IF(NERR.EQ.0) GO TO 9016                                          LS070783
      NERR=0                                                            LS070783
      IERR=1                                                            LS070783
      WRITE(72,9012)I                                                    LS070783
 9016 IF(ISUN.EQ.0) GO TO 9018                                          LS070783
      IF(KT.EQ.0.AND.LT.EQ.2400) GO TO 9018                             LS070783
      WRITE(72,9017)                                                     LS070783
 9018 CONTINUE                                                          LS070783
      IJK=16*IHS
      NEF=2*IHS
      QC=CD
      NE=IJK
      L=0
      NSS=ISS
      NHS=IHS
      NST=NST+NHS
      WRITE(72,9020) NSS,NHS,NY,NM,ND,JY,JM,JD,NE
C          ** COMPUTE WYDYS FOR EACH HYDROGRAPH SEGMENT (IHS) **
C              WRITE STORM PERIOD RECORD TO DISK (12)
      II=NY
      JJ=NM
      KK=ND
      CALL WYDYS(IWY,II,JJ,MTH(JJ),KK,JWYD,RODYS)
   30 FWY=IWY
      FDY=JWYD
      MXDY=RODYS
      GO TO 55
   40 UVWY(367)=OWY
      WRITE(72,9030) OWY
      WRITE(11) UVWY
      CALL UVRET(OWY)
      DO 50 J=1,367
   50 UVWY(J)=0
      OWY=OWY+1
   55 IF(OWY.LT.FWY) GO TO 40
      II=JY
      JJ=JM
      KK=JD
      CALL WYDYS(IWY,II,JJ,MTH(JJ),KK,JWYD,RODYS)
   60 LWY=IWY
      LDY=JWYD
      IF(FWY.NE.LWY) GO TO 80
      DO 70 J=FDY,LDY
      UVWY(J)=CD
   70 CONTINUE
      GO TO 120
   80 DO 90 J=FDY,MXDY
      UVWY(J)=CD
   90 CONTINUE
      UVWY(367)=OWY
      WRITE(72,9030) OWY
      WRITE(11) UVWY
      CALL UVRET(OWY)
      DO 100 J=1,367
  100 UVWY(J)=0
      DO 110 J=1,LDY
      UVWY(J)=CD
  110 CONTINUE
      OWY=OWY+1
  120 DO 190 J=1,NHS
      L=16*(J-1)
      IF(J.EQ.1) GO TO 128
      READ(32,8014) KD,KT,LD,LT,IC2,RC1,RC2                             LS103183
      IF(KD.LT.ND.OR.KD.GT.LD) NERR=1                                   LS070783
      IF(LD.GT.JD) NERR=1                                               LS070783
      IF(J.EQ.NHS.AND.LD.NE.JD) NERR=1                                  LS070783
      IF(NERR.EQ.0) GO TO 8015                                          LS070783
      NERR=0                                                            LS070783
      IERR=1                                                            LS070783
      WRITE(72,9012)I                                                    LS070783
 8015 CONTINUE                                                          LS070783
  128 IF(KD.LT.ND) GO TO 130
      BY=NY
      BM=NM
      GO TO 140
  130 BY=JY
      BM=JM
  140 II=BY
      JJ=BM
      KK=KD
      CALL WYDYS(IWY,II,JJ,MTH(JJ),KK,JWYD,RODYS)
  150 FHA(L+13)=IWY
      FHA(L+14)=JWYD
      IF(LD.LT.ND) GO TO 160
      EY=NY
      EM=NM
      GO TO 170
  160 EY=JY
      EM=JM
  170 II=EY
      JJ=EM
      KK=LD
      CALL WYDYS(IWY,II,JJ,MTH(JJ),KK,JWYD,RODYS)
  180 FHA(L+15)=IWY
      FHA(L+16)=JWYD
      FHA(L+1)=BY
      FHA(L+2)=BM
      FHA(L+3)=KD
      FHA(L+4)=KT
      FHA(L+5)=EY
      FHA(L+6)=EM
      FHA(L+7)=LD
      FHA(L+8)=LT
      FHA(L+9)=IC1
      FHA(L+10)=IC2
      FHAF(L+1)=RC1
      FHAF(L+2)=RC2
  190 CONTINUE
      WRITE(12) NSS,QC,NHS,NE,NEF,(FHA(J),J=1,NE),(FHAF(JK),JK=1,NEF)
      GO TO 250
  250 CONTINUE
C                                                                       AL072183
      GO TO 252                                                         AL072183
 251  CONTINUE                                                          AL072183
        WRITE(72,9021) ICHK,NSP                                          AL072183
        IERR = 1                                                        AL072183
 252  CONTINUE                                                          AL072183
C                                                                       AL072183
C          ** SET UVWY FOR REMAINING YEARS **
      WRITE(72,9030) OWY
      UVWY(367)=OWY
      WRITE(11) UVWY
      CALL UVRET(OWY)
      OWY=OWY+1
      DO 260 J=1,367
  260 UVWY(J)=0
      IF(OWY.GT.EWY)GO TO 275
      DO 270 J=OWY,EWY
      UVWY(367)=J
      WRITE(11) UVWY
      WRITE(72,9030) J
  270 CONTINUE
  275 OWY=0
      CALL UVRET(OWY)
C          ** COMPUTE START AND END TIMES FOR EACH HYDROGRAPH **
C             SEGMENT (IHS)
C      **READ INFILTRATION AND UPLAND EROSION PARAMETERS**
C
      WRITE(72,14)
      IF(ISIM.LT.3)GO TO 20
      WRITE(72,15)
      DO 17 I=1,NRU
      READ(33,26)X(1,I),X(2,I),X(4,I),X(3,I),KRA(I),HCA(I),
     1KFA(I),MMA(I),ENA(I)
      WRITE(72,18)I,X(1,I),X(2,I),X(4,I),X(3,I),KRA(I),HCA(I),KFA(I),
     1MMA(I),ENA(I)
   17 CONTINUE
      GO TO 25
   20 WRITE(72,22)
      DO 12 I=1,NRU
      READ(33,13)X(1,I),X(2,I),X(4,I),X(3,I)
      WRITE(72,21)I,X(1,I),X(2,I),X(4,I),X(3,I)
   12 CONTINUE
   25 REWIND 12
      DO 1210 I=1,NRU                                                   LS070783
      IF(X(1,I).LT.0..OR.X(1,I).GT.20. .OR.                             LS070783
     +   X(2,I).LT.0..OR.X(2,I).GT.100..OR.                             LS070783
     +   X(4,I).LT.0..OR.X(4,I).GT.100..OR.                             AL072883
     +   X(3,I).LT.0..OR.X(3,I).GT.2. )    NERR=1                       AL072883
      IF(NERR.EQ.0) GO TO 1211                                          LS070783
      NERR=0                                                            LS070783
      IERR=1                                                            AL072883
      WRITE(72,1213)I,X(1,I),X(2,I),X(4,I),X(3,I)                        LS070783
 1211 IF(ISIM.LT.3) GO TO 1210                                          LS070783
      IF(KRA(I).LT.0..OR.KRA(I).GT.100..OR                              LS070783
     +  .HCA(I).LT.0..OR.HCA(I).GT.100..OR.                             LS070783
     +   KFA(I).LT.0..OR.KFA(I).GT.100..OR.                             LS070783
     +   MMA(I).LT.0..OR.MMA(I).GT.100..OR.                             0485LS
     +   ENA(I).LT.0..OR.ENA(I).GT.100.) NERR=1                         0485LS
      IF(NERR.EQ.0) GO TO 1210                                          LS070783
      NERR=0                                                            LS070783
      IERR=1                                                            AL072883
      WRITE(72,1212)I,KRA(I),HCA(I),KFA(I),MMA(I),ENA(I)                 LS070783
 1210 CONTINUE
      IHS=0
      WRITE(72,1200)
      DO 600 N=1,NSP
      IF(N.GT.NSP) GO TO 600
      READ(12) NSS,QC,NHS,NE,NEF,(FHA(J),J=1,NE),(FHAF(JK),JK=1,NEF)
      ND=0
      IY=FHA(13)
      JY=FHA(16*(NHS-1)+15)
      IF(JY.LE.IY) GO TO 330
      ND=1
  280 II=365
      IF(MOD(IY,4).EQ.0) II=366
      I=FHA(14)
  290 ASSIGN 300 TO KRTN
      GO TO 400
  300 I=I+1
      IF(I.LE.II) GO TO 290
  305 I=1
      II=FHA(16*(NHS-1)+16)
  310 ASSIGN 320 TO KRTN
      GO TO 400
  320 I=I+1
      IF(I.LT.10) IY=JY
      IF(I.GT.II) GO TO 360
      GO TO 310
  330 I=FHA(14)
      II=FHA(16*(NHS-1)+16)
  340 ASSIGN 350 TO KRTN
      GO TO 400
  350 I=I+1
      IF(I.LE.II) GO TO 340
  360 IHS=IHS+NHS
      GO TO 600
C          ** DAILY ANALYSIS FOR START AND END TIMES **
C              WRITE DAILY RECORD TO DISK (13)
  400 ISS=0
      J=0
      DO 440 K=1,NHS
      L=(K-1)*16
      IF(ND.EQ.0) GO TO 408
      IF(FHA(L+16).GE.FHA(L+14)) GO TO 408
      IF(FHA(L+14).LE.I.OR.FHA(L+16).GE.I) GO TO 410
      GO TO 440
  408 IF(FHA(L+14).LE.I.AND.FHA(L+16).GE.I) GO TO 410
      GO TO 440
  410 IF(ISS.NE.0) GO TO 430
  420 ISS=1
      IBS=K
      IS=IHS+K
  430 J=J+1
  440 CONTINUE
      IKJ=2*J
      IF(J.EQ.0) IKJ=1
      NEL=IKJ
      NS=J
      WY=IY
      WYD=I
      J=0
      RFL=0
      SFL=0
      IES=NS+IBS-1
      DO 520 K=IBS,IES
      L=(K-1)*16
      IF(FHA(L+16).NE.I) GO TO 500
  450 J=J+1
      M=2*(J-1)
      IF(FHA(L+14).NE.I) GO TO 470
  460 WY=IY
      WYD=I
      IB=FHA(L+4)
      IC=IB/100
      IR=IB-100*IC
      IB=60*IC+IR
      DFH(M+1)=IB
      GO TO 480
  470 RFL=1
      DFH(M+1)=0.
  480 IB=FHA(L+8)
      IC=IB/100
      IR=IB-100*IC
      IB=60*IC+IR
      DFH(M+2)=IB
      GO TO 520
  500 IF(FHA(L+14).NE.I) GO TO 515
  510 J=J+1
      M=2*(J-1)
      SFL=1
      IB=FHA(L+4)
      IC=IB/100
      IR=IB-100*IC
      IB=60*IC+IR
      WY=IY
      WYD=I
      DFH(M+1)=IB
      DFH(M+2)=1440.
      GO TO 520
  515 IF(NS.EQ.0) GO TO 520
      SFL=1
      RFL=1
      J=J+1
      M=2*(J-1)
      DFH(M+1)=0.
      DFH(M+2)=1440.
  520 CONTINUE
      WRITE(13) WY,WYD,NS,IS,RFL,SFL,DFH
      NS2=2*NS
      WRITE(72,521) WY,WYD,NS,IS,RFL,SFL,(DFH(NS1),NS1=1,NS2)
      J=0
      DO 550 K=1,NHS
      J=J+1
      M=2*(J-1)
      DFH(M+1)=0.
      DFH(M+2)=0.
  550 CONTINUE
      GO TO KRTN, (300,320,350)
  600 CONTINUE
C          ** COMPUTE OBSERVED STORM VOLUME AND PEAK **
      REWIND 11
      REWIND 12
      REWIND 13
      REWIND 14
      DO 800 IWY=BWY,EWY
      READ(11) UVWY
      MXDY=365
      IF(MOD(UVWY(367),4).EQ.0) MXDY=366
      DO 790 I=1,MXDY
      IF(UVWY(I).EQ.0) GO TO 790
      READ(13) WY,WYD,NS,IS,RFL,SFL,DFH
      IF(UVWY(I).LT.2) GO TO 790
  610 READ(14) UVDEL,UVSTA,UVPARM,UVSTAT,UVYR,UVMO,UVDY,
     1UVRPD,UVRDG1,UVNRDG,UVNVAL,(UVDATA(JK),JK=1,UVNRDG)
      RPD=UVRPD
      DT=1440./RPD
      DTD=DT/1440.
      CRO=0.
      RT=0.
      IF(NS.LE.0) GO TO 630
  620 INS=1
      ISS=IS
      TB=DFH(1)
      TE=DFH(2)
      CTSW=1
      IF(RFL.NE.0) GO TO 640
      QMAX=0.
      STRO=0.
      GO TO 640
  630 CTSW=0
      GO TO 750
  640 KCK=UVRDG1+UVNRDG-1
      DO 740 K=1,UVRPD
      RT=RT+DT
      IF(K.LT.UVRDG1) GO TO 700
      IF(K.GT.KCK) GO TO 700
      IF(RT.LE.TB) GO TO 700
  690 KDAT=K-UVRDG1+1
      D=UVDATA(KDAT)
      CRO=CRO+D
      IF(D.GT.QMAX) QMAX=D
  700 IF(RT.LT.TE) GO TO 740
  710 OBRO(ISS)=CRO*DTD
      OBPK(ISS)=QMAX
      IF(INS.EQ.1) OBRO(ISS)=OBRO(ISS)+STRO
      INS=INS+1
      ISS=ISS+1
      IF(INS.GT.NS) GO TO 730
  720 M=2*(INS-1)
      TB=DFH(M+1)
      TE=DFH(M+2)
      CRO=0.
      QMAX=0.
      STRO=0.
      GO TO 740
  730 CTSW=0
      GO TO 750
  740 CONTINUE
  750 IF(RFL.EQ.0) STRO=0.
      IF(SFL.EQ.1) STRO=CRO*DTD+STRO
  790 CONTINUE
  800 CONTINUE
      IHS=0
      DO 900 I=1,NSP
      READ(12) NSS,QC,NHS,NE,NEF,(FHA(J),J=1,NE),(FHAF(JK),JK=1,NEF)
      DO 890 J=1,NHS
      L=(J-1)*16
      IHS=IHS+1
      IF(FHA(L+10).NE.1) GO TO 820
  810 OBPK(IHS)=FHAF(L+1)
      OBRO(IHS)=FHAF(L+2)
      GO TO 890
  820 KT=FHA(L+4)
      KH=KT/100
      KM=(KT-100*KH)+60*KH
      RKM=KM
      LT=FHA(L+8)
      LH=LT/100
      LM=(LT-100*LH)+60*LH
      RLM=LM
      IF(FHA(L+16).LT.FHA(L+14)) GO TO 840
  830 DUR=(FHA(L+16)-FHA(L+14))*1440.-RKM+RLM
      GO TO 850
  840 IWY=FHA(L+13)
      MXDY=365
      IWY4=IWY
      IF(MOD(IWY4,4).EQ.0) MXDY=366
      DUR=(MXDY-FHA(L+14)+FHA(L+16))*1440.-RKM+RLM
  850 OBPK(IHS)=OBPK(IHS)-FHAF(L+1)
      OBRO(IHS)=(OBRO(IHS)-FHAF(L+1)*DUR/1440.)*23.8017/DAT
  890 CONTINUE
  900 CONTINUE
      REWIND 11
      REWIND 12
      REWIND 13
      REWIND 14
C          ** IF ROUTE SWITCH ON (=1) READ AND COMPUTE FLOW **
C             PLANE CHARACTERISTICS AND WRITE TO DISK (16)
      IF(ISIM.LT.2) GO TO 1100
      SIMOPT(2)=1
      SIMOPT(3)=0
      IF(ISIM.EQ.3) SIMOPT(3)=1
      IDT=1
      ISAVE=0
      READ(34,8040) NOFSEG
      IF(NOFSEG.LE.50) GO TO 9055
      IERR=1
      WRITE(72,9056)
 9055 CONTINUE
      WRITE(72,9060) NOFSEG
C          ** SEQUENCE AND CHARACTERIZE FLOW PLANE SEGMENTS **
      DO 999 IOF=1,NOFSEG
      IF(IOF.GT.NOFSEG) GO TO 999
      IC=IOF
      READ(34,8070) PCRID,IMPV,TYPE,PRTIN,PRTOUT,NDX,FLGTH,SLOPE,
     1FRN,PARM1,PARM2,IRU,THRES,DTM,DTOS
      IF(TYPE.EQ.4.OR.TYPE.EQ.5.OR.TYPE.EQ.6.OR.TYPE.EQ.99) GO TO 8074  LS070783
      IERR=1
      WRITE(72,3000) TYPE
 8074 IF(IMPV.GE.0.AND.IMPV.LE.1.) GO TO 3010                           LS070783
      NERR=1                                                            LS070783
      WRITE(72,3005) IMPV                                                LS070783
 3010   IF(TYPE.EQ.99.AND.NDX.NE.1) THEN                                0485LS
        NDX=1                                                           0485LS
        WRITE(72,3023)                                                   0485LS
        END IF                                                          0485LS
      IF(NDX.GT.0.AND.NDX.LE.10) GO TO 3020                             0485LS
      NERR=1                                                            LS070783
      WRITE(72,3025) NDX                                                 LS070783
 3020 IF(IRU.LE.NRU) GO TO 3030                                         LS070783
      NERR=1                                                            LS070783
      WRITE(72,3015) IRU, NRU                                            LS070783
 3030 IF(DTM.GE.1.) GO TO 3040                                          LS070783
      NERR=1                                                            LS070783
      WRITE(72,3035) DTM                                                 LS070783
 3040 IF(NERR.EQ.1) IERR=1                                              LS070783
      NERR=0                                                            LS070783
 8077 CONTINUE                                                          AL082683
      IF (TYPE.EQ.4.OR.TYPE.EQ.99) GO TO 8075                           LS0786
      IF(PRTIN.LT.0.OR.PRTIN.GT.3.OR                                     1083KF
     +  .PRTOUT.LT.0.OR.PRTOUT.GT.3.OR.                                  1083KF
     +   FRN.LT.0.001.OR.FRN.GT.1.0.OR.                                 AL072883
     +    SLOPE.LT.0.0001.OR.SLOPE.GT.0.5.OR.                           AL072883
     +   THRES.LT.0..OR.THRES.GT.6.) NERR=1                             LS061884
      IF(NERR.EQ.0) GO TO 8075                                          LS070783
      NERR=0                                                            LS070783
      IERR=1                                                            AL072883
      WRITE(72,3050)PRTIN,PRTOUT,FRN,THRES,SLOPE                         AL072883
 8075 CONTINUE                                                          LS070783
      IF(TYPE.NE.4) GO TO 8076                                          LS070783
      IF(PARM1.LT.0.001.OR.PARM1.GT.100.0.OR.                           AL072883
     +   PARM2.LT.0.5.OR.PARM2.GT.3.0)  NERR=1                          AL072883
      IF(NERR.EQ.0) GO TO 8076                                          LS070783
      NERR=0                                                            LS070783
      IERR=1                                                            AL072883
      WRITE(72,3060)                                                     LS070783
 8076 CONTINUE                                                          LS070783
      KRU(IOF)=IRU
      IDS=KDS(IRU)
      IF(IMPV.EQ.0.) GO TO 8066
      IF(IMPV.LT.1.) GO TO 8064
      FRNI=FRN
      OIP1=PARM1
      OIP2=PARM2
      GO TO 8066
 8064 READ(34,8065)FRNI,OIP1,OIP2
 8066 IF(DTOS.EQ.0.)DTOS=DTM
      IF(DTM.GT.5.) DTM=5.
      NXS=NDX+1
      NDELS=1440./DTM
      IF(PRTOUT.GT.0) ODELS=1440./DTOS
      IF(TYPE.NE.99) GO TO 8071
      DX=FLGTH
      GO TO 8072
 8071 CALL AM(TYPE,FRN,SLOPE,PARM1,PARM2,ALPR,CMP)
      ALPR1=1./ALPR
      CMP1=1./CMP
      IF(IMPV.EQ.0.) GO TO 8073
      CALL AM(TYPE,FRNI,SLOPE,OIP1,OIP2,ALPI,CMI)
      ALPI1=1./ALPI
      CMI1=1./CMI
 8073 DX=FLGTH/NDX
      DTS=DTM*60.
      DTDX=DTS/DX
      DTDXM=DTDX*CMP
      DXDT=DX/DTS
 8072 OFKEYS(IC)=PCRID
      WRITE(72,9070) IC,PCRID,IDS,IRU,THRES,TYPE,PRTIN,PRTOUT,NDX,FLG
     1TH,SLOPE,FRN,PARM1,PARM2,ALPR,CMP,DTM,DTOS
C     THRES=THRES/12.                                                   LS061884
      PCRIDA(IC)=PCRID
      TYPEA(IC)=TYPE
      PROUTA(IC)=PRTOUT
      PRTINA(IC)=PRTIN
      NXSA(IC)=NXS
      NDELSA(IC)=NDELS
      ODELSA(IC)=ODELS
      THRESA(IC)=THRES
      DXA(IC)=DX
      DTMA(IC)=DTM
      DTSA(IC)=DTS
      DTOSA(IC)=DTOS
      DTDXMA(IC)=DTDXM
      DXDTA(IC)=DXDT
      DTDXA(IC)=DTDX
      ALPRA(IC)=ALPR
      ALPR1A(IC)=ALPR1
      CMPA(IC)=CMP
      CMP1A(IC)=CMP1
      OFIPA(IC)=IMPV
      IF(IC.GT.NOFSEG) GO TO 1095
      ALPIA(IC)=ALPI
      ALPI1A(IC)=ALPI1
      CMIA(IC)=CMI
      CMI1A(IC)=CMI1
 1095 CONTINUE
  930 IF(PRTIN.EQ.0) GO TO 940
      ISAVE=ISAVE+1
      IF(ISIM.EQ.3) ISAVE=ISAVE+1
      IF(PRTIN.EQ.2) GO TO 935
      IF(PRTIN .EQ. 3) GO TO 936                                         1083KF
      IPRT=IPRT+1
      JPR=1
      IF(ODELS.GT.IDT) IDT=ODELS
      IF(ISIM.EQ.3) IPRT=IPRT+1
      GO TO 940
  935 JPL=1
      GO TO 940                                                          1083KF
  936 CONTINUE                                                           1083KF
      JPG = JPG + 1                                                      1083KF
  940 IF(PRTOUT.EQ.0) GO TO 999
      ISAVE=ISAVE+1
      IF(ISIM.EQ.3) ISAVE=ISAVE+1
      IF(PRTOUT.EQ.2) GO TO 950
      IF(PRTOUT .EQ. 3) GO TO 945                                        1083KF
      IPRT=IPRT+1
      JPR=1
      IF(ODELS.GT.IDT) IDT=ODELS
      IF(ISIM.EQ.3) IPRT=IPRT+1
      GO TO 999
  950 JPL=1
      GO TO 999                                                          1083KF
  945 CONTINUE                                                           1083KF
      JPG = JPG + 1                                                      1083KF
  999 CONTINUE
C          ** READ AND COMPUTE CHANNEL ROUTING CHARACTERISTICS **
C             WRITE TO DISK (16)
      DO 8085 I=1,50
      OFLT(I)=0.
      DO 8085 J=1,50
      OFAR(I,J)=0.
 8085 CONTINUE
      READ(34,8080) NCRSEG
      IF(NCRSEG.LE.50) GO TO 8079                                       LS070783
      WRITE(72,8078)                                                     LS070783
      IERR=1                                                            LS070783
 8079 CONTINUE                                                          LS070783
      IF(NCRSEG.EQ.0) GO TO 1100
      IC=NOFSEG
      IRIT=0
      WRITE(72,9080) NCRSEG
      WRITE(72,9081)
      DO 1005 III=1,50
      DO 1004 JJJ=1,3
      UPA(III,JJJ)=0
 1004 CONTINUE
 1005 RITEA(III)=0
      DO 1080 I=1,NCRSEG
      IF(I.GT.NCRSEG) GO TO 1080
      IC=IC+1
      READ(34,8090) PCRID,UP1,UP2,UP3,RBC,LBC,TYPE,PRTIN,
     1PRTOUT,NDX,FLGTH,SLOPE,FRN,PARM1,PARM2,THRES,DTM,DTOS
      IF(DTOS.EQ.0.) DTOS=DTM
      IF(TYPE.GE.1.AND.TYPE.LE.9) GO TO 3200                            LS070783
      IERR=1                                                            LS070783
      WRITE(72,3210)                                                     LS070783
 3200 IF(NDX.LE.10) GO TO 3220                                          LS070783
      IERR=1                                                            LS070783
      WRITE(72,3025)                                                     LS070783
 3220 IF(DTM.GE.1.) GO TO 3240                                          LS070783
      IERR=1                                                            LS070783
      WRITE(72,3035)                                                     LS070783
 3240 CONTINUE                                                          AL082683
      IF (TYPE.GE.4) GO TO 3300                                         AL082683
      IF(PRTIN.LT.0.OR.PRTIN.GT.3.OR.                                    1083KF
     +   PRTOUT.LT.0.OR.PRTOUT.GT.3.OR.                                  1083KF
     +   FRN.LT.0..OR.FRN.GT.1..OR.                                     LS070783
     +   SLOPE.LT.0.0001.OR.SLOPE.GT.0.5.OR.                            AL072883
     +   THRES.LT.0..OR.THRES.GT.1.) NERR=1                             LS070783
      IF(NERR.EQ.0) GO TO 3300                                          LS070783
      WRITE(72,3050) PRTIN,PRTOUT,FRN,THRES,SLOPE                        AL072883
      NERR=0                                                            LS070783
      IERR = 1                                                          AL072883
 3300 CONTINUE                                                          LS070783
      IF(TYPE.NE.1.AND.TYPE.NE.3) GO TO 8097                            LS070783
      IF(PARM1.GE.0.001.OR.PARM1.LE.100.) GO TO 8098                    AL072883
      WRITE(72,3400)                                                     LS070783
      IERR=1                                                            AL072883
 8097 IF(TYPE.NE.4) GO TO 8098                                          LS070783
      IF(PARM1.LT.0.0001.OR.PARM1.GT.100..OR.                           AL072883
     +   PARM2.LT.0.5.OR.PARM2.GT.3.0)  NERR=1                          LS070783
      IF(NERR.EQ.0) GO TO 8098                                          LS070783
      NERR=0                                                            LS070783
      IERR=1                                                            AL072883
      WRITE(72,3410)                                                     LS070783
 8098 CONTINUE                                                          LS070783
 1020 DA=0.0
      UPSW=1
      IF(UP1.EQ.BLNK4.AND.UP2.EQ.BLNK4.AND.UP3.EQ.BLNK4) UPSW=0
      UPSWA(I)=UPSW
      IF(RBC.EQ.BLNK4) GO TO 1030
      DO 1025 ISQ=1,NOFSEG
      IF(RBC.NE.OFKEYS(ISQ)) GO TO 1025
      RBA(I)=ISQ
      IOF=RBA(I)
      IF(TYPEA(IOF).NE.99) GO TO 1021
      DA=DXA(IOF)*FLGTH
      GO TO 1040
 1021 DA=FLGTH*(NXSA(IOF)-1)*DXA(IOF)
      OFAR(I,IOF)=OFAR(I,IOF)+DA
      GO TO 1040
 1025 CONTINUE
 1030 RBA(I)=0
 1040 IF(LBC.EQ.BLNK4) GO TO 1045
      DO 1042 ISQ=1,NOFSEG
      IF(LBC.NE.OFKEYS(ISQ)) GO TO 1042
      LBA(I)=ISQ
      IOF=LBA(I)
      IF(TYPEA(IOF).NE.99)GO TO 1041
      DA=DA+(DXA(IOF)*FLGTH)
      GO TO 1048
 1041 OFAR(I,IOF)=OFAR(I,IOF)+DA
      DA=DA+(FLGTH*(NXSA(IOF)-1)*DXA(IOF))
      GO TO 1048
 1042 CONTINUE
 1045 LBA(I)=0
 1048 DA=DA/43560.
      IDA(I)=DA
      CDA(I)=DA
      CUPA(I,1)=UP1
      CUPA(I,2)=UP2
      CUPA(I,3)=UP3
      IF(UPSW.EQ.0) GO TO 1050
      NCR=I-1
      DO 2000 JJ=1,NCR
      J=I-JJ
      JC=J+NOFSEG
      DO 2010 K=1,3
      IF(PCRIDA(JC).NE.CUPA(I,K)) GO TO 2010
      UPA(I,K)=JC
      DA=DA+CDA(J)
      IF(ISIM.LT.3) GO TO 2010
      DO 1500 IOF=1,NOFSEG
      OFAR(I,IOF)=OFAR(I,IOF)+OFAR(J,IOF)
 1500 CONTINUE
 2010 CONTINUE
 2000 CONTINUE
      CDA(I)=DA
      NCR=I-2
      DO 2050 JJ=1,NCR
      J=I-JJ-1
      JC=J+NOFSEG
      DO 2040 K=1,3
      IF(PCRIDA(JC).NE.CUPA(I,K)) GO TO 2040
      RITEA(J)=1
 2040 CONTINUE
 2050 CONTINUE
 1050 NXS=NDX+1
      IF(TYPE.EQ.8) NXS=NXS-1
      NDELS=1440./DTM
      ODELS=1440./DTOS
      IF(TYPE.EQ.7)GO TO 1051
      IF(TYPE.NE.8) GO TO 1054
      K=NDX
      KK=NSOS(NDX)
      DO 1052 J=1,KK
      O1=O2(K,J)*.5
      WV5(K,J)=(S2(K,J)*288.)+O1
      WV15(K,J)=(S2(K,J)*96.)+O1
 1052 CONTINUE
      DO 1053 J=2,KK
      O1=O2(K,J)-O2(K,J-1)
      S5(K,J)=O1/(WV5(K,J)-WV5(K,J-1))
      C5(K,J)=O2(K,J)-(S5(K,J)*WV5(K,J))
      S15(K,J)=O1/(WV15(K,J)-WV15(K,J-1))
      C15(K,J)=O2(K,J)-(S15(K,J)*WV15(K,J))
 1053 CONTINUE
      GO TO 1051
 1054 CALL AM(TYPE,FRN,SLOPE,PARM1,PARM2,ALPR,CMP)
      ALPR1=1./ALPR
      CMP1=1./CMP
      DX=FLGTH/NDX
      DTS=60.*DTM
      DTDX=DTS/DX
      DTDXM=DTDX*CMP
      DXDT=DX/DTS
 1051 WRITE(72,9090)I,PCRID,UP1,UP2,UP3,RBC,LBC,IDA(I),CDA(I),THRES,TYPE
     1,PRTIN,PRTOUT,NDX,FLGTH,SLOPE,FRN,PARM1,PARM2,ALPR,CMP,DTM,DTOS
      IF(I.NE.NCRSEG) GO TO 1065                                        LS070783
      PERDIF=(CDA(I)-DAT)/DAT                                           LS070783
      IF(ABS(PERDIF).LT..01) GO TO 1065                                 LS070783
      IF(ABS(PERDIF).GE..05) IERR=1                                     LS070783
      PERDIF=PERDIF*100.                                                LS070783
      WRITE(72,1066) PERDIF                                              LS070783
 1065 CONTINUE
      PCRIDA(IC)=PCRID                                                  AL080983
      TYPEA(IC)=TYPE
      PROUTA(IC)=PRTOUT
      PRTINA(IC)=PRTIN
      NXSA(IC)=NXS
      NDELSA(IC)=NDELS
      ODELSA(IC)=ODELS
      THRESA(IC)=THRES
      DXA(IC)=DX
      DTMA(IC)=DTM
      DTSA(IC)=DTS
      DTOSA(IC)=DTOS
      DTDXMA(IC)=DTDXM
      DXDTA(IC)=DXDT
      DTDXA(IC)=DTDX
      ALPRA(IC)=ALPR
      ALPR1A(IC)=ALPR1
      CMPA(IC)=CMP
      CMP1A(IC)=CMP1
 1090 CONTINUE
 1055 IF(PRTIN.EQ.0) GO TO 1060
      ISAVE=ISAVE+1
      IF(ISIM.EQ.3) ISAVE=ISAVE+1
      IF(PRTIN.EQ.2)GO TO 1058
      IF(PRTIN .EQ. 3) GO TO 1059                                        1083KF
      IPRT=IPRT+1
      JPR=1
      IF(NDELS.GT.IDT) IDT=NDELS
      IF(ISIM.EQ.3) IPRT=IPRT+1
      GO TO 1060
 1058 JPL=1
      GO TO 1060                                                         1083KF
 1059 CONTINUE                                                           1083KF
      JPG = JPG + 1                                                      1083KF
 1060 IF(PRTOUT.EQ.0) GO TO 1080
      ISAVE=ISAVE+1
      IF(ISIM.EQ.3) ISAVE=ISAVE+1
      IF(PRTOUT.EQ.2) GO TO 1070
      IF(PRTOUT .EQ. 3) GO TO 1075                                       1083KF
      IPRT=IPRT+1
      JPR=1
      IF(ODELS.GT.IDT) IDT=ODELS
      IF(ISIM.EQ.3) IPRT=IPRT+1
      GO TO 1080
 1070 JPL=1
      GO TO 1080                                                         1083KF
 1075 CONTINUE                                                           1083KF
      JPG = JPG + 1                                                      1083KF
 1080 CONTINUE
      DO 1085 IOF=1,NOFSEG
 1085 OFLT(IOF)=OFAR(NCRSEG,IOF)/((NXSA(IOF)-1)*DXA(IOF))
      DTMN=1440./IDT
C     ZERO OUT RECORDS ON SCRATCH FILE 17                               II070783
      NUMDA = NOFSEG + NCRSEG                                           AL081683
      IF (ISIM.EQ.3) NUMDA = NUMDA*2                                    II070783
      DO 1130 I=1,1440                                                  II070783
        UINFX(I) = 0.0                                                  IILS0586
 1130 CONTINUE                                                          II070783
      DO 1150 J = 1,NUMDA                                               II070783
        WRITE (17,REC=J) (UINFX(I),I=1,1440)                            IILS0586
 1150 CONTINUE                                                          II070783
 1100 CONTINUE
      RETURN
      END
C
C
C
      SUBROUTINE   WYDYS
     O                   ( IWY,
     I                     II, JJ, MTH, KK,
     O                     JWYD, RODYS )
C
C     + + + PURPOSE + + +
C          ** WYDYS COMPUTATION **
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IWY, II, JJ, MTH, KK, JWYD
      INTEGER   RODYS                                                   1189 KF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IWY    - ?????
C     II     - ?????
C     JJ     - ?????
C     MTH    - ?????
C     KK     - ?????
C     JWYD   - ?????
C     RODYS  - ?????
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   IWY4, LEAP
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + END SPECIFICATIONS + + +
  200 IWY=II
      IF(JJ.GT.9) IWY=II+1
      LEAP=0
      IWY4=IWY
      IF(MOD(IWY4,4).EQ.0) LEAP=1
      IF(IWY.EQ.1900) LEAP=0
      JWYD=MTH+KK
      IF(JJ.GT.2.AND.JJ.LT.10) JWYD=JWYD+LEAP
      RODYS=365+LEAP
      RETURN
      END
C
C
C
      SUBROUTINE   AM
     I               ( ITYP, AN, SLOP, PAR1, PAR2,
     O                 ALPHA, RM )
C
C     + + + PURPOSE + + +
C     Computes alpha and m
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   ITYP
      REAL      AN, SLOP, PAR1, PAR2, ALPHA, RM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ITYP   - ?????
C     AN     - ?????
C     SLOP   - ?????
C     PAR1   - ?????
C     PAR2   - ?????
C     ALPHA  - ?????
C     RM     - ?????
C
C     + + + LOCAL VARIABLES + + +
      REAL   X1, X2, X3
C
C     + + + INTRINSICS + + +
      INTRINSIC   SQRT
C
C     + + + END SPECIFICATIONS + + +
C
      GO TO (10,20,30,40,50,60,100,100,100), ITYP                       1084KF
   10 ALPHA=(1.49*SQRT(SLOP))/(AN*PAR1**0.6667)
      RM=1.67
      GO TO 100
   20 GO TO 100
   30 X1=PAR1+PAR2
      X2=PAR1*PAR1+1.
      X3=PAR2*PAR2+1.
      ALPHA=(1.18*SQRT(SLOP)/AN)*(SQRT(X1)/(SQRT(X2)+SQRT(X3)))**.666667
      RM=1.33
      GO TO 100
   40 ALPHA=PAR1
      RM=PAR2
      GO TO 100
   50 ALPHA=1.49*SQRT(SLOP)/AN
      RM=1.67
      GO TO 100                                                         LS102783
   60 ALPHA=(64.4*SLOP)/(.0000141*AN*PAR1*PAR1)                         LS102783
      RM=3.0                                                            LS102783
  100 RETURN
      END
