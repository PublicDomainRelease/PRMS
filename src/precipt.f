C
C
C
      SUBROUTINE   PRECIP
C
C     + + + PURPOSE + + +
C     Compute precipitation form and depth
C
C     + + + COMMONS + + +
      INCLUDE 'call.inc'
      INCLUDE 'cdpr.inc'
      INCLUDE 'csm.inc'
      INCLUDE 'csno.inc'
      INCLUDE 'csnop.inc'
      INCLUDE 'cdvdt.inc'
      INCLUDE 'csize.inc'
      INCLUDE 'cgeo1.inc'
C
C     + + + LOCAL VARIALBLES + + +
      INTEGER   J, IRU, IMIX, INTCP
      REAL      FIV9, PDV, PCOR, TM, TN, TV, B, A, SNW, RN, PRN,
     #          TSN, TRN, CALPS, CALN, PNDZ, COMP, CALPR, COV,
     #          STOR, D, PTF
C
C     + + + EXTERNALS + + +
      EXTERNAL   SRFRO, CALOSS, CALIN
C
C     + + + DATA INITIALIZATIONS + + +
      DATA FIV9/.5556/
C
C     + + + END SPECIFICATIONS + + +
C--
C---  INITIALIZE VARIABLES
      IF(IPPT.EQ.0) GO TO 22                                            GL1085
      IPPT=0
      ISNO=0
      DO 10 J=1,NRU
        PWEQV(J)=0.
        PACT(J)=0.
        PICE(J)=0.
        PKDEF(J)=0.
        FREWT(J)=0.
        TCAL(J)=0.
        XIN(J)=0.
        INSW(J)=0
   10 CONTINUE
C--
C---  COMPUTE PRECIP TYPE AND DEPTH ON EACH HRU
C--
   22 DO 1000 IRU=1,NRU
      IDS=KDS(IRU)
      PDV=DVP(JWDY,IDS)
      IF(PDV.LE.0.) GO TO 1000
      PRMX(IRU)=0.
      IMIX=0
      IF(IDUS(3).NE.0) GO TO 24
      PCOR=DRCOR(IRU)
      IF(MPC1.EQ.1) PCOR=PCR(IRU)                                       GL1085
      GO TO 120
   24 TM=TMXF(IRU)
      TN=TMNF(IRU)
C---  CHECK FOR START OF THUNDERSTORM PERIOD
   30 TV=TAVC(IRU)
      IF(IPSR(JLDY).NE.1) GO TO 40
      TV=0.
      PCOR=DSCOR(IRU)
      IF(MPC1.EQ.1) PCOR=PCS(IRU)                                       GL1085
      GO TO 100
   40 IF(IPSR(JLDY).NE.2) GO TO 50
      PCOR=DRCOR(IRU)
      IF(MPC1.EQ.1) PCOR=PCR(IRU)                                       GL1085
      GO TO 120
   50 IF(TN.LT.BST) GO TO 70
   60 PCOR=DRCOR(IRU)
      IF(MPC1.EQ.1) PCOR=PCR(IRU)                                       GL1085
      GO TO 120
   70 PCOR=DSCOR(IRU)
      IF(MPC1.EQ.1) PCOR=PCS(IRU)                                       GL1085
      IF(TM.LE.BST) GO TO 100
      IF(TM.GT.PAT(MO)) GO TO 60
C--
C---  RAIN-SNOW MIXTURE PRECIP EVENT
C--
   95 PPT=PDV*PCOR
      PPTI(IRU)=PPT
C---  COMPUTE PROPORTION OF RAIN AND SNOW
      B=TM-BST
      A=TM-TN
      PRMX(IRU)=(B/A)*AJMX(MO)
      IF(PRMX(IRU).GE.1.) GO TO 60
      SNW=PPT*(1.-PRMX(IRU))
      RN=PPT-SNW
C---  COMPUTE INTERCEPTION AND NET PRECIP
      PPT=RN
      ASSIGN 92 TO INTCP
      GO TO 450
   92 PRN=PTN(IRU)
      PPT=SNW
      ASSIGN 93 TO INTCP
      GO TO 400
   93 PSN(IRU)=PTN(IRU)
      PTN(IRU)=PRN+PSN(IRU)
      PPT=RN+SNW
C---  COMPUTE TEMPERATURE OF RAIN AND SNOW
      TSN=(((TN+BST)*.5)-32.)*FIV9
      IF(TSN.GT.0.) TSN=0.
      TRN=(((TM+BST)*.5)-32.)*FIV9
      IF(TRN.LT.0.) TRN=0.
      IF(PWEQV(IRU).GT.0.) GO TO 91
      CALL SRFRO(IRU,1)
      GO TO 105
C---  DISTRIBUTE RAIN AND SNOW
   91 IMIX=1
      GO TO 130
C--
C---  ALL SNOW PRECIP EVENT
C--
  100 PPT=PDV*PCOR
      PPTI(IRU)=PPT
      ASSIGN 104 TO INTCP
      GO TO 400
  104 PSN(IRU)=PTN(IRU)
  105 PWEQV(IRU)=PWEQV(IRU)+PSN(IRU)
      IF(PWEQV(IRU).GT.0.) GO TO 2020
 2020 PICE(IRU)=PICE(IRU)+PSN(IRU)
      IF(TV.GE.0.) GO TO 108
      CALPS=TV*PSN(IRU)*1.27
      IF(FREWT(IRU).LE.0.) GO TO 106
      CALL CALOSS(CALPS,IRU)
      GO TO 110
  106 PKDEF(IRU)=PKDEF(IRU)-CALPS
  108 PACT(IRU)=-PKDEF(IRU)/(PWEQV(IRU)*1.27)
  110 NSW(IRU)=1
      ISNO=ISNO+1
      GO TO 1000
C--
C---  ALL RAIN PRECIP EVENT
C--
  120 PPT=PDV*PCOR
      PPTI(IRU)=PPT
      ASSIGN 122 TO INTCP
      GO TO 450
  122 PRN=PTN(IRU)
      IF(PWEQV(IRU).GT.0.) GO TO 125
      CALL SRFRO(IRU,1)
      GO TO 1000
  125 TRN=TAVC(IRU)
      IF(TRN.LE.0.) TRN=(((TM+BST)*.5)-32.)*FIV9
      IF(TRN.LT.0.) TRN=0.
C---  COMPUTE AFFECT OF RAIN ON PACK CONDITIONS
  130 PWEQV(IRU)=PWEQV(IRU)+PRN
      IF(PKDEF(IRU).LE.0.) GO TO 840
      CALN=(80.+TRN)*2.54
      PNDZ=PKDEF(IRU)/CALN
      COMP=PRN-PNDZ
      IF(COMP) 810,800,830
C---  EXACTLY ENOUGH RAIN TO BRING PACK ISOTHERMAL
  800 PKDEF(IRU)=0.
      PACT(IRU)=0.
      GO TO 820
C---  RAIN NOT SUFFICIENT TO BRING PACK ISOTHERMAL
  810 PKDEF(IRU)=PKDEF(IRU)-(CALN*PRN)
      PACT(IRU)=-PKDEF(IRU)/(PWEQV(IRU)*1.27)
  820 PICE(IRU)=PICE(IRU)+PRN
      GO TO 850
C---  RAIN IN EXCESS OF AMOUNT REQ TO BRING ISOTHERMAL
  830 PKDEF(IRU)=0.
      PACT(IRU)=0.
      PICE(IRU)=PICE(IRU)+PNDZ
      FREWT(IRU)=COMP
      CALPR=TRN*COMP*2.54
      CALL CALIN(CALPR,IRU)
      GO TO 850
C---  SNOWPACK ISOTHERMAL WHEN RAIN OCCURRED
  840 FREWT(IRU)=FREWT(IRU)+PRN
      CALPR=TRN*PRN*2.54
      CALL CALIN(CALPR,IRU)
  850 IF(IMIX.EQ.0) GO TO 1000
      IMIX=0
      TV=TSN
      GO TO 105
C--
C---  COMPUTE DEPTH OF INTERCEPTION
C--
  400 IF(ICOV(IRU).LE.1) GO TO 550                                      GL0985
      COV=COVDNW(IRU)
      IF(ITSW(IRU).EQ.1) COV=COVDNS(IRU)
      STOR=SNST(IRU)
      INSW(IRU)=1
      GO TO 500
  450 IF(ICOV(IRU).GT.1) GO TO 460                                      GL0985
      IF(PWEQV(IRU).GT.0.) GO TO 550                                    GL0985
      IF(ICOV(IRU).EQ.0) GO TO 550                                      GL0985
  460 COV=COVDNS(IRU)                                                   GL0985
      STOR=RNSTS(IRU)
      INSW(IRU)=0
      IF(ITSW(IRU).EQ.1) GO TO 500
      COV=COVDNW(IRU)
      STOR=RNSTW(IRU)
  500 INTSW=1
      D=STOR-XIN(IRU)
      IF(PPT.GE.D) GO TO 510
      XIN(IRU)=XIN(IRU)+PPT
      PTF=0.
      GO TO 520
  510 XIN(IRU)=STOR
      PTF=PPT-D
  520 PTN(IRU)=(PPT*(1.-COV))+(PTF*COV)
      GO TO 580                                                         GL0985
  550 XIN(IRU)=0.                                                       GL0985
      PTN(IRU)=PPT                                                      GLO985
  580 GO TO INTCP, (92,93,104,122)                                      GL0985
 1000 CONTINUE
      RETURN
      END
C
C
C
      SUBROUTINE   INTLOS
C
C     + + + PURPOSE + + +
C     Calculate interception losses
C
C     + + + COMMONS + + +
      INCLUDE 'call.inc'
      INCLUDE 'cdpr.inc'
      INCLUDE 'csno.inc'
      INCLUDE 'cet.inc'
      INCLUDE 'cupr.inc'
      INCLUDE 'csnop.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   IRU
      REAL      EVCAN, COV, TA, SW, T, TC, AIR, CAN, BAL, XMLT, Z,
     #          XLOS, D, SNW, CALPS
C
C     + + + EXTERNALS + + +
      EXTERNAL   CALOSS
C
C     + + + END SPECIFICATIONS + + +
C
C               THIS IS FOR EVAP AND SUBLIMATION OF INTERCEPTED SNOW
C                 EXCESS IS ADDED TO NET PRECIP OR SNOWPACK
      INTSW=0
      DO 100 IRU=1,NRU
      IF(XIN(IRU).LE.0.)GO TO 100
      EVCAN=EPAN
      IF(EPAN.LE.0.) EVCAN=PET(IRU)/EVC(MO)
      IF(INSW(IRU).EQ.0) GO TO 50
C               COMPUTE SNOW INTERCEPTION LOSS
      IF(PP.GT.0.) GO TO 95
      COV=COVDNW(IRU)
      IF(ITSW(IRU).EQ.1) COV=COVDNS(IRU)
      TA=(TAVF(IRU)+TMXF(IRU))*.5
      IF(TA.LT.32.) GO TO 95
      SW=SWRD(IRU)*.19
      T=0.
      TC=.585E-7*((TA+273.16)**4)
      AIR=TC*(1.-COV)*.85
      CAN=COV*TC
   30 BAL=SW+AIR+CAN
      IF(BAL.LE.0.) GO TO 100
      XMLT=BAL/203.2
      Z=XIN(IRU)-XMLT
      IF(Z.LE.0.) GO TO 31
      INTSW=1
      XIN(IRU)=Z
      XLOS=XMLT
      GO TO 32
   31 XLOS=XIN(IRU)
      XIN(IRU)=0.
   32 D=EVCAN-XLOS
      IF(D.GE.0.) GO TO 33
      XINLOS(IRU)=XINLOS(IRU)+EVCAN
      SNW=-D*COV*PERV(IRU)
      T=0.
      GO TO 34
   33 XINLOS(IRU)=XINLOS(IRU)+XLOS
      GO TO 100
C                 ADD SNOW TO SNOWPACK
   34 PWEQV(IRU)=PWEQV(IRU)+SNW
      PICE(IRU)=PICE(IRU)+SNW
      ISNO=ISNO+1
      IF(T.GE.0) GO TO 35
      CALPS=T*SNW*1.27
      IF(FREWT(IRU).LE.0) GO TO 300
      CALL CALOSS(CALPS,IRU)
      GO TO 310
  300 PKDEF(IRU)=PKDEF(IRU)-CALPS
   35 IF(PWEQV(IRU).LE.0.) GO TO 310
      PACT(IRU)=-PKDEF(IRU)/(PWEQV(IRU)*1.27)
  310 PTN(IRU)=SNW
      PSN(IRU)=SNW
      GO TO 100
C               COMPUTE INTERCEPTION LOSS FOR RAIN
   50 COV=COVDNS(IRU)
      IF(ITSW(IRU).EQ.1) GO TO 52
      COV=COVDNW(IRU)
   52 D=EVCAN-XIN(IRU)
      IF(D.GE.0.) GO TO 54
      XIN(IRU)=-D
      XINLOS(IRU)=XINLOS(IRU)+EVCAN
      GO TO 95
   54 XINLOS(IRU)=XINLOS(IRU)+XIN(IRU)
      XIN(IRU)=0.
      GO TO 100
   95 INTSW=1
  100 CONTINUE
      RETURN
      END
C
C
C
      SUBROUTINE   SNOBAL
C
C     + + + PURPOSE + + +
C     ?????
C
C     + + + COMMONS + + +
      INCLUDE 'call.inc'
      INCLUDE 'cdpr.inc'
      INCLUDE 'csno.inc'
      INCLUDE 'csnop.inc'
      INCLUDE 'csize.inc'
      INCLUDE 'copsno.inc'
      INCLUDE 'cet.inc'
      INCLUDE 'copsn2.inc'
      INCLUDE 'csm.inc'
      INCLUDE 'csncv.inc'
      INCLUDE 'cgeo1.inc'
C
C     + + + SAVES + + +
      REAL      DENINV, SETDEN, SET1
      SAVE      DENINV, SETDEN, SET1
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   INT(50), IRU, IG, MG, L, IN
      REAL      ACUM(15), AMLT(15), FIV9, TRD, TMNC, TMXC, TN, TX, EMIS,
     #          ESV, SWN, CEC, DPT1, EFFK, CST, AIRN, TS, SNON, SKYN,
     #          CANN, CECT, CALN, QCONDN, PKD, PKT, PKS, CALD, AIRD,
     #          SNOD, SKYD, CAND, CECD, QCONDD, EZ, COV, Z, CAL
C
C     + + + INTRINSICS + + +
      INTRINSIC   SQRT
C
C     + + + EXTERNALS + + +
      EXTERNAL   CALIN, CALOSS
C
C     + + + DATA INITIALIZATIONS + + +
      DATA ACUM/.80,.77,.75,.72,.70,.69,.68,.67,.66,.65,.64,.63,
     +.62,.61,.60/
      DATA AMLT/.72,.65,.60,.58,.56,.54,.52,.50,.48,.46,.44,.43,
     +.42,.41,.40/
      DATA INT/50*1/
      DATA FIV9/.5556/
C
C     + + + OUTPUT FORMATS + + +
C1017 FORMAT(' QD-QN-PKDEF-PWEQV-PICE-FREWT-TN-TX ',I3,8F11.2)
C2000 FORMAT(1X,2I2,2I4,2F5.1,F5.2,2F6.2,F7.3,3F6.2,F9.2,F6.2,
C    12F6.1,F9.2,F6.2,3F6.1,I4)
c3501 FORMAT('IRU,CALN,CALD',I5,2F8.3)
C
C     + + + END SPECIFICATIONS + + +
C
C            INITIALIZE SWITCHES AT START OF EACH WATER YEAR
      IF(ISNOSW.EQ.0)GO TO 5
      ISNOSW=0
      DENINV=1./DENI
      SETDEN=SETCON/DENMX
      SET1=1./(1.+SETCON)
      DO 4 IRU=1,NRU
      DPT(IRU)=0.
    4 PSS(IRU)=0.
    5 TRD=ORAD/HORAD
      ISNO=0
      DO 1100 IRU=1,NRU
      IF(PWEQV(IRU).LE.0.) GO TO 995
      ISNO=ISNO+1
C--
C---  COMPUTE ALBEDO
C--
C---  CHECK FOR NEW SNOWFALL
      IF(NSW(IRU).EQ.1) GO TO 10
C---  CHECK IF ALBEDO ADJUSTMENT NEEDED
      IF(LST(IRU).EQ.0) GO TO 50
C---  ADJUST ALBEDO FOR SHALLOW SNOWFALL
      SLST(IRU)=SALB(IRU)-3.
      IF(SLST(IRU).LT.1.) SLST(IRU)=1.
      IG=ISO(IRU)                                                       LS0287
      IF(IG.EQ.2) GO TO 6                                               GL1085
      IF(SLST(IRU).GT.5.) SLST(IRU)=5.
    6 LST(IRU)=0                                                        GL1085
      SNSV(IRU)=0.
      GO TO 50
   10 NSW(IRU)=0
      MG=MSO(IRU)
      IF(MG.NE.2) GO TO 20
C---  NEW SNOW IN MELT STAGE
C---  CHECK FOR PROPORTION RAIN AND TOTAL SNOW DEPTH
      IF(PRMX(IRU).GE.RMXM) GO TO 50
      IF(PSN(IRU).GT.ARSM) GO TO 30                                     GL1085
      SNSV(IRU)=SNSV(IRU)+PSN(IRU)
      IF(SNSV(IRU).GT.ARSM) GO TO 30                                    GL1085
      IF(LST(IRU).EQ.0) SALB(IRU)=SLST(IRU)
      SLST(IRU)=0.
      LST(IRU)=1
      GO TO 50
C---  NEW SNOW IN ACCUM STAGE
C---  CHECK FOR PROPORTION RAIN AND TOTAL SNOW DEPTH
   20 IF(PRMX(IRU).LE.0.) GO TO 30
      IF(PRMX(IRU).GE.RMXA) GO TO 40
      IF(PSN(IRU).GE.ARSA) GO TO 30                                     GL1085
      SLST(IRU)=SLST(IRU)-3.
      IF(SLST(IRU).LT.0.) SLST(IRU)=0.
      IF(SLST(IRU).GT.5.) SLST(IRU)=5.
      GO TO 40
   30 SLST(IRU)=0.
   40 LST(IRU)=0
      SNSV(IRU)=0.
C---  SET DAYS SINCE LAST SNOWFALL COUNTER
   50 L=SLST(IRU)+.5
      SLST(IRU)=SLST(IRU)+1.
      IF(L) 120,120,60
   60 IN=INT(IRU)
      IF(IN.EQ.2) GO TO 90
C---  COMPUTE ALBEDO FOR WINTER ACCUM PERIOD
      IF(L-15) 70,70,80
   70 ALB(IRU)=ACUM(L)
      GO TO 150
   80 L=L-12
C---  COMPUTE ALBEDO FOR SPRING MELT PERIOD
   90 IF(L-15) 110,110,100
  100 L=15
  110 ALB(IRU)=AMLT(L)
      GO TO 150
C---  NEW SNOW CONDITION, SET ALBEDO TO MAX
  120 MG=MSO(IRU)
      IF(MG.EQ.2) GO TO 130
      ALB(IRU)=.91
      INT(IRU)=1
      GO TO 150
  130 ALB(IRU)=.81
      INT(IRU)=2
C---  COMPUTE 12 HR AVERAGE TEMPERATURES
  150 TMNC=(TMNF(IRU)-32.)*FIV9
      TMXC=(TMXF(IRU)-32.)*FIV9
      TN=(TMNC+TAVC(IRU))*.5
      TX=(TMXC+TAVC(IRU))*.5
      EMIS=EAIR
      IF(PP.GT.0.) EMIS=1.
      ESV=EMIS
      SWN=SWRD(IRU)*(1.-ALB(IRU))*TRNCF(IRU)
      CEC=CECN(MO)*.5                                                   LS110483
      IF(ICOV(IRU).EQ.3) CEC=CEC*.5                                     LS110483
C               COMPUTE DENSITY AND CST
      PSS(IRU)=PSS(IRU)+PSN(IRU)
      DPT1=((PSN(IRU)*DENINV)+(SETDEN*PSS(IRU))+DPT(IRU))*SET1
      DPT(IRU)=DPT1
      DEN(IRU)=PWEQV(IRU)/DPT1
      EFFK=.0154*DEN(IRU)
      CST=DEN(IRU)*(SQRT(EFFK*13751.))
C---  CHECK TO SEE IF SHOULD FORCE SPRING MELT STAGE
      IG=ISO(IRU)
      MG=MSO(IRU)
      IF(IG.EQ.2) GO TO 200
      IF(MG.EQ.1) GO TO 200
      IF(PACT(IRU).LT.0.) GO TO 190
      LSO(IRU)=LSO(IRU)+1
      IF(LSO(IRU).LE.4) GO TO 200
      ISO(IRU)=2
  190 LSO(IRU)=0
C               COMPUTE NIGHT PERIOD
C
  200 AIRN=.585E-7*((TN+273.16)**4)
      TS=0.
      IF(TN.LT.0.) GO TO 210
      SNON=325.7
      GO TO 220
  210 TS=TN
      SNON=AIRN
  220 IF(PP.LE.0.) GO TO 225
      IF(MO.LT.MTSS.OR.MO.GT.MTSE) GO TO 225
      EMIS=.85                                                          LS110483
      IF(IDUS(5).EQ.0) GO TO 225
      IF(TRD.GT..33) EMIS=EAIR
  225 SKYN=(1.-COVDNW(IRU))*((EMIS*AIRN)-SNON)
      CANN=COVDNW(IRU)*(AIRN-SNON)
      CECT=0.                                                           LS110483
      IF(TN.LE.0.) GO TO 226                                            LS110483
      IF(PP.GT.0.) CECT=CEC*TN                                          LS110483
  226 CALN=SKYN+CANN+CECT                                               LS110483
      IF(TS.LT.0.) GO TO 228
      IF(CALN.LE.0.) GO TO 228
      CALL CALIN(CALN,IRU)
      GO TO 280
  228 QCONDN=CST*(TS-PACT(IRU))
      IF(QCONDN) 230,240,250
  230 IF(PACT(IRU).LT.0.) GO TO 236
      CALL CALOSS(QCONDN,IRU)
      GO TO 280
  236 PKDEF(IRU)=PKDEF(IRU)-QCONDN
      PACT(IRU)=-PKDEF(IRU)/(PWEQV(IRU)*1.27)
      GO TO 280
  240 IF(PACT(IRU).LT.0.) GO TO 280
      IF(CALN.GT.0.) CALL CALIN(CALN,IRU)
      GO TO 280
  250 IF(TS.LT.0.) GO TO 256
      PKD=PKDEF(IRU)-QCONDN
      IF(PKD.LT.0.) GO TO 254
  252 PKDEF(IRU)=PKD
      PACT(IRU)=-PKD/(PWEQV(IRU)*1.27)
      GO TO 280
  254 PKDEF(IRU)=0.
      PACT(IRU)=0.
      GO TO 280
  256 PKT=-TS*PWEQV(IRU)*1.27
      PKS=PKDEF(IRU)-PKT
      PKD=PKS-QCONDN
      IF(PKD.LT.0.) GO TO 258
      PKDEF(IRU)=PKD+PKT
      PACT(IRU)=-PKDEF(IRU)/(PWEQV(IRU)*1.27)
      GO TO 280
  258 PKDEF(IRU)=PKT
      PACT(IRU)=TS
C               COMPUTE DAY PERIOD
C
  280 CALD=0.                                                           GL0985
      IF(PWEQV(IRU).LE.0.) GO TO 350                                    GL0985
      AIRD=.585E-7*((TX+273.16)**4)
      EMIS=ESV
      TS=0.
      IF(TX.LT.0.) GO TO 290
      SNOD=325.7
      GO TO 300
  290 TS=TX
      SNOD=AIRD
  300 IF(PP.LE.0.) GO TO 308
      IF(MO.LT.MTSS.OR.MO.GT.MTSE) GO TO 308
      IF(IDUS(5).EQ.0) GO TO 308
      IF(TRD.LE..33) GO TO 308
      IF(TRD.GE..5) GO TO 306
      EMIS=1.29-(.882*TRD)
      GO TO 308
  306 EMIS=.95-(.2*TRD)
  308 SKYD=(1.-COVDNW(IRU))*((EMIS*AIRD)-SNOD)
      CAND=COVDNW(IRU)*(AIRD-SNOD)
      CECD=0.                                                           LS110483
      IF(TX.LE.0.) GO TO 307                                            LS110483
      IF(PP.GT.0.) CECD=CEC*TX                                          LS110483
  307 CALD=SWN+SKYD+CAND+CECD                                           LS110483
      IF(TS.LT.0.) GO TO 309
      IF(CALD.LE.0.) GO TO 309
      CALL CALIN(CALD,IRU)
      GO TO 350
  309 QCONDD=CST*(TS-PACT(IRU))
      IF(QCONDD) 310,320,330
  310 IF(PACT(IRU).LT.0.) GO TO 316
      CALL CALOSS(QCONDD,IRU)
      GO TO 350
  316 PKDEF(IRU)=PKDEF(IRU)-QCONDD
      PACT(IRU)=-PKDEF(IRU)/(PWEQV(IRU)*1.27)
      GO TO 350
  320 IF(PACT(IRU).LT.0.) GO TO 350
      IF(CALD.GT.0.) CALL CALIN(CALD,IRU)
      GO TO 350
  330 IF(TS.LT.0.) GO TO 336
      PKD=PKDEF(IRU)-QCONDD
      IF(PKD.LT.0.) GO TO 334
  332 PKDEF(IRU)=PKD
      PACT(IRU)=-PKD/(PWEQV(IRU)*1.27)
      GO TO 350
  334 PKDEF(IRU)=0.
      PACT(IRU)=0.
      GO TO 350
  336 PKT=-TS*PWEQV(IRU)*1.27
      PKS=PKDEF(IRU)-PKT
      PKD=PKS-QCONDD
      IF(PKD.LT.0.) GO TO 338
      PKDEF(IRU)=PKD+PKT
      PACT(IRU)=-PKDEF(IRU)/(PWEQV(IRU)*1.27)
      GO TO 350
  338 PKDEF(IRU)=PKT
      PACT(IRU)=TS
  350 TCAL(IRU)=CALN+CALD
c     WRITE(43,3501)IRU,CALN,CALD
  420 IF(ITSW(IRU).EQ.0) GO TO 800
      IF(ICOV(IRU).GT.1) GO TO 990
C--
C---  DAYS ET REMOVED FROM SNOWPACK
C--
  800 IF(PWEQV(IRU).GT.0.) GO TO 805                                    GL0985
      SNEV(IRU)=0.                                                      GL0985
      GO TO 995                                                         GL0985
  805 IF(ICOV(IRU).GT.1) GO TO 810                                      GL0985
      EZ=CTW*PET(IRU)*ASC(IRU)                                          GL0386
      GO TO 820
  810 COV=COVDNW(IRU)
      IF(ITSW(IRU).EQ.1) COV=COVDNS(IRU)
      EZ=(CTW*PET(IRU)*ASC(IRU))-(XINLOS(IRU)*COV)                      GL081585
  820 IF(EZ.GT.0.) GO TO 900
      SNEV(IRU)=0.
      GO TO 990
  900 Z=PWEQV(IRU)-EZ
      IF(Z.GT.0.) GO TO 910
C---  DEPLETES SNOWPACK ENTIRELY
      SNEV(IRU)=PWEQV(IRU)
      PWEQV(IRU)=0.
      PICE(IRU)=0.
      PKDEF(IRU)=0.
      FREWT(IRU)=0.
      GO TO 995
C---  PARTIALLY DEPLETES SNOWPACK
  910 PICE(IRU)=PICE(IRU)-EZ
      CAL=PACT(IRU)*EZ*1.27
      PKDEF(IRU)=PKDEF(IRU)+CAL
      PWEQV(IRU)=Z
      SNEV(IRU)=EZ
  990 IF(PWEQV(IRU).LE.0.) GO TO 995
      DPT(IRU)=PWEQV(IRU)/DEN(IRU)
      PSS(IRU)=PWEQV(IRU)
      IF(LST(IRU).EQ.0) GO TO 1000                                      GL1085
      SNSV(IRU)=SNSV(IRU)-SMLT(IRU)                                     GL1085
      IF(SNSV(IRU).LT.0.) SNSV(IRU)=0.                                  GL1085
      GO TO 1000
  995 DPT(IRU)=0.
      PSS(IRU)=0.
      SNSV(IRU)=0.                                                      GL1085
      LST(IRU)=0                                                        GL1085
      PST(IRU)=0.                                                       GL081585
      IASW(IRU)=0                                                       GL081585
      ASC(IRU)=0.                                                       GL0985
 1000 CONTINUE
C1000 WRITE(43,1017)IRU,QCONDD,QCONDN,PKDEF(IRU),PWEQV(IRU),PICE(IRU),
C    1FREWT(IRU),TN,TX
C     GO TO 1100
C     WRITE(43,2000) MO,MDY,MYR,IRU,TMXC,TMNC,PTN(IRU),PWEQV(IRU),
C    1FREWT(IRU),PACT(IRU),DEN(IRU),DEP(IRU),DPT(IRU),TN,QCONDN,
C    2CALN,AIRN,TX,QCONDD,CALD,AIRD,SWN,ISO(IRU)
 1100 CONTINUE
      RETURN
      END
C
C
C
      SUBROUTINE   CALIN
     I                  (CAL,IRU)
C
C     + + + PURPOSE + + +
C     ?????
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IRU
      REAL      CAL
C
C     + + + ARGUMENT DEFINTIONS + + +
C     CAL    - ?????
C     IRU    - ?????
C
C     + + + COMMONS + + +
      INCLUDE 'csno.inc'
      INCLUDE 'copsno.inc'
      INCLUDE 'csncv.inc'
      INCLUDE 'cjm.inc'
C
C     + + + LOCAL VARIALBLES + + +
      REAL      COMP, AS, PMLT, APMLT, APICE, PWCAP
C
C     + + + OUTPUT FORMATS + + +
C9000 FORMAT(3X,'IRU-PW-PI-FW-AS-PML-SML',I5,6F5.2)                     KD1291
C
C     + + + END SPECIFICATIONS + + +
C
C               SEE IF CALORIE DEFICIT EXISTS IN PACK (CAL IS POSITIVE)
      COMP=CAL-PKDEF(IRU)
      AS=ASC(IRU)                                                       GL081585
      IF(NDC.EQ.0) AS=1.                                                GL0386
      IF(COMP) 10,20,30
C               CALORIE DEFICIT EXISTS, RECOMPUTE PACT
   10 PKDEF(IRU)=PKDEF(IRU)-CAL
      PACT(IRU)=-PKDEF(IRU)/(PWEQV(IRU)*1.27)
      RETURN
C               NO CALORIE DEFICIT AND NO CALORIE EXCESS
   20 PACT(IRU)=0.
      PKDEF(IRU)=0.
      RETURN
C               CALORIE EXCESS, MELT SOME SNOW
   30 PMLT=COMP/203.2
      APMLT=PMLT*AS                                                     GL081585
      PKDEF(IRU)=0.
      PACT(IRU)=0.
      APICE=PICE(IRU)/AS                                                GL081585
      IF(PMLT.LT.APICE) GO TO 40                                        GL081585
C               CALORIES SUFFICIENT TO MELT ENTIRE PACK
      SMLT(IRU)=SMLT(IRU)+PWEQV(IRU)
      PWEQV(IRU)=0.
      PICE(IRU)=0.
      FREWT(IRU)=0.
      DPT(IRU)=0.
      PSS(IRU)=0.
      PST(IRU)=0.                                                       GL081585
      IASW(IRU)=0                                                       GL081585
      ASC(IRU)=0.                                                       GL0985
      RETURN
C               CALORIES SUFFICIENT TO MELT PART OF PACK
   40 PICE(IRU)=PICE(IRU)-APMLT                                         GL081585
      FREWT(IRU)=FREWT(IRU)+APMLT                                       GL081585
      PWCAP=FWCAP*PICE(IRU)
      COMP=FREWT(IRU)-PWCAP
      IF(COMP.LE.0.) RETURN
      SMLT(IRU)=SMLT(IRU)+COMP
      FREWT(IRU)=PWCAP
      PWEQV(IRU)=PWEQV(IRU)-COMP
      DPT(IRU)=PWEQV(IRU)/DEN(IRU)
      PSS(IRU)=PWEQV(IRU)
C     WRITE(72,9000) IRU,PWEQV(IRU),PICE(IRU),FREWT(IRU),AS,APMLT,
C    1SMLT(IRU)
      RETURN
      END
C
C
C
      SUBROUTINE   CALOSS
     I                   (CAL,IRU)
C
C     + + + PURPOSE + + +
C     ?????
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IRU
      REAL      CAL
C
C     + + + ARGUMENT DEFINTIONS + + +
C     CAL    - ?????
C     IRU    - ?????
C
C     + + + COMMONS + + +
      INCLUDE 'csno.inc'
      INCLUDE 'cjm.inc'
C
C     + + + LOCAL VARIABLES + + +
      REAL    CALND, COMP, FROZ
C
C     + + + END SPECIFICATIONS + + +
C               IF FREWT EXISTS, FREEZE IT FIRST
      IF(FREWT(IRU).GT.0.) GO TO 20
C               REMEMBER CAL IS NEGATIVE
      PKDEF(IRU)=PKDEF(IRU)-CAL
      GO TO 40
C               COMPUTE CALORIES AVAILABLE IF ALL FREWT FREEZES
   20 CALND=FREWT(IRU)*203.2
      COMP=CAL+CALND
      IF(COMP) 24,26,30
C               ALL FREWT FREEZES
   24 PKDEF(IRU)=-COMP
   26 PICE(IRU)=PICE(IRU)+FREWT(IRU)
      FREWT(IRU)=0.
      GO TO 40
C               ONLY PART OF FREWT FREEZES
   30 FROZ=-CAL/203.2
      PICE(IRU)=PICE(IRU)+FROZ
      FREWT(IRU)=FREWT(IRU)-FROZ
      RETURN
C               RECOMPUTE PACK CONDITIONS
   40 IF(PWEQV(IRU).GT.0.) GO TO 42
      WRITE(*,*) 'IRU =',IRU,'   PWEQV = ',PWEQV(IRU)
      RETURN
   42 PACT(IRU)=-PKDEF(IRU)/(PWEQV(IRU)*1.27)
      RETURN
      END
