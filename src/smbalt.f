C
C
C
      SUBROUTINE   SMBAL
C
C     + + + PURPOSE + + +
C     Compute soil moisture balance
C
C     + + + COMMONS + + +
      INCLUDE 'call.inc'
      INCLUDE 'cbs.inc'
      INCLUDE 'cdpr.inc'
      INCLUDE 'csm.inc'
      INCLUDE 'csno.inc'
      INCLUDE 'csnop.inc'
      INCLUDE 'cvol.inc'
      INCLUDE 'cuvrt.inc'
      INCLUDE 'cimprv.inc'
      INCLUDE 'csncv.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   J, IRU, IET, ISL, K
      REAL      ETT, OG, COV, XXN, YET, ENF, BZ, DIF, SS, PCT1, PCT2,
     #          ET1, ET2
C
C     + + + EXTERNALS + + +
      EXTERNAL   SRFRO
C
C     + + + END SPECIFICATIONS + + +
C
  220 DO 222 J=1,NRES
  222 EXSS(J)=0.
      DO 100 IRU=1,NRU
      ETT=0.
      OG=1.-ASC(IRU)                                                    GL0985
      COV=COVDNW(IRU)
      IF(ITSW(IRU).EQ.1) COV=COVDNS(IRU)
      XXN=XINLOS(IRU)*COV
  200 YET=PET(IRU)-XXN-SNEV(IRU)
      IF(UVWY(JWDY).EQ.0) GO TO 210
      ENF=ENFIL(IRU)-AET(IRU)
      GO TO 10
  210 IF(SMLT(IRU).LE.0.) GO TO 2                                       LS0287
      IF(PTN(IRU).LE.PSN(IRU).OR.PWEQV(IRU).GT.0.) GO TO 12             LS0287
      GO TO 11
   12 CALL SRFRO(IRU,3)
      ENF=ENFIL(IRU)+SMLT(IRU)
      GO TO 3
   11 CALL SRFRO(IRU,2)
    2 ENF=ENFIL(IRU)
    3 IF(YET.GT.0.) GO TO 5
      YET=0.
    4 IET=1
      GO TO 10
    5 IF(ITSW(IRU).EQ.0) GO TO 7
    8 IF(ICOV(IRU).GT.0) GO TO 9
    7 IF(OG.LT.0.01) GO TO 4
      IET=2
      GO TO 10
    9 IET=3
C               COMPUTE SOIL WATER INCREASE FROM INFILTRATION
   10 IF(DARP(IRU).GT.0.) GO TO 13
      RECHR(IRU)=0.
      SMAV(IRU)=0.
      EXCS(IRU)=0.
      GO TO 30                                                          LS110483
   13 ISL=ISOIL(IRU)
      BZ=SMAV(IRU)-RECHR(IRU)
      RECHR(IRU)=RECHR(IRU)+ENF
      IF(RECHR(IRU).LE.REMX(IRU)) GO TO 16
      DIF=RECHR(IRU)-REMX(IRU)
      RECHR(IRU)=REMX(IRU)
      BZ=BZ+DIF
   16 SMAV(IRU)=BZ+RECHR(IRU)
      IF(SMAV(IRU).LE.SMAX(IRU)) GO TO 30
      BZ=SMAX(IRU)-REMX(IRU)
      EXCS(IRU)=SMAV(IRU)-SMAX(IRU)
      SMAV(IRU)=SMAX(IRU)
      IF(ICHG.EQ.0) GO TO 20
C***
C***  CALCULATE SURFACE RUNOFF PERVIOUS AREA (SNOW)
C***
      IF(PWEQV(IRU).LE.0.) GO TO 20
      IF(EXCS(IRU).LE.SRX(IRU)) GO TO 20
      SX(IRU)=EXCS(IRU)-SRX(IRU)
      SAS=SAS+(SX(IRU)*DARP(IRU))
      SRO(IRU)=SRO(IRU)+SX(IRU)
      EXCS(IRU)=SRX(IRU)
C***
C***  ROUTE SOIL WATER EXCESS WITH CONSTANT SEEPAGE TO GW
C***
   20 K=KGW(IRU)
      IF(EXCS(IRU).GT.SEP(IRU)) GO TO 22
      SS=EXCS(IRU)
      EXCS(IRU)=0.
      GO TO 26
   22 EXCS(IRU)=EXCS(IRU)-SEP(IRU)
      SS=SEP(IRU)
   26 GW(K)=GW(K)+(DARP(IRU)*SS)
      UGS(IRU)=SS
      IF(EXCS(IRU).LE.0.) GO TO 30                                      LS0287
C***
C***  ADD EXCESS SOIL WATER TO SUBSURFACE RESERVOIR
C***
   28 K=KRES(IRU)
      EXSS(K)=EXSS(K)+(EXCS(IRU)*DARP(IRU))
      USS(IRU)=EXCS(IRU)
   30 IF(UVWY(JWDY).NE.0) GO TO 100
      IF(DARP(IRU).GT.0.) GO TO 31
      EVIMP(IRU)=SNEV(IRU)
      AET(IRU)=0.
      GO TO 81
   31 IF(IET.EQ.1) GO TO 80
      PCT1=SMAV(IRU)/SMAX(IRU)
      PCT2=RECHR(IRU)/REMX(IRU)
      ET1=YET
      ET2=YET
      GO TO (40,50,60),ISL
C              SANDY SOIL
   40 IF(PCT1.LT..25) ET1=YET*.5*PCT1
      IF(PCT2.LT..25) ET2=YET*.5*PCT2
      GO TO 70
C               LOAM SOIL
   50 IF(PCT1.LT..5) ET1=YET*PCT1
      IF(PCT2.LT..5) ET2=YET*PCT2
      GO TO 70
C               CLAY SOIL
   60 IF(PCT1.GE..67) GO TO 65
      IF(PCT1.LE..33) GO TO 62
      ET1=YET*PCT1
      GO TO 65
   62 ET1=YET*.5*PCT1
   65 IF(PCT2.GE..67) GO TO 70
      IF(PCT2.LE..33) GO TO 68
      ET2=YET*PCT2
      GO TO 70
   68 ET2=YET*.5*PCT2
   70 IF(IET.EQ.3) GO TO 71                                             GL0985
      ET2=ET2*OG                                                        GL0985
   71 IF(ET2.GT.RECHR(IRU)) GO TO 72
      RECHR(IRU)=RECHR(IRU)-ET2
      GO TO 74
   72 ET2=RECHR(IRU)
      RECHR(IRU)=0.
   74 IF(IET.EQ.2) GO TO 76
      IF(ET2.GE.ET1) GO TO 76
      SMAV(IRU)=SMAV(IRU)-ET1
      IF(SMAV(IRU).LE.0.)SMAV(IRU)=0.
      ETT=ET1
      GO TO 80
   76 SMAV(IRU)=SMAV(IRU)-ET2
      IF(SMAV(IRU).LE.0.) SMAV(IRU)=0.
      ETT=ET2
   80 AET(IRU)=SNEV(IRU)+ETT
   81 IF(RSTOR(IRU).LE.0.)GO TO 100
      IF(PWEQV(IRU).GT.0.)GO TO 100
      DIF=RSTOR(IRU)-PET(IRU)+SNEV(IRU)
      IF(DIF.GE.0.) GO TO 90
      EVIMP(IRU)=EVIMP(IRU)+RSTOR(IRU)
      RSTOR(IRU)=0.
      GO TO 100
   90 EVIMP(IRU)=PET(IRU)
      RSTOR(IRU)=DIF
  100 CONTINUE
      RETURN
      END
C
C
C
      SUBROUTINE   BASFLW
C
C     + + + PURPOSE + + +
C     Compute base flow
C
C     + + + COMMONS + + +
      INCLUDE 'cbs.inc'
      INCLUDE 'cswtch.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   J, K
      REAL      GR, GRX, XK1, XK2, XK3, C1, C2, CFR, S0S, RASA,
     #          GF, GAD, BASA, SNK
C
C     + + + INTRINSICS + + +
      INTRINSIC   EXP, SQRT
C
C     + + + END SPECIFICATIONS + + +
C
      IF(ISBAS.EQ.0) GO TO 15
C               INITIALIZE
      ISBAS=0
   15 RAS=0.
      BAS=0.
C               COMPUTE SUBSURFACE FLOW
      DO 10 J=1,NRES
      RASQ(J)=0.
      IF(RES(J).LT..000000001) RES(J)=0.0
      GR=RES(J)/ARS(J)
      GRX=EXSS(J)/ARS(J)
      IF(GR.LE.0..AND.GRX.LE.0.) GO TO 10                               LS0287
      XK1=RCF(J)
      XK2=RCP(J)
      IF(XK1.LE.0..AND.GRX.LE.0.) GO TO 22                              LS0287
      IF(XK2.GT.0.) GO TO 11
      C2=1.-EXP(-XK1)
      CFR=GRX*(1.-C2/XK1)+GR*C2
      GO TO 12
   22 C1=XK2*GR                                                         LS102783
      CFR=GR*(C1/(1.+C1))                                               LS102783
      GO TO 12                                                          LS102783
   11 XK3=SQRT(XK1**2.+4.*XK2*GRX)
      S0S=GR-((XK3-XK1)/(2.*XK2))
      C1=XK2*S0S/XK3
      C2=1.-EXP(-XK3)
      CFR=GRX+(S0S*(1.+C1)*C2)/(1.+C1*C2)
   12 IF(CFR.LT.0.) CFR=0.
      RASA=CFR*ARS(J)
      IF(RASA.GT.(RES(J)+EXSS(J))) RASA=RES(J)+EXSS(J)                  LS102783
      RAS=RAS+RASA
      RES(J)=RES(J)+EXSS(J)-RASA
      IF(RES(J).LE.0.) RES(J)=0.
      RASQ(J)=RASA
   10 CONTINUE
C               COMPUTE SEEPAGE TO GW
      DO 50 J=1,NRES
      K=KRSP(J)
      GF=RES(J)/ARS(J)
      IF(GF.LT.0.) GF=0.
      GAD=RSEP(J)*((GF/RESMX(J))**REXP(J))
      IF(GAD.GT.GF) GAD=GF
      SSGW=SSGW+GAD*ARS(J)
      RES(J)=RES(J)-(GAD*ARS(J))
      IF(RES(J).LE.0.) RES(J)=0.
      GW(K)=GW(K)+(GAD*ARS(J))
   50 CONTINUE
C               COMPUTE GW DISCHARGE
   18 DO 20 J=1,NGW
      BASQ(J)=0.
      IF(GW(J).LE.0.) GO TO 20                                          LS0287
      BASA=RCB(J)*GW(J)
      BAS=BAS+BASA
      GW(J)=GW(J)-BASA
      IF(GW(J).LE.0.) GW(J)=0.
      BASQ(J)=BASA
      IF(GSNK(J).LE.0.) GO TO 20
      SNK=GSNK(J)*GW(J)
      GWSNK(J)=GWSNK(J)+SNK
      GW(J)=GW(J)-SNK
   20 CONTINUE
C               SUM STORAGE IN GW AND SUBSURFACE RESERVOIRS
      SGW=0.
      SRS=0.
      DO 60 J=1,NRES
   60 SRS=SRS+RES(J)
      DO 70 J=1,NGW
   70 SGW=SGW+GW(J)
      RETURN
      END
C
C
C
      SUBROUTINE   SRFRO
     I                  (IRU,IRS)
C
C     + + + PURPOSE + + +
C     Compute surface runoff
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IRU, IRS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IRU    - ?????
C     IRS    - ?????
C
C     + + + COMMONS + + +
      INCLUDE 'call.inc'
      INCLUDE 'csno.inc'
      INCLUDE 'csm.inc'
      INCLUDE 'cswtch.inc'
      INCLUDE 'cimprv.inc'
      INCLUDE 'csnop.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
      REAL      PTX, PTC, PTXI, CAP, SMIDX, SRP, SRD, ST, PA, SRIP
C
C     + + + END SPECIFICATIONS + + +
C
      IF(ISSRO.EQ.0) GO TO 20
      ISSRO=0
      DO 10 I=1,NRU
   10 SCT(I)=SCX(I)-SCN(I)
   20 GO TO (25,28,100), IRS
   25 PTX=PTN(IRU)-PSN(IRU)
      PTC=PTX
      PTXI=PPT-PSN(IRU)
      GO TO 29
   28 PTC=PTN(IRU)
      PTX=SMLT(IRU)
      PTXI=SMLT(IRU)
   29 IF(ISSR1.GT.0) GO TO 35
C***
C***  CONTRIBUTING AREA CALCULATION
C***
   30 IF(DARP(IRU).LE.0.) GO TO 40                                      LS0287
      CAP=SCT(IRU)*(RECHR(IRU)/REMX(IRU))
      CAP=CAP+SCN(IRU)
      GO TO 40
C***
C***  CONTRIBUTING AREA COMPUTATION INCLUDING RAIN INTENSITY
C***
   35 IF(DARP(IRU).LE.0.) GO TO 40                                      LS0287
      SMIDX=SMAV(IRU)+(.5*PTC)
      CAP=SCN(IRU)*(10.**(SC1(IRU)*SMIDX))
      IF(CAP.GT.SCX(IRU)) CAP=SCX(IRU)
C***
C***  CALCULATE SURFACE RUNOFF FROM PERVIOUS AREA (RAIN)
C***
   40 IF(DARP(IRU).GT.0.) GO TO 41
      SRP=0.
      ENFIL(IRU)=0.
      GO TO 130
   41 SRD=CAP*PTX
      ENFIL(IRU)=ENFIL(IRU)+PTX-SRD
      SRP=SRD*DARP(IRU)
      GO TO 130
  100 SRP=0.
      PTXI=SMLT(IRU)
C***
C***  CALCULATE SURFACE RUNOFF FROM IMPERVIOUS AREA (RAIN/SNOW)
C***
  130 ST=RETIP(IRU)-RSTOR(IRU)
      PA=PTXI-ST
      IF(PA.GT.0.)GO TO 140
      RSTOR(IRU)=RSTOR(IRU)+PTXI
      SRIP=0.
      GO TO 150
  140 RSTOR(IRU)=RETIP(IRU)
      SRIP=PA*DARIP(IRU)
  150 SRO(IRU)=SRO(IRU)+(SRP+SRIP)/DARU(IRU)
      SAS=SAS+SRP+SRIP
      RETURN
      END
