C
C
C
      SUBROUTINE   UNITD
C
C     + + + PURPOSE + + +
C     ?????
C
C     + + + COMMONS + + +
      INCLUDE 'call.inc'
      INCLUDE 'copt.inc'
      INCLUDE 'cdpr.inc'
      INCLUDE 'cdsm.inc'
      INCLUDE 'csize.inc'
      INCLUDE 'csm.inc'
      INCLUDE 'cvol.inc'
      INCLUDE 'cupr.inc'
      INCLUDE 'cpcrch.inc'
      INCLUDE 'cuntrf.inc'
      INCLUDE 'cdfhsg.inc'
      INCLUDE 'cinitc.inc'
      INCLUDE 'cqsout.inc'
      INCLUDE 'cswtch.inc'
      INCLUDE 'cet.inc'
      INCLUDE 'cunss.inc'
      INCLUDE 'csed.inc'
      INCLUDE 'cimprv.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   CDELS, CTSW, UVPARM, TYPE, PRTIN, PRTOUT, ODELS, RITE,
     #          RB, UTSW, UVNRDG, UVSTAT, UVYR, UVMO, UVDY, UVRPD,
     #          UVRDG1, I, J, IXWY, IC, IRU, JNS, ISS, IPOF, LINT, KS,
     #          KE, K, KK, INT, IX, M, IOF, NXS, NDELS, IMX, IMP, INISW,
     #          IP, IV, JT, JQ, NDLS, JL
      REAL      H(11), Q(11), T(11), UVSTA(4), UVDATA(1440),
     #          P(5), PR2(1440), QRIP(1440), OFLT(50),
     #          KR, KF, MM, KSAT, INFIL, KRRSQ, LBC, IM2FS,
     #          SM1, UVNVAL, RPD, DT, DTD, PT, DELP, DTH, RFNS, TB, TE,
     #          CROP, CROI, PSP, RGF, DRN, XXNLOS, XXET, STIMP, STIMX,
     #          EVIP, BMS, BMSM, SMS, RNS, COVD, RNST, RC, EVP, EVDEL,
     #          ETP, ETDEL, ETO, RT, T15, UNFIL, UET, FACP, FACI, COEF,
     #          RF, SR, RFN, D, PTF, SRN, PS, FR, QRP, FIN, XUINF,
     #          UNET, ET, THRES, DX, DTM, DTS, DTOS, DTDXM, DXDT, DTDX,
     #          ALPR, ALPR1, CMP, CMP1, ALPI, ALPI1, CMI, CMI1, OFIMP,
     #          OFPRV, D50, HC, EN, ALP, ALP1, CM, CM1, HMX, CSD, SAV,
     #          SO, PE, PRR, PR, QDX, QDT, HM, QM, TM, HPM, QPM, TPM,
     #          HJ, QJ, HPJ, QPJ, THETA, TJ, TPJ, HBAR, CHDT, CQDX,
     #          HDTQDX, TC, TR, DC, ER, EF, TX, TIN, TOUT, TD, FAC, TU,
     #          F2, F1, JJ
      CHARACTER*4 PCRID                                                 LS0287
      LOGICAL   UVDEL
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (QLAT(1),QRIP(1))
      EQUIVALENCE (BSED(1),OFLT(1))                                     LS0586
C
C     + + + INTRINSICS + + +
      INTRINSIC   AMOD, EXP
C
C     + + + EXTERNALS + + +
      EXTERNAL   UNSM
C
C     + + + DATA INTITALIZATIONS + + +
      DATA IM2FS/.001388889/
C
C     + + + OUTPUT FORMATS + + +
   95 FORMAT(1X,A1,4A4,8I5,F8.0,(10(10F8.2,2X/)))
C  95 FORMAT(1X,I3,12F8.4)
C 325 FORMAT(1X,I3,6F10.5)
C2000 FORMAT((6E12.7))
C2001 FORMAT(' UNITD-SM1',F12.6)
C
C     + + + END SPECIFICATIONS + + +
C
      IF(ISX1.EQ.0) GO TO 6
      ISX1=0
C     DEFINE FILE 17(200,5760,L,IV)                                     XXXXXXXX
      REWIND 13
      REWIND 15
C     REWIND 17                                                         XXXXXXXX
      REWIND 18
    4 DO 5 I=1,50
      TSUW(I)=0.
    5 BRO(I)=0.0
    6 DO 8 I=1,50
      DO 8 J=1,96
      UINF(I,J)=0.0
      UNBAS(I,J)=0.0
    8 CONTINUE
   10 READ(13)WY,WYD,NS,IS,RFL,SFL,DFH
      DO 15 I=1,24
   15 CONTINUE
      INTSW=1
      IXWY=IWY
      IF(WY.EQ.IXWY.AND.WYD.EQ.JWDY) GO TO 20
      GO TO 10
   20 IF(ISIM.LT.2) GO TO 30
      IC=0
      QSWY=IWY
      QSMO=IMO
      QSDY=IDY
C           ** BEGIN RAIN GAGE LOOP **
      SM1=0.0
   30 DO 1000 IDS=1,NDS
      READ(15)UVDEL,UVSTA,UVPARM,UVSTAT,UVYR,UVMO,UVDY,UVRPD,
     1UVRDG1,UVNRDG,UVNVAL,(UVDATA(J),J=1,UVNRDG)
C     WRITE(72,95)UVDEL,UVSTA,UVPARM,UVSTAT,UVYR,UVMO,UVDY,UVRPD,
C    1UVRDG1,UVNRDG,UVNVAL,(UVDATA(J),J=1,UVNRDG)
      RPD=UVRPD
      DT=1440./RPD
      DTD=DT/1440.
      IF(RPD.GT.288.) GO TO 50
      PT=5.0
      CDELS=DT/PT
      GO TO 60
   50 PT=DT
      CDELS=1
   60 DELP=1.0/CDELS
      DTH=PT/60.0
      P(IDS)=0.
      DO 62 J=1,UVNRDG
      IF(UVDATA(J).GT.1000.) UVDATA(J)=0.
   62 P(IDS)=P(IDS)+UVDATA(J)
C          ** BEGIN HRU LOOP **
      DO 999 IRU=1,NRU
      IF(IDS.NE.KDS(IRU)) GO TO 999
      PPT=0.
      RFNS=0.
      IF(NS.LE.0) GO TO 70
      JNS=1
      ISS=IS
      TB=DFH(1)
      TE=DFH(2)
      CTSW=1
      CROP=0.
      CROI=0.
      GO TO 72
   70 CTSW=0
   72 PSP=X(2,IRU)
      RGF=X(4,IRU)
      DRN=24.0*X(1,IRU)*DTD*X(3,IRU)
      IF(ICHG.NE.1) GO TO 80
      BMSA(IRU)=RECHR(IRU)
      SMSA(IRU)=0.
   80 INFIL=0.
      XXNLOS=0.
      XXET=0.
      IPOF=0
      IF(DARIP(IRU).GT.0.) IPOF=1
      STIMP=RSTOR(IRU)
      STIMX=RETIP(IRU)
      EVIP=0.
      BMS=BMSA(IRU)
      BMSM=REMX(IRU)
      SMS=SMSA(IRU)
      RNS=XIN(IRU)
      COVD=COVDNS(IRU)
      RNST=RNSTS(IRU)
      IF(ITSW(IRU).EQ.1) GO TO 90
      COVD=COVDNW(IRU)
      RNST=RNSTW(IRU)
   90 RC=UPCOR(IRU)
      EVP=EPAN
      IF(EPAN.LT..001) EVP=PET(IRU)/EVC(MO)                             0485LS
      EVDEL=EVP*DTD
      ETP=PET(IRU)
      ETDEL=ETP*DTD
      ETO=ETDEL*(1.-COVD)
      RT=0.
      T15=15.
      UNFIL=0.
      UET=0.
      LINT=1
      FACP=DARP(IRU)/DAT
      FACI=DARIP(IRU)/DAT
      KSAT=DTH*X(1,IRU)
      IF(IMPERV(IRU).LT.1.) COEF=(RGF-1.0)/BMSM                         0485LS
C                                                                       LS0786
   92 J=0
      KS=UVRDG1-1
      KE=KS+UVNRDG
C     WRITE(72,95)IRU,DTH,BMS,BMSM,SMS,RNS,ETP,PSP,RGF,DRN,KSAT,COEF,FAC
C          ** COMPUTE RAINFALL EXCESS **
      DO 550 K=1,UVRPD
      IF(K.LE.KS.OR.K.GT.KE) GO TO 400
      KK=K-KS
      RF=UVDATA(KK)
      IF(RF.GT.1000.) RF=0.
      IF(RF.LE.0.) GO TO 400
  210 RF=RF*RC
      SR=RF*DELP
      PPT=PPT+RF
      RFN=RF
        IF(IMPERV(IRU).LT.1.) THEN                                      0485LS
        D=RNST-RNS
          IF(D.LT..00001) THEN                                          0485LS
          D=0.                                                          0485LS
          RNS=RNST                                                      0485LS
          END IF                                                        O485LS
        IF(D.EQ.0.) GO TO 245
        IF(D.LT.0.) GO TO 220
        IF(RF.GE.D) GO TO 220
        RNS=RNS+RF
        PTF=0.
        GO TO 230
  220   RNS=RNST
        PTF=RF-D
  230   RFN=(RF*(1.-COVD))+(PTF*COVD)
  245   RFNS=RFNS+RFN
        IF(RFN.LE.0.) GO TO 270
        SRN=RFN*DELP
        IF(SMS.LT..00001) SMS=0.                                        0485LS
        IF(SMS.GT.0.) GO TO 260                                         0485LS
  250   PS=PSP*(RGF-COEF*BMS)
        IF (KSAT.GT.0.) GO TO 251                                       0485LS
        FR=0.                                                           XXXXXX
        GO TO 270                                                       XXXXXX
C       ABOVE 3 LINES STARTED IN COLUMN 6 INSTEAD OF 7                  XXXXXX
  251   FR=KSAT*(1.0+PS/SRN)
        GO TO 270
  260   FR=KSAT*(1.+PS/SMS)
        END IF                                                          0485LS
  270 DO 320 I=1,CDELS
      J=J+1
      IF(IPOF.EQ.0) GO TO 272
      D=STIMX-STIMP
      IF(SR.LE.D) GO TO 271
      STIMP=STIMX
      QRIP(J)=SR-D
      GO TO 2720
  271 STIMP=STIMP+SR
  272 QRIP(J)=0.
 2720   IF(IMPERV(IRU).LT.1.) THEN                                      0485LS
        IF(RFN.GT.0.) GO TO 273
 2721   QRP=0.
        UPE(J)=0.
        GO TO 274
  273   QRP=SRN-.5*FR
        IF(SRN.LT.FR) QRP=.5*SRN*SRN/FR
        FIN=SRN-QRP
        INFIL=INFIL+FIN
        SMS=SMS+FIN
        IF(KSAT.GT.0.) GO TO 2730                                       0485LS
        FR=0.                                                           XXXXXXXX
        GO TO 2731                                                      XXXXXXXX
C       ABOVE 2 LINES STARTED IN COLUMN 6 INSTEAD OF 7                  XXXXXXXX
 2730   FR=KSAT*(1.0+PS/SMS)
 2731   UPE(J)=QRP
  274   RT=RT+PT
        PR2(J)=SR
        END IF                                                          0485LS
      IF(ISUN.EQ.1.AND.IMPERV(IRU).LT.1.) THEN                          LS0786S
C***
C***  UNIT SUBSURFACE COMPUTATIONS
C***
        IF(AMOD(RT,15.).LT..01) THEN                                    0485LS
        XUINF=INFIL-UNFIL
        UNFIL=INFIL
        UNET=XXET-UET
        UET=XXET
        INT=RT/15.
        DO 2740 IX=LINT,INT
        UINF(IRU,IX)=(XUINF-UNET)/(INT-LINT+1)
 2740   CONTINUE
        END IF                                                          0485LS
        LINT=INT+1
      END IF                                                            0485LS
C***
  275 IF(CTSW.NE.1) GO TO 320
  280 IF(RT.LE.TB) GO TO 285
      CROP=CROP+QRP
      CROI=CROI+QRIP(J)
  285 IF(RT.LT.TE) GO TO 320
  290 BRO(ISS)=BRO(ISS)+(CROP*FACP)+(CROI*FACI)
      JNS=JNS+1
      ISS=ISS+1
      IF(JNS.GT.NS) GO TO 310
      M=2*(JNS-1)
      TB=DFH(M+1)
      TE=DFH(M+2)
      CROP=0.
      CROI=0.
      GO TO 320
  310 CTSW=0
  320 CONTINUE
C     WRITE(72,325)ISS,BRO(ISS),CRO,QR,SMS,BMS,INFIL
      GO TO 550
  400 DO 410 I=1,CDELS
      J=J+1
      QRIP(J)=0.
      PR2(J)=0.
  410 UPE(J)=0.
        IF(IMPERV(IRU).LT.1.) THEN                                      0485LS
        IF(RNS.GE.EVDEL) GO TO 430
        XXNLOS=XXNLOS+RNS
        IF(RNS.GE.ETDEL) GO TO 414
        ET=ETO+((ETDEL-RNS)*COVD)
        RNS=0.
        GO TO 442
  414   RNS=0.
        GO TO 440
  430   RNS=RNS-EVDEL
        XXNLOS=XXNLOS+EVDEL
  440   ET=ETO
        END IF                                                          0485LS
  442 IF(IPOF.EQ.0) GO TO 449
      IF(STIMP.GT.EVDEL) GO TO 444
      EVIP=EVIP+STIMP
      STIMP=0.
      GO TO 448
  444 EVIP=EVIP+EVDEL
      STIMP=STIMP-EVDEL
  448 IF(IMPERV(IRU).LT.1.) GO TO 449                                   0485LS
      RT=RT+DT
      GO TO 505
  449 IF(SMS.GE.ET) GO TO 460
      BMS=BMS+SMS-ET
      SMS=0.
      IF(BMS.GE.0.) GO TO 450
      XXET=XXET+(ET+BMS)
      BMS=0.
      GO TO 500
  450 XXET=XXET+ET
      GO TO 500
  460 SMS=SMS-ET
      XXET=XXET+ET
  470 IF(SMS.LE.DRN) GO TO 490
  480 SMS=SMS-DRN
      BMS=BMS+DRN
      GO TO 500
  490 BMS=BMS+SMS
      SMS=0.
  500 IF(BMS.GT.BMSM) BMS=BMSM
      RT=RT+DT
      IF(ISUN.EQ.1.AND.IMPERV(IRU).LT.1.) THEN                          0485LS
C***
C***  UNIT SUBSURFACE COMPUTATIONS
C***
        IF(AMOD(RT,15.).LT..01) THEN                                    0485LS
        XUINF=INFIL-UNFIL
        UNFIL=INFIL
        UNET=XXET-UET
        UET=XXET
        INT=RT/15.
        DO 503 IX=LINT,INT
        UINF(IRU,IX)=(XUINF-UNET)/(INT-LINT+1)
  503   CONTINUE
        END IF                                                          0485LS
        LINT=INT+1
      END IF                                                            0485LS
C***
  505 IF(CTSW.NE.1) GO TO 550
  510 IF(RT.LT.TE) GO TO 550
  520 BRO(ISS)=BRO(ISS)+(CROP*FACP)+(CROI*FACI)
      JNS=JNS+1
      ISS=ISS+1
      IF(JNS.GT.NS) GO TO 540
  530 M=2*(JNS-1)
C          ** ROUTE OVERLAND FLOW **
      TB=DFH(M+1)
      TE=DFH(M+2)
      CROP=0.
      CROI=0.
      GO TO 550
  540 CTSW=0
  550 CONTINUE
C     WRITE(1,*)'UNITD-UPE'
C     WRITE(1,2000)(UPE(JT),JT=1,288)
C     WRITE(1,*)'UNITD-QRIP'
C     WRITE(1,2000)(QRIP(JT),JT=1,288)
C     WRITE(72,2000)(UINF(1,JQ),JQ=1,96)
C     WRITE(72,2001) SM1
C          ** COMPUTE DAILY FLOW MODEL COMPONENTS **
      BMSA(IRU)=BMS
      SMSA(IRU)=SMS
      XIN(IRU)=RNS
      EVIMP(IRU)=EVIP
      RSTOR(IRU)=STIMP
      ENFIL(IRU)=INFIL
      XINLOS(IRU)=XINLOS(IRU)+XXNLOS
      AET(IRU)=AET(IRU)+XXET
      PTN(IRU)=RFNS
      PPTI(IRU)=PPT
      SRO(IRU)=(RFNS-INFIL)*PERV(IRU)+PPT*IMPERV(IRU)
      SAS=SAS+(SRO(IRU)*DARU(IRU))
C          ** IF FLOW ROUTING SWITCH IS ON (=1) DO OVERLAND **
C             FLOW AND SEDIMENT
      IF(ISIM.LT.2) GO TO 999
  600 DO 998 IOF=1,NOFSEG
      IF(KRU(IOF).NE.IRU) GO TO 998
      IC=IOF
      PCRID=PCRIDA(IC)
      TYPE=TYPEA(IC)
      PRTIN=PRTINA(IC)
      PRTOUT=PROUTA(IC)
      NXS=NXSA(IC)
      NDELS=NDELSA(IC)
      ODELS=ODELSA(IC)
      THRES=THRESA(IC)
      DX=DXA(IC)
      DTM=DTMA(IC)
      DTS=DTSA(IC)
      DTOS=DTOSA(IC)
      DTDXM=DTDXMA(IC)
      DXDT=DXDTA(IC)
      DTDX=DTDXA(IC)
      ALPR=ALPRA(IC)
      ALPR1=ALPR1A(IC)
      CMP=CMPA(IC)
      CMP1=CMP1A(IC)
      ALPI=ALPIA(IC)
      ALPI1=ALPI1A(IC)
      CMI=CMIA(IC)
      CMI1=CMI1A(IC)
      OFIMP=OFIPA(IC)
      OFPRV=1.-OFIMP
      D50=D50A(IC)
      KR=KRA(IC)
      HC=HCA(IC)
      KF=KFA(IC)
      MM=MMA(IC)
      EN=ENA(IC)
      IMX=1
      IF(OFIMP.GT.0.) IMX=2
      DO 870 IMP=1,IMX
        IF(IMP.EQ.2) GO TO 605
        IF(OFIMP.GE.1.) GO TO 870                                       LS0786
        ALP=ALPR
        ALP1=ALPR1
        CM=CMP
        CM1=CMP1
        GO TO 610
  605   ALP=ALPI
        ALP1=ALPI1
        CM=CMI
        CM1=CMI1
  610   IF(RFL.NE.1) GO TO 620
        INISW=0
        HMX=QMXA(IC)
        DO 615 K=1,NXS
        IF(IMP.EQ.1) GO TO 612
        H(K)=AAI(K,IC)
        Q(K)=QQI(K,IC)
        GO TO 615
  612   H(K)=AA(K,IC)
        Q(K)=QQ(K,IC)
        T(K)=TT(K,IC)
  615   CONTINUE
        GO TO 640
  620   INISW=1
        HMX=0.
        DO 630 J=1,NXS
        H(J)=0.
        Q(J)=0.
  630   T(J)=0.
C            ** ADJUST UPE AS NECESSARY FOR DIFFERENCE BETWEEN **
C               UPE TIME AND ROUTE TIME
  640   IP=1
        CSD=0.
        SAV=PT
        DO 860 I=1,NDELS
        IF(SAV.GE.DTM) GO TO 660
        IF(SAV.LT..001) GO TO 650                                       0485LS
        SO=SAV/DTM
        PE=(SO*UPE(IP))+((1.-SO)*UPE(IP+1))
        PRR=(SO*PR2(IP))+((1.-SO)*PR2(IP+1))
        IF(IMP.EQ.2) PE=(SO*QRIP(IP))+((1.-SO)*QRIP(IP+1))
        SAV=PT-(DTM-SAV)
        IP=IP+1
        GO TO 670
  650   IP=IP+1
        SAV=PT
  660   SAV=SAV-DTM
        PE=UPE(IP)
        PRR=PR2(IP)
        IF(IMP.EQ.2) PE=QRIP(IP)
  670   IF(TYPE.NE.99)GO TO 675
        PR=IM2FS*PE/PT
        PRR=IM2FS*PRR/PT
        QDX=PR*DX
        IF(IMP.EQ.2) THEN                                               LS0786
          INQ(I)=QDX                                                    LS0786
          IF(OFIMP.GE.1.) QA(I)=0.                                      LS0786
        ELSE                                                            LS0786
          QA(I)=QDX                                                     LS0786
        END IF                                                          LS0786
        GO TO 860
  675   IF(PE.LE.0.) GO TO 690
  680   PR=IM2FS*PE/PT
        PRR=IM2FS*PRR/PT
        QDT=DTS*PR
        QDX=PR*DX
        GO TO 700
  690   QDT=0.
        QDX=0.
        PRR=0.
  700   IF(HMX.LE.THRES.AND.PE.LE..00001) GO TO 710                     0485LS
        GO TO 730
  710 IF(IMP.EQ.2) THEN                                                  LS0786
        INQ(I)=0.                                                        LS0786
      ELSE                                                               LS0786
        QA(I)=0.                                                         LS0786
        HA(I)=0.                                                         LS0786
        TA(I)=0.                                                         LS0786
      END IF                                                             LS0786
        IF(INISW.NE.0) GO TO 860
        DO 720 J=1,NXS
        H(J)=0.
        Q(J)=0.
  720   T(J)=0.
        HMX=0.
        INISW=1
        GO TO 860
C            ** ROUTE OVERLAND FLOW **
  730   INISW=0
        HM=0.
        QM=0.
        TM=0.
        HPM=0.
        QPM=0.
        TPM=0.
        HMX=0.
        DO 850 J=2,NXS
        HJ=H(J)
        QJ=Q(J)
        IF(J.GT.2) GO TO 735
        HPJ=HJ+QDT-DTDX*QJ
        IF(HPJ.GT.0.) GO TO 780
        HPJ=0.
        QPJ=0.
        GO TO 790
  735   THETA=0.
        IF(HJ.GT.0.) THETA=DTDXM*QJ/HJ
        IF(THETA.LT.1.) GO TO 770
  740   QPJ=QPM+QDX-DXDT*(HPM-HM)
        IF(QPJ.GT.0.) GO TO 760
  750   HPJ=0.
        QPJ=0.
        GO TO 790
  760   HPJ=(ALP1*QPJ)**CM1
        GO TO 790
  770   HPJ=HJ+QDT+DTDX*(QM-QJ)
        IF(HPJ.GT.0.) GO TO 780
        HPJ=0.
        QPJ=0.
        GO TO 790
  780   QPJ=ALP*(HPJ**CM)
  790   IF(HPJ.GT.HMX) HMX=HPJ
        IF(IMP.EQ.2) GO TO 840
        IF(ISIM.NE.3) GO TO 840
C            ** ROUTE SEDIMENT **
  800   TJ=T(J)
        IF(HPJ.GT.0.) GO TO 801
        TPJ=0.
        GO TO 830
  801   HBAR=(HPM+HPJ)*.5
        CHDT=(TJ*HJ)/DTS
        CQDX=(TPM*QPM)/DX
        HDTQDX=(HPJ/DTS)+(QPJ/DX)
        TC=MM*HBAR**EN
        TR=((TM*QM)+(TJ*QJ))*.5
        DC=TC-TR
        IF(DC.GT.0.) GO TO 805
        ER=0.
        EF=DC/DX
        GO TO 815
  805   ER=KR*PRR*PRR*EXP(-HC*HBAR*HBAR)
        TX=TR+ER*DX
        IF(TX.LT.TC) GO TO 810
        EF=0.
        ER=(TC-TR)/DX
        GO TO 815
  810   EF=KF*(TC-TX)/DX
  815   TPJ=(EF+ER+CHDT+CQDX)/HDTQDX
  830   TPM=TPJ
        TM=TJ
        T(J)=TPJ
  840   HM=HJ
        QM=QJ
        HPM=HPJ
        QPM=QPJ
        H(J)=HPJ
        Q(J)=QPJ
  850   CONTINUE
        IF(IMP.EQ.2) GO TO 855
        QA(I)=QPJ
        HA(I)=HPJ
        TA(I)=TPJ*QPJ*OFPRV
        CSD=CSD+TA(I)
        GO TO 860
  855   IF(OFIMP.GE.1.) QA(I)=0.                                        LS0786
        INQ(I)=QPJ                                                      LS0786
  860   CONTINUE
C     WRITE(1,*)'UNITD-INQ'
C     WRITE(1,2000)(INQ(JT),JT=1,288)
        IF(SFL.NE.1) GO TO 870
        IF(IMP.EQ.2) GO TO 865
        QMXA(IC)=HMX
        DO 862 J=1,NXS
        AA(J,IC)=H(J)
        QQ(J,IC)=Q(J)
  862   TT(J,IC)=T(J)
        GO TO 870
  865   DO 866 J=1,NXS
        AAI(J,IC)=H(J)
  866   QQI(J,IC)=Q(J)
  870 CONTINUE
      IF(IMX.EQ.1) GO TO 875
      DO 872 I=1,NDELS
      QA(I)=OFIMP*INQ(I)+OFPRV*QA(I)
  872 CONTINUE
C     WRITE(1,*)'UNITD-QA'
C     WRITE(1,2000)(QA(JT),JT=1,288)
  875 IF(ISIM.LT.3) GO TO 880
      TSUW(IOF)=CSD*DTS*.0011023*OFLT(IOF)*OFPRV
      IV=IC+NOFSEG+1
      WRITE(17,REC=IV)(TA(JT),JT=1,NDELS)
  880 IV=IC+1
      WRITE(17,REC=IV)(QA(JQ),JQ=1,NDELS)
C     WRITE(72,2000)(UPE(JQ),JQ=1,NDELS)
C     WRITE(72,2000)(QA(JQ),JQ=1,NDELS)
  900 IF(PRTIN.LT.1) GO TO 910
      QSIO=1
      QSQS=1
      IF(PRTIN.EQ.2) QSQS=3
      IF(PRTIN .EQ. 3) QSQS = 5                                          1083KF
      QSRPD=NDELS
      NDLS=UVRPD*CDELS
      WRITE(18) QSWY,QSMO,QSDY,QSIO,QSQS,PCRID,QSRPD
      WRITE(18) (UPE(JL),JL=1,NDLS)
  910 IF(PRTOUT.LT.1) GO TO 998
      IF(DTOS.LE.DTM) GO TO 980
      DO 920 J=2,NDELS
      QA(J)=QA(J)+QA(J-1)
      IF(ISIM.EQ.3) TA(J)=TA(J)+TA(J-1)
  920 CONTINUE
      TIN=1440./NDELS
      TOUT=1440./ODELS
      TD=0.
      FAC=TIN/TOUT
      DO 950 J=1,ODELS
      TD=TD+TOUT
      K=TD/TIN
      TU=K*TIN
      F2=(TD-TU)/TIN
      IF(F2.GE..01) GO TO 940
  930 QA(J)=QA(K)
      IF(ISIM.EQ.3) TA(J)=TA(K)
      GO TO 950
  940 F1=(1.0-F2)
      QA(J)=QA(K)*F1+QA(K+1)*F2
  950 CONTINUE
      DO 960  JJ=2,ODELS
      J=ODELS-JJ+2
      QA(J)=(QA(J)-QA(J-1))*FAC
      IF(ISIM.EQ.3) TA(J)=(TA(J)-TA(J-1))*FAC
  960 CONTINUE
      QA(1)=QA(1)*FAC
      IF(ISIM.EQ.3) TA(1)=TA(1)*FAC
  980 QSIO=2
      QSQS=1
      IF(PRTOUT.EQ.2) QSQS=3
      IF(PRTOUT .EQ. 3) QSQS = 5                                         1083KF
      QSRPD=ODELS
      WRITE(18) QSWY,QSMO,QSDY,QSIO,QSQS,PCRID,QSRPD
      WRITE(18) (QA(JL),JL=1,ODELS)
      IF(ISIM.NE.3) GO TO 998
      QSQS=2
      IF(PRTOUT.EQ.2) QSQS=4
      IF(PRTOUT .EQ. 3) QSQS = 6                                         1083KF
      WRITE(18) QSWY,QSMO,QSDY,QSIO,QSQS,PCRID,QSRPD
      DO 991 JL=1,ODELS
      IF(QA(JL).GT.0.) GO TO 990
      TLAT(JL)=0.0
      GO TO 991
  990 TLAT(JL)=(TA(JL)*35.311)/QA(JL)
  991 CONTINUE
      WRITE(18) (TLAT(JL),JL=1,ODELS)
  998 CONTINUE
  999 CONTINUE
 1000 CONTINUE
      IF(ISUN.EQ.1) CALL UNSM
      PP=0.
      DO 1020 IDS=1,NDS
 1020 PP=PP+(P(IDS)*PPAR(IDS))
      PP=PP/DAT
      RETURN
      END
C
C
C
      SUBROUTINE   UNSM
C
C     + + + PURPOSE + + +
C     ?????
C
C     + + + COMMONS + + +
      INCLUDE 'call.inc'
      INCLUDE 'cbs.inc'
      INCLUDE 'cdfhsg.inc'
      INCLUDE 'csm.inc'
      INCLUDE 'cunss.inc'
      INCLUDE 'cvol.inc'
      INCLUDE 'cimprv.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   IRU, J, K, I, ISS, KK, M, ITB, ITE, L, II
      REAL      ENF, EXC, USEP, S, SSS, GR, GX, GXD, XK1, XK2, XK3, XX4,
     #          C1, C2, CFR, SOS, XD1, XN1, XN2, RASA, GF, GAD, BASA,
     #          SNK, SUM1, TB, TE
C
C     + + + INTRINSICS + + +
      INTRINSIC   EXP, SQRT, AMOD
C
C     + + + OUTPUT FORMATS + + +
C2001 FORMAT(I10,2F12.7)
C6001 FORMAT(' K-I-GW1-GWIN-GWFL-GW2-SU2   ',2I5,5(2X,E12.6))           KD1291
C
C     + + + END SPECIFICATIONS + + +
C
      RAS=0.0
      BAS=0.0
      DO 9 IRU=1,NRU
      UGS(IRU)=0.
    9 USS(IRU)=0.
      DO 10 J=1,NRES
      EXCS(J)=0.0
   10 RASQ(J)=0.0
      DO 11 K=1,NGW
   11 BASQ(J)=0.0
      DO 200 I=1,96
      DO 90 IRU=1,NRU
      IF(DARP(IRU).GT.0.) THEN                                          LS0786
      ENF=UINF(IRU,I)
      IF(ENF.LE.0.)GO TO 20
      SMAV(IRU)=SMAV(IRU)+ENF
      RECHR(IRU)=RECHR(IRU)+ENF
      IF(RECHR(IRU).GT.REMX(IRU)) RECHR(IRU)=REMX(IRU)
      IF(SMAV(IRU).LE.SMAX(IRU)) GO TO 25
      EXC=SMAV(IRU)-SMAX(IRU)
      IF(EXC.LT..00001) EXC=0.                                          0485LS
      SMAV(IRU)=SMAX(IRU)
      GO TO 30
   20 RECHR(IRU)=RECHR(IRU)+ENF
      IF(RECHR(IRU).LT..00001)   RECHR(IRU)=0.                          0485LS
      SMAV(IRU)=SMAV(IRU)+ENF
      IF(SMAV(IRU).LT..00001) SMAV(IRU)=0.                              0485LS
   25 EXC=0.
   30 K=KGW(IRU)
      J=KRES(IRU)
      USEP=SEP(IRU)/96.
      IF(EXC.GT.USEP) GO TO 34
      S=EXC
      GO TO 38
   34 S=USEP
      SSS=EXC-USEP
      EXCS(J)=EXCS(J)+SSS*DARP(IRU)
      USS(IRU)=USS(IRU)+SSS
   38 GW(K)=GW(K)+(S*DARP(IRU))
      UGS(IRU)=UGS(IRU)+S
      END IF
   90 CONTINUE
C     XX1=GW(1)/ARS(1)
C     XX4=0.
      DO 150 J=1,NRES
      IF(RES(J).LT..00001) RES(J)=0.                                    0485LS
      IF(RES(J).LE.0..AND.EXCS(J).LE.0.) GO TO 145
      GR=RES(J)/ARS(J)
      GX=EXCS(J)/ARS(J)
      GXD=GX*96.
      XK1=RCF(J)
      XK2=RCP(J)
        IF(XK1.GT.0..AND.GX.GT.0.) THEN                                 0485LS
        IF(XK2.GT.0.) GO TO 110
        C2=1.-EXP(-XK1*.010417)
        CFR=GX*(1.-C2/(XK1*.010417))+GR*C2
        GO TO 120
        END IF                                                          0485LS
  105 C1=GR*XK2*.010417                                                 LS110483
      CFR=(GR*C1)/(1.+C1)                                               LS110483
      GO TO 120                                                         LS110483
  110 XK3=SQRT(XK1**2.+4.*XK2*GXD)
      SOS=GR-((XK3-XK1)/(2.*XK2))
      XD1=(XK2/XK3)*SOS
      XN1=1.+XD1                                                        LS110483
      XN2=1.-EXP(-XK3*.010417)
      XD1=(XK2/XK3)*SOS
      CFR=GX+((SOS*XN1*XN2)/(1.+XD1*XN2))
  120 RASA=CFR*ARS(J)
      UINF(J,I)=CFR
      RAS=RAS+RASA
      RASQ(J)=RASQ(J)+RASA
      RES(J)=RES(J)-RASA+EXCS(J)
      EXCS(J)=0.
      IF(RES(J).LE.0.) RES(J)=0.
      K=KRSP(J)
      GF=RES(J)/ARS(J)
      IF(GF.LT.0.) GF=0.
      GAD=(RSEP(J)*((GF/RESMX(J))**REXP(J)))/96.
      XX4=XX4+GAD
      IF(GAD.GT.GF) GAD=GF
      SSGW=SSGW+GAD*ARS(J)
      RES(J)=RES(J)-(GAD*ARS(J))
C     XX3=RES(J)/ARS(J)
      IF(RES(J).LE..00001) RES(J)=0.                                    0485LS
      GW(K)=GW(K)+(GAD*ARS(J))
      GO TO 150
  145 UINF(J,I)=0.
  150 CONTINUE
      DO 100 K=1,NGW
      IF(GW(K).LE.0.) GO TO 95
      BASA=(RCB(K)*GW(K))/96.
      UNBAS(K,I)=BASA/ARG(K)
      GW(K)=GW(K)-BASA
      BAS=BAS+BASA
      BASQ(K)=BASQ(K)+BASA
      IF(GSNK(K).LE.0.) GO TO 100
      SNK=(GSNK(K)*GW(K))/96.
      GW(K)=GW(K)-SNK
      GWSNK(K)=GWSNK(K)+SNK
C     XX2=GW(K)/ARG(K)
C     SU2=XX1+XX4-UNBAS(K,I)-XX2
C     WRITE(72,6001)K,I,XX1,XX4,UNBAS(K,I),XX2,SU2
      GO TO 100
   95 UNBAS(K,I)=0.
  100 CONTINUE
  200 CONTINUE
      SGW=0.
      SRS=0.
      DO 220 J=1,NRES
  220 SRS=SRS+RES(J)
      DO 240 K=1,NGW
  240 SGW=SGW+GW(K)
      J=0
      ISS=IS
      DO 300 KK=1,NS
      J=J+1
      SUM1=0.
      M=2*(J-1)
      TB=DFH(M+1)
      TE=DFH(M+2)
      ITB=TB/15.+1
      ITE=TE/15.
      TB=AMOD(TB,15.)
      TE=AMOD(TE,15.)
      IF(TE.GT.0.)ITE=ITE+1
      IF(TE.LE.0.)TE=1.                                                 0485LS
      DO 500 L=1,NRES
      SUM1=SUM1+(1.-TB)*UINF(L,ITB)*ARS(L)
      I=ITB+1
      II=ITE-1
      DO 490 M=I,II
      SUM1=SUM1+UINF(L,M)*ARS(L)
  490 CONTINUE
      SUM1=SUM1+(TE*UINF(L,ITE)*ARS(L))
  500 CONTINUE
      DO 600 L=1,NGW
      SUM1=SUM1+(1.-TB)*UNBAS(L,ITB)*ARG(L)
      DO 690 M=I,II
      SUM1=SUM1+UNBAS(L,M)*ARG(L)
  690 CONTINUE
      SUM1=SUM1+(TE*UNBAS(L,ITE)*ARG(L))
  600 CONTINUE
C     WRITE(72,2001) ISS,BRO(ISS),SUM1
      SUM1=SUM1/DAT
      BRO(ISS)=BRO(ISS)+SUM1
      ISS=ISS+1
  300 CONTINUE
      RETURN
      END
C
C
C
      SUBROUTINE   ROUTE
C
C     + + + PURPOSE + + +
C     ?????
C
C     + + + COMMONS + + +
      INCLUDE 'call.inc'
      INCLUDE 'cbs.inc'
      INCLUDE 'csize.inc'
      INCLUDE 'cvol.inc'
      INCLUDE 'copt.inc'
      INCLUDE 'cpcrch.inc'
      INCLUDE 'cdfhsg.inc'
      INCLUDE 'cinitc.inc'
      INCLUDE 'cuntrf.inc'
      INCLUDE 'cqsout.inc'
      INCLUDE 'cswtch.inc'
      INCLUDE 'cunss.inc'
      INCLUDE 'cplot.inc'
      INCLUDE 'csed.inc'
      INCLUDE 'cstach.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   IPO, I, J, JM, LB, NXS, NDELS, INISW, K, IJ, LRPD,
     #          JIN, JOUT, KSM, IC, IV, JT, JQ, IRU, KG, KSS, LATSW,
     #          IP, IX1, KX1, JNS, ISS, ISM, JJ, IPY, IOF,
     #          UVSTAT, UVMO, UVDY, UVYR, UVRPD, UVRDG1, UVNRDG,
     #          RB, RITE, UPSW, TYPE, PRTIN, PRTOUT, ODELS, UVPARM
      REAL      A(11), Q(11), T(11), DIS(30), STO(30),
     #          SMQA(1440), SMTA(1440),
     #          LBC, KR, KF, MM, IM2FS,
     #          UNRES(50,96), OFLT(50),
     #          THRES, DX, DTM, DTS, DTOS, DTDXM, DXDT, DTDX, ALP1,
     #          CM1, ALP, CM, FLGTH, QMX, PQT, AF, AT, AL, ADIF, AQUO,
     #          SAV, SS1C, SS2C, SS1, SS2, SO, TB, TE, RT, QPM, TPM,
     #          QL, TL, APM, AM, QM, TM, AJ, QJ, THETA, QPJ, APJ, TJ,
     #          TPJ, CADT, CQDX, ADTQDX, UVNVAL, TIN, TOUT, TD,
     #          FAC, TU, F2, F1, RQD
      CHARACTER*4 PCRID                                                 LS0287
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (UINF(1,1),UNRES(1,1))                                 LS0586
      EQUIVALENCE (BSED(1),OFLT(1))                                      LS0586
      EQUIVALENCE (SEDSG,RQD)
C
C     + + + EXTERNALS + + +
      EXTERNAL   RESVRU
C
C     + + + DATA INITIALIZATIONS + + +
      DATA IPO,IM2FS/0,.001388889/
C
C     + + + OUTPUT FORMATS + + +
  655 FORMAT(1X,'U',A2,4X,A16,8X,I4,7I2,26X,A4,10(288A4))
 2000 FORMAT(' SEDIMENT IN TONS/DAY, ',I2,'/',I2,'/',I4,' FOR OVERLAN',  KD1291
     1'D FLOW PLANES 1 THROUGH ',I2)                                     KD1291
 2100 FORMAT((10F12.5))
C4999 FORMAT(4I5)
C5000 FORMAT((10F12.7))
C
C     + + + END SPECIFICATIONS + + +
C
C     DEFINE FILE 17(200,5760,L,IV)                                     XXXXXXXX
C     ABOVE LINE RELOCATED TO MAIN
C***
C*** LOOP FOR EACH CHANNEL AND RESERVOIR SEGMENT(NCRSEG)***
C***
      RQD=0.
      IF(NCRSEG.EQ.0)RETURN
      DO 1000 I=1,NCRSEG
      J=I+NOFSEG
      JM=J-1
      PCRID=PCRIDA(J)
      LB=LBA(I)
      RB=RBA(I)
      RITE=RITEA(I)
      UPSW=UPSWA(I)
      TYPE=TYPEA(J)
      PRTIN=PRTINA(J)
      PRTOUT=PROUTA(J)
      NXS=NXSA(J)
      NDELS=NDELSA(J)
      ODELS=ODELSA(J)
      THRES=THRESA(J)
      DX=DXA(J)
      DTM=DTMA(J)
      DTS=DTSA(J)
      DTOS=DTOSA(J)
      DTDXM=DTDXMA(J)
      DXDT=DXDTA(J)
      DTDX=DTDXA(J)
      ALP1=ALPR1A(J)
      CM1=CMP1A(J)
      ALP=ALPRA(J)
      CM=CMPA(J)
      FLGTH=DX*(NXS-1)
C***
C***INITIALIZE A,Q, AND T ARRAYS***
C***
      IF(RFL.EQ.0) GO TO 20
      INISW=0
      QMX=QMXA(J)
      DO 10 K=1,NXS
      A(K)=AA(K,J)
      Q(K)=QQ(K,J)
   10 T(K)=TT(K,J)
      GO TO 40
   20 INISW=1
      QMX=0.0
      DO 30 J=1,NXS
      A(J)=0.0
      Q(J)=0.0
   30 T(J)=0.0
      IF(ISUN.EQ.0) GO TO 40
      INISW=0
      IF(TYPE.GT.5) GO TO 40
      PQT=PROLST
      AF=0.
      AT=CDA(NCRSEG)
      DO 33 J=1,3
      IF(UPA(I,J).EQ.0) GO TO 33
      IJ=UPA(I,J)-NOFSEG
      AF=AF+CDA(IJ)
   33 CONTINUE
      AL=CDA(I)
      ADIF=AL-AF
      AQUO=ADIF/(NXS-1)
      DO 35 J=1,NXS
      Q(J)=(AF/AT)*PQT
      A(J)=(ALP1*Q(J))**CM1
      AF=AF+AQUO
   35 CONTINUE
   40 CONTINUE
C***
C***DETERMINE AND ACCUMULATE UPSTREAM INPUT TO SEGMENT I***
C***
      IF(UPSW.EQ.0) GO TO 200
      IF(JM.NE.UPA(I,1)) GO TO 70
      LRPD=NDELSA(JM)
      IF(LRPD.EQ.NDELS) GO TO 82
      JIN=LRPD
      JOUT=NDELS
      ASSIGN 82 TO KSM
      GO TO 900
   70 IC=UPA(I,1)
      LRPD=NDELSA(IC)
      IV=IC+1
C     WRITE(1,*)'ROUTE IC',IC,IV
      IF(ISIM.LT.3) GO TO 80
      IV=IC+1+NOFSEG+NCRSEG
      READ(17,REC=IV)(TA(JT),JT=1,LRPD)
      IV=IC+1+NOFSEG
   80 READ(17,REC=IV)(QA(JQ),JQ=1,LRPD)
      IF(LRPD.EQ.NDELS) GO TO 82
      JIN=LRPD
      JOUT=NDELS
      ASSIGN 82 TO KSM
      GO TO 900
   82 DO 85 J=1,NDELS
   85 INQ(J)=QA(J)
      IF(ISIM.LT.3) GO TO 90
      DO 87 J=1,NDELS
      INS(J)=TA(J)
   87 CONTINUE
   90 IF(UPA(I,2).EQ.0) GO TO 200
      IF(UPA(I,2).EQ.UPA(I,1)) GO TO 100
      IC=UPA(I,2)
      LRPD=NDELSA(IC)
      IV=IC+1
      IF(ISIM.LT.3) GO TO 95
      IV=IC+1+NOFSEG+NCRSEG
      READ(17,REC=IV)(TA(JT),JT=1,LRPD)
      IV=IC+1+NOFSEG
   95 READ(17,REC=IV)(QA(JQ),JQ=1,LRPD)
      IF(LRPD.EQ.NDELS) GO TO 100
      JIN=LRPD
      JOUT=NDELS
      ASSIGN 100 TO KSM
      GO TO 900
  100 DO 107 J=1,NDELS
      IF(ISIM.LT.3) GO TO 105
      IF(INQ(J).GT.0..OR.QA(J).GT.0.) GO TO 104
      INS(J)=0.
      GO TO 105
  104 INS(J)=(INS(J)*INQ(J)+TA(J)*QA(J))/(INQ(J)+QA(J))
  105 INQ(J)=INQ(J)+QA(J)
  107 CONTINUE
  110 IF(UPA(I,3).EQ.0) GO TO 200
      IF(UPA(I,3).EQ.UPA(I,2)) GO TO 120
      IC=UPA(I,3)
      LRPD=NDELSA(IC)
      IV=IC+1
      IF(ISIM.LT.3) GO TO 115
      IV=IC+1+NOFSEG+NCRSEG
      READ(17,REC=IV)(TA(JT),JT=1,LRPD)
      IV=IC+1+NOFSEG
  115 READ(17,REC=IV)(QA(JQ),JQ=1,LRPD)
      IF(LRPD.EQ.NDELS) GO TO 120
      JIN=LRPD
      JOUT=NDELS
      ASSIGN 120 TO KSM
      GO TO 900
  120 DO 130 J=1,NDELS
      IF(ISIM.LT.3) GO TO 125
      IF(INQ(J).GT.0..OR.QA(J).GT.0.) GO TO 124
      INS(J)=0.
      GO TO 125
  124 INS(J)=(INS(J)*INQ(J)+TA(J)*QA(J))/(INQ(J)+QA(J))
  125 INQ(J)=INQ(J)+QA(J)
  130 CONTINUE
C***
C***DETERMINE AND ACCUMULATE LATERAL INFLOWS TO SEGMENTS***
C***
  200 IF(RB.EQ.0) GO TO 250
      IRU=KRU(RB)
      KG=KGW(IRU)
      KSS=KRES(IRU)
      LATSW=1
      LRPD=NDELSA(RB)
      IF(ISIM.LT.3) GO TO 210
      IV=RB+1+NOFSEG
      READ(17,REC=IV)(TA(JT),JT=1,LRPD)
  210 IV=RB+1
      READ(17,REC=IV)(QA(JQ),JQ=1,LRPD)
      IF(LRPD.EQ.NDELS) GO TO 230
      JIN=LRPD
      JOUT=NDELS
      ASSIGN 230 TO KSM
      GO TO 900
  230 IP=1
      SAV=15.
      SS1C=0.
      SS2C=0.
      DO 240 J=1,NDELS
      IF(ISUN.NE.1) GO TO 240
      IF(SAV.GE.DTM) GO TO 236
      IF(SAV.LT..001) GO TO 234                                         0485LS
      IF(DTM.LE.15.)GO TO 233
      SS1=0.
      SS2=0.
      IX1=DTM/15.
      DO 232 KX1=1,IX1
      SS1=SS1+UNBAS(KG,IP)
      SS2=SS2+UNRES(KSS,IP)
      IP=IP+1
  232 CONTINUE
      SS1C=((SS1/DTM)*IM2FS)*DXA(RB)*(NXSA(RB)-1)
      SS2C=((SS2/DTM)*IM2FS)*DXA(RB)*(NXSA(RB)-1)
      GO TO 240
  233 SO=SAV/DTM
      SS1=(SO*UNBAS(KG,IP))+((1.-SO)*UNBAS(KG,IP+1))
      SS2=(SO*UNRES(KSS,IP))+((1.-SO)*UNRES(KSS,IP+1))
      SAV=15.-(DTM-SAV)
      IP=IP+1
      GO TO 238
  234 IP=IP+1
      SAV=15.
  236 SAV=SAV-DTM
      SS1=UNBAS(KG,IP)
      SS2=UNRES(KSS,IP)
  238 SS1C=((SS1/15.)*IM2FS)*DXA(RB)*(NXSA(RB)-1)
      SS2C=((SS2/15.)*IM2FS)*DXA(RB)*(NXSA(RB)-1.)
  240 QLAT(J)=QA(J)+SS1C+SS2C
      IF(ISIM.LT.3) GO TO 250
      DO 245 J=1,NDELS
      TLAT(J)=TA(J)
  245 CONTINUE
  250 IF(LB.EQ.0) GO TO 300
      IRU=KRU(LB)
      KG=KGW(IRU)
      KSS=KRES(IRU)
      LATSW=1
      LRPD=NDELSA(LB)
      IF(ISIM.LT.3) GO TO 260
      IV=LB+1+NOFSEG
      READ(17,REC=IV)(TA(JT),JT=1,LRPD)
  260 IV=LB+1
C     WRITE(72,2005)IV
      READ(17,REC=IV)(QA(JQ),JQ=1,LRPD)
C     WRITE(72,2006)(QA(JQ),JQ=1,NDELS)
      IF(LRPD.EQ.NDELS) GO TO 270
      JIN=LRPD
      JOUT=NDELS
      ASSIGN 270 TO KSM
      GO TO 900
  270 IP=1
      SAV=15.
      SS1C=0.
      SS2C=0.
      IF(RB.NE.0) GO TO 275
      DO 274 J=1,NDELS
      QLAT(J)=0.0
      TLAT(J)=0.0
  274 CONTINUE
  275 DO 280 J=1,NDELS
      IF(ISUN.NE.1) GO TO 280
      IF(SAV.GE.DTM) GO TO 277
      IF(SAV.LT..001) GO TO 276                                         0485LS
      IF(DTM.LE.15.) GO TO 2750
      SS1=0.
      SS2=0.
      IX1=DTM/15.
      DO 2751 KX1=1,IX1
      SS1=SS1+UNBAS(KG,IP)
      SS2=SS2+UNRES(KSS,IP)
      IP=IP+1
 2751 CONTINUE
      SS1C=((SS1/DTM)*IM2FS)*DXA(LB)*(NXSA(LB)-1)
      SS2C=((SS2/DTM)*IM2FS)*DXA(LB)*(NXSA(LB)-1)
      GO TO 280
 2750 SO=SAV/DTM
      SS1=(SO*UNBAS(KG,IP))+((1.-SO)*UNBAS(KG,IP+1))
      SS2=(SO*UNRES(KSS,IP))+((1-SO)*UNRES(KSS,IP+1))
      SAV=15.-(DTM-SAV)
      IP=IP+1
      GO TO 278
  276 IP=IP+1
      SAV=15.
  277 SAV=SAV-DTM
      SS1=UNBAS(KG,IP)
      SS2=UNRES(KSS,IP)
  278 SS1C=((SS1/15.)*IM2FS)*DXA(LB)*(NXSA(LB)-1.)
      SS2C=((SS2/15.)*IM2FS)*DXA(LB)*(NXSA(LB)-1.)
  280 QLAT(J)=QLAT(J)+QA(J)+SS1C+SS2C
      IF(ISIM.LT.3) GO TO 300
      DO 282 J=1,NDELS
      TLAT(J)=TLAT(J)+TA(J)
  282 CONTINUE
C***
C***
C***PRINT INPUTS TO SEGMENTS IF PRTIN=1***
C***
  300 IF(PRTIN.EQ.0) GO TO 350
      QSIO=1
      QSQS=1
      IF(PRTIN.EQ.2) QSQS=3
      IF(PRTIN .EQ. 3) QSQS = 5                                          1083KF
      QSID=PCRID
      QSRPD=NDELS
      WRITE(18)QSWY,QSMO,QSDY,QSIO,QSQS,QSID,QSRPD
      WRITE(18)(INQ(J),J=1,NDELS)
      IF(ISIM.LT.3) GO TO 350
      QSQS=2
      IF(PRTIN.EQ.2) QSQS=4
      IF(PRTIN .EQ. 3) QSQS = 6                                          1083KF
      WRITE(18)QSWY,QSMO,QSDY,QSIO,QSQS,QSID,QSRPD
      WRITE(18)(INS(J),J=1,NDELS)
C***
  350 IF(NCRSEG.EQ.0)GO TO 1000
      IF(TYPE.NE.7) GO TO 355
      DO 359 J=1,NDELS
      QA(J)=INQ(J)
      IF(ISIM.EQ.3) TA(J)=INS(J)
  359 CONTINUE
      IF(I.NE.NCRSEG) GO TO 651
      GO TO 370
  355 IF(TYPE.LT.8) GO TO 356
      CALL RESVRU(NDELS,NXS,TYPE)
      GO TO 370                                                          GHL1290
  356 IF(TYPE.GE.5) GO TO 700
      DO 360 K=1,NDELS
  360 QA(K)=0.0
      IF(ISIM.LT.3) GO TO 370
      DO 365 K=1,NDELS
  365 TA(K)=0.0
  370 DO 650 JNS=1,NS
      ISS=IS+JNS-1
      TB=DFH(2*JNS-1)
      TE=DFH(2*JNS)
      IF(JNS.EQ.1) GO TO 400
      IF(TB.EQ.DFH(2*JNS-2)) GO TO 400
      IF(INISW.EQ.1) GO TO 380
      INISW=1
      QMX=0.0
  380 IF(TYPE.GE.7) GO TO 400                                            GHL1290
      DO 385 J=1,NXS
      A(J)=0.0
  385 Q(J)=0.0
      IF(ISIM.LT.3) GO TO 400
      DO 390 J=1,NXS
  390 T(J)=0.0
  400 RT=TB
      K=TB/DTM
  410 IF(RT.GE.TE) GO TO 650
      RT=RT+DTM
      K=K+1
      IF(TYPE.GE.7) GO TO 495                                            GHL1290
      IF(UPSW.EQ.1) GO TO 420
      QPM=0.0
      TPM=0.0
      GO TO 430
  420 QPM=INQ(K)
      IF(ISIM.EQ.3) TPM=INS(K)
  430 IF(LATSW.EQ.1) GO TO 440
      QL=0.0
      TL=0.0
      GO TO 450
  440 QL=QLAT(K)
      IF(ISIM.EQ.3) TL=TLAT(K)
  450 IF(QMX.LE.THRES.AND.QPM.LT..00001.AND.QL.LT..00001) GO TO 550     0485LS
      APM=0.0
      IF(QPM.GT.0.0) APM=(ALP1*QPM)**CM1
      AM=A(1)
      QM=Q(1)
      TM=T(1)
      INISW=0
      A(1)=APM
      Q(1)=QPM
      T(1)=TPM
C***
C***CHANNEL ROUTING***
C***
      QMX=0.0
      DO 490 J=2,NXS
      AJ=A(J)
      QJ=Q(J)
      THETA=0.0
      IF(AJ.GT.0.0) THETA=DTDXM*QJ/AJ
      IF(THETA.LT.1.0) GO TO 470
      QPJ=QPM+QL*DX-DXDT*(APM-AM)
      IF(QPJ.LE.0.0) GO TO 465
      APJ=(ALP1*QPJ)**CM1
      GO TO 480
  465 APJ=0.0
      QPJ=0.0
      GO TO 480
  470 APJ=AJ+QL*DTS+DTDX*(QM-QJ)
      IF(APJ.LE.0.0) GO TO 475
      QPJ=ALP*(APJ**CM)
      GO TO 480
  475 APJ=0.0
      QPJ=0.0
  480 IF(QPJ.GT.QMX) QMX=QPJ
  481 IF(ISIM.LT.3) GO TO 485
C***
C***SEDIMENT ROUTING ***
C***
      TJ=T(J)
      IF(APJ.GT.0.) GO TO 483
      TPJ=0.
      GO TO 484
  483 CADT=(TJ*AJ)/DTS
      CQDX=(TPM*QPM)/DX
      ADTQDX=(APJ/DTS)+(QPJ/DX)
      TPJ=(TL+CADT+CQDX)/ADTQDX
  484 TPM=TPJ
      TM=TJ
      T(J)=TPJ
  485 AM=AJ
      QM=QJ
      APM=APJ
      QPM=QPJ
      A(J)=APJ
      Q(J)=QPJ
  490 CONTINUE
      QA(K)=QPJ
      IF(ISIM.EQ.3) TA(K)=TPJ
  495 IF(I.NE.NCRSEG) GO TO 410
      IF(QA(K).GT.BPK(ISS)) BPK(ISS)=QA(K)
      PQ(ISS)=PQ(ISS)+DTM*60.*QA(K)
      RQD=RQD+QA(K)*DTM*60.
      IF(ISIM.EQ.3) PSED(ISS)=PSED(ISS)+(DTM*60.*QA(K)*TA(K)*.0011023)
      GO TO 410
  550 QA(K)=0.0
      IF(ISIM.EQ.3) TA(K)=0.0
      IF(INISW.EQ.1) GO TO 410
      INISW=1
      QMX=0.0
      DO 555 J=1,NXS
      A(J)=0.0
  555 Q(J)=0.0
      IF(ISIM.LT.3) GO TO 410
      DO 556 J=1,NXS
  556 T(J)=0.0
      GO TO 410
  650 CONTINUE
  651 IF(RITE.NE.1) GO TO 700
      IC=NOFSEG*(ISIM-1)+I
      IV=IC+1
      WRITE(17,REC=IV)(QA(JQ),JQ=1,NDELS)
      IF(I.NE.NCRSEG.OR.IUOUT.NE.1) GO TO 660
      UVYR=IWY
      IF(IMO.GE.10) UVYR=IWY-1
      UVPARM=60
      UVSTAT=11
      UVMO=IMO
      UVDY=IDY
      UVRPD=NDELS
      UVRDG1=1
      UVNRDG=NDELS
      UVNVAL=999999.
      WRITE(19,655)SCODE,STAIDC(8), UVPARM,UVYR,UVMO,UVDY,
     1UVSTAT,UVRPD,UVRDG1,UVNRDG,UVNVAL,(QA(JQ),JQ=1,NDELS)
  660 CONTINUE                                                          XXXXXXXX
C     WRITE(72,4999)I,IC,IV,NDELS
C     IF(I.EQ.1) WRITE(72,5000)(QA(JQ),JQ=1,NDELS)
      IF(ISIM.LT.3) GO TO 700
      IC=IC+NCRSEG
      IV=IC+1
      WRITE(17,REC=IV)(TA(JT),JT=1,NDELS)
  700 IF(PRTOUT.EQ.0) GO TO 800
      IF(DTOS.EQ.DTM) GO TO 710
      IPO=1
      JIN=NDELS
      JOUT=ODELS
      ASSIGN 710 TO KSM
      GO TO 900
  710 QSIO=2
      QSQS=1
      IF(PRTOUT.EQ.2) QSQS=3
      IF(PRTOUT .EQ. 3) QSQS = 5                                         1083KF
      QSID=PCRID
      QSRPD=ODELS
      IF(IPO.EQ.0)GO TO 714
      DO 712 ISM=1,ODELS
  712 SMTA(ISM)=SMTA(ISM)*35.311
      GO TO 720
  714 DO 715 ISM=1,ODELS
      SMQA(ISM)=QA(ISM)
      IF(ISIM.EQ.3) SMTA(ISM)=TA(ISM)*35.311
  715 CONTINUE
  720 IPO=0
      WRITE(18)QSWY,QSMO,QSDY,QSIO,QSQS,QSID,QSRPD
      WRITE(18)(SMQA(J),J=1,ODELS)
      IF(ISIM.LT.3) GO TO 800
      QSQS=2
      IF(PRTOUT.EQ.2) QSQS=4
      IF(PRTOUT .EQ. 3) QSQS = 6                                         1083KF
      WRITE(18)QSWY,QSMO,QSDY,QSIO,QSQS,QSID,QSRPD
      WRITE(18)(SMTA(J),J=1,ODELS)
  800 IF(SFL.EQ.0) GO TO 1000
      J=I+NOFSEG
      QMXA(J)=QMX
      DO 810 K=1,NXS
      AA(K,J)=A(K)
      QQ(K,J)=Q(K)
  810 TT(K,J)=T(K)
      GO TO 1000
C***
  900 DO 901 ISM=1,1440
      SMQA(ISM)=0.
      SMTA(ISM)=0.
  901 CONTINUE
      SMQA(1)=QA(1)
      IF(ISIM.EQ.3) SMTA(1)=TA(1)
      DO 920 J=2,JIN
      SMQA(J)=QA(J)+SMQA(J-1)
      IF(ISIM.EQ.3) SMTA(J)=TA(J)+SMTA(J-1)
  920 CONTINUE
      TIN=1440./JIN
      TOUT=1440./JOUT
      TD=0.0
      FAC=TIN/TOUT
      DO 950 J=1,JOUT
      TD=TD+TOUT
      K=TD/TIN
      TU=K*TIN
      F2=(TD-TU)/TIN
      IF(F2.GE..01) GO TO 940
  930 SMQA(J)=SMQA(K)
      IF(ISIM.EQ.3) SMTA(J)=SMTA(K)
      GO TO 950
  940 F1=(1.0-F2)
      SMQA(J)=SMQA(K)*F1+SMQA(K+1)*F2
  950 CONTINUE
      DO 960 JJ=2,JOUT
      J=JOUT-JJ+2
      SMQA(J)=(SMQA(J)-SMQA(J-1))*FAC
      IF(ISIM.EQ.3) SMTA(J)=(SMTA(J)-SMTA(J-1))*FAC
  960 CONTINUE
      SMQA(1)=SMQA(1)*FAC
      IF(ISIM.EQ.3) SMTA(1)=SMTA(1)*FAC
      IF(IPO.EQ.1) GO TO 980
      DO 970 ISM=1,NDELS
      QA(ISM)=SMQA(ISM)
      IF(ISIM.EQ.3) TA(ISM)=SMTA(ISM)
  970 CONTINUE
  980 GO TO KSM, (90,82,100,120,230,270,710)
 1000 CONTINUE
C     WRITE(1,*)'ROUTE 1000 IC',IC
      IF(ISIM.LT.3) GO TO 3000
      IPY=QSWY
      IF(QSMO.GE.10) IPY=QSWY-1
      WRITE(43,2000)QSMO,QSDY,IPY,NOFSEG
      WRITE(43,2100)(TSUW(IOF),IOF=1,NOFSEG)
 3000 RETURN
      END
C
C
C
      SUBROUTINE   RESVRU
     I                   (NDELS,NXS,TYPE)
C
C     + + + PURPOSE + + +
C     ?????
C
C     + + + DUMMY ARUGMENTS + + +
      INTEGER   NDELS, NXS, TYPE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NDELS  - ?????
C     NXS    - ?????
C     TYPE   - ?????
C
C     + + + COMMONS + + +
      INCLUDE 'cuntrf.inc'
      INCLUDE 'cstor.inc'
      INCLUDE 'cdfhsg.inc'                                                LS691
C
C     + + + LOCAL VARIALBES + + +
      INTEGER   K, J, L, JJ
      REAL      IN1, IN2, DTR, DTR1, DTR12, SUM, S2O2, AVIN,
     #          Q2, XKT, C2, Q2SUM, Q1, S1O1, ST                         KF1191
C
C     + + + INTRINSICS + + +
      INTRINSIC   FLOAT, EXP
C
C     + + + END SPECIFICATIONS + + +
C
      K=NXS
      DTR=FLOAT(NDELS)
      DTR1=1./DTR
      SUM=0.
      Q2SUM = 0.                                                         LS691
      IF(RFL.EQ.0) THEN                                                  LS691
        Q2=QRO(K)                                                        LS691
        IN2=DIN1(K)                                                      LS691
      END IF                                                             LS691
      IF(TYPE.EQ.9) GO TO 200
C     S2O2=(STO(K)*DTR)-(QRO(K)*.5)                                      LS691
      DO 100 J=1,NDELS
      Q1=Q2                                                              LS691
      S1O1=(STO(K)*DTR)-(Q1*.5)                                          LS691
      IN1=IN2
      IN2=INQ(J)
      SUM=SUM+IN2
      AVIN=(IN1+IN2)*.5
      S2O2=S1O1+AVIN                                                     LS691
      L=NSOS(K)
      IF(NDELS.EQ.288) GO TO 40
      DO 20 JJ=2,L
      IF(S2O2.LT.WV15(K,JJ)) GO TO 30
   20 CONTINUE
      JJ=L
   30 Q2=S15(K,JJ)*S2O2+C15(K,JJ)
         GO TO 70
   40 DO 50 JJ=2,L
      IF(S2O2.LT.WV5(K,JJ)) GO TO 60
   50 CONTINUE
      JJ=L
   60 Q2=S5(K,JJ)*S2O2+C5(K,JJ)
   70 ST=S2O2-(Q2*.5)                                                    LS691
      IF(ST.LT.0.) THEN                                                  LS691
        Q2=S2O2+(Q1*.5)                                                  LS691
        ST=0.                                                            LS691
      END IF                                                             LS691
      STO(K)=ST*DTR1                                                     LS691
   80 QA(J)=Q2
      Q2SUM=Q2SUM+Q2                                                     LS691
  100 CONTINUE
C******2 LINES DELETED                                                   LS691
      GO TO 300
  200 XKT=RCS(K)*DTR1
      DTR12=DTR1*.5
      DO 250 J=1,NDELS
      IN1=IN2
      IN2=INQ(J)
      SUM=SUM+IN2
      AVIN=(IN1+IN2)*DTR12
      C2=1.-EXP(-XKT)
       Q2=(AVIN*(1.-(C2/XKT)))+(STO(K)*C2)
      STO(K)=STO(K)+AVIN-Q2
      QA(J)=Q2*DTR
  250 CONTINUE
      QRO(K)=Q2*DTR
  300 DIN1(K)=SUM*DTR1
      QRO(K)=Q2SUM*DTR1                                                  LS691
      RETURN
      END
