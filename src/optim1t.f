C
C
C
      SUBROUTINE   OPINIT
     O                   (B4, U, N, IERR)
C
C     + + + PURPOSE + + +
C     ?????
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   N, IERR
      REAL      B4, U(2)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     B4     - ?????
C     U      - ?????
C     N      - ?????
C     IERR   - ?????
C
C     + + + COMMONS + + +
      INCLUDE 'cbs.inc'
      INCLUDE 'copt.inc'
      INCLUDE 'csent.inc'
      INCLUDE 'csent1.inc'
      INCLUDE 'cvol.inc'
      INCLUDE 'csent3.inc'
      INCLUDE 'copt2.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   JC(6), IC, L, IDSW, NVR, J, I, IER, NUMBER, NT,
     #          NPARSW, ISW, NUIT
      REAL      VB(20,20), GOE
C
C     + + + INTRINSICS + + +
      INTRINSIC   ALOG
C
C     + + + EXTERNALS + + +
      EXTERNAL   CHECK, MATINV, PARAM, BDRY
C
C     + + + DATA INITIALIZATIONS + + +
      DATA JC/2H04,2H05,2H06,2H07,2H08,2H09/                            LS110383
C
C     + + + INPUT FORMATS + + +
    1 FORMAT(A2,8X,4I5)                                                 LS110383
    4 FORMAT(A2,8X,I5,F5.2,I5)                                          LS110383
   21 FORMAT(A2,8X,35I2)                                                LS110483
  810 FORMAT(A2,8X,4I5,F5.2,2F5.1)                                      LS110483
  811 FORMAT(A2,8X,35I2,(10X,35I2))                                     LS110483
  812 FORMAT(A2,8X,14F5.2,(10X,14F5.2))                                 LS110483
C
C     + + + OUTPUT FORMATS + + +
    2 FORMAT('1ROSENBROCK OPTIMIZATION',/,                              KD1291
     1' -----------------------')                                       KD1291
    3 FORMAT('0STARTING SET',   ' HAS BEEN REJECTED BECAUSE X(',I2,     KD1291
     1')LIES OUTSIDE THE CONSTRAINTS')                                  KD1291
    5 FORMAT('1GAUSS-NEWTON OPTIMIZATION',/,                            KD1291
     1' -------------------------')                                     KD1291
    6 FORMAT('1SENSITIVITY ANALYSIS',/,                                 KD1291
     1' --------------------')                                          KD1291
    7 FORMAT(4X,'NVAR=',15I4)                                           KD1291
   10 FORMAT(///,' THE FOLLOWING PARAMETERS ARE TO BE ADJUSTED:',       KD1291
     1/,5X,'LOP=',15I4)                                                 KD1291
   11 FORMAT(3X,'ILOPL=',15I4)                                          KD1291
   14 FORMAT('0THE STARTING PARAMETER VALUES ARE',/,(1X,9E14.6))        KD1291
   15 FORMAT('0','THE INITIAL STEP SIZE INCREMENTS ARE',/,(1X,9E14.6))  KD1291
   30 FORMAT(6X,'IOPT',I5,26X,'NTRY',I5)                                KD1291
   31 FORMAT(6X,'ISEN',I5,26X,'PINC',F5.3)                              KD1291
   32 FORMAT(8X,'NV',I5,24X,'IPRIOR',I5,/,6X,'IOBF',I5,24X,'ITRANS',I5, KD1291
     1/,7X,'MFS',I5,27X,'MFN',I5)                                       KD1291
  403 FORMAT('0','THE LOWER CONSTRAINTS ARE',/,(1X,9E14.6))             KD1291
  405 FORMAT('0','THE UPPER CONSTRAINTS ARE',/,(1X,9E14.6))             KD1291
900   FORMAT(1X,'NUMBER= ',I5,5X,'TERMINATE IN OPINIT')                 KD1291
  955 FORMAT('0THE TRANSFORMED LOWER CONSTRAINTS ARE',/,(1X,9E14.6))    KD1291
  956 FORMAT('0THE TRANSFORMED UPPER CONSTRAINTS ARE',/,(1X,9E14.6))    KD1291
  972 FORMAT(///,' NORMAL PRIOR INFORMATION',//,' COVARIANCE MATRIX:')  KD1291
  973 FORMAT(15F8.4)
  990 FORMAT('0THE TRANSFORMED INITIAL STEP SIZE INCREMENTS ARE',/,     KD1291
     1(1X,9E14.6))
 1000 FORMAT(5X,'*****ERROR - NUMBER OF VARIABLES FOR OPTIMIZATION',    LS070883
     +/,10X,'LIMITED TO 20, FOUND ',I5)                                 LS070883
 1020 FORMAT(5X,'*****ERROR - NUMBER OF VARIABLES FOR SENSITIVITY',     LS070883
     +/,10X,'ANALYSIS IS LIMITED TO 20, FOUND ',I5)                     LS070883
 1040 FORMAT(5X,'*****WARNING - STEPSIZE FOR SENSITIVITY ANALYSIS',     LS070883
     +/,10X,'SHOULD BE LESS THAN 1, FOUND ',F6.3)                       LS070883
 1090 FORMAT(5X,'*****ERROR - LOP=',I5,' IS NOT A VALID PARAMETER #')   LS070883
 1110 FORMAT(5X,'*****WARNING - STEPSIZE',F6.3,' SHOULD BE <1')         LS070883
C1220 FORMAT(5X,'*****ERROR - HRU NUMBER ',I5,' NOT BEING USED IN',     LS070883
 1280 FORMAT(5X,'*****ERROR - TESTNO(',I2,') SHOULD BE 0 OR 1')         LS070883
C    +/,10X,'SIMULATION')                                               LS070883
C
C     + + + END SPECIFICATIONS + + +
C
      IF(IOPT.EQ.0) GO TO 1271                                          LS110383
      READ(37,1) IC,NV,NTRY,IPRIOR                                      LS110383
      IF(IC.NE.JC(1)) CALL CHECK(7,IC,JC(1))                            LS110383
      IF(IOPT.NE.0)WRITE(72,1)IC,NV,NTRY,IPRIOR                          LS110383
      IF(NV.LE.20) GO TO 1010                                           LS070883
      IERR=1
      WRITE(72,1000) NV                                                  LS070883
 1010 CONTINUE
      B4=-1.
      GO TO 1050                                                        LS110783
 1271 IF(IOPT.EQ.0)READ(37,4)IC,NV,PINC,IPRIOR                          LS110383
      IF(IC.NE.JC(1)) CALL CHECK(7,IC,JC(1))                            LS110383
      IF(IOPT.EQ.0)WRITE(72,4)IC,NV,PINC,IPRIOR                          LS110383
      IF(NV.LE.20) GO TO 1030                                           LS070883
      IERR=1                                                            LS070883
      WRITE(72,1020) NV                                                  LS070883
 1030 IF(PINC.LT.1.) GO TO 1050                                         LS070883
      WRITE(72,1040) PINC                                                LS070883
 1050 CONTINUE                                                          LS070883
      DO 200 L=1,NV
      READ(37,810) IC,LOP(L),ILOPL(L),NVAR(L),IDSW,P(L),GU(L),HU(L)     LS110383
      IF(IC.NE.JC(2)) CALL CHECK(7,IC,JC(2))                            LS110383
      WRITE(72,810) IC,LOP(L),ILOPL(L),NVAR(L),IDSW,P(L),GU(L),HU(L)     LS110383
 1070 IF(LOP(L).LE.30) GO TO 1100                                       LS110383
      IF(LOP(L).GE.50.AND.LOP(L).LE.54) GO TO 1100                      LS070883
      IF(LOP(L).GE.60.AND.LOP(L).LE.61) GO TO 1100                      LS070883
      IF(LOP(L).GE.70.AND.LOP(L).LE.77) GO TO 1100                      LS110383
      IF(LOP(L).GE.80.AND.LOP(L).LE.86) GO TO 1100                      LS070883
      IF(LOP(L).GE.90.AND.LOP(L).LE.104) GO TO 1100                     LS070883
      IERR=1                                                            LS070883
      WRITE(72,1090) LOP(L)                                              LS070883
 1100 IF(P(L).LE.1) GO TO 1120                                          LS070883
      WRITE(72,1110) P(L)                                                LS070883
 1120 CONTINUE                                                          LS070883
      NVR=NVAR(L)
      IF(IDSW.EQ.0) GO TO 201
      READ(37,811)IC , (NDVR(L,J),J=1,NVR)                              LS110483
      IF(IC.NE.JC(3)) CALL CHECK(7,IC,JC(3))                            LS110483
      WRITE(72,811)IC ,(NDVR(L,J),J=1,NVR)                               LS110483
C1210 DO 1250 J=1,NVR                                                   LS070883
C     IF(NDVR(L,J).LE.NRU) GO TO 1250                                   LS070883
C     IERR=1                                                            LS070883
C     WRITE(72,1220) NDVR(L,J)                                           LS070883
C1250 CONTINUE                                                          LS070883
      GO TO 200
  201 DO 202 J=1,NVR
  202 NDVR(L,J)=J
  200 CONTINUE
      DO 205 L=1,NV                                                     GL1085
      GUS(L)=GU(L)                                                      GL1085
      HUS(L)=HU(L)                                                      GL1085
  205 CONTINUE                                                          GL1085
      IF(IOPT.LT.0)PINC=P(1)
      IF(IOPT.LT.3.AND.ISEN.LT.3) GO TO 1305
      READ (37,21) IC , (TESTNO(I),I=1,NST)                             LS110483
      IF(IC.NE.JC(4)) CALL CHECK(7,IC,JC(4))                            LS110483
      WRITE(72,21)IC ,(TESTNO(I),I=1,NST)                                LS110483
 1270 DO 1300 J=1,NST                                                   LS070883
      IF(TESTNO(J).EQ.0.OR.TESTNO(J).EQ.1) GO TO 1300                   LS070883
      WRITE(72,1280) J                                                   LS070883
      IERR=1                                                            LS070883
 1300 CONTINUE                                                          LS070883
 1305 CONTINUE
      IF(IPRIOR.EQ.0) GO TO 210
      DO 211 L=1,NV
  211 READ(37,812)IC ,(VB(L,J),J=1,NV)                                  LS110483
      IF(IC.NE.JC(5)) CALL CHECK(7,IC,JC(5))                            LS110483
      CALL MATINV(VB,VBIN,NV,IER)                                       XXXXXXXX
  210 IF(ISEN.NE.2.AND.IOPT.NE.2) GO TO 1350                            LS110483
      READ(37,810)IC,IP,IQ                                              LS110483
      IF(IC.NE.JC(6)) CALL CHECK(7,IC,JC(6))                            LS110483
      IR=IP+IQ
 1350 IF(IOPT.GT.0)WRITE(72,2)                                           LS110483
      IF(IOPT.LT.0)WRITE(72,5)
      IF(IOPT.EQ.0)WRITE(72,6)
      IF(IOPT.NE.0)WRITE(72,30)IOPT,NTRY
      IF(IOPT.EQ.0)WRITE(72,31)ISEN,PINC
      WRITE(72,32)NV,IPRIOR,IOBF,ITRANS,MFS,MFN
      WRITE(72,10)(LOP(J),J=1,NV)
      WRITE(72,11)(ILOPL(J),J=1,NV)
      WRITE(72,7)(NVAR(J),J=1,NV)
      IF(IOPT.EQ.0) GO TO 41
      WRITE(72,403)  (GU(I),I=1,NV)
      WRITE(72,405)  (HU(I),I=1,NV)
      NUMBER=99
      DO 940 I=1,NV
      IF(ILOPL(I).EQ.0)GO TO 940
      IF(HU(I).LE.0.)GO TO 301
      HU(I)=20.+ALOG(HU(I))
      IF(GU(I).LE.0.)GO TO 950
      GU(I)=20.+ALOG(GU(I))
      GO TO 940
  950 GU(I)=0.
      IF(HU(I).LE.GU(I))GO TO 301
  940 CONTINUE
      WRITE(72,955)(GU(I),I=1,NV)
      WRITE(72,956)(HU(I),I=1,NV)
  960 DO 20 I = 1,NV
      G(I)=GU(I)
   20 H(I)=HU(I)
      IF(IOPT.LT.0) GO TO 41
      DO 9 I= 1,NV
      NT = NV + I
      GOE = 0.0001 * (H(I)-G(I))
      G(NT) = G(I) + GOE
      H(NT) = H(I) - GOE
    9 CONTINUE
      NUMBER = 1
      IF(ISIM.GT.0)GO TO 41
      DO 43 I=1,5
      DO 43 J=1,50
      X(I,J)=0.
   43 CONTINUE
   41 NPARSW=0
      CALL PARAM(NPARSW)
      IF(IOPT.EQ.0)RETURN
      WRITE(72,14) (XX(I),I=1,NV)
      WRITE(72,15) (P(I),I=1,NV)
      DO 980 I=1,NV
      IF(ILOPL(I).EQ.0)GO TO 980
      P(I)=P(I)/XX(I)
  980 CONTINUE
      WRITE(72,990)(P(I),I=1,NV)
      IF(IPRIOR.EQ.0)GO TO 970
      WRITE(72,972)
      DO 971 I=1,NV
  971 WRITE(72,973)(VB(I,J),J=1,NV)
  970 DO 505 I = 1,NV
      SX(I) = 1.0
  505 CONTINUE
      DO 606 I=1,NV
      IF((XX(I).GT.G(I)).AND.(XX(I).LT.H(I))) GO TO 606
      WRITE(72,3) I
      NUMBER=2
  606 CONTINUE
      IF(NUMBER.EQ.2) GO TO 301
      ISW = 1
      U(1) = 0.0
      U(2) = U(1)
      IF(IOPT.GT.0)CALL BDRY(XX,NUIT,G,H,NV,1,U)                        LS0287
      ISW = 2
      DO 18 I = 1,NV
      CX(I) = XX(I)
   18 CONTINUE
      N=1
      RETURN
  301 WRITE(72,900)NUMBER
      STOP
      END
C
C
C
      SUBROUTINE   PARAM
     O                  ( NPARSW )
C
C     + + + PURPOSE + + +
C     ?????
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NPARSW
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NPARSW - ?????
C
C     + + + COMMONS + + +
      INCLUDE 'cbs.inc'
      INCLUDE 'cdpr.inc'
      INCLUDE 'cet.inc'
      INCLUDE 'copt.inc'
      INCLUDE 'cpcrch.inc'
      INCLUDE 'crd.inc'
      INCLUDE 'csm.inc'
      INCLUDE 'csno.inc'
      INCLUDE 'cwx.inc'
      INCLUDE 'csent.inc'
      INCLUDE 'cvol.inc'
      INCLUDE 'csent1.inc'
      INCLUDE 'csnop.inc'
      INCLUDE 'csncv.inc'
      INCLUDE 'copt2.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   ISIGN(20), I, J, L, K, K1, LL, K0
      REAL      Z
C
C     + + + INTRINSICS + + +
      INTRINSIC   ALOG, FLOAT, EXP
C
C     + + + OUTPUT FORMATS + + +
  607 FORMAT(' SIGN ERROR IN PARAM')                                    KD1291
 8001 FORMAT(' ERROR')                                                  KD1291
C
C     + + + END SPECIFICATIONS + + +
C
      IF(NPARSW.EQ.1) GO TO 1000
      DO 40 I=1,20
      XXAVE(I)=0.
      DO 40 J=1,50
      XXDEV(I,J)=0.
   40 CONTINUE
      DO 500 L=1,NV
      K=LOP(L)
      K1=NVAR(L)
      DO 500 LL=1,K1
      J=NDVR(L,LL)
      IF(K.LT.50)GO TO 501
      IF(K.LT.60)GO TO 502
      IF(K.LT.70)GO TO 503
      IF(K.LT.80)GO TO 504
      IF(K.LT.90)GO TO 505
      GO TO 506
  501 GO TO (701,702,703,704,705,706,707,708,709,710,
     1711,712,713,714,715,716,717,718,719,720,721,722,723,724,725,726,
     2727,728,729,730),K
  502 K0=K-49
      GO TO (750,751,752,753,754),K0
  503 K0=K-59
      GO TO (760,761),K0
  504 K0=K-69
      GO TO (770,771,772,773,774,775,776,777),K0                        LS110483
  505 K0=K-79
      GO TO (780,781,782,782,782,785,786),K0
  506 K0=K-89
      GO TO (790,791,792,793,794,795,500,797,798,799,800,801,802,       GL081584
     1803,804),K0                                                       GL1085
  701 Z=COVDNS(J)
      GO TO 499
  702 Z=COVDNW(J)
      GO TO 499
  703 Z=TRNCF(J)
      GO TO 499
  704 Z=SNST(J)
      GO TO 499
  705 Z=CTX(J)
      GO TO 499
  706 Z=TXAJ(J)+2.                                                      LS103183
      GO TO 499
  707 Z=TNAJ(J)+2.                                                      LS103183
      GO TO 499
  708 Z=SMAX(J)
      GO TO 499
  709 Z=REMX(J)
      GO TO 499
  710 Z=SRX(J)
      GO TO 499
  711 Z=SCX(J)
      GO TO 499
  712 Z=SCN(J)
      GO TO 499
  713 Z=RNSTS(J)
      GO TO 499
  714 Z=RNSTW(J)
      GO TO 499
  715 Z=X(1,J)
      GO TO 499
  716 Z=X(2,J)
      GO TO 499
  717 Z=X(3,J)
      GO TO 499
  718 Z=X(4,J)
      GO TO 499
  719 Z=X(5,J)
      GO TO 499
  720 Z=D50A(J)
      GO TO 499
  721 Z=KRA(J)
      GO TO 499
  722 Z=HCA(J)
      GO TO 499
  723 Z=KFA(J)
      GO TO 499
  724 Z=MMA(J)
      GO TO 499
  725 Z=ENA(J)
      GO TO 499
  726 Z=SC1(J)
      GO TO 499
  727 Z=SEP(J)
      GO TO 499
  728 Z=DRCOR(J)
      GO TO 499
  729 Z=DSCOR(J)
      GO TO 499
  730 Z=TST(J)
      GO TO 499
  750 Z=RCF(J)
      GO TO 499
  751 Z=RCP(J)
      GO TO 499
  752 Z=RSEP(J)
      GO TO 499
  753 Z=RESMX(J)
      GO TO 499
  754 Z=REXP(J)
      GO TO 499
  760 Z=RCB(J)
      GO TO 499
  761 Z=GSNK(J)
      GO TO 499
  770 Z=TLX(J)
      GO TO 499
  771 Z=TLN(J)
      GO TO 499
  772 Z=RDM(J)
      GO TO 499
  773 Z=RDC(J)+20.                                                      GL1185
      GO TO 499
  774 Z=EVC(J)
      GO TO 499
  775 Z=PAT(J)
      GO TO 499
  776 Z=CECN(J)                                                         LS110483
      GO TO 499                                                         LS110483
  777 Z=AJMX(J)                                                         LS103184
      GO TO 499                                                         LS103184
  780 Z=ALPRA(J)
      GO TO 499
  781 Z=CMPA(J)
      GO TO 499
  785 Z=ALPRA(J+NOFSEG)
      GO TO 499
  786 Z=CMPA(J+NOFSEG)
      GO TO 499
  790 Z=CTS(J)                                                          LS110483
      GO TO 499
  791 CONTINUE
      GO TO 499
  792 Z=BST
      GO TO 499
  793 Z=SETCON
      GO TO 499
  794 Z=PARS
      GO TO 499
  795 Z=PARW
      GO TO 499
C 796 Z=CSEL                                                            GL081584
C     GO TO 499                                                         GL081584
  797 Z=RMXA
      GO TO 499
  798 Z=RMXM
      GO TO 499
  799 Z=CTW
      GO TO 499
  800 Z=EAIR
      GO TO 499
  801 Z=FWCAP
      GO TO 499
  802 Z=DENI
      GO TO 499
  803 Z=DENMX
      GO TO 499
  804 Z=AIMX(J)
  499 XXDEV(L,LL)=Z
  500 CONTINUE
      DO 605 L=1,NV
      ISIGN(L)=0
      IF(XXDEV(L,1).LT.0.) ISIGN(L)=1
  605 CONTINUE
      DO 606 L=1,NV
      K1=NVAR(L)
      DO 606 LL=1,K1
      IF(ISIGN(L).EQ.1)XXDEV(L,LL)=-XXDEV(L,LL)
      IF(XXDEV(L,LL).GE.0.)GO TO 606
      WRITE(72,607)
      STOP
  606 CONTINUE
      DO 600 L=1,NV
      K1=NVAR(L)
      DO 601 LL=1,K1
      IF(XXDEV(L,LL).LE.0.)GO TO 601
      IF(ILOPL(L).EQ.1)XXDEV(L,LL)=20.+ALOG(XXDEV(L,LL))
      XXAVE(L)=XXAVE(L)+XXDEV(L,LL)
  601 CONTINUE
      XXAVE(L)=XXAVE(L)/FLOAT(K1)
      XX(L)=XXAVE(L)
      DO 602 LL=1,K1
  602 XXDEV(L,LL)=XXDEV(L,LL)-XXAVE(L)
  600 CONTINUE
      RETURN
 1000 DO 200 L=1,NV
      K1=NVAR(L)
      XXAVE(L)=XX(L)
      DO 201 LL=1,K1
      XXDEV(L,LL)=XXAVE(L)+XXDEV(L,LL)
      IF(ILOPL(L).EQ.1)XXDEV(L,LL)=EXP(-20.+XXDEV(L,LL))
  201 CONTINUE
  200 CONTINUE
      DO 510 L=1,NV
      K=LOP(L)
      K1=NVAR(L)
      DO 510 LL=1,K1
      J=NDVR(L,LL)
      Z=XXDEV(L,LL)
      IF(ISIGN(L).EQ.1)Z=-Z
      IF(ISEN.EQ.1) GO TO 210                                           GL1085
      IF(Z.GT.HUS(L)) Z=HUS(L)                                          GL1085
      IF(Z.LT.GUS(L)) Z=GUS(L)                                          GL1085
  210 IF(K.LT.50)GO TO 511                                              GL1085
      IF(K.LT.60)GO TO 512
      IF(K.LT.70)GO TO 513
      IF(K.LT.80)GO TO 514
      IF(K.LT.90)GO TO 515
      GO TO 516
  511 GO TO (301,302,303,304,305,306,307,308,309,310,
     1311,312,313,314,315,316,317,318,319,320,321,322,323,324,325,326,
     2327,328,329,330),K
  512 K0=K-49
      GO TO (350,351,352,353,354),K0
  513 K0=K-59
      GO TO (360,361),K0
  514 K0=K-69
      GO TO (370,371,372,373,374,375,376,377),K0                        LS110483
  515 K0=K-79
      GO TO (380,381,782,782,782,385,386),K0
  516 K0=K-89
      GO TO (390,391,392,393,394,395,510,397,398,399,400,401,402,       GL081584
     1403,404),K0                                                       GL1085
  301 COVDNS(J)=Z
      GO TO 510
  302 COVDNW(J)=Z
      GO TO 510
  303 TRNCF(J)=Z
      GO TO 510
  304 SNST(J)=Z
      GO TO 510
  305 CTX(J)=Z
      GO TO 510
  306 TXAJ(J)=Z-2.                                                      LS103183
      GO TO 510
  307 TNAJ(J)=Z-2.                                                      LS103183
      GO TO 510
  308 SMAX(J)=Z
      GO TO 510
  309 REMX(J)=Z
      GO TO 510
  310 SRX(J)=Z
      GO TO 510
  311 SCX(J)=Z
      GO TO 510
  312 SCN(J)=Z
      GO TO 510
  313 RNSTS(J)=Z
      GO TO 510
  314 RNSTW(J)=Z
      GO TO 510
  315 X(1,J)=Z
      GO TO 510
  316 X(2,J)=Z
      GO TO 510
  317 X(3,J)=Z
      GO TO 510
  318 X(4,J)=Z
      GO TO 510
  319 X(5,J)=Z
      GO TO 510
  320 D50A(J)=Z
      GO TO 510
  321 KRA(J)=Z
      GO TO 510
  322 HCA(J)=Z
      GO TO 510
  323 KFA(J)=Z
      GO TO 510
  324 MMA(J)=Z
      GO TO 510
  325 ENA(J)=Z
      GO TO 510
  326 SC1(J)=Z
      GO TO 510
  327 SEP(J)=Z
      GO TO 510
  328 DRCOR(J)=Z
      GO TO 510
  329 DSCOR(J)=Z
      GO TO 510
  330 TST(J)=Z
      GO TO 510
  350 RCF(J)=Z
      GO TO 510
  351 RCP(J)=Z
      GO TO 510
  352 RSEP(J)=Z
      GO TO 510
  353 RESMX(J)=Z
      GO TO 510
  354 REXP(J)=Z
      GO TO 510
  360 RCB(J)=Z
      GO TO 510
  361 GSNK(J)=Z
      GO TO 510
  370 TLX(J)=Z
      GO TO 510
  371 TLN(J)=Z
      GO TO 510
  372 RDM(J)=Z
      GO TO 510
  373 RDC(J)=Z-20.                                                      GL1185
      GO TO 510
  374 EVC(J)=Z
      GO TO 510
  375 PAT(J)=Z
      GO TO 510
  376 CECN(J)=Z                                                         LS110483
      GO TO 510                                                         LS110483
  377 AJMX(J)=Z                                                         LS103184
      GO TO 510                                                         LS103184
  380 ALPRA(J)=Z
      ALPR1A(J)=1./Z
      GO TO 510
  381 CMPA(J)=Z
      CMP1A(J)=1./Z
      DTDXMA(J)=DTDXA(J)*Z
      GO TO 510
  385 ALPRA(J+NOFSEG)=Z
      ALPR1A(J+NOFSEG)=1./Z
      GO TO 510
  386 CMPA(J+NOFSEG)=Z
      CMP1A(J+NOFSEG)=1./Z
      DTDXMA(J+NOFSEG)=DTDXA(J+NOFSEG)*Z
      GO TO 510
  390 CTS(J)=Z                                                          LS110483
      GO TO 510
  391 CONTINUE
      GO TO 510
  392 BST=Z
      GO TO 510
  393 SETCON=Z
      GO TO 510
  394 PARS=Z
      GO TO 510
  395 PARW=Z
      GO TO 510
C 396 CSEL=Z                                                            GL081584
C     GO TO 510                                                         GL081584
  397 RMXA=Z
      GO TO 510
  398 RMXM=Z
      GO TO 510
  399 CTW=Z
      GO TO 510
  400 EAIR=Z
      GO TO 510
  401 FWCAP=Z
      GO TO 510
  402 DENI=Z
      GO TO 510
  403 DENMX=Z
      GO TO 510
  404 AIMX(J)=Z
  510 CONTINUE
      DO 950 L=1,NV
      K1=NVAR(L)
      DO 951 LL=1,K1
      IF(ILOPL(L).EQ.1)XXDEV(L,LL)=20.+ALOG(XXDEV(L,LL))
      XXDEV(L,LL)=XXDEV(L,LL)-XXAVE(L)
  951 CONTINUE
  950 CONTINUE
      RETURN
  782 WRITE(72,8001)
      STOP
      END
C
C
C
      SUBROUTINE   ROSOPT
     O                   ( IERR )
C
C     + + + PURPOSE + + +
C     ?????
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IERR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IERR   - ?????
C
C     + + + COMMONS + + +
      INCLUDE 'cplot.inc'
      INCLUDE 'cdates.inc'
      INCLUDE 'cdatop.inc'
      INCLUDE 'call.inc'
      INCLUDE 'cbs.inc'
      INCLUDE 'cdpr.inc'
      INCLUDE 'cdvdt.inc'
      INCLUDE 'cet.inc'
      INCLUDE 'copt.inc'
      INCLUDE 'csize.inc'
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
      INCLUDE 'copt2.inc'
      INCLUDE 'csncv.inc'
      INCLUDE 'cstor.inc'
C
C     + + + SAVES + + +
      INTEGER   IEND, IXT
      REAL      OBFPRV, ALP, GS, SDX(20), COR(20), VSUM
      SAVE      OBFPRV, ALP, IEND, IXT, GS, SDX, COR, VSUM
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   NSWIT, ITER, NUM, K, NUS, I, J, NXT, L, NMP, NM, NDP,
     #          IVAL, II, KSWIT, IM, MOPP, MDIFF, NN, JJ, NC, NFCC,
     #          NOEND, K1, LL, IOBFT, MFST, MFNT, NPARSW
      REAL      PHIOPT(10), OBFU(10), U(2), STMX(20), AS, UCOR, DIFFL,
     #          DIFFOP, DIFFLP, DIFFPK, HKS, ERR, B4, WITE, SIGS,
     #          XTM, UMIN
C
C     + + + INTRINSICS + + +
      INTRINSIC   ALOG, ABS, FLOAT, EXP, INT, MOD, IABS
C
C     + + +  EXTERNALS + + +
      EXTERNAL   COROPT, SENST, PARAM, OPINIT, SUB1
C
C     + + + DATA INITIALIZATIONS + + +
      DATA NSWIT/1/,ITER/1/,AS/1.1/                                     LS0287
C
C     + + + OUTPUT FORMATS + + +
8     FORMAT('0','THE INITIAL PARAMETER VALUES WERE',/,(1X,10E13.4))    KD1291
   46 FORMAT('1')                                                       KD1291
   59 FORMAT('1')                                                       KD1291
   76 FORMAT(/,' OBJ. FNC. : ',2F10.3)                                  KD1291
   79 FORMAT('1ITERATION NUMBER',I3)                                    KD1291
80    FORMAT('0','THE FINAL PARAMETER VALUES ARE',/,(1X,10E13.4))       KD1291
82    FORMAT('0','THE INITIAL AND FINAL OBJ FUNCTION VALUES ARE',/,1X,  KD1291
     12D13.5)
   83 FORMAT(////,' FINAL RESULTS FOR ITERATION',I3,/,                  KD1291
     1' CORRECTION VECTOR MULTIPLIER:',F10.3)                           KD1291
   85 FORMAT(19X,'OLD',7X,'NEW',/,' PARAMETERS :',2F10.3)               KD1291
   86 FORMAT(13X,2F10.3)
   87 FORMAT(///,' INITIAL CORRECTION VECTOR:',/,12F10.4)               KD1291
   88 FORMAT(' PARAMETER',I3,2X,                                        KD1291
     1'OUTSIDE CONSTRAINTS, ALPHA HALVED, ALP=',F10.3)                  KD1291
   90 FORMAT(10X,6I5)
   99 FORMAT(////,' CORRECTION VECTOR MULTIPLIER:',F10.3)               KD1291
  171 FORMAT(//,' MULTIPLIER LT .01; SET TO .001 AND TERMINATE')        KD1291
  172 FORMAT(' OBJ FNC INCREASED, MULTIPLIER HALVED')                   KD1291
  943 FORMAT(///,' SUMMARY OF NEW PARAMETER VALUES:')                   KD1291
  949 FORMAT(//,' AUTOREGRESSIVE, MOVING AVE. PARAMETERS:',/,10F10.3)   KD1291
  951 FORMAT(//,' LOP=',I3)                                             KD1291
  952 FORMAT(12F10.4)
  989 FORMAT('1')                                                       KD1291
C2141 FORMAT(A27,I5,3I3,5F8.2)
C
C     + + + END SPECIFICATIONS + + +
C
      IF(IOPT.GT.0) GO TO 5000
      IF(NDSN.EQ.0) GO TO 55
      IF(IOPT.LE.-3) GO TO 72
      IF(IOPT.EQ.-1) GO TO 71
      CALL COROPT(NYR,UCOR,PHI,NUM)
      OBFOPT=UCOR
      GO TO 73
   71 OBFOPT = OBF(IOBF+2*ITRANS)
      GO TO 73
   72 DO 74 K=1,10
   74 OBFU(K)=0.
      NUS=0
      DO 75 K=1,NST
      IF(TESTNO(K).EQ.0) GO TO 75
      NUS=NUS+1
      OBSRUN(NUS)=OBRO(K)
      IF(ITRANS.EQ.1)OBSRUN(NUS)=ALOG(OBSRUN(NUS))
      DIFFL=ALOG(BRO(K)/OBRO(K))
      DIFFOP=OBRO(K)-BRO(K)
      OBFU(1)=OBFU(1)+ABS(DIFFOP)
      OBFU(2)=OBFU(2)+DIFFOP*DIFFOP
      OBFU(3)=OBFU(3)+ABS(DIFFL)
      OBFU(4)=OBFU(4)+DIFFL*DIFFL
      IF(IOPT.GT.-4) GO TO 75
      DIFFLP=ALOG(OBPK(K)/BPK(K))
      DIFFPK=OBPK(K)-BPK(K)
      OBFU(5)=OBFU(5)+ABS(DIFFPK)
      OBFU(6)=OBFU(6)+DIFFPK*DIFFPK
      OBFU(7)=OBFU(7)+ABS(DIFFLP)
      OBFU(8)=OBFU(8)+DIFFLP*DIFFLP
      OBFU(9)=OBFU(9)+DIFFOP*DIFFPK
      OBFU(10)=OBFU(10)+DIFFL*DIFFLP
      OBSRUN(NUS)=OBPK(K)
      IF(ITRANS.EQ.1)OBSRUN(NUS)=ALOG(OBSRUN(NUS))
   75 CONTINUE
      IF(IOPT.EQ.-5)OBFU(9)=OBFU(2)*OBFU(6)-OBFU(9)*OBFU(9)
      IF(IOPT.EQ.-5)OBFU(10)=OBFU(4)*OBFU(8)-OBFU(10)*OBFU(10)
      IF(IOPT.EQ.-3)OBFOPT=OBFU(IOBF+2*ITRANS)
      IF(IOPT.EQ.-4)OBFOPT=OBFU(IOBF+2*ITRANS+4)
      IF(IOPT.EQ.-5)OBFOPT=OBFU(ITRANS+9)
      NOBF=NUS
   73 UCOR=OBFOPT
      IF(IPRIOR.EQ.0)GO TO 78
      VSUM=0.
      DO 106 I=1,NV
      DO 106 J=1,NV
      VSUM=VSUM+VBIN(I,J)*(XX(I)-CX(I))*(XX(J)-CX(J))
  106 CONTINUE
      VSUM=VSUM/FLOAT(NOBF)
      OBFOPT=OBFOPT*EXP(VSUM)
   78 WRITE(72,76) OBFPRV,OBFOPT
      IF(IEND.EQ.1) GO TO 192
      REWIND 8
      IF(OBFOPT.LT.OBFPRV) GO TO 150
      ALP=ALP/2.
      IF(ALP.GE..01)GO TO 168
      WRITE(72,171)
      HKS=.001
      NXT=1000
      GO TO 190
  168 WRITE(72,172)
      IXT=IXT+1
      NXT=INT(FLOAT(IXT)/FLOAT(NV))
      GO TO 170
  150 IF(OBFOPT.GE.OBFPRV-ALP*GS*(2.-1./AS)) GO TO 180
      HKS=ALP*AS
      GO TO 190
  180 HKS=ALP*ALP*GS/(OBFOPT-OBFPRV+2.*ALP*GS)
  190 DO 191 I=1,NV
  191 XX(I)=SDX(I)+HKS*COR(I)
      NREC=2
      IEND=1
      REWIND 8
      IF(ISEN.LT.3) IWSW2=2
      WRITE(72,83)ITER,HKS
      GO TO 194
  192 U(1)=-OBFOPT
      IEND=0
      IF(ITER+NXT.GE.NTRY)GO TO 53
      ERR=ABS((OBFOPT-OBFPRV)/OBFOPT)
      OBFPRV=OBFOPT
      IF(ERR.GT.0.0001) GO TO 58
   53 NDOP=1
      REWIND 8
      IF(IOPT.NE.-2)GO TO 220
      DO 54 L=1,IR
   54 PHIOPT(L)=PHI(L)
      GO TO 220
   58 ITER=ITER+1
      WRITE(72,79)ITER
      NDSN=-1
   55 ISEN=-IOPT
      IERR=0
      CALL SENST(IERR)
      IF(NDSN.EQ.0)RETURN
      IF(ITER.GT.1) GO TO 56
      B4=-1.
      NXT=0
      IXT=0
      IF(IPRIOR.EQ.0)GO TO 108
      VSUM=0.
      DO 109 I=1,NV
      DO 109 J=1,NV
      VSUM=VSUM+VBIN(I,J)*(XX(I)-CX(I))*(XX(J)-CX(J))
  109 CONTINUE
      VSUM=VSUM/FLOAT(NOBF)
      UCOR=OBFOPT
      OBFOPT=OBFOPT*EXP(VSUM)
  108 WITE=-OBFOPT
      OBFPRV=OBFOPT
   56 IF(ISEN.LT.3)NMP=NMOBF
      IF(ISEN.GE.3)NMP=1
      REWIND 8
      DO 60 I=1,NV
   60 COR(I)=0.
      DO 77 NM=1,NMP
      NDP=NOBF
      IF(ISEN.LT.3)READ(8)NDP
      IF(ISEN.LT.3)READ(8)((ASENS(IVAL,II),II=1,NV),IVAL=1,NDP),
     1(OBSRUN(IVAL),IVAL=1,NDP),(SMVOPT(IVAL),IVAL=1,NDP)
C     WRITE(72,2141)'ROSOP1,YR,MO,IWSW2,NDY,ASEN',IWY,NM,IWSW2,NDP,
C    *(ASENS(IV,1),IV=1,5)
C     WRITE(72,2141)'ROSOP1,YR,MO,IWSW2,NDY,OBSR',IWY,NM,IWSW2,NDP,
C    *(OBSRUN(II),II=1,5)
C     WRITE(72,2141)'ROSOP1,YR,MO,IWSW2,NDY,SMVO',IWY,NM,IWSW2,NDP,
C    *(SMVOPT(II),II=1,5)
      DO 77 J=1,NDP
      DO 77 I=1,NV
      DO 77 K=1,NV
      COR(I)=COR(I)+ZINV(I,K)*ASENS(J,K)*(OBSRUN(J)-SMVOPT(J))
   77 CONTINUE
      IF(IPRIOR.EQ.0)GO TO 105
      SIGS=UCOR/FLOAT(NOBF)
      DO 104 I=1,NV
      DO 104 J=1,NV
      DO 104 K=1,NV
      COR(I)=COR(I)+SIGS*ZINV(I,K)*VBIN(J,K)*(CX(J)-XX(J))
  104 CONTINUE
  105 ALP=1.
      WRITE(72,87)(COR(I),I=1,NV)
   93 KSWIT=0
      DO 84 I=1,NV
      XTM=XX(I)+ALP*COR(I)
      IF(XTM.GT.G(I).AND.XTM.LT.H(I))GO TO 84
      KSWIT=KSWIT+1
      WRITE(72,88)I,ALP
      IF(KSWIT.GT.20)STOP
      GO TO 89
   84 CONTINUE
      GO TO 92
   89 ALP=ALP/2.
      GO TO 93
   92 DO 102 I=1,NV
  102 SDX(I)=XX(I)
      GS=0.
      DO 101 I=1,NV
      DO 101 J=1,NV
      GS=GS+COR(I)*COR(J)*ZINFC(I,J)
  101 CONTINUE
  170 DO 96 I=1,NV
   96 XX(I)=SDX(I)+ALP*COR(I)
      WRITE(72,99)ALP
      IF(IOPT.NE.-2)GO TO 95
      IWSW2=2
      REWIND 8
      NREC=2
      GO TO 194
   95 IWSW2=0
  194 NPARSW=1
      CALL PARAM(NPARSW)
      DO 97 I=1,NV
      IF(I.EQ.1)WRITE(72,85)SDX(I),XX(I)
      IF(I.GT.1)WRITE(72,86)SDX(I),XX(I)
   97 CONTINUE
      GO TO 220
 5000 GO TO (27,26,29,31,35),NSWIT
   27 IF(INIT.EQ.1)GO TO 35
      INIT=1
      NYR=0
      IPTEMP=IPLOT
      IF(BYRIN.EQ.0) GO TO 26
      BYR=BYRIN
      BMO=BMOIN
      BDY=BDYIN
      EYR=EYRIN
      EMO=EMOIN
      EDY=EDYIN
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
C               OPTIMIZATION ITERATION
   26 WRITE(38) XIN,INSW,SMAV,RECHR,GW,RES,PWEQV,PICE,PACT,PKDEF,
     1FREWT,TCAL,LST,SNSV,SALB,SLST,JLDY,ITSW,JSOL,MXDY,
     2INTSW,JWDY,MOPP,DPT,PSS,ISO,LSO,MSO,MPC1,ISNO,IT,TSM,             GL0386
     3QRO,DIN1,STO,IASW,ASC,ASCSV,PKSV,SCRV,PST                         GL0386
      IF(IPSW.EQ.1) WRITE(38)IPSR
      DO 9 J=1,10
      OFSPR(J)=0.
    9 OBF(J)=0.
      NOBF=0
      NMOBF=0
      BYR=BYROP
      BMO=BMOOP
      BDY=BDYOP
      EYR=EYROP
      EMO=EMOOP
      EDY=EDYOP
      MYR=BYR
      MO=BMO
      BWY=BYR
      IF(BMO.GE.10)BWY=BWY+1
      EWY=EYR
      IF(EMO.GE.10)EWY=EWY+1
      NYRI=NYR
      IF(BYRIN.EQ.0.OR.NYRI.GT.1)GO TO 36
      MDIFF=ME-MB+1
      IF(MDIFF.LT.12)NYRI=0
   36 NYR=EWY-BWY+1
      IF((BMO.NE.10.OR.BDY.NE.1).AND.BYRIN.NE.0)IOSW=2
      ISSRO=1
      ISX1=1
      ISTMP=1
      ISSOL=1
      ISNOYR=0
      IF(IOSW.EQ.2)ISNOYR=BWY
      NSWIT=3
      IF(IOPT.NE.2)GO TO 37
      IWSW2=2
C     DEFINE FILE 9(1000,128,L,NREC)                                    XXXXXXXX
C     ABOVE LINE RELOCATED TO MAIN                                      XXXXXXXX
      NREC=2
   37 IWSW=1
      IOBSW=0
      DO 12 I=1,12
      IM=MOD(MFS+I-2,12)+1
      IF(IM.EQ.BMO)IOBSW=1
      IF(IM.EQ.MFN)GO TO 13
   12 CONTINUE
   13 IOBSWK=IOBSW
      RETURN
   29 ENDFILE 38
      IPLOT=0
   35 NDOP=0
      CALL OPINIT(B4,U,NN,IERR)
      IF(IERR.EQ.1)RETURN
      VSUM=0.
      IF(IOPT.NE.2) GO TO 43
      DO 42 JJ=1,IR
   42 PHIOPT(JJ)=0.
      UMIN=1.E12
   43 NC=NV
      WRITE(72,46)
      NPRNT=0
      IWSW=0
      IOSW=1
      NSWIT=4
   31 IF(IOPT.LT.3) GO TO 33
      DO 300 K=1,10
  300 OBFU(K)=0.
      DO 310 K=1,NST
      IF(TESTNO(K).EQ.0) GO TO 310
      DIFFL=ALOG(BRO(K)/OBRO(K))
      DIFFOP=OBRO(K)-BRO(K)
      OBFU(1)=OBFU(1)+ABS(DIFFOP)
      OBFU(2)=OBFU(2)+DIFFOP*DIFFOP
      OBFU(3)=OBFU(3)+ABS(DIFFL)
      OBFU(4)=OBFU(4)+DIFFL*DIFFL
      IF(IOPT.LT.4) GO TO 310
      DIFFLP=ALOG(BPK(K)/OBPK(K))
      DIFFPK=OBPK(K)-BPK(K)
      OBFU(5)=OBFU(5)+ABS(DIFFPK)
      OBFU(6)=OBFU(6)+DIFFPK*DIFFPK
      OBFU(7)=OBFU(7)+ABS(DIFFLP)
      OBFU(8)=OBFU(8)+DIFFLP*DIFFLP
      OBFU(9)=OBFU(9)+DIFFOP*DIFFPK
      OBFU(10)=OBFU(10)+DIFFL*DIFFLP
  310 CONTINUE
      IF(IOPT.EQ.5)OBFU(9)=OBFU(2)*OBFU(6)-OBFU(9)*OBFU(9)
      IF(IOPT.EQ.5)OBFU(10)=OBFU(4)*OBFU(8)-OBFU(10)*OBFU(10)
      IF(IOPT.EQ.3)U(NN)=OBFU(IOBF+2*ITRANS)
      IF(IOPT.EQ.4)U(NN)=OBFU(IOBF+2*ITRANS+4)
      IF(IOPT.EQ.5)U(NN)=OBFU(ITRANS+9)
      GO TO 179
   33 IF(IOPT.NE.2) GO TO 169
      DO 41 JJ=1,IR
   41 PHI(JJ)=PHIOPT(JJ)
      CALL COROPT(NYR,U(NN),PHI,NUM)
      REWIND 8
      IF(UMIN.LT.U(NN))GO TO 179
      UMIN=U(NN)
      DO 34 JJ=1,IR
   34 PHIOPT(JJ)=PHI(JJ)
      GO TO 179
  169 U(NN)=OBF(IOBF+2*ITRANS)
  179 IF(IPRIOR.EQ.1)U(NN)=U(NN)*EXP(VSUM)
      CALL SUB1(NV,XX,NC,G,H,B4,WITE,NFCC,NOEND,U,NN,NTRY,P,NDOP,SDX,
     1PHI,IOPT,IR)                                                      LS0287
C               SET NEW PARAMETER VALUES IN MODEL
      DO 48 L=1,NV
      STMX(L)=XX(L)
   48 XX(L)=SDX(L)
      NPARSW=1
      CALL PARAM(NPARSW)
      IF(IPRIOR.EQ.0) GO TO 39
      VSUM=0.
      DO 38 I=1,NV
      DO 38 J=1,NV
      VSUM=VSUM+VBIN(I,J)*(XX(I)-CX(I))*(XX(J)-CX(J))
   38 CONTINUE
      VSUM=VSUM/FLOAT(NOBF)
   39 DO 49 L=1,NV
   49 XX(L)=STMX(L)
  220 NOBF=0
      NMOBF=0
      REWIND 38
      IF(NDOP.EQ.1)GO TO 250
  240 READ(38) XIN,INSW,SMAV,RECHR,GW,RES,PWEQV,PICE,PACT,PKDEF,
     1FREWT,TCAL,LST,SNSV,SALB,SLST,JLDY,ITSW,JSOL,MXDY,
     2INTSW,JWDY,MOPP,DPT,PSS,ISO,LSO,MSO,MPC1,ISNO,IT,TSM,             GL0386
     3QRO,DIN1,STO,IASW,ASC,ASCSV,PKSV,SCRV,PST                         GL0386
      IF(IPSW.EQ.1)READ(38)IPSR
      IF(BYRIN.EQ.0) IPPT=1
      IF(BYRIN.EQ.0) ISNOSW=1
      ISSRO=1
      ISX1=1
      ISTMP=1
      ISSOL=1
      ISBAS=1
      ISNOYR=0
      IF((BMO.NE.10.OR.BDY.NE.1).AND.BYRIN.NE.0)ISNOYR=BWY
      IF(IOPT.EQ.2)NREC=2
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
  250 IF(IOPT.LT.0) WRITE(72,59)
      WRITE(72,8)(CX(I),I=1,NV)
      WRITE(72,80) (XX(I),I=1,NV)
      WITE=-WITE
      U(1)=B4*U(1)
      WRITE(72,82)WITE,U(1)
      WRITE(72,943)
      DO 950 L=1,NV
      WRITE(72,951) LOP(L)
      K1=NVAR(L)
      DO 953 LL=1,K1
      XXDEV(L,LL)=XXAVE(L)+XXDEV(L,LL)
      IF(ILOPL(L).EQ.1)XXDEV(L,LL)=EXP(-20.+XXDEV(L,LL))
      IF(XXDEV(L,LL).GT.HUS(L)) XXDEV(L,LL)=HUS(L)                      GL1085
      IF(XXDEV(L,LL).LT.GUS(L)) XXDEV(L,LL)=GUS(L)                      GL1085
  953 CONTINUE
      WRITE(72,952)(XXDEV(L,J),J=1,K1)
  950 CONTINUE
      IF(IABS(IOPT).EQ.2)WRITE(72,949)(PHIOPT(L),L=1,IR)
      WRITE(72,989)
      IPLOT=IPTEMP
      NPRNT=1
      NSWIT=5
      READ(37,90)IOPT,ISEN,IOBFT,ITRANS,MFST,MFNT
      REWIND 30
      IF(IOPT.EQ.0.AND.ISEN.EQ.0) GO TO 240
      IOBF=IOBFT
      MFS=MFST
      MFN=MFNT
      IOBSW=0
      DO 91 I=1,12
      IM=MOD(MFS+I-2,12)+1
      IF(IM.EQ.BMO)IOBSW=1
      IF(IM.EQ.MFN)GO TO 911
   91 CONTINUE
  911 IOBSWK=IOBSW
      INIT=-1
      NDSN=0                                                            LS0485
      NDOP=0                                                            LS0485
      ITER=1                                                            LS0485
      IEND=0                                                            LS0485
      IFTS=1                                                            LS0485
      NCNT=-1                                                           LS0485
      IF(ISEN.GT.2)GO TO 240
      IWSW2=2
C     DEFINE FILE 9(1000,128,L,NREC)                                    XXXXXXXX
C     ABOVE LINE RELOCATED TO MAIN                                      XXXXXXXX
      NREC=2
      GO TO 240
      END
C
C
C
      SUBROUTINE   SENST
     O                  ( IERR )
C
C     + + + PURPOSE + + +
C     ?????
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IERR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IERR    - ?????
C
C     + + + COMMONS + + +
      INCLUDE 'cdates.inc'
      INCLUDE 'cdatop.inc'
      INCLUDE 'copsno.inc'
      INCLUDE 'call.inc'
      INCLUDE 'cbs.inc'
      INCLUDE 'cdpr.inc'
      INCLUDE 'cdvdt.inc'
      INCLUDE 'cet.inc'
      INCLUDE 'copt.inc'
      INCLUDE 'csize.inc'
      INCLUDE 'csm.inc'
      INCLUDE 'csno.inc'
      INCLUDE 'csed.inc'
      INCLUDE 'cvol.inc'
      INCLUDE 'csent.inc'
      INCLUDE 'cplot.inc'
      INCLUDE 'cswtch.inc'
      INCLUDE 'copsn2.inc'
      INCLUDE 'csent1.inc'
      INCLUDE 'csent2.inc'
      INCLUDE 'csent3.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   NZ(20), NDEN(120), ND(12), LSET(20,20),
     #          NSWIT, LMAX, ICOR, I, IM, MOPP, J, MDIFF, N, NDF, K,
     #          NUS, NPARSW, NHIST, NHIST2, NM, IJ, NDAY, IVAL, II,
     #          K1, K2, NUM, LAGMX, LAG, KTM, KT, LAGMXM, NDAYP,
     #          LMAX1, ICOUNT, IL, M, L, NSET, ISET, KSET, MSET,
     #          JSET, IER, IN, KINS, IK, JJ, KK, IU, NPBF, LL, IMIN,
     #          IMINP, JL, IIL, NTD, IIU, NMP, NDP, NVP, JP, LP, IW
      REAL      ZCOR(20,20), U(2), XPAR(20), OBFU(10), OTEMP(22,20),
     #          SENP(60,20), PREDQ(31),
     #          CT(20,20), DT(20), XPIN(20), CTIN(20,20),
     #          PDENTM(60), PDENT(367), ZINDIG(20),
     #          B4, RESVAR, RESSTD, RESABS, DIFFL, DIFFOP, DIFFLP,
     #          DIFFPK, HSTINT, XTQ, XMQ, ER, AER, SGR, UCOR, RVAR1,
     #          STDE1, ZBS, E1, E2, E3, E4, E5V, E10V, E20V, E50V,
     #          SE1, S5E, SE2, S10E, SE3, S20E, SE4, S50E, SIGS, XL,
     #          XU, OBMIN, FHAT, RSL, RSU, PARVAC, PARSTV, PARVAR,
     #          PARSTD
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD, FLOAT, SQRT, ALOG, ABS, IABS, INT, EXP
C
C     + + + EXTERNALS + + +
      EXTERNAL   OPINIT, SENMAT, PARAM, COROPT, MATINV, ROSOPT
C
C     + + + DATA INITIALIZATIONS
      DATA NSWIT/1/
      DATA LMAX/10/
      DATA ND/92,123,151,182,212,243,273,304,335,0,31,61/               LS031384
C
C     + + + OUTPUT FORMATS + + +
   35 FORMAT('1ITERATION NUMBER  1')                                    KD1291
90    FORMAT(10X,6I5)
245   FORMAT(/////,' SENSITIVITY MATRIX (ASENS)',/)                     KD1291
252   FORMAT(I4,10F12.3,/,4X,10F12.3)                                   LS0287
265   FORMAT(///////,' INFORMATION MATRIX (ZINFC)',/)                   KD1291
272   FORMAT(4X,9E14.4)
  290 FORMAT(//////,' ERROR PROPOGATION:',/,                            KD1291
     14X,'TABLE OF MEAN SQUARED RUNOFF PREDICTION ERROR',/,             KD1291
     24X,'RESULTING FROM PARAMETER ERRORS',//,                          KD1291
     330X,'MAGNITUDE OF PARAMETER ERROR',/,                             KD1291
     47X,'LOP',20X,'5%',9X,'10%',9X,'20%',9X,'50%',/)                   KD1291
  293 FORMAT(I10,10X,4F12.5)
  295 FORMAT(5X,'JOINT',10X,4F12.5)                                     KD1291
305   FORMAT(/,' OUTPUT INSENSITIVE TO PARAMETER ',I3)                  KD1291
  358 FORMAT(//,' TERMINATE',//,' INFORMATION MATRIX IS SINGULAR',/,    KD1291
     1' PROBABLE CAUSE IS THAT TWO OR MORE PARAMETERS',/,               KD1291
     2' ARE VERY HIGHLY CORRELATED')                                    KD1291
365   FORMAT(/////,' NEW INFORMATION MATRIX',/)                         KD1291
  400 FORMAT(///////,' RESIDUALS ANALYSIS',//,' VARIANCE',F12.5,/,      KD1291
     1' STD DEV ',F12.5,/,' MEAN ABS',F12.5)                            KD1291
407   FORMAT(///////,3X,'PARAMETER',9X,'VALUE',17X,                     KD1291
     1'STANDARD ERROR',/,40X,'JOINT',5X,'INDIVIDUAL')                   KD1291
415   FORMAT(6X,I2,9X,F11.4,6X,F11.4,3X,F11.4)
425   FORMAT(///////,' PARAMETER CORRELATION MATRIX (ZINV) ',/)         KD1291
432   FORMAT(10F12.3)
435   FORMAT(///////,' DIAGONAL ELEMENTS OF HAT MATRIX (PDENT)',/)      KD1291
  460 FORMAT(10F10.3)
  461 FORMAT(/)
  535 FORMAT(' ESTIMATED RESIDUAL DENSITY AT ZERO=',F8.3)               KD1291
  633 FORMAT(I4,9F14.4,(/,4X,9F14.4))                                   LS0287
  710 FORMAT(10X,14I5)
  775 FORMAT(//,12F10.3)
C2141 FORMAT(A27,I5,3I3,5F8.2)
C2043 FORMAT(A11,20X,5F7.2)
 2910 FORMAT(12X,'%> VAR  ',4F12.5)                                     KD1291
 2920 FORMAT(12X,'%> SE   ',4F12.5,/)                                   KD1291
C
C     + + + END SPECIFICATIONS + + +
C
      ICOR=0
      IF(INIT.NE.-1)GO TO 10
      INIT=1
      NDSN=0
      GO TO 31
   10 GO TO (27,28,29,7050,31),NSWIT
27    IF(INIT.EQ.1)GO TO 31
      INIT=1
      NYR=0
      IPTEMP=IPLOT                                                      LS110483
      IF(BYRIN.EQ.0)GO TO 28
      BYR=BYRIN
      BMO=BMOIN
      BDY=BDYIN
      EYR=EYRIN
      EMO=EMOIN
      EDY=EDYIN
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
   28 WRITE(38) XIN,INSW,SMAV,RECHR,GW,RES,PWEQV,PICE,PACT,PKDEF,
     1FREWT,TCAL,LST,SNSV,SALB,SLST,JLDY,ITSW,JSOL,MXDY,
     2INTSW,JWDY,MOPP,DPT,PSS,ISO,LSO,MSO,MPC1,ISNO,IT,TSM
      IF(IPSW.EQ.1) WRITE(38)IPSR
      DO 9 J=1,10
    9 OBF(J)=0.
      BYR=BYROP
      BMO=BMOOP
      BDY=BDYOP
      EYR=EYROP
      EMO=EMOOP
      EDY=EDYOP
      MYR=BYR
      MO=BMO
      MDY=BDY
      BWY=BYR
      IF(BMO.GE.10)BWY=BWY+1
      EWY=EYR
      IF(EMO.GE.10)EWY=EWY+1
      NYRI=NYR
      IF(BYRIN.EQ.0.OR.NYRI.GT.1)GO TO 36
      MDIFF=ME-MB+1
      IF(MDIFF.LT.12)NYRI=0
   36 NYR=EWY-BWY+1
      NSWIT=3
      IWSW=1
      ISSRO=1
      ISX1=1
      ISTMP=1
      ISSOL=1
      ISNOYR=0
      IF((BMO.NE.10.OR.BDY.NE.1).AND.BYRIN.NE.0)ISNOYR=BWY
      NOBF=0
      NMOBF=0
C     DEFINE FILE 9(1000,128,L,NREC)                                    XXXXXXXX
C     ABOVE STATEMENT RELOCATED TO MAIN                                 XXXXXXXX
      NREC=2
      IF(ISEN.LT.3)IWSW2=2
      IOBSW=0
      DO 12 I=1,12
      IM=MOD(MFS+I-2,12)+1
      IF(IM.EQ.BMO)IOBSW=1
      IF(IM.EQ.MFN)GO TO 13
   12 CONTINUE
   13 IOBSWK=IOBSW
      RETURN
   29 ENDFILE 38
   31 NPRNT=0
      IPLOT=0
      CALL OPINIT(B4,U,N,IERR)
      IF(IERR.EQ.1) RETURN
      IF(IOPT.NE.0)WRITE(72,35)
      IWSW=0
      IOSW=1
      NSWIT=4
      NDF=NV
      IF(ISEN.GE.3) GO TO 32
      OBFOPT=OBF(IOBF+2*ITRANS)
      IF(ISEN.EQ.2)NDF=NDF+IR
      RESVAR=OBF(2+2*ITRANS)/FLOAT(NOBF-NDF)
      RESSTD=SQRT(RESVAR)
      RESABS=OBF(1+2*ITRANS)/FLOAT(NOBF-NDF)
      GO TO 7051
   32 DO 33 K=1,10
   33 OBFU(K)=0.
      NUS=0
      DO 34 K=1,NST
      IF(TESTNO(K).EQ.0) GO TO 34
      NUS=NUS+1
      OBSRUN(NUS)=OBRO(K)
      IF(ITRANS.EQ.1)OBSRUN(NUS)=ALOG(OBSRUN(NUS))
      DIFFL=ALOG(BRO(K)/OBRO(K))
      DIFFOP=OBRO(K)-BRO(K)
      OBFU(1)=OBFU(1)+ABS(DIFFOP)
      OBFU(2)=OBFU(2)+DIFFOP*DIFFOP
      OBFU(3)=OBFU(3)+ABS(DIFFL)
      OBFU(4)=OBFU(4)+DIFFL*DIFFL
      IF(ISEN.LT.4)GO TO 34
      OBSRUN(NUS)=OBPK(K)
      IF(ITRANS.EQ.1)OBSRUN(NUS)=ALOG(OBSRUN(NUS))
      DIFFLP=ALOG(OBPK(K)/BPK(K))
      DIFFPK=OBPK(K)-BPK(K)
      OBFU(5)=OBFU(5)+ABS(DIFFPK)
      OBFU(6)=OBFU(6)+DIFFPK*DIFFPK
      OBFU(7)=OBFU(7)+ABS(DIFFLP)
      OBFU(8)=OBFU(8)+DIFFLP*DIFFLP
      OBFU(9)=OBFU(9)+DIFFOP*DIFFPK
      OBFU(10)=OBFU(10)+DIFFL*DIFFLP
   34 CONTINUE
      IF(ISEN.EQ.5)OBFU(9)=OBFU(2)*OBFU(6)-OBFU(9)*OBFU(9)
      IF(ISEN.EQ.5)OBFU(10)=OBFU(4)*OBFU(8)-OBFU(10)*OBFU(10)
      IF(ISEN.EQ.3)OBFOPT=OBFU(IOBF+2*ITRANS)
      IF(ISEN.EQ.4)OBFOPT=OBFU(IOBF+2*ITRANS+4)
      IF(ISEN.EQ.5)OBFOPT=OBFU(IOBF+9)
      NOBF=NUS
      RESVAR=OBFU(2+2*ITRANS)/FLOAT(NOBF-NDF)
      RESSTD=SQRT(RESVAR)
      RESABS=OBFU(1+2*ITRANS)/FLOAT(NOBF-NDF)
 7050 IF(ISEN.EQ.3) WRITE(8) (BRO(K),K=1,NST)
      IF(ISEN.EQ.4) WRITE(8) (BPK(K),K=1,NST)
      IF(NDSN.NE.-1)GO TO 7051
      NDSN=0
      ICOR=1
 7051 CALL SENMAT
C               SET NEW PARAMETER VALUES IN MODEL
      NPARSW=1
      CALL PARAM(NPARSW)
  201 IF(NDSN.EQ.1)GO TO 250
  220 NOBF=0
      NMOBF=0
      REWIND 38
  240 IF(ISEN.LT.3)IWSW2=1
  241 READ(38) XIN,INSW,SMAV,RECHR,GW,RES,PWEQV,PICE,PACT,PKDEF,
     1FREWT,TCAL,LST,SNSV,SALB,SLST,JLDY,ITSW,JSOL,MXDY,
     2INTSW,JWDY,MOPP,DPT,PSS,ISO,LSO,MSO,MPC1,ISNO,IT,TSM
      IF(IPSW.EQ.1)READ(38)IPSR
      IF(BYRIN.EQ.0) IPPT=1
      IF(BYRIN.EQ.0) ISNOSW=1
      ISSRO=1
      ISX1=1
      ISTMP=1
      ISSOL=1
      ISBAS=1
      ISNOYR=0
      IF((BMO.NE.10.OR.BDY.NE.1).AND.BYRIN.NE.0)ISNOYR=BWY
      IOBSW=IOBSWK
      DO 202 J=1,10
  202 OBF(J)=0.
      IF(IABS(ISEN).LE.2)RETURN
      DO 246 J=1,NST
      PQ(J)=0.
      PSED(J)=0.
  246 BPK(J)=0.
      RETURN
  250 WRITE(72,245)
      DO 600 I=1,NV
      DO 600 J=1,NV
      ZINFC(I,J)=0.
  600 CONTINUE
      IF(ISEN.GE.3)GO TO 630
      REWIND 8
      NHIST=120
      DO 606 I=1,NHIST
  606 NDEN(I)=0
      NHIST2=60
      HSTINT=.1
      XTQ=0.                                                            LS031384
      IF(MXDY.EQ.365.AND.ND(3).EQ.151) GO TO 6060                       LS031384
      J=1                                                               LS031384
      IF(MXDY.EQ.365) J=-1                                              LS031384
      DO 6061 I=3,9                                                     LS031384
      ND(I)=ND(I)+J                                                     LS031384
 6061 CONTINUE                                                          LS031384
 6060 CONTINUE                                                          LS031384
      NREC=2                                                            LS0485
      DO 254 NM=1,NMOBF
      IJ=MFS+NM-1                                                       LS031384
      IF(IJ.GT.12)IJ=IJ-12                                              LS031384
      XMQ=0.                                                            LS031384
      READ(8)NDAY
      READ(8)((ASENS(IVAL,II),II=1,NV),IVAL=1,NDAY),
     1(OBSRUN(IVAL),IVAL=1,NDAY),(SMVOPT(IVAL),IVAL=1,NDAY)
      READ(9,REC=NREC)NDAY,(PREDQ(II),II=1,31)                          LS0485
C     WRITE(72,2043)'SENST PREDQ',(PREDQ(II),II=1,5)
      NREC=NREC+2                                                       LS0485
C     WRITE(72,2141)'SENST1,YR,MO,IWSW2,NDY,ASEN',IWY,NM,IWSW2,NDAY,
C    *(ASENS(IV,1),IV=1,5)
C     WRITE(72,2141)'SENST1,YR,MO,IWSW2,NDY,OBSR',IWY,NM,IWSW2,NDAY,
C    *(OBSRUN(II),II=1,5)
C     WRITE(72,2141)'SENST1,YR,MO,IWSW2,NDY,SMVO',IWY,NM,IWSW2,NDAY,
C    *(SMVOPT(II),II=1,5)
      DO 2560 K1=1,NDAY                                                 LS031384
C     IDAY=ND(IJ)+K1                                                    LS0485
      DO 2550 K2=1,NV                                                   LS031384
      IF (PREDQ(K1).GT.0.) THEN                                         LS0888
      SENP(K1,K2)=ASENS(K1,K2)*XOPT(K2)/PREDQ(K1)                       LS0485
      ELSE                                                              LS0888
        SENP(K1,K2)=0.                                                  LS0888
      END IF                                                            LS0888
 2550 CONTINUE                                                          LS031384
      XTQ=XTQ+PREDQ(K1)*PREDQ(K1)                                       LS0485
      XMQ=XMQ+PREDQ(K1)*PREDQ(K1)                                       LS0485
 2560 CONTINUE                                                          LS031384
C     XMQ=XMQ+PRPRO(IDAY)*PRPRO(IDAY)                                   LS0485
      DO 255 IVAL=1,NDAY
  255 WRITE(70,252)IVAL,(SENP(IVAL,I),I=1,NV)                           KF0993
      IF(ISEN.EQ.2)GO TO 603
      DO 602 I=1,NV
      DO 602 J=1,I
      DO 604 IVAL=1,NDAY
  604 ZINFC(I,J)=ZINFC(I,J)+ASENS(IVAL,I)*ASENS(IVAL,J)
      ZINFC(J,I)=ZINFC(I,J)
  602 CONTINUE
  603 CONTINUE
      IF(IOBF.EQ.2)GO TO 254
      DO 607 IVAL=1,NDAY
      ER=OBSRUN(IVAL)-SMVOPT(IVAL)
      IF(ITRANS.EQ.1)GO TO 608
      IF(ER.EQ.0.)GO TO 609
      AER=ABS(ER)
      SGR=1.
      IF(ER.LT.0.)SGR=-1.
      I=FLOAT(NHIST2)+(1./HSTINT)*SGR*ALOG(1.+AER)
      GO TO 610
  609 I=NHIST2
      GO TO 610
  608 I=FLOAT(NHIST2)+(1./HSTINT)*ER
  610 IF(I.LT.1)I=1
      IF(I.GT.NHIST)I=NHIST
      NDEN(I)=NDEN(I)+1
  607 CONTINUE
  254 CONTINUE
      IF(ISEN-2)299,631,299
  630 XTQ=0.                                                            LS031384
      DO 6310 K1=1,NOBF                                                 LS031384
      DO 6300 K2=1,NV                                                   LS031384
        IF(BRO(IVAL).GT.0.) THEN                                        LS0888
          SENP(K1,K2)=ASENS(K1,K2)*XOPT(K2)/BRO(IVAL)                   LS0888
        ELSE                                                            LS0888
          SENP(K1,K2) = 0.                                              LS0888
        END IF                                                          LS0888
 6300 CONTINUE                                                          LS031384
      XTQ=XTQ+BRO(IVAL)*BRO(IVAL)                                       LS031384
 6310 CONTINUE                                                          LS031384
      DO 632 IVAL =1,NOBF                                               LS031384
  632 WRITE(70,633)IVAL,(SENP(IVAL,I),I=1,NV)                           KF0993
      DO 635 I=1,NV
      DO 635 J=1,I
      DO 636 IVAL=1,NOBF
  636 ZINFC(I,J)=ZINFC(I,J)+ASENS(IVAL,I)*ASENS(IVAL,J)
      ZINFC(J,I)=ZINFC(I,J)
  635 CONTINUE
      GO TO 299
  631 IF(ICOR.EQ.1)GO TO 253
      CALL COROPT (NYR,UCOR,PHI,NUM)
      REWIND 8
      OBFOPT=UCOR
  253 IF(IP.EQ.0)GO TO 264
      LAGMX=IP
      DO 276 I=1,IP
  276 PDENT(I)=PHI(I)
  264 IF(IQ.EQ.0)GO TO 289
      PDENTM(1)=1.
      DO 267 LAG=1,LMAX
      PDENTM(LAG+1)=0.
      KTM=IQ
      IF(LAG.LT.IQ)KTM=LAG
      DO 269 KT=1,KTM
  269 PDENTM(LAG+1)=PDENTM(LAG+1)+PHI(IP+KT)*PDENTM(LAG+1-KT)
      IF(ABS(PDENTM(LAG+1)).LE.0.0001)GO TO 273
      LAGMXM=LAG
  267 CONTINUE
  273 DO 277 I=1,LAGMXM
  277 PDENTM(I)=PDENTM(I+1)
  289 REWIND 8
      REWIND 45
      DO 296 I=1,NV
      DO 296 J=1,NV
      ZINFC(I,J)=0.
  296 CONTINUE
      DO 281 NM=1,NMOBF
      READ(8)NDAY
      NDAYP=NDAY+LMAX
      LMAX1=LMAX+1
      READ(8)((ASENS(IVAL,II),II=1,NV),IVAL=LMAX1,NDAYP),
     1(OBSRUN(IVAL),IVAL=LMAX1,NDAYP),(SMVOPT(IVAL),IVAL=LMAX1,NDAYP)
C     WRITE(72,2141)'SENST2,YR,MO,IWSW2,NDY,ASEN',IWY,NM,IWSW2,NDAY,
C    *(ASENS(IV,1),IV=1,5)
C     WRITE(72,2141)'SENST2,YR,MO,IWSW2,NDY,OBSR',IWY,NM,IWSW2,NDAY,
C    *(OBSRUN(II),II=1,5)
C     WRITE(72,2141)'SENST2,YR,MO,IWSW2,NDY,SMVO',IWY,NM,IWSW2,NDAY,
C    *(SMVOPT(II),II=1,5)
      DO 261 IVAL=1,LMAX                                                LS110483
      OTEMP(1,IVAL)=OBSRUN(NDAY+IVAL)                                   LS110483
      OTEMP(2,IVAL)=SMVOPT(NDAY+IVAL)                                   LS110483
      DO 261 II=1,NV                                                    LS110483
      OTEMP(2+II,IVAL)=ASENS(NDAY+IVAL,II)                              LS110483
  261 CONTINUE                                                          LS110483
      IF(IP.EQ.0)GO TO 283
      ICOUNT=1
      IL=NDAYP+1
  286 DO 266 K=1,NDAY
      DO 266 M=1,LAGMX
C     IF(K+M-NDAY.GE.1)GO TO 266                                        LS110483
      OBSRUN(IL-K)=OBSRUN(IL-K)-PDENT(M)*OBSRUN(IL-K-M)
      SMVOPT(IL-K)=SMVOPT(IL-K)-PDENT(M)*SMVOPT(IL-K-M)
      DO 266 L=1,NV
      ASENS(IL-K,L)=ASENS(IL-K,L)-PDENT(M)*ASENS(IL-K-M,L)
  266 CONTINUE
      IF(ICOUNT.EQ.2.OR.IQ.EQ.0)GO TO 263
  283 DO 285 IM=1,LAGMXM
  285 PDENT(IM)=PDENTM(IM)
      LAGMX=LAGMXM
      ICOUNT=2
      GO TO 286
  263 WRITE(45)NDAY
      WRITE(45)((ASENS(IVAL,II),II=1,NV),IVAL=LMAX1,NDAYP),
     1(OBSRUN(IVAL),IVAL=LMAX1,NDAYP),(SMVOPT(IVAL),IVAL=LMAX1,NDAYP)
C     WRITE(72,2141)'SENST3,YR,MO,IWSW2,NDY,ASEN',IWY,NM,IWSW2,NDAY,
C    *(ASENS(IV,1),IV=1,5)
C     WRITE(72,2141)'SENST3,YR,MO,IWSW2,NDY,OBSR',IWY,NM,IWSW2,NDAY,
C    *(OBSRUN(II),II=1,5)
C     WRITE(72,2141)'SENST3,YR,MO,IWSW2,NDY,SMVO',IWY,NM,IWSW2,NDAY,
C    *(SMVOPT(II),II=1,5)
      DO 298 I=1,NV
      DO 298 J=1,I
      DO 297 IVAL=LMAX1,NDAYP
  297 ZINFC(I,J)=ZINFC(I,J)+ASENS(IVAL,I)*ASENS(IVAL,J)
      ZINFC(J,I)=ZINFC(I,J)
  298 CONTINUE
      DO 287 IVAL=1,LMAX
      OBSRUN(IVAL)=OTEMP(1,IVAL)                                        LS110483
      SMVOPT(IVAL)=OTEMP(2,IVAL)                                        LS110483
      DO 287 II=1,NV
      ASENS(IVAL,II)=OTEMP(2+II,IVAL)                                   LS110483
  287 CONTINUE
  281 CONTINUE
      REWIND 8
      REWIND 45
      DO 291 NM=1,NMOBF
      READ(45)NDAY
      WRITE(8)NDAY
      READ(45)((ASENS(IVAL,II),II=1,NV),IVAL=1,NDAY),
     1(OBSRUN(IVAL),IVAL=1,NDAY),(SMVOPT(IVAL),IVAL=1,NDAY)
      WRITE(8)((ASENS(IVAL,II),II=1,NV),IVAL=1,NDAY),
     1(OBSRUN(IVAL),IVAL=1,NDAY),(SMVOPT(IVAL),IVAL=1,NDAY)
C     WRITE(72,2141)'SENST4,YR,MO,IWSW2,NDY,ASEN',IWY,NM,IWSW2,NDAY,
C    *(ASENS(IV,1),IV=1,5)
C     WRITE(72,2141)'SENST4,YR,MO,IWSW2,NDY,OBSR',IWY,NM,IWSW2,NDAY,
C    *(OBSRUN(II),II=1,5)
C     WRITE(72,2141)'SENST4,YR,MO,IWSW2,NDY,SMVO',IWY,NM,IWSW2,NDAY,
C    *(SMVOPT(II),II=1,5)
  291 CONTINUE
  299 WRITE(72,265)
      DO 270 I=1,NV
      DO 2700 J=1,NV                                                    LS031384
      IF(XTQ.GT.0.) THEN                                                LS0888
      SENP(I,J)=ZINFC(I,J)*XOPT(I)*XOPT(J)/XTQ                          LS031384
      ELSE                                                              LS0888
        SENP(I,J)=0.                                                    LS0888
      END IF                                                            LS0888
 2700 CONTINUE
270   WRITE(72,272)(SENP(I,J),J=1,NV)                                    LS031384
      IF(IOPT.NE.0) GO TO 310
      WRITE(72,290)
      RVAR1=(1./RESVAR)*100.                                            GL1185
      STDE1=(1./RESSTD)*100.                                            GL1185
      DO 292 I=1,NV
      ZBS=ZINFC(I,I)*XOPT(I)*XOPT(I)/FLOAT(NOBF)
      E1=ZBS*.05*.05
      E2=ZBS*.1*.1
      E3=ZBS*.2*.2
      E4=ZBS*.5*.5
      WRITE(72,293)LOP(I),E1,E2,E3,E4
      E5V=E1*RVAR1                                                      GL1185
      E10V=E2*RVAR1                                                     GL1185
      E20V=E3*RVAR1                                                     GL1185
      E50V=E4*RVAR1                                                     GL1185
      SE1=SQRT(E1+RESVAR)                                               GL1185
      S5E=(SE1-RESSTD)*STDE1                                            GL1185
      SE2=SQRT(E2+RESVAR)                                               GL1185
      S10E=(SE2-RESSTD)*STDE1                                           GL1185
      SE3=SQRT(E3+RESVAR)                                               GL1185
      S20E=(SE3-RESSTD)*STDE1                                           GL1185
      SE4=SQRT(E4+RESVAR)                                               GL1185
      S50E=(SE4-RESSTD)*STDE1                                           GL1185
      WRITE(72,2910) E5V,E10V,E20V,E50V                                  GL1185
      WRITE(72,2920) S5E,S10E,S20E,S50E                                  GL1185
  292 CONTINUE
      ZBS=0.
      DO 294 I=1,NV
      ZBS=ZBS+ZINFC(I,I)*XOPT(I)*XOPT(I)
  294 CONTINUE
      ZBS=ZBS/FLOAT(NOBF)
      E1=ZBS*.05*.05
      E2=ZBS*.1*.1
      E3=ZBS*.2*.2
      E4=ZBS*.5*.5
      WRITE(72,295)E1,E2,E3,E4
      IF(ISEN.NE.10)GO TO 310
  705 READ(71,710)NSET
      IF(NSET.EQ.0) GO TO 310
      DO 715 ISET=1,NSET
  715 READ(71,710)(LSET(ISET,KSET),KSET=1,NV)
      DO 720 KSET=1,NSET
      DO 721 MSET=1,KSET
      CT(KSET,MSET)=0.
      DO 725 ISET=1,NV
      DO 725 JSET=1,NV
      CT(KSET,MSET)=CT(KSET,MSET)+FLOAT(LSET(KSET,ISET))*
     1FLOAT(LSET(MSET,JSET))*ZINFC(ISET,JSET)
  725 CONTINUE
      CT(MSET,KSET)=CT(KSET,MSET)
  721 CONTINUE
  720 CONTINUE
      DO 730 KSET=1,NSET
      DT(KSET)=0.
      DO 735 ISET=1,NV
      DO 735 JSET=1,NV
      DT(KSET)=DT(KSET)+FLOAT(LSET(KSET,ISET))*ZINFC(ISET,JSET)*
     1XOPT(JSET)
  735 CONTINUE
  730 CONTINUE
      DO 741 ISET=1,NSET
      DT(ISET)=DT(ISET)/FLOAT(NOBF)
      DO 741 JSET=1,NSET
      CT(ISET,JSET)=CT(ISET,JSET)/FLOAT(NOBF)
  741 CONTINUE
      CALL MATINV(CT,CTIN,NSET,IER)
      DO 740 KSET=1,NSET
      XPIN(KSET)=0.
      DO 745 ISET=1,NSET
  745 XPIN(KSET)=XPIN(KSET)+CTIN(KSET,ISET)*DT(ISET)
  740 CONTINUE
      DO 750 KSET=1,NV
      XPAR(KSET)=0.
      DO 750 ISET=1,NSET
      XPAR(KSET)=XPAR(KSET)+FLOAT(LSET(ISET,KSET))*XPIN(ISET)
  750 CONTINUE
      ZBS=0.
      DO 770 I=1,NV
      DO 770 J=1,NV
      ZBS=ZBS+ZINFC(I,J)*(XPAR(I)-XOPT(I))*(XPAR(J)-XOPT(J))
  770 CONTINUE
      ZBS=ZBS/FLOAT(NOBF)
      WRITE(72,775)ZBS,(XPAR(IN),IN=1,NV)
      GO TO 705
  310 IF(IPRIOR.EQ.1)GO TO 280
      KINS=0
330   II=0
      DO 300 I=1,NV
      II=II+1
      IF(ZINFC(II,II).LE.0.)GO TO 320
300   CONTINUE
      IF(KINS.EQ.0)GO TO 350
364   WRITE(72,365)
      DO 370 I=1,NV
      DO 3700 J=1,NV                                                    LS031384
        IF(XTQ.GT.0.) THEN                                              LS0888
          SENP(I,J)=ZINFC(I,J)*XOPT(J)*XOPT(I)/XTQ                      LS0888
        ELSE                                                            LS0888
          SENP(I,J)=0.                                                  LS0888
        END IF                                                          LS0888
 3700 CONTINUE                                                          LS033183
370   WRITE(72,272)(SENP(I,J),J=1,NV)
      GO TO 350
  320 KINS=KINS+1
      IK=LOP(II)
      NZ(KINS)=II+KINS-1
      WRITE(72,305)IK
      NV=NV-1
      IF(NV.EQ.0)GO TO 353
      IF(NV.LT.II)GO TO 364
      DO 325 J=II,NV
      JJ=J+1
      LOP(J)=LOP(JJ)
      XOPT(J)=XOPT(JJ)
325   CONTINUE
      DO 340 J=II,NV
      JJ=J+1
      DO 340 K=1,J
      KK=K
      IF(K.GE.II)KK=KK+1
      ZINFC(J,K)=ZINFC(JJ,KK)
      ZINFC(K,J)=ZINFC(J,K)
340   CONTINUE
      GO TO 330
  280 SIGS=OBFOPT/FLOAT(NOBF)
      DO 282 I=1,NV
      DO 282 J=1,NV
      ZINFC(I,J)=ZINFC(I,J)+SIGS*VBIN(I,J)
  282 CONTINUE
  350 DO 351 II=1,NV
  351 DIAG(II)=SQRT(ZINFC(II,II))
      DO 352 II=1,NV
      DO 352 JJ=1,II
      ZINFC(II,JJ)=ZINFC(II,JJ)/(DIAG(II)*DIAG(JJ))
      ZINFC(JJ,II)=ZINFC(II,JJ)
  352 CONTINUE
  353 IF(IOPT.EQ.0)WRITE(72,400)RESVAR,RESSTD,RESABS
      IF(NV.EQ.0)GO TO 315
      IER=0
      CALL MATINV(ZINFC,ZINV,NV,IER)
      IF(IER.EQ.0)GO TO 359
      WRITE(72,358)
      GO TO 315
  359 DO 355 II=1,NV
      DO 355 JJ=1,II
      ZINFC(II,JJ)=ZINFC(II,JJ)*DIAG(II)*DIAG(JJ)
      ZINFC(JJ,II)=ZINFC(II,JJ)
      ZINV(II,JJ)=ZINV(II,JJ)/(DIAG(II)*DIAG(JJ))
      ZINV(JJ,II)=ZINV(II,JJ)
  355 CONTINUE
      IF(IOPT.NE.0)RETURN
      IF(IOBF.EQ.2) GO TO 540
      XL=FLOAT(NOBF)/4.
      XU=3.*XL
      IL=INT(XL)
      IU=INT(XU)+1
      IF(ISEN.LT.3)GO TO 531
      DO 505 L=1,NOBF
  505 OBSRUN(L)=OBSRUN(L)-SMVOPT(L)
      NPBF=NOBF-1
      DO 530 L=1,NPBF
      OBMIN=1.E8
      DO 510 LL=L,NOBF
      IF(OBSRUN(LL).GT.OBMIN) GO TO 510
      OBMIN=OBSRUN(LL)
      IMIN=LL
  510 CONTINUE
      IF(IMIN.EQ.L) GO TO 530
      IMINP=IMIN-L
      DO 520 JL=1,IMINP
  520 OBSRUN(IMIN-JL+1)=OBSRUN(IMIN-JL)
      OBSRUN(L)=OBMIN
  530 CONTINUE
      FHAT=FLOAT(IU-IL-1)/(FLOAT(NOBF)*(OBSRUN(IU)-OBSRUN(IL)))
      GO TO 540
  531 IIL=0
      NTD=0
      DO 620 I=1,NHIST
      NTD=NTD+NDEN(I)
      IF(IIL.NE.0)GO TO 621
      IF(NTD.LT.IL)GO TO 620
      IIL=I
  621 IF(NTD.LT.IU)GO TO 620
      IIU=I
      GO TO 623
  620 CONTINUE
  623 IF(IIL.EQ.IIU)IIU=IIU+1
      RSL=FLOAT(IIL-NHIST2)*HSTINT+HSTINT/2.
      RSU=FLOAT(IIU-NHIST2)*HSTINT+HSTINT/2.
      IF(ITRANS.EQ.1)GO TO 622
      SGR=1.
      IF(RSL.LT.0.)SGR=-1.
      RSL=(EXP(ABS(RSL))-1.)*SGR
      SGR=1.
      IF(RSU.LT.0.)SGR=-1.
      RSU=(EXP(ABS(RSU))-1.)*SGR
  622 FHAT=FLOAT(IU-IL-1)/(FLOAT(NOBF)*(RSU-RSL))
  540 IF(IOBF.EQ.1) WRITE(72,535)FHAT
      WRITE(72,407)
      IF(IOBF.EQ.2) GO TO 550
      RESVAR=1./(4.*FHAT*FHAT)
      RESSTD=1./(2.*FHAT)
  550 DO 410 I=1,NV
      K=LOP(I)
      PARVAC=RESVAR/ZINFC(I,I)
      PARSTV=SQRT(PARVAC)
      PARVAR=ZINV(I,I)*RESVAR
      ZINDIG(I)=SQRT(ZINV(I,I))
      PARSTD=ZINDIG(I)*RESSTD
      WRITE(72,415)K,XOPT(I),PARSTD,PARSTV
410   CONTINUE
      DO 420 I=1,NV
      DO 420 J=1,I
      ZCOR(I,J)=ZINV(I,J)/(ZINDIG(I)*ZINDIG(J))
      ZCOR(J,I)=ZCOR(I,J)
420   CONTINUE
      WRITE(72,425)
      DO 430 I=1,NV
430   WRITE(72,432)(ZCOR(I,J),J=1,NV)
      WRITE(72,435)
      IF(ISEN.LT.3)NMP=NMOBF
      IF(ISEN.GE.3)NMP=1
      REWIND 8
      DO 440 NM=1,NMP
      NDP=NOBF
      NVP=NV+KINS
      IF(ISEN.LT.3)READ(8) NDP
      IF(ISEN.LT.3)READ(8)((ASENS(IVAL,II),II=1,NVP),IVAL=1,NDP),
     1(OBSRUN(IVAL),IVAL=1,NDP),(SMVOPT(IVAL),IVAL=1,NDP)
C     WRITE(72,2141)'SENST5,YR,MO,IWSW2,NDY,ASEN',IWY,NM,IWSW2,NDAY,
C    *(ASENS(IV,1),IV=1,5)
C     WRITE(72,2141)'SENST5,YR,MO,IWSW2,NDY,OBSR',IWY,NM,IWSW2,NDAY,
C    *(OBSRUN(II),II=1,5)
C     WRITE(72,2141)'SENST5,YR,MO,IWSW2,NDY,SMVO',IWY,NM,IWSW2,NDAY,
C    *(SMVOPT(II),II=1,5)
      DO 441 I=1,NDP
      PDENT(I)=0.
      KK=0
      DO 441 K=1,NV
  446 KK=KK+1
      IF(KINS.EQ.0)GO TO 447
      DO 445 JP=1,KINS
      IF(KK.EQ.NZ(JP))GO TO 446
  445 CONTINUE
  447 LL=0
      DO 441 L=1,NV
  456 LL=LL+1
      IF(KINS.EQ.0)GO TO 457
      DO 455 LP=1,KINS
      IF(LL.EQ.NZ(LP))GO TO 456
  455 CONTINUE
  457 PDENT(I)=PDENT(I)+ASENS(I,KK)*ASENS(I,LL)*ZINV(L,K)
  441 CONTINUE
      WRITE(72,460)(PDENT(IW),IW=1,NDP)
      WRITE(72,461)
  440 CONTINUE
  315 NSWIT=5
      READ(37,90)IOPT,ISEN,IOBF,ITRANS,MFS,MFN
      IF(IOPT.EQ.0.AND.ISEN.EQ.0)STOP
      IF(IOPT.GE.1)GO TO 500
      REWIND 8
      NDSN=0
      IF(IABS(ISEN).LT.3)IWSW2=2
      REWIND 38
      NOBF=0
      NMOBF=0
      NREC=2
      IOBSW=0
      DO 91 I=1,12
      IM=MOD(MFS+I-2,12)+1
      IF(IM.EQ.BMO)IOBSW=1
      IF(IM.EQ.MFN)GO TO 911
   91 CONTINUE
  911 IOBSWK=IOBSW
      GO TO 241
  500 CALL ROSOPT(IERR)
      RETURN
      END
C
C
C
      SUBROUTINE   COROPT
     ?                   ( NYR,
     O                     UCOR,
     M                     PHIOPT,
     ?                     NUMDAY )
C
C     + + + PURPOSE + + +
C     ?????
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NYR, NUMDAY
      REAL      UCOR, PHIOPT(10)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NYR
C     UCOR
C     PHIOPT
C     NUMDAY
C
C     + + + COMMONS + + +
      INCLUDE 'csent.inc'
      INCLUDE 'csent1.inc'
      INCLUDE 'copt.inc'
      INCLUDE 'csent3.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   LMAX, LMAX1, K, IT, IEND, I, J, JJ, IY, NDAY, NA, L,
     #          MN, M, IQ1, IP1, IFCT, JSUM, KN, LT, IT1, LC, LS,
     #          JLC, LE
      REAL      VAROR, VARWN, PWRT(31), OWRT(31)
      DOUBLE PRECISION SS, ZINFX(20,2), ZINT(20,2), PX(20),
     #                 A(376), Z(376), ASENT(376,9), PHI(10),
     #                 PHI0(10), COR(10), COR1(10), AM(20,20),
     #                 B(20,20), CORP(20), TAU(20), TAU1(10,10),
     #                 TAU2(10,10), TAUT(20,20), TAUINV(20,20),
     #                 P1, Q1, VAT, CP, F(10), X(10), HX(10),
     #                 SM, SD, SP, ZMA, ZMN, CHISQ, CHISQ1, ZN, VAR
C
C     + + + INTRINSICS + + +
      INTRINSIC   ALOG, IABS, DSQRT, DABS, INT, FLOAT
C
C     + + + EXTERNALS + + +
      EXTERNAL   MATIND
C
C     + + + DATA INITIALIZATIONS + + +
      DATA LMAX/10/                                                     LS0485
C
C     + + + OUTPUT FORMATS + + +
   52 FORMAT(' ITERATION NO.',I3)                                       KD1291
   53 FORMAT(///,3X,'SUB-',/,' ITERATION',10X,'SUM OF SQUARES',10X,     KD1291
     1'ARMA PARAMETERS')                                                KD1291
  900 FORMAT(I7,13X,D14.6,10X,10F9.3)
  909 FORMAT(10X,8D13.5)
  930 FORMAT(////,19X,'CHI-SQUARED',5X,'CORRELATIONS',/,19X,            KD1291
     1'STATISTIC',/,35X,'LAG',I2,9I10)                                  KD1291
  931 FORMAT(/,34X,'LAG',I3,9I10)                                       KD1291
  940 FORMAT(' BEFORE ARMA',/,3X,' MODELING',3X,F12.1,3X,10F10.2)       KD1291
  941 FORMAT(' BEFORE ARMA',/,3X,' MODELING',18X,10F10.2)               KD1291
  949 FORMAT(/,' VARIANCE, ORIGINAL SERIES=',F10.3,/,                   KD1291
     1' VARIANCE, WHITE NOISE SERIES=',F10.3)                           KD1291
  960 FORMAT(' AFTER ARMA',/,3X,' MODELING',3X,F12.1,3X,10F10.2)        KD1291
  961 FORMAT(' AFTER ARMA',/,3X,' MODELING',18X,10F10.2)                KD1291
 1127 FORMAT(' COUNT ERROR, MA INITIALIZATION')                         KD1291
C2140 FORMAT(A30,I8,3X,3I3,5F6.2)
C
C     + + + END SPECIFICATIONS + + +
C
      LMAX1=LMAX+1
   50 DO 55 K=1,LMAX
   55 COR1(K)=0.0D0
      VAR=0.0D0
      IT=0
      IEND=0
      NCNT=NCNT+1
      IF(IOPT.EQ.2)WRITE(72,52)NCNT
      WRITE(72,53)
   80 IT=IT+1
      DO 90 I=1,IR
      PX(I)=0.0D0
      DO 90 J=1,IR
      ZINFX(I,J)=0.0D0
   90 CONTINUE
      DO 95 I=1,LMAX
   95 COR(I)=0.0D0
      SS=0.0D0
      SM=0.0D0
      SD=0.0D0
      SP=0.0D0
      IF(IT.GT.1)GO TO 298
      IF(IFTS.EQ.1)GO TO 220
      DO 215 JJ=1,IR
  215 PHI0(JJ)=PHIOPT(JJ)
      GO TO 299
  220 IFTS=0
      DO 100 I=1,10
  100 Z(I)=0.0D0
      NREC=2
      DO 1000 IY=1,NMOBF
      READ(9,REC=NREC)NDAY,(PWRT(J),J=1,31)
C     WRITE(72,2140)'COROPT,MO,IWSW2,NREC,NDAYS,PR5',IY,IWSW2,
C    *NREC,NDAYS,(PWRT(J),J=1,5)
      NREC=NREC+1
      READ(9,REC=NREC)NDAY,(OWRT(J),J=1,31)
C     WRITE(72,2140)'COROPT,MO,IWSW2,NREC,NDAYS,OB5',IY,IWSW2,
C    *NREC,NDAYS,(OWRT(J),J=1,5)
      NREC=NREC+1
      NA=NDAY+LMAX
      DO 210 L=1,NDAY
      IF(ITRANS.EQ.0)Z(L+LMAX)=OWRT(L)-PWRT(L)
      IF(ITRANS.EQ.1)Z(L+LMAX)=ALOG(OWRT(L)+1.)-ALOG(PWRT(L)+1.)
  210 CONTINUE
      DO 325 MN=LMAX1,NA
      VAR=VAR+Z(MN)*Z(MN)
      DO 325 M=1,LMAX
      COR1(M)=COR1(M)+Z(MN)*Z(MN-M)
  325 CONTINUE
      DO 200 L=1,LMAX
  200 Z(LMAX+1-L)=Z(NA+1-L)
 1000 CONTINUE
      ZN=NOBF
      IF(IP.EQ.0)GO TO 1010
      DO 1030 I=1,IP
      X(I)=COR1(IQ+I)
      DO 1030 J=1,IP
      M=IABS(IQ+I-J)
      IF(M.GT.0)AM(I,J)=COR1(M)
      IF(M.EQ.0)AM(I,J)=VAR
 1030 CONTINUE
      CALL MATIND(AM,B,IP)
      DO 1040 I=1,IP
      PHI0(I)=0.0D0
      DO 1040 L=1,IP
      PHI0(I)=PHI0(I)+B(I,L)*X(L)
 1040 CONTINUE
 1010 IF(IQ.EQ.0) GO TO 299
      IQ1=IQ+1
      IP1=IP+1
      DO 1050 J=1,IQ1
      CORP(J)=0.0D0
      DO 1050 I=1,IP1
      IF(I.EQ.1)P1=-1.0D0
      IF(I.GT.1)P1=PHI0(I-1)
      DO 1050 K=1,IP1
      IF(K.EQ.1)Q1=-1.0D0
      IF(K.GT.1)Q1=PHI0(K-1)
      M=IABS(J-1+I-K)
      IF(M.EQ.0)CP=VAR
      IF(M.GT.0)CP=COR1(M)
      CORP(J)=CORP(J)+Q1*P1*CP
 1050 CONTINUE
      DO 1051 J=1,IQ1
 1051 CORP(J)=CORP(J)/ZN
      IFCT=0
      TAU(1)=DSQRT(CORP(1))
      DO 1060 L=2,IQ1
 1060 TAU(L)=0.0D0
 1150 DO 1090 J=1,IQ1
      JSUM=IQ1-J+1
      F(J)=-CORP(J)
      DO 1090 I=1,JSUM
      F(J)=F(J)+TAU(I)*TAU(I+J-1)
 1090 CONTINUE
      DO 1110 I=1,IQ1
      IF(DABS(F(I)).GT.1.0D-2)GO TO 1120
 1110 CONTINUE
      GO TO 1130
 1120 IFCT=IFCT+1
      IF(IFCT.LE.30)GO TO 1125
      WRITE(72,1127)
      STOP
 1125 DO 1070 I=1,IQ1
      DO 1070 J=1,IQ1
      IF(J.LT.I)TAU2(I,J)=0.0D0
      IF(J.GE.I)TAU2(I,J)=TAU(J-I+1)
      IF(I+J-1.GT.IQ1)TAU1(I,J)=0.0D0
      IF(I+J-1.LE.IQ1)TAU1(I,J)=TAU(I+J-1)
 1070 CONTINUE
      DO 1080 I=1,IQ1
      DO 1080 J=1,IQ1
      TAUT(I,J)=TAU1(I,J)+TAU2(I,J)
 1080 CONTINUE
      CALL MATIND(TAUT,TAUINV,IQ1)
      DO 1100 I=1,IQ1
      HX(I)=0.0D0
      DO 1100 J=1,IQ1
      HX(I)=HX(I)+TAUINV(I,J)*F(J)
 1100 CONTINUE
      DO 1140 I=1,IQ1
 1140 TAU(I)=TAU(I)-HX(I)
      GO TO 1150
 1130 DO 1160 I=1,IQ
 1160 PHI0(I+IP)=-TAU(I+1)/TAU(1)
  299 DO 104 K=1,LMAX
  104 COR1(K)=0.0D0
  298 DO 102 I=1,LMAX
      A(I)=0.0D0
      Z(I)=0.0D0
      DO 102 K=1,IR
      ASENT(I,K)=0.0D0
  102 CONTINUE
      NREC=2
      DO 1002 IY=1,NMOBF
      READ(9,REC=NREC)NDAY,(PWRT(J),J=1,31)
C     WRITE(72,2140)'COROPT,MO,I2SW2,NREC,NDAYS,PR5',IY,IWSW2,
C    *NREC,NDAYS,(PWRT(J),J=1,5)
      NREC=NREC+1
      READ(9,REC=NREC)NDAY,(OWRT(J),J=1,31)
C     WRITE(72,2140)'COROP2,MO,IWSW2,NREC,NDAYS,OB5',IY,IWSW2,
C    *NREC,NDAYS,(OWRT(J),J=1,5)
      NREC=NREC+1
      NA=NDAY+LMAX
      DO 212 L=1,NDAY
      IF(ITRANS.EQ.0)Z(L+LMAX)=OWRT(L)-PWRT(L)
      IF(ITRANS.EQ.1)Z(L+LMAX)=ALOG(OWRT(L)+1.)-ALOG(PWRT(L)+1.)
  212 CONTINUE
      IF(IT.GT.1)GO TO 332
      DO 330 MN=LMAX1,NA
      DO 330 M=1,LMAX
      COR1(M)=COR1(M)+Z(MN)*Z(MN-M)
  330 CONTINUE
  332 DO 300 K=LMAX1,NA
      A(K)=Z(K)
      IF(IP.EQ.0)GO TO 301
      DO 310 L=1,IP
  310 A(K)=A(K)-PHI0(L)*Z(K-L)
  301 IF(IQ.EQ.0)GO TO 300
      DO 320 L=1,IQ
  320 A(K)=A(K)+PHI0(L+IP)*A(K-L)
  300 CONTINUE
      DO 340 MN=LMAX1,NA
      SD=SD+Z(MN)
      SP=SP+Z(MN)*Z(MN)
      SM=SM+A(MN)
  340 SS=SS+A(MN)*A(MN)
      UCOR=SS
      DO 350 M=1,LMAX
      DO 350 MN=LMAX1,NA
      COR(M)=COR(M)+A(MN)*A(MN-M)
  350 CONTINUE
      IF(IEND.EQ.1)GO TO 1003
      DO 405 L=1,IR
  405 PHI(L)=PHI0(L)
      DO 410 K=1,IR
      PHI(K)=PHI0(K)+.0005D0
      IF(K.EQ.1)GO TO 412
      PHI(K-1)=PHI0(K-1)
  412 DO 420 KN=LMAX1,NA
      ASENT(KN,K)=Z(KN)
      IF(IP.EQ.0) GO TO 400
      DO 430 L=1,IP
  430 ASENT(KN,K)=ASENT(KN,K)-PHI(L)*Z(KN-L)
  400 IF(IQ.EQ.0) GO TO 420
      DO 440 L=1,IQ
  440 ASENT(KN,K)=ASENT(KN,K)+PHI(L+IP)*ASENT(KN-L,K)
  420 CONTINUE
  410 CONTINUE
      PHI(IR)=PHI0(IR)
      DO 680 L=1,LMAX
      DO 680 LT=1,IR
      ASENT(LMAX1-L,LT)=ASENT(NA+1-L,LT)
  680 CONTINUE
      DO 435 KN=LMAX1,NA
      DO 435 L=1,IR
      ASENT(KN,L)=(A(KN)-ASENT(KN,L))/.0005D0
  435 CONTINUE
      DO 500 I=1,IR
      DO 500 J=1,IR
      DO 500 L=LMAX1,NA
      ZINFX(I,J)=ZINFX(I,J)+ASENT(L,I)*ASENT(L,J)
  500 CONTINUE
      DO 600 I=1,IR
      DO 600 L=LMAX1,NA
      PX(I)=PX(I)+ASENT(L,I)*A(L)
  600 CONTINUE
 1003 DO 670 L=1,LMAX
      Z(LMAX1-L)=Z(NA+1-L)
      A(LMAX1-L)=A(NA+1-L)
  670 CONTINUE
 1002 CONTINUE
      CHISQ=0.0D0
      ZN=NOBF
      IF(IT.GT.1)GO TO 625
      CHISQ1=0.0D0
      ZMA=SD*SD/ZN
      DO 630 M=1,LMAX
      COR1(M)=(COR1(M)-ZMA)/(SP-ZMA)
  630 CHISQ1=CHISQ1+COR1(M)*COR1(M)
      CHISQ1=CHISQ1*ZN
      IT1=0
      DO 675 L=1,IR
  675 PHI(L)=0.0D0
      WRITE(72,900)IT1,SP,(PHI(L),L=1,IR)
  625 ZMN=SM*SM/ZN
      DO 650 M=1,LMAX
      COR(M)=(COR(M)-ZMN)/(SS-ZMN)
  650 CHISQ=CHISQ+COR(M)*COR(M)
      CHISQ=CHISQ*ZN
      WRITE(72,900)IT,SS,(PHI0(L),L=1,IR)
      IF(IEND.EQ.0)GO TO 950
      LC=INT(FLOAT(LMAX)/10.)
      IF(LMAX-10*LC.GT.0)LC=LC+1
      LS=1
      DO 920 JLC=1,LC
      LE=LS+9
      IF(LE.GT.LMAX)LE=LMAX
      IF(JLC.EQ.1)WRITE(72,930)(L,L=LS,LE)
      IF(JLC.GT.1)WRITE(72,931)(L,L=LS,LE)
      IF(JLC.EQ.1)WRITE(72,940)CHISQ1,(COR1(L),L=LS,LE)
      IF(JLC.GT.1)WRITE(72,941)(COR1(L),L=LS,LE)
      IF(JLC.EQ.1)WRITE(72,960)CHISQ,(COR(L),L=LS,LE)
      IF(JLC.GT.1)WRITE(72,961)(COR(L),L=LS,LE)
      LS=LS+10
  920 CONTINUE
      DO 902 K=1,IR
  902 PHIOPT(K)=PHI0(K)
      VAROR=(SP-ZMA)/(ZN-FLOAT(IR))
      VARWN=(SS-ZMN)/(ZN-FLOAT(IR))
      WRITE(72,949)VAROR,VARWN
      RETURN
  950 CALL MATIND(ZINFX,ZINT,IR)
      DO 700 I=1,IR
      PHI(I)=PHI0(I)
      DO 700 L=1,IR
      PHI(I)=PHI(I)+ZINT(I,L)*PX(L)/2.D0
  700 CONTINUE
      DO 800 I=1,IR
      IF(DABS(PHI(I)-PHI0(I)).GT.0.0005D0)GO TO 805
  800 CONTINUE
      IEND=1
  805 IF(IT.GT.29)RETURN
      DO 850 I=1,IR
  850 PHI0(I)=PHI(I)
      GO TO 80
      END
