C
C
C
      PROGRAM   PRMS
C
C     + + + PURPOSE + + +
C
C   PRMS SOURCE CODE
C   COPIED AND MODIFIED 4-22-83   ( FLAGGED BY XXXXXXXX )
C        ALL READ AND WRITE STATEMENTS CHANGED FOR PRIME
C        PROGRAM-WIDE FILE CHANGES :  OLD    NEW
C                                      21     19
C                                      22     32
C                                      23     33
C                                      24     34
C                                      25     35
C                                      26     36
C                                      27     37
C                                       2     42
C                                       3     43
C                                       4     38
C                                      10     45
C    PRMS COPIED AND MODIFIED 10-83   ( FLAGGED BY 1083KF )
C       OPTION TO OUTPUT DAILY AND UNIT DATA TO PLTGEN FILE
C       PLTGEN FILES HAS BEEN ADDED.
C       NEW FILES ARE :
C                 DAILY PLTGEN FILE    40
C                 UNIT PLTGEN FILE 1   50
C                 UNIT PLTGEN FILE 2   52
C                 PEAK PLTGEN FILE 1   51
C                 PEAK PLTGEN FILE 2   53
C
C
C
C     + + + COMMONS + + +
      INCLUDE 'cdates.inc'
      INCLUDE 'cstach.inc'
      INCLUDE 'cuvrt.inc'
      INCLUDE 'call.inc'
      INCLUDE 'cdpr.inc'
      INCLUDE 'cwx.inc'
      INCLUDE 'cdvdt.inc'
      INCLUDE 'cet.inc'
      INCLUDE 'copt.inc'
      INCLUDE 'csize.inc'
      INCLUDE 'csno.inc'
      INCLUDE 'copsn2.inc'
      INCLUDE 'csnop.inc'
      INCLUDE 'cpkadj.inc'
      INCLUDE 'cvol.inc'
      INCLUDE 'cdumv.inc'
      INCLUDE 'cplot.inc'
      INCLUDE 'cswtch.inc'
      INCLUDE 'cdtmn1.inc'
      INCLUDE 'cscs.inc'
      INCLUDE 'csncv.inc'
      INCLUDE 'cwdsn.inc'
      INCLUDE 'cjm.inc'
      INCLUDE 'cprobn.inc'
      INCLUDE 'cdatop.inc'                                              LS0790
      INCLUDE 'cbs.inc'                                                 KF0993
C
C     + + + SAVES + + +
      INTEGER   NXPK
      SAVE      NXPK
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   RETC, ICHK, IERR, M1, J, IRU, ITERSW
      REAL      P                                                       KF0895
      CHARACTER*64 VERSN                                                KF 0494
C
C     + + + EXTERNALS + + +
      EXTERNAL   PRFILE, DATIN, INVIN, ROSOPT, SENST, PROBGN, DVRETR
      EXTERNAL   TIMEY, PKADJ, TEMP, SOLRAD, PETS, UNITD, ROUTE, PRECIP
      EXTERNAL   INTLOS, SNOCOV, SNOBAL, SMBAL, BASFLW, SUMALL, DVPLOT
      EXTERNAL   SUMUNT, UVPLOT, SVHYD, PROBARY, RDDSN
C
C     + + + INPUT FORMATS + + +
 1100 FORMAT((10X,14F5.2))                                              LS0885
C
C     + + + OUTPUT FORMATS + + +
 2001 FORMAT(' !!!! BAD TIME SERIES FILE !!!!',                         LS0985
     *       '    FMT, RETC =',2I5,' !!!!')                             LS0985
 2000 FORMAT(/, 5X, 8('*****')/10X,' TOO MANY CRITICAL ERRORS.'/13X,
     *       'SIMULATION NOT DONE.'/10X,'CORRECT INPUT AND TRY AGAIN.',/KD1291
     *       ,5X,8('*****'))                                            KD1291
 9000   FORMAT(A70,5X,' WATER YEAR=',I5)                                KD1291
C
C     + + + END SPECIFICATIONS + + +
C
C     version and unix what info                                        KF 0494
      INCLUDE 'versn.inc'                                               KF 0494
C
C     open users input and output files                                 KF1191
      CALL PRFILE ( RETC )
      IF(RETC.NE.0) GO TO 1000                                          LS0985
C
C     open temporary files                                              KF0993
      CALL OPTEMP                                                       KF0993
C
C     read input card groups                                            KF0993
      IERR=0
      CALL DATIN (IERR,ICHK)                                            MM071283
      IF(ISIM.GT.0) CALL INVIN(IERR)                                    KF0993
C     get output data set numbers for basin and hru                     KF0993
      IF (IDOUT .EQ. 3) CALL RDDSN ( RETC )                             KF0993
      IF (RETC .NE. 0) GO TO 999                                        KF0993
      IF (IPOP2 .GT. 3) CALL SBGET ( NRU, NRES, NGW, DARU, RETC )       KF0993
      IF (RETC .NE. 0) GO TO 999                                        KF0993
C
  500 IF(IOPT.EQ.0) GO TO 600
      CALL ROSOPT(IERR)
      GO TO 700
  600 IF(ISEN.EQ.0) GO TO 650                                           LS0888
      CALL SENST(IERR)
  650 IF(PROB.EQ.0) GO TO 700                                           LS0888
      CALL PROBGN                                                       LS0888
  700 JDS=NDS
      IF(IERR.EQ.1.AND.ICHK.EQ.0) GO TO 999
      DO 300 IWY=BWY,EWY
      NXPK=1                                                            LS0885
      CALL DVRETR
      CALL TIMEY(1)
      DO 140 M1=MB,ME
      IMO=M1+9
      IF(IMO.GT.12) IMO=IMO-12
      MO=IMO
      CALL TIMEY(2)
      DO 120 IDY=IDB,IDE
      MDY=IDY
C               ADD ONE TO DATE COUNTERS AND CHECK FOR MXDY
      JLDY=JLDY+1
      JSOL=JSOL+1
      JWDY=JWDY+1
C     IF(KYR.EQ.IWY.AND.KJDY.EQ.JLDY) CALL DATEXC
      IF(JSOL.LE.MXDY) GO TO 8
      JSOL=JSOL-MXDY
    8 IF(NXPK.GT.0) THEN                                                LS0885
        IF(JLDY.EQ.IDPK(NXPK)) THEN                                     LS0885
          CALL PKADJ                                                    LS0885
          NXPK=NXPK+1                                                   LS0885
          IF(NXPK.LE.NDPK) THEN                                         LS0885
            IF(IPKAD.EQ.1) THEN                                         LS0885
              IF(IOSW.EQ.1) THEN                                        LS0885
                READ(38)PKAD                                            LS0885
              ELSE                                                      LS0885
                IF(IOSW.EQ.0) READ(36,1100)(PKAD(J),J=1,NRU)            LS0885
                IF(IWSW.EQ.1) WRITE(38)PKAD                             LS0885
              END IF                                                    LS0885
            END IF                                                      LS0885
          ELSE                                                          LS0885
            NXPK=0                                                      LS0885
          END IF                                                        LS0885
        END IF                                                          LS0885
      END IF                                                            LS0885
      IF(JLDY.NE.ISP2) GO TO 88
      DO 87 IRU=1,NRU
   87 ISO(IRU)=2
   88 IF(JLDY.NE.ISP1) GO TO 9
      DO 89 IRU=1,NRU
   89 MSO(IRU)=2
    9 ORO=DS(JWDY,1)
      EPAN=DS(JWDY,2)
C     TMX=DS(JWDY,3)                                                    GL081584
C     TMN=DS(JWDY,4)                                                    GL081584
      ORAD=DS(JWDY,5)
      DUM1=DS(JWDY,6)
      DUM2=DS(JWDY,7)
      DO 90 J=1,NTS                                                     GL081584
      TMX(J)=DVTX(JWDY,J)                                               GL081584
      TMN(J)=DVTN(JWDY,J)                                               GL081584
   90 CONTINUE                                                          GL081584
      DO 92 J=1,NPLW                                                    GL081584
      PLWE(J)=DVSP(JWDY,J)                                              GL081584
   92 CONTINUE                                                          GLO81584
      P=0.
      DO 10 IDS=1,NDS
      IF(DVP(JWDY,IDS).GT.90.) DVP(JWDY,IDS)=0.                         GL0985
      IF(DVP(JWDY,IDS).LT.0.) DVP(JWDY,IDS)=0.                          GL0985
   10 P=P+DVP(JWDY,IDS)*PPAR(IDS)
      PP=P/DAT
      IF(IDUS(3).EQ.0) GO TO 20
      CALL TEMP
      CALL SOLRAD
   20 CALL PETS
      IF(ISIM.EQ.0) GO TO 50
C     ***  CHECK FOR UNIT DATA  ***
      IF(UVWY(JWDY).EQ.0) GO TO 50
      IF(ISIM.GT.0) CALL UNITD
      IF(ISIM.GT.1) CALL ROUTE
      ICHG=0
      IF(ISUN.EQ.1) GO TO 70
      GO TO 60
   50 ICHG=1
      IF(PP.LE.0.) GO TO 40
      CALL PRECIP
   40 IF(INTSW.EQ.1) CALL INTLOS
      IF(ISNO.EQ.0) GO TO 60
      IF(NDC.GT.0) CALL SNOCOV                                          GL081585
      CALL SNOBAL
   60 CALL SMBAL
      CALL BASFLW
   70 CALL SUMALL(1)
  120 CONTINUE
      LMO=MO
  140 CONTINUE
      IF(NPRNT.EQ.0) GO TO 300                                          LS0485
      IF(IPLOT.EQ.1) CALL DVPLOT
  300 CONTINUE
      ITERSW = 0                                                          LS0790
      IF(IOPT.NE.0.OR.ISEN.NE.0.OR.PROB.NE.0) ITERSW = 1                  LS0790
      IF(NPRNT.EQ.1) THEN                                                 LS0790
        IF(ITERSW.EQ.0) THEN                                              LS0790
          CALL SUMUNT                                                     LS0790
        ELSE IF (EWY.EQ.EYROP) THEN                                       LS0790
          CALL SUMUNT                                                     LS0790
        END IF                                                            LS0790
      END IF                                                              LS0790
      IF(ISIM.NE.0) REWIND 11                                           MM071183
      IF(ITERSW.EQ.1) GO TO 500                                           LS0790
      IF(ISIM.LT.2.OR.ISAVE.EQ.0) GO TO 1000
      IF(JPR.EQ.0) GO TO 900
      REWIND 18
C     CALL PRTHYD                                                       XXXXXXXX
  900 IF(JPL.EQ.0) GO TO 950                                             1083KF
      REWIND 18
      CALL UVPLOT
  950 CONTINUE                                                           1083KF
      IF(JPG .EQ. 0) GO TO 1000                                          1083KF
      REWIND 18                                                          1083KF
      CALL SVHYD ( ISAVE, NOFSEG, NCRSEG )                               KF1191
      GO TO 1000
  999 CONTINUE
      WRITE (72,2000)
 1000 CONTINUE
      IF(IPROB.EQ.1) THEN                                               LS0888
        WRITE(70,9000) TITL,IWY                                         LS0888
        CALL PROBARY                                                    LS0888
      END IF                                                            LS0888
      STOP
      END
C
C
C
      BLOCK DATA
C
C     + + + PURPOSE + + +
C     Initialize common elements.
C
C     + + + COMMONS + + +
      INCLUDE 'call.inc'
      INCLUDE 'copt.inc'
      INCLUDE 'csize.inc'
      INCLUDE 'copsno.inc'
      INCLUDE 'csno.inc'
      INCLUDE 'cdsm.inc'
      INCLUDE 'cvol.inc'
      INCLUDE 'csent.inc'
      INCLUDE 'cdvdt.inc'
      INCLUDE 'cdpr.inc'
      INCLUDE 'cswtch.inc'
      INCLUDE 'cimprv.inc'
      INCLUDE 'copsn2.inc'
      INCLUDE 'cuvrt.inc'
      INCLUDE 'csed.inc'
      INCLUDE 'csent3.inc'
      INCLUDE 'csncv.inc'
      INCLUDE 'cstor.inc'                                                LS691
C
C     + + + DATA INITIALIZATIONS + + +
      DATA IFTS/1/,NCNT/-1/                                             LS0485
      DATA PSED,PQ,BSED,TSUW,SEDSG/200*0.0,1.3/
      DATA TSM/50*0./
      DATA RSTOR/50*0.0/                                                LS0287
      DATA NDSN/0/
      DATA ISNOSW/1/
      DATA LST,SNSV,SALB,SLST/50*1,50*0.,50*0.,50*0./
      DATA ISBAS,ISSRO,ISX1,ISSR1,ISUN/1,1,1,0,0/
      DATA ISSOL,ISTMP,ISNOYR,LMO,IPPT/1,1,0,0,1/
      DATA NDY/31,28,31,30,31,30,31,31,30,31,30,31/
      DATA IOSW,INIT,NYRI,NPRNT,IWSW,IWSW2,NOBF,NMOBF/0,0,0,1,0,0,0,0/
      DATA BRO/50*0./
      DATA SMSA/50*0./
      DATA BPK/50*0./
      DATA OBRO,OBPK/50*0.,50*0./
      DATA SWRD,TMXF,TMNF,TAVF,TAVC/50*0.,50*0.,50*0.,50*0.,50*0./
      DATA PWEQV,XIN,PST,ASC/50*0.,50*0.,50*0.,50*0./                   GL081585
      DATA DS/366*0.,1098*0.,1098*0./
      DATA DVTX/732*0.,1098*0./                                         GL081584
      DATA DVTN/732*0.,1098*0./                                         GL081584
      DATA DVSP/732*0.,1098*0./                                         GL081584
      DATA ISNO,INTSW,IASW/0,0,50*0/                                    GL081585
      DATA IPSR/366*0/
      DATA UVWY/367*0/
      DATA IDWN/10*0/                                                     LS691
C
C     + + + END SPECIFICATIONS + + +
C
      END
C
C
C
      SUBROUTINE TEMP
C
C     + + + PURPOSE + + +
C     Computes temperature for HRUs.
C
C     + + + COMMONS + + +
      INCLUDE 'call.inc'
      INCLUDE 'copt.inc'
      INCLUDE 'csize.inc'
      INCLUDE 'cwx.inc'
      INCLUDE 'cscs.inc'
C
C     + + + SAVES + + +
      INTEGER   IFTSW                                                   KF 0595
      REAL   ELFAC(50), TCRX(50), TCRN(50)
      SAVE   ELFAC, TCRX, TCRN, IFTSW                                   KF 0595
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   J, K, IRU                                               KF 0595
      REAL      ELCR(50), FIV9, ELDIF, TMXC, TMNC
C
C     + + + DATA INITIALIZATIONS + + +
      DATA FIV9,IFTSW/.5556,1/                                          GL081584
C
C     + + + END SPECIFICATIONS + + +
C
      IF(ILPS.EQ.1) THEN                                                LS0287
        IF(IFTSW.EQ.1) THEN                                             LS0287
          IFTSW=0                                                       LS0287
          ELDIF=(CSEL(2)-CSEL(1))/1000.                                 LS0287
          DO 2 J=1,NRU                                                  LS0287
            ELCR(J)=(ELV(J)-CSEL(1))/1000.                              LS0287
C           MAKE ELFAC NEG TO ACCOUNT FOR TLX BEING ASSUMED NEG         GL081584
            ELFAC(J)=-ELCR(J)/ELDIF                                     LS0287
    2     CONTINUE                                                      LS0287
        END IF                                                          LS0287
        DO 15 J=1,NRU                                                   LS0287
          TCRX(J)=((TMX(2)-TMX(1))*ELFAC(J))-TXAJ(J)                    LS0287
          TCRN(J)=((TMN(2)-TMN(1))*ELFAC(J))-TNAJ(J)                    LS0287
   15   CONTINUE                                                        LS0287
      ELSE                                                              LS0287
        IF(ISTMP.EQ.1.OR.MDY.EQ.1) THEN                                 LS0287
C         INITIALIZE VARIABLES AND TEMP ADJUSTMENT FACTORS
          ISTMP=0                                                       LS0287
          DO 10 J=1,NRU                                                 LS0287
            K=KTS(J)                                                    LS0287
            ELCR(J)=(ELV(J)-CSEL(K))/1000.                              LS0287
            TCRX(J)=(TLX(MO)*ELCR(J))-TXAJ(J)
            TCRN(J)=(TLN(MO)*ELCR(J))-TNAJ(J)
   10     CONTINUE
        END IF                                                          LS0287
      END IF                                                            LS0287
C               COMPUTE HRU TEMPERATURES
      DO 40 IRU=1,NRU
      K=KTS(IRU)                                                        GL081584
      IF(PARMCA(4).EQ.20) GO TO 30
      TMXF(IRU)=TMX(K)-TCRX(IRU)                                        GL081584
      TMNF(IRU)=TMN(K)-TCRN(IRU)                                        GL081584
      TAVF(IRU)=(TMXF(IRU)+TMNF(IRU))/2.
      TAVC(IRU)=(TAVF(IRU)-32.)*FIV9
      GO TO 40
   30 TMXC=TMX(K)-TCRX(IRU)                                             GL081584
      TMNC=TMN(K)-TCRN(IRU)                                             GL081584
      TMXF(IRU)=(TMXC/FIV9)+32.
      TMNF(IRU)=(TMNC/FIV9)+32.
      TAVF(IRU)=(TMXF(IRU)+TMNF(IRU))*.5
      TAVC(IRU)=(TAVF(IRU)-32.)*FIV9
   40 CONTINUE
      RETURN
      END
C
C
C
      SUBROUTINE TIMEY
     ?                 ( INTRY )
C
C     + + + PURPOSE + + +
C     ?????
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INTRY
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INTRY  - ?????
C
C     + + + COMMONS + + +
      INCLUDE 'call.inc'
      INCLUDE 'cdpr.inc'
      INCLUDE 'cdvdt.inc'
      INCLUDE 'copt.inc'
      INCLUDE 'csize.inc'
      INCLUDE 'cdates.inc'
      INCLUDE 'csnop.inc'
      INCLUDE 'copsn2.inc'
      INCLUDE 'cpkadj.inc'
C
C     + + + SAVES + + +
      INTEGER   JSL, MXJD, MTH(12)                                      KF 0595
      SAVE      JSL, MXJD, MTH                                          KF 0595
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   KSV(30), IWJ, J, K, IYR, K1, JJ, K2,                    KF 0595
     #          IRU, INC
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + DATA INITIALIZATIONS + + +
      DATA MTH/92,123,151,182,212,243,273,304,335,0,31,61/
C
C     + + + INPUT FORMATS + + +
  800 FORMAT(10X,14I5)
  809 FORMAT((10X,14I5))                                                LS0885
  810 FORMAT((10X,14F5.2))                                              LS0885
C
C     + + + END SPECIFICATIONS + + +
C
C               SET DATE COUNTERS
      IF(INTRY.EQ.2) GO TO 150
      MB=1
      IF(IWY.NE.BWY) GO TO 10
      MB=BMO+3
      IF(BMO.GT.9) MB=BMO-9
   10 ME=12
      IF(IWY.NE.EWY) GO TO 50
      ME=EMO+3
      IF(EMO.GT.9) ME=EMO-9
   50 IWJ=IWY
      J=MOD(IWJ,4)
      IF(J.NE.0) GO TO 70
      JSL=0
      MXJD=366
      NDY(2)=29
      IF(MTH(3).EQ.152) GO TO 90
      DO 60 K=3,9
   60 MTH(K)=MTH(K)+1
      GO TO 90
   70 MXJD=365
      NDY(2)=28
      IF(MTH(3).EQ.151) GO TO 85
      DO 80 K=3,9
   80 MTH(K)=MTH(K)-1
   85 JSL=0
      IF(MOD((IWJ-1),4).EQ.0) JSL=1
   90 IYR=IWY-BWY+1
      IYR=IYR+NYRI
      IF(IPSW.EQ.0) GO TO 400
      IF(IOSW.EQ.1) GO TO 330
      IF(IOSW.EQ.2) GO TO 340
      DO 300 J=1,366
  300 IPSR(J)=0
      READ(35,800)K1,(KSV(J),J=1,K1)
      IF(KSV(1).EQ.0) GO TO 315
      DO 310 J=1,K1
      JJ=KSV(J)
  310 IPSR(JJ)=1
  315 READ(35,800) K2,(KSV(J),J=1,K2)
      IF(KSV(1).EQ.0) GO TO 340
      DO 320 J=1,K2
      JJ=KSV(J)
  320 IPSR(JJ)=2
  340 IF(IWSW.EQ.1)WRITE(38)IPSR
      GO TO 400
  330 READ(38)IPSR
  400 IF(IPKAD.GT.0) THEN                                               LS0885
        IF(IOSW.EQ.1)THEN                                               LS0885
          READ(38)NDPK,IDPK                                             LS0885
          IF(IPKAD.EQ.1) READ(38)PKAD                                   LS0885
        ELSE                                                            LS0885
          IF(IOSW.EQ.0)THEN                                             LS0885
            READ(36,809) NDPK,(IDPK(J),J=1,NDPK)                        LS0885
            IF(IPKAD.EQ.1)READ(36,810)(PKAD(J),J=1,NRU)                 LS0885
          END IF                                                        LS0885
          IF(IWSW.EQ.1) THEN                                            LS0885
            WRITE(38) NDPK,IDPK                                         LS0885
            IF(IPKAD.EQ.1) WRITE(38)PKAD                                LS0885
          END IF                                                        LS0885
        END IF                                                          LS0885
      END IF                                                            LS0885
      IF(IOSW.EQ.2) IOSW=0
      DO 110 IRU=1,NRU
      ISO(IRU)=1
      LSO(IRU)=0
      MSO(IRU)=1
  110 CONTINUE
      RETURN
C
  150 MYR=IWY
      IF(MO.GT.9) MYR=IWY-1
      IDB=1
      IF(IWY.NE.BWY) GO TO 200
      IF(MO.EQ.BMO) IDB=BDY
  200 IDE=NDY(MO)
      IF(IWY.NE.EWY) GO TO 220
      IF(MO.EQ.EMO) IDE=EDY
  220 INC=IDB-1
      JWDY=MTH(MO)+INC
      IF(MO.GT.9) GO TO 250
      MXDY=MXJD
      JLDY=MTH(MO)-92+INC
      GO TO 260
  250 MXDY=365+JSL
      JLDY=MTH(MO)+273+JSL
  260 JSOL=JLDY+10
C          CHECK FOR START AND END OF PPT ADJUST PERIOD                 GL1085
      IF(MO.EQ.MPCS) MPC1=1                                             GL1085
      IF(MO.EQ.MPCN) MPC1=0                                             GL1085
      LMO=MO                                                            GL1085
      RETURN
      END
C
C
C
      SUBROUTINE SOLRAD
C
C     + + + PURPOSE + + +
C     Computes solar radiation.
C
C     + + + COMMONS + + +
      INCLUDE 'call.inc'
      INCLUDE 'copt.inc'
      INCLUDE 'crd.inc'
      INCLUDE 'cwx.inc'
      INCLUDE 'cdpr.inc'
      INCLUDE 'cet.inc'
      INCLUDE 'cgeo1.inc'
C
C     + + + SAVES + + +
      INTEGER   KCK, NPER, LSV, LP(24)                                  KF 0595
      REAL      CNT, SPER, HPSV(20), HSSV(20), SSV(20), PSV(20)
      SAVE CNT,SPER,HPSV,HSSV,SSV,PSV,KCK,NPER,LSV,LP                   LS 0595
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   L, J, K, IP, JP, IR, KP, KP1, JKEEP                     KF 0595
      REAL      CRAD(20), DRAD(20), SOLF(26), HDY(20), C, PA, DD,
     #          TDIF, RAJ, SKY, DDA,R
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + DATA INITIALIZATIONS + + +
      DATA LP/19,13,15,13,15,14,14,15,14,15,14,21,20,15,14,
     +15,15,14,15,14,14,14,14,18/
      DATA SOLF/.20,.35,.45,.51,.56,.59,.62,.64,.655,.67,.682,.69,.70,
     +.71,.715,.72,.722,.724,.726,.728,.73,.734,.738,.742,.746,.75/
C
C     + + + END SPECIFICATIONS + + +
      IF(ISSOL.EQ.0) GO TO 40
C***
C***  INITIALIZE VARIABLES
C***
      ISSOL=0
      L=MYR
      LP(5)=15
      IF(JSOL.LE.10) L=MYR+1
      J=MOD(L,4)
      IF(J.NE.0) GO TO 10
      LP(5)=16
   10 NPER=1
      DO 20 J=1,24
        JKEEP = J                                                       KF 0595
        NPER=NPER+LP(J)
        IF (JSOL.LT.NPER) GO TO 30
   20 CONTINUE
   30 LSV=JKEEP                                                         KF 0595
      SPER=LP(JKEEP)                                                    KF 0595
      C=NPER-JSOL
      CNT=SPER-C
      KCK=1
      IF(CNT.LE.0.) KCK=0
      GO TO 60
C***
C***  COMPUTE LOCATION OF JSOL IN SOLAR TABLE RAD
C***
   40 IF(NPER.NE.JSOL) GO TO 140
      IF(LSV.NE.24) GO TO 50
      NPER=1
      LSV=0
      LP(5)=15
      L=MYR+1
      J=MOD(L,4)
      IF(J.NE.0) GO TO 140
      LP(5)=16
      GO TO 140
   50 LSV=LSV+1
      SPER=LP(LSV)
      NPER=NPER+LP(LSV)
   60 K=13-LSV
      IF(K) 70,80,90
   70 IP=13+K
      JP=IP-1
      GO TO 100
   80 IP=13
      JP=12
      GO TO 100
   90 IP=13-K
      JP=IP+1
  100 DO 110 J=1,NRD
      HPSV(J)=SSH(J,IP)
      HSSV(J)=SSH(J,JP)-HPSV(J)
        PSV(J)=RAD(J,IP)
  110 SSV(J)=RAD(J,JP)-PSV(J)
      IF(KCK.EQ.1) GO TO 130
      DO 120 J=1,NRD
      HDY(J)=HPSV(J)
  120 DRAD(J)=PSV(J)
      CNT=1.
      GO TO 160
  130 KCK=0
C***
C***  LINEARLY INTERPOLATE BETWEEN TABLE VALUES
C***
  140 R=CNT/SPER
      DO 150 J=1,NRD
      HDY(J)=HPSV(J)+(R*HSSV(J))
  150 DRAD(J)=PSV(J)+(R*SSV(J))
      CNT=CNT+1.
  160 HORAD=DRAD(1)
      IF(MRDC.EQ.0) GO TO 185
      IF(ORAD.LE.0..OR.ORAD.GE.10000.) GO TO 195                        LS0287
  170 DO 180 J=1,NRD
  180 CRAD(J)=(DRAD(J)/HORAD)*ORAD/COSSL(J)                             LS0588
  185 DO 190 J=1,NRU
        IR=IRD(J)
      DYL(J)=HDY(IR)
  190 SWRD(J)=CRAD(IR)
      RETURN
C---
C---  COMPUTE ORAD
C---
  195 PA=1.0
      IF(MRDC.EQ.2) GO TO 300
C***
C***  COMPUTE USING DEGREE DAY COEFF
C***
      DD=(RDM(MO)*TMX(1))+RDC(MO)+1.                                    GL081584
      IF(PP.LE.0.) GO TO 230
      IF(TMX(ITSOL).LT.TSOLX(MO)) GO TO 200                             GL1085
      TDIF=TMX(ITSOL)-TSOLX(MO)                                         GL1085
      PA=RTB+(RTC*TDIF)                                                 GL1085
      IF(PA.GT.1.0) PA=1.0                                              GL1085
      GO TO 230                                                         GL1085
  200 PA=PARW
      IF(TMXF(1).LT.PAT(MO)) GO TO 230
      IF(MO.GT.4.AND.MO.LT.10) PA=PARS
  230 IF(DD.LT.1.) DD=1.
      IF(DD.LT.26.) GO TO 240
      RAJ=RDMX
      GO TO 250
  240 KP=DD
      DDA=KP
      KP1=KP+1
C     LINEARLY INTERPOLATE BETWEEN TABLE VALUES
      RAJ=SOLF(KP)+((SOLF(KP1)-SOLF(KP))*(DD-DDA))
  250 RAJ=RAJ*PA
      IF(RAJ.LT..2) RAJ=.2
      ORAD=RAJ*HORAD
      GO TO 170
C***
C***  COMPUTE USING SKY COVER
C***
  300 SKY=(RDM(MO)*(TMX(1)-TMN(1)))+RDC(MO)                             GL081584
      IF(SKY.LE.0.) SKY=0.
      IF(SKY.GT.1.) SKY=1.
      IF(PP.LE.0.) GO TO 310
      PA=PARW
      IF(MO.GT.4.AND.MO.LT.10) PA=PARS
  310 RAJ=RDB+(1.-RDB)*((1.-SKY)**RDP)
      IF(RAJ.GT.RDMX) RAJ=RDMX
      RAJ=RAJ*PA
      ORAD=RAJ*HORAD
      GO TO 170
      END
C
C
C
      SUBROUTINE PETS
C
C     + + + PURPOSE + + +
C     Computes evaporation.
C
C     + + + COMMONS + + +
      INCLUDE 'call.inc'
      INCLUDE 'cet.inc'
      INCLUDE 'cdates.inc'
      INCLUDE 'cdpr.inc'
      INCLUDE 'csize.inc'
      INCLUDE 'csent3.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   J, IRU
      REAL      FIV9, CT, TMXC, FRAC, ELH, RIN, VPSAT, VDSAT
C
C     + + + INTRINSICS + + +
      INTRINSIC   EXP
C
C     + + + DATA INITIALIZATIONS + + +
      DATA FIV9/.5556/                                                  LS110483
C
C     + + + END SPECIFICATIONS + + +
C
C               CHECK FOR START OR END OF TRANSPIRATION PERIOD
      IF(MDY.NE.IDB) GO TO 8
      DO 5 J=1,NRU
      IF(MO.NE.ITST(J)) GO TO 4
      IT(J)=1
      TSM(J)=0.
      IF(IDUS(4).NE.0) GO TO 4
      ITSW(J)=1
      IT(J)=0
    4 IF(MO.NE.ITND(J)) GO TO 5
      ITSW(J)=0
      IT(J)=0
      IF(XIN(J).LE.0.) GO TO 5
      XINLOS(J)=XINLOS(J)+(XIN(J)*(COVDNS(J)-COVDNW(J)))
    5 CONTINUE
C               CHECK FOR START OF ACTIVE ET PERIOD
    8 CT=CTS(MO)                                                        LS110483
      IF(IDUS(4).EQ.0) GO TO 20                                         LS110483
      DO 10 IRU=1,NRU
      IF(IT(IRU).EQ.0) GO TO 10
      IF(TMXF(IRU).LE.32.) GO TO 10                                     LS110483
      IF(PARMCA(3).EQ.20) GO TO 11                                      LS110483
      TSM(IRU)=TSM(IRU)+TMXF(IRU)
      GO TO 12                                                          LS110483
   11 TMXC=(TMXF(IRU)-32.)*FIV9                                         LS110483
      TSM(IRU)=TSM(IRU)+TMXC                                            LS110483
   12 IF(TSM(IRU).LE.TST(IRU)) GO TO 10                                 LS110483
      ITSW(IRU)=1
      IT(IRU)=0
      IF(XIN(IRU).LE.0.) GO TO 10
      IF(COVDNS(IRU).GT.0.) GO TO 9
      XIN(IRU)=0.
      GO TO 10
    9 FRAC=COVDNW(IRU)/COVDNS(IRU)
      XIN(IRU)=FRAC*XIN(IRU)
   10 CONTINUE
C               SELECT ET COMPUTATION PROCEDURE
   20 IF(IPET.EQ.2) GO TO 60
      IF(IPET.EQ.1) GO TO 40
C               MODIFIED JENSEN-HAISE EQUATION
      DO 30 IRU=1,NRU
      ELH=(597.3-(.57*TAVC(IRU)))*2.54
      RIN=SWRD(IRU)/ELH
      PET(IRU)=CT*(TAVF(IRU)-CTX(IRU))*RIN                              LS110483
      IF(PET(IRU).LT.0.) PET(IRU)=0.
   30 CONTINUE
      GO TO 100
C               HAMON EQUATION
   40 DO 50 IRU=1,NRU
      VPSAT=6.108*EXP(17.26939*TAVC(IRU)/(TAVC(IRU)+237.3))
      VDSAT=216.7*VPSAT/(TAVC(IRU)+273.3)
      PET(IRU)=CT*DYL(IRU)*DYL(IRU)*VDSAT                               LS110483
   50 CONTINUE
      GO TO 100
C               EVAP PAN DATA
   60 DO 70 IRU=1,NRU
      PET(IRU)=EPAN*EVC(MO)
   70 CONTINUE
  100 RETURN
      END
