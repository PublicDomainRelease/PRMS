C
C
C
      SUBROUTINE   SUMALL
     I                    ( INTRY )
C
C     + + + PURPOSE + + +
C     Compile and print summary information.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INTRY
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INTRY  - ?????
C
C     + + + COMMONS + + +
      INCLUDE 'cdates.inc'
      INCLUDE 'call.inc'
      INCLUDE 'cbs.inc'
      INCLUDE 'copt.inc'
      INCLUDE 'crd.inc'
      INCLUDE 'csize.inc'
      INCLUDE 'cstach.inc'
      INCLUDE 'csm.inc'
      INCLUDE 'csno.inc'
      INCLUDE 'csnop.inc'
      INCLUDE 'csncv.inc'
      INCLUDE 'cwx.inc'
      INCLUDE 'cdvdt.inc'
      INCLUDE 'cdpr.inc'
      INCLUDE 'csent.inc'
      INCLUDE 'cplot.inc'
      INCLUDE 'cuvrt.inc'
      INCLUDE 'cimprv.inc'
      INCLUDE 'cupr.inc'
      INCLUDE 'cunss.inc'
      INCLUDE 'csed.inc'
      INCLUDE 'cstor.inc'
      INCLUDE 'csent2.inc'
      INCLUDE 'csent3.inc'
      INCLUDE 'cswtch.inc'
      INCLUDE 'cgeo1.inc'
      INCLUDE 'cjm.inc'
      INCLUDE 'csvwdm.inc'                                              KF1191
C
C     + + + SAVES + + +
      INTEGER   IBDY
      INTEGER   DATEW(6), IPTH, IPTD, IEOR                                KF 1191
      REAL      OBSRUN(366), YGWSNK(50), YGW(50), YRES(50), SOF(10),
     #          SUG, SUS, UPPT(50), UAET(50), UPET(50), UAPT(50),
     #          URO(50), UINT(50), USMLT(50), UG(50), US(50), UGUGS(50),
     #          UUGS(50), UGWSS(50), UBASQ(50), URUSS(50), USTOGW(50),
     #          URASQ(50), SMPPT, SMETA, SMAPT, SMET, SMRO, SMINT, SMML,
     #          SSAS, SRAS, SBAS, SMOA, SMOAIN, SMOBIN, SMPRIN, SMOB,
     #          SMIR, SDBAS, SDRAS, SDSAS, SOFP(10), SSOF(10), AUPT(50),
     #          AUAPT(50), AUINT(50), AUPET(50), AUAET(50), AUSMLT(50),
     #          AURO(50), AUG(50), AUS(50), AGUGS(50), AUGS(50),
     #          AGWSS(50), ABASQ(50), ARUSS(50), ARASQ(50), ASTOGW(50),
     #          ASINT, ASRO, ASOA, ASOB, ASET, ASETA, ASAPT,
     #          ASML, ASIR, ASOAIN, ASOBIN, ASPRIN, TGWS
      CHARACTER*4 PLID
      SAVE IBDY,OBSRUN,YGWSNK,YGW,YRES,SOF,SUG,SUS,PLID,                LS0287
     *UPPT,UAET,UPET,UAPT,URO,UINT,USMLT,UG,US,UGUGS,UUGS,UGWSS,UBASQ,  LS0287
     *URUSS,USTOGW,URASQ,                                               LS0287
     *SMPPT,SMETA,SMAPT,SMET,SMRO,SMINT,SMML,SSAS,SRAS,SBAS,SMOA,       LS0287
     *SMOAIN,SMOBIN,SMPRIN,SMOB,SMIR,SDBAS,SDRAS,SDSAS,SOFP,SSOF,       LS0287
     *AUPT,AUAPT,AUINT,AUPET,AUAET,AUSMLT,AURO,AUG,AUS,AGUGS,AUGS,      LS0287
     *AGWSS,ABASQ,ARUSS,ARASQ,ASTOGW,ASPPT,ASINT,ASRO,ASOA,ASOB,ASET,   LS0287
     *ASETA,ASAPT,ASML,ASIR,ASOAIN,ASOBIN,ASPRIN,                       LS0287
     *TGWS                                                              LS0287
      SAVE      DATEW, IPTH, IPTD                                         KF 1191
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   PMON(13), MOPR(12),IEOY, NNRU, NINE, J,MNRU, IRU,
     #          I,       IRS, IGW, II, KR, NDAYS, NOB, NDAYS1, K,         KF1191
     #          ICY, ISIX, LD, LCY, KCY, ICNT, ICMO, IPARM, JSTAT
      INTEGER   KNTD, TSSTEP, DTOVWR, QFLG, TCODE, RETC, TSF              KF1191
      REAL      XIT(50), STOIN(10), EAMN(4), PEA(2),
     #          PWRT(31), PHRU(10), DMON(12), DVPQ(12,31),
     #          PRP(6), ORP(6), OF(10),
     #          GWSS(50), SSTOGW(50), GUGS(50), RUSS(50), GS(50),
     #          UNRES(50,96), OFLT(50), RQD,
     #          VNO, PRODIF, TIME, OROT, PRO, PROSAV, RO,
     #          PRIN, OAIN, OBIN, COV, G, YG, R, YR, DIFFOP, OFLGO,
     #          OFLGP, DIFLG, ET, SM, PW, PAR, SML, SAR, ETA, APT,
     #          XINT, A, RS, DBAS, DRAS, DSAS, TRO, CBAS, CRAS, CSAS,
     #          TMXPRT, TMNPRT, DAYS, PMN, OMN, AMN, ASPPT, DAYY,
     #          APMN, AOMN, AJMN
      CHARACTER*4 SAK, BLANK, Z, SNZ, RNZ, UNZ
      CHARACTER*15 STAIQ                                                LS0287
      LOGICAL   FIRST
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (UINF(1,1),UNRES(1,1))
      EQUIVALENCE (BSED(1),OFLT(1))                                     LS0287
      EQUIVALENCE (SEDSG,RQD)                                           LS0287
C
C     + + + INTRINSICS + + +
      INTRINSIC   MIN0, ABS, ALOG, INT, SQRT, FLOAT, MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL    RESVRD, WDTPUT, SBSAV, SBWRT                          KF0993
C
C     + + + DATA INTITIALIZATIONS + + +
      DATA DMON/31.,28.,31.,30.,31.,30.,31.,31.,30.,31.,30.,31./
      DATA PMON/0,31,61,92,123,151,182,212,243,273,304,335,365/
      DATA BLANK/1H /,SNZ/1HS/,RNZ/1HR/,UNZ/1HU/
      DATA VNO/999999./
      DATA DVPQ/372*999999./
      DATA IEOY/1/, NNRU / 10 /                                          0884KF
      DATA PRODIF/0./
      DATA MOPR/12*0/                                                   LS110483
      DATA FIRST / .TRUE. /
      DATA  DATEW, IPTH, IPTD, TSF, TSSTEP, TCODE, DTOVWR, QFLG         KF1191
     >     /  6*0,    0,    0,  10,      1,     4,      1,    0 /       KF1191
C                                                                        1083KF
C     + + + OUTPUT FORMATS + + +
   50 FORMAT(/,' IRU    MO DY  YR   SWR TMX TMN OPPT NPPT INT  INLS P', KD1291
     1'ET  AET  SMAV PWEQV DEN  PACT ALB  TCAL SMLT INFL UGS  USS  SRO')KD1291
   51 FORMAT(1X,I3,3X,2I3,I5,F5.0,2F4.0,8F5.2,2F6.2,F4.2,F6.1,5F5.2)
   91 FORMAT(6X,   ' RES#   SSR-IN  SSR-STO  SSR-FLOW  SSR-TO-GW',      KD1291
     1'   GW-IN   GWSS-IN   GW-STOR   GW-FLOW   GW-SINK')               KD1291
   92 FORMAT(7X,I3,2X,2F8.3,2X,F8.3,3X,F8.3,9X,F8.3,1X,F8.3,3F10.3)
  110 FORMAT('1',A70    ,//,' MO DY YEAR TMX TMN  ORAD  O-PPT  N-PPT I',KD1291
     1'NLOS P-ET A-ET SMAV  %SN#SN PWEQV SMELT GW-ST RS-ST GW-FL RS-FL',KD1291
     2'  SRO  TRO   P-ROFF   O-ROFF',//)                                KD1291
  115 FORMAT(1X,2(I2,1X),I4,2F4.0,F7.1,F6.2,A1,F6.2,1X,3F5.2,F6.2,1X,
     12I3,4F6.2,4F6.3,2F8.2)
  224 FORMAT(/1X,   'MUS  IRU MO   YEAR   SA    ELEV    O-PPT   N-PPT ',KD1291
     1'  INTCP   POTET   ACTET   SM-AV   PWEQV   SMELT     UGS     USS',KD1291
     2'     SRO')                                                       KD1291
  225 FORMAT(5X,I4,I3,3X,I4,2X,A4,F8.0,12F8.2)
  310 FORMAT(/,2X,'MO SUMMARY',I3,'/',I4,7X,'O-PPT  N-PPT INLOS P-ET',  KD1291
     1'  A-ET',19X,'SMELT             GW-FL RS-FL  SRO         P-ROFF ',KD1291
     2' O-ROFF',/,27X,F5.2,1X,4F6.2,18X,F5.2,12X,2F6.2,F5.2,3X,2F9.2,   KD1291
     3/,114X,2('(',F7.2,')'))                                           KD1291
  315 FORMAT(/,105X,'MEAN CFS',2F9.2)                                   KD1291
  317 FORMAT(3X,'SW RES STORAGE(IN) FOR RES -',10(I2,1X,F6.2))          KD1291
  319 FORMAT(/,47X,'ERROR SUMMARY',/,28X,'ABS VALUE OBJ FNC',           KD1291
     120X,'SUM OF SQUARES OBJ FNC',/,22X,'NO LOG',17X,'LOG',14X,        KD1291
     2'NO LOG',17X,'LOG',/,'     SUM',4F20.2,/,' CUM SUM',4F20.2,/,     KD1291
     3'    MEAN',4F20.2,/,' PERCENT',F20.2,20X,F20.2,//)                KD1291
  419 FORMAT(/,8X,'IRU        SL-AS   ELEV     O-PPT    N-PPT  INTLOS', KD1291
     1'      PET      AET   SMELT     UGS      USS     SRO')            KD1291
  420 FORMAT(1X,'AUS ',2I4,6X,A4,F8.0,9F9.2)                            KD1291
  510 FORMAT(/,1X, 'ANNUAL SUMMARY ', I4,                                KF0595
     $             ' OBSERVED PRECIP=', F6.2,                            KF0595
     $             '  POTENTIAL ET=', F6.2,                              KF0595
     $             '  PREDICTED RUNOFF(IN) =', F10.2,                    KF0595
     $             '   OBSERVED RUNOFF(IN) =', F10.2,                    KF0595
     $      /,19X, '       NET PRECIP=', F6.2,                           KF0595
     $             '     ACTUAL ET=', F6.2,                              KF0595
     $        18X, '(CFS)=', F10.2, 18X, '(CFS)=', F10.2,                KF0595
     $      /,19X, 'INTERCEPTION LOSS=', F6.2,                           KF0595
     $             '      SNOWMELT=', F6.2,                              KF0595
     $         8X, 'MEAN DAILY(CFS)=', F10.2,                            KF0595
     $         8X, 'MEAN DAILY(CFS)=', F10.2, / )                        KF0595
  530 FORMAT(3X,'GW IN=',F6.2,'  SSR IN=',F6.2,'  SSR TO GW=',F6.2,     KD1291
     1'  SURFACE RO=',F6.2,'  SSR FLOW=',F6.2,'  GW FLOW=',F6.2,        KD1291
     2'  GW SINK=',F6.2)                                                KD1291
  701 FORMAT(  A4, T6, 'PRMS GENERATED PLOT FILE',                       1083KF
     *      /, A4, T6, 'TIME INTERVAL: 1440 MINS',                       1083KF
     *             T40,'LAST MONTH IN PRINTOUT YEAR:  9',                1083KF
     *      /, A4, T6, 'NO. OR CURVES PLOTTED:',                         1083KF
     *             T30,'POINT-VALUED:  0',                               1083KF
     *             T49,'MEAN-VALUED:', I3,                                884KF
     *             T67,'TOTAL', I3,                                       884KF
     *      /, A4, T6, 'LABEL FLAG:  0',                                 1083KF
     *             T30,'PIVL:    1',                                     1083KF
     *             T50,'IDELT: 1440' )                                   1083KF
  702 FORMAT(  A4, T6, 'PLOT TITLE:   ', A40 ,                           1083KF
     *      /, A4, T6, 'Y-AXIS LABEL: ','DISCHARGE, IN CUBIC FEET PER ', 0312AL
     *             'SECOND',                                             0312AL
     *      /, A4, T6, 'SCALE INFO:',                                    1083KF
     *             T19,'YMIN:', G10.2,                                   1083KF
     *      /, A4, T19,'YMAX:', G10.2,                                   1083KF
     *      /, A4, T19,'TIME:', G10.2, 4X, 'INTERVALS/INCH',             1083KF
     *      /, A4, T6, 'DATA FOR EACH CURVE ',                           1083KF
     *             T26,'(POINT-VALUED FIRST, THEN MEAN-VALUED):',        1083KF
     *      /, A4, T6, 'LABEL',                                          1083KF
     *             T30,'LINTYP     INTEQ    COLCOD      TRAN   TRANCOD') 1083KF
  703 FORMAT(  A4, T6, 'PRED DISCH (CFS)', T62, 'AVER',                  1083KF
     *      /, A4, T6, 'OBSV DISCH, CFS ', T62, 'AVER',                  1083KF
     *      /, A4, T6, 'OBSV PRECP, IN  ', T62, 'SUM ',                  1083KF
     *      /, A4, T6, 'POTENTIAL ET, IN', T62, 'SUM ',                  1083KF
     *      /, A4, T6, 'ACTUAL ET, IN   ', T62, 'SUM ',                  1083KF
     *      /, A4, T6, 'SOIL MOISTURE,IN', T62, 'AVER',                  1083KF
     *      /, A4, T6, 'GW DISCH, CFS   ', T62, 'AVER',                  1083KF
     *      /, A4, T6, 'INTERFLOW, CFS  ', T62, 'AVER',                  1083KF
     *      /, A4, T6, 'SURFACE FLOW,CFS', T62, 'AVER',                  1083KF
     *      /, A4 )                                                      1083KF
  704 FORMAT(  A4, T6, 'TIME SERIES (PT-VALUED, THEN MEAN-VALUED):',     1083KF
     *      /, A4,                                                       1083KF
     *      /, A4, T6, 'DATE/TIME                      VALUES',          1083KF
     *      /, A4 )                                                      1083KF
  705 FORMAT(  A4, I6, 2I3, ' 24 00', 10( 2X, G12.5 ) )                   884KF
  706 FORMAT(  A4, T6, 'RU',I2, '  SR',I2, '  GW',I2, T62, 'AVER' )       884KF
  707 FORMAT(  A4 )                                                       884KF
  951 FORMAT (2X, 'Error writing basin time series to wdm file',        KF1191
     >      /,2X, '   file = ', I5,                                     KF1191
     >      /,2X, '  index = ', I5,                                     KF1191
     >      /,2X, '    dsn = ', I5,                                     KF1191
     >      /,2X, '   retc = ', I5,                                     KF1191
     >      /,2X, '   date = ', I5,5I3 )                                KF1191
  961 FORMAT (2X, 'Error writing hru time series to wdm file',          KF1191
     >      /,2X, '   file = ', I5,                                     KF1191
     >      /,2X, '  index = ', I5,                                     KF1191
     >      /,2X, '    dsn = ', I5,                                     KF1191
     >      /,2X, '   retc = ', I5,                                     KF1191
     >      /,2X, '   date = ', I5,5I3 )                                KF1191
 2000 FORMAT('1',43X,'OBSERVED AND PREDICTED RUNOFF FOR WY ',I4//)      KD1291
 2100 FORMAT('0',2X,'DAY',8X,'OCTOBER',13X,'NOVEMBER',11X,'DECEMBER',   KD1291
     113X,'JANUARY',13X,'FEBRUARY',13X,'MARCH')                         KD1291
 2200 FORMAT(4X,6(8X,'OBS',5X,'PRED'))                                  KD1291
 2960 FORMAT(A4,12X,200A4,166A4)
 2961 FORMAT(I10,12X,200E16.8,166E16.8)
 2550 FORMAT(3X,I2,6(4X,F7.2,2X,F7.2))                                    ****
 2920 FORMAT(2X,A2,'USGS ',A15,3A4,A2,A2,A4,200A4,172A4,78X,A2,44X)     KD1291
 3000 FORMAT('0',2X,'DAY',9X,'APRIL',16X,'MAY',18X,'JUNE',16X,'JULY',   KD1291
     115X,'AUGUST',12X,'SEPTEMBER')                                     KD1291
 3010 FORMAT(/,28X,'O-PPT',2X,'N-PPT',2X,'XINT',1X,'POTET',1X,          KD1291
     1'ACTET',23X,'SMELT',15X,'IRLOS P-ROFF  TO-ROFF  O-ROFF')          KD1291
C7001 FORMAT (' ',3I5,1X,F8.1,F5.2)                                     KD1291
C
C     + + + END SPECIFICATIONS + + +
C                                                                         884KF
C-    - WRITE HEADER RECORDS TO DAILY PLTGEN FILES                        884KF
C                                                                         884KF
Ckf   IF( .NOT. FIRST ) GO TO 720                                         884KF
Ckf     FIRST = .FALSE.                                                   884KF
Ckf     PLID = TITL                                                     LS0287
Ckf     TIME = 1.0                                                       1083KF
Ckf     IF(IDOUT .EQ. 3) THEN                                             884KF
Ckf                                                                       884KF
Ckf       - BASIN AVERAGED DAILY PLTGEN FILE                              884KF
Ckf                                                                       884KF
Ckf       NINE = 9                                                        884KF
Ckf       WRITE(40,701) PLID, PLID, PLID, NINE, NINE, PLID                884KF
Ckf       WRITE(40,702) PLID, TITL,                                     LS0287
Ckf  *                  PLID, PLID, PLMN, PLID, PLMX, PLID, TIME,        1083KF
Ckf  *                  PLID, PLID                                       1083KF
Ckf       WRITE(40,703) ( PLID, J = 1, 10 )                              1083KF
Ckf       WRITE(40,704) ( PLID, J = 1, 4 )                               1083KF
Ckf       ENDIF                                                           884KF
Ckf     IF(IPOP2 .EQ. 4) THEN                                             884KF
Ckf                                                                       884KF
Ckf       - HRU DAILY PLTGEN FILE ( MAX OF FIRST 10 )                     884KF
Ckf                                                                       884KF
Ckf       NNRU = MIN0( 10, NRU )                                          884KF
Ckf       MNRU = 10 - NNRU                                                884KF
Ckf       WRITE(43,701) PLID, PLID, PLID, NNRU, NNRU, PLID                884KF
Ckf       WRITE(43,702) PLID, TITL,                                     LS0287
Ckf  *                  PLID, PLID, PLMN, PLID, PLMX, PLID, TIME,         884KF
Ckf  *                  PLID, PLID                                        884KF
Ckf       WRITE(43,706) (PLID, IRU, KRES(IRU), KGW(IRU), IRU = 1, NNRU)   884KF
Ckf       IF(MNRU .GT. 0) WRITE(43,707) ( PLID, J = 1, MNRU )             884KF
Ckf       WRITE(43,704) ( PLID, J = 1, 4 )                                884KF
Ckf       ENDIF                                                           884KF
Ckf                                                                      1083KF
  720 CONTINUE                                                           1083KF
C     write (72,8001) intry, idout, iptd, ipop2, ipth
C8001 format (1x, '-> sumall:  intry=',i2,' idout=',i2,' iptd=',i3,
C    $            ' ipop2=',i2,' ipth=',i3)
      GO TO (1,9700,350,600),INTRY                                      LS0888
    1 CONTINUE
C-
C---COMPUTE OBSERVED AND PREDICTED DAILY FLOW-
C-
      IF(IEOY.EQ.0) GO TO 100
      DO 5 I=1,366
      PRPRO(I)=0.
      PRORO(I)=0.
    5 CONTINUE
      IF(IPUN3.LE.IPUN4) GO TO 14                                       LS110483
      DO 1111 J=IPUN3,12                                                LS110483
 1111 MOPR(J)=1                                                         LS110483
      DO 12 J=1,IPUN4                                                   LS110483
   12 MOPR(J)=1                                                         LS110483
      GO TO 20                                                          LS110483
   14 DO 16 J=IPUN3,IPUN4                                               LS110483
   16 MOPR(J)=1                                                         LS110483
   20 IEOY=0                                                            LS110483
  100 IEOR=0                                                            LS061884
      IF(IWY.EQ.EWY.AND.IMO.EQ.EMO.AND.IDY.EQ.EDY) IEOR=1               LS061884
      IF(IWY.EQ.BWY.AND.IMO.EQ.BMO.AND.IDY.EQ.BDY) IBDY=JWDY            LS061884
      OROT=ORO+DIRL                                                     LS0287
      IF(NSTOR.EQ.0) GO TO 120
      IF(UVWY(JWDY).EQ.0) CALL RESVRD                                   LS691
      PRO=QRO(NSTOR+1)
      PROSAV=PRO
      DO 101 J=1,NSTOR
  101 STOIN(J)=(STO(J)*23.76)/DAT
      GO TO 130
  120 PRO=(SAS+RAS+BAS)*.04208754
      PROSAV=PRO                                                        LS103183
      IF(PRODIF.LE.0.) GO TO 125
      PRO=PRO+PRODIF
  125 PRODIF=0.
      IF(ISIM.LT.2.OR.UVWY(JWDY).EQ.0.OR.ISUN.EQ.0) GO TO 130
      RO=RQD/86400.
      PRODIF=PRO-RO
      PRO=RO
  130 PRPRO(JWDY)=PRO
      PROLST=PRO
      OBSRUN(JWDY)=ORO
      PRORO(JWDY)=ORO
      DVPQ(MO,MDY)=PRO
      PRIN=(PRO*23.76)/DAT
      OAIN=(OROT*23.76)/DAT
      OBIN=(ORO*23.76)/DAT
      GWS=SGW/DAT
      DO 135 J=1,50                                                     LS0287
      GUGS(J)=0.                                                        LS0287
      RUSS(J)=0.                                                        LS0287
  135 CONTINUE                                                          LS0287
      DO 102 IRU=1,NRU
      COV=COVDNW(IRU)
      AET(IRU)=PERV(IRU)*AET(IRU)+IMPERV(IRU)*EVIMP(IRU)
      IF(ITSW(IRU).EQ.1) COV=COVDNS(IRU)
      XIT(IRU)=XIN(IRU)*COV*PERV(IRU)
      PTN(IRU)=PTN(IRU)*PERV(IRU)+PPTI(IRU)*IMPERV(IRU)
      XINLOS(IRU)=XINLOS(IRU)*COV*PERV(IRU)
      IRS=KRES(IRU)
      IGW=KGW(IRU)
      GUGS(IGW)=GUGS(IGW)+UGS(IRU)*DARU(IRU)
      RUSS(IRS)=RUSS(IRS)+USS(IRU)*DARU(IRU)
  102 CONTINUE
      DO 85 IGW=1,NGW
      GUGS(IGW)=GUGS(IGW)/ARG(IGW)
      BASQ(IGW)=BASQ(IGW)/ARG(IGW)
      GS(IGW)=(GWSNK(IGW)-YGWSNK(IGW))/ARG(IGW)
      IF(GS(IGW).LT.0.) GS(IGW)=0.
      G=GW(IGW)/ARG(IGW)
      YG=YGW(IGW)/ARG(IGW)
      GWSS(IGW)=G-YG-GUGS(IGW)+BASQ(IGW)+GS(IGW)
      IF(GWSS(IGW).LT.0.) GWSS(IGW)=0.
   85 CONTINUE
      DO 86 IRS=1,NRES
      RUSS(IRS)=RUSS(IRS)/ARS(IRS)
      RASQ(IRS)=RASQ(IRS)/ARS(IRS)
      R=RES(IRS)/ARS(IRS)
      YR=YRES(IRS)/ARS(IRS)
      SSTOGW(IRS)=YR-R+RUSS(IRS)-RASQ(IRS)
      IF(SSTOGW(IRS).LT.0.) SSTOGW(IRS)=0.
   86 CONTINUE
C-
C---COMPUTE OBJECTIVE FUNCTION-
C-
  105 DIFFOP=ORO-PRO
      OF(1)=ABS(DIFFOP)
      OF(2)=DIFFOP*DIFFOP
      OFLGO=ALOG(ORO+1.)
      OFLGP=ALOG(PRO+1.)
      DIFLG=OFLGO-OFLGP
      OF(3)=ABS(DIFLG)
      OF(4)=DIFLG*DIFLG
      DO 106 J=1,4
  106 SOF(J)=SOF(J)+OF(J)
      IF(NPRNT.EQ.0) GO TO 200
C                                                                       LS0888
C---WRITE PROBABILITY FILE FOR PERIOD OF RECORD                         LS0888
C                                                                       LS0888
      IF (DEBUG.EQ.0) GO TO 7000                                        LS0888
      PRINT *,' PROB FILE MYR ',MYR,' MO MDY PRO ',MO,MDY,PRO           LS0888
 7000 IF (PROB.NE.1.AND.IPROB.NE.1) GO TO 745                           LS0888
      WRITE (65) MYR,MO,MDY,PRO,PP,ORO                                  LS0888
C
C     FORMATTED WRITE DURING CHECK OUT                                  JKM03/86
C
C     WRITE (65,7001) MYR,MO,MDY,PRO,PP                                 JKM03/86
C
 745  IF(IPOP1 .EQ. 0  .AND.  IPOP2 .EQ. 0  .AND.  IDOUT .NE. 3)        LS0888
     *  GO TO 200                                                        1083KF
      IF(IPOP2.EQ.0) GO TO 9700
   10 IF(IPOP2.LT.3) GO TO 95
      IF(JLDY.LT.IPUN1.OR.JLDY.GT.IPUN2) GO TO 95
      IF(IPOP2 .GT. 3) GO TO 750                                          884KF
C-
C---WRITE HRU DAILY VALUES-
C-
      WRITE(43,50)
      DO 90 IRU=1,NRU
      IDS=KDS(IRU)
      IRS=KRES(IRU)
      IGW=KGW(IRU)
      WRITE(43,51)IRU,MO,MDY,MYR,SWRD(IRU),TMXF(IRU),TMNF(IRU),
     1DVP(JWDY,IDS),PTN(IRU),
     1XIT(IRU),XINLOS(IRU),PET(IRU),AET(IRU),SMAV(IRU),PWEQV(IRU),
     1DEN(IRU),PACT(IRU),ALB(IRU),TCAL(IRU),SMLT(IRU),
     2ENFIL(IRU),UGS(IRU),USS(IRU),SRO(IRU)
   90 CONTINUE
      II=NRES
      IF(NGW.GT.NRES) II=NGW
      WRITE(43,91)
      DO 93 J=1,II
      R=0.
      G=0.
      IF(J.LE.NRES) R=RES(J)/ARS(J)
      IF(J.LE.NGW) G=GW(J)/ARG(J)
      WRITE(43,92)J,RUSS(J),R,RASQ(J),SSTOGW(J),GUGS(J),GWSS(J),G,
     1BASQ(J),GS(J)
   93 CONTINUE
      GO TO 95                                                            884KF
C                                                                         884KF
C-    - WRITE DAILY HRU DISCHARGES TO PLTGEN FILE (MAX OF 10)             884KF
C                                                                         884KF
  750 CONTINUE                                                            884KF
Ckf   DO 755 IRU = 1, NNRU                                                884KF
Ckf      IRS = KRES(IRU)                                                  884KF
Ckf      IGW = KGW(IRU)                                                   884KF
Ckf      PHRU(IRU) = ( SRO(IRU) + RASQ(IRS) + BASQ(IGW) )                 884KF
Ckf  *               * DARU(IRS) / 23.76                                  884KF
Ckf55    CONTINUE                                                         884KF
Ckf   WRITE(43,705) PLID, MYR, MO, MDY, ( PHRU(IRU), IRU = 1, NNRU)       884KF
C     save selected hru and subbasins for writting to wdm file            KF0993
      IF (DATEW(1) .EQ. 0) THEN                                           KF1191
C       set starting date                                                 KF1191
        DATEW(1) = MYR                                                    KF1191
        DATEW(2) = MO                                                     KF1191
        DATEW(3) = MDY                                                    KF1191
      END IF                                                              KF1191
      CALL SBSAV ( NRU, SRO, RASQ, BASQ, KRES, KGW, DARU )                KF0993
C
   95 DO 98 IRU=1,NRU
      IDS=KDS(IRU)
      UPPT(IRU)=UPPT(IRU)+DVP(JWDY,IDS)
      UAET(IRU)=UAET(IRU)+AET(IRU)
      UPET(IRU)=UPET(IRU)+PET(IRU)
      UAPT(IRU)=UAPT(IRU)+PTN(IRU)
      URO(IRU)=URO(IRU)+SRO(IRU)+SX(IRU)
      UINT(IRU)=UINT(IRU)+XINLOS(IRU)
      USMLT(IRU)=USMLT(IRU)+SMLT(IRU)
      UG(IRU)=UG(IRU)+UGS(IRU)
      US(IRU)=US(IRU)+USS(IRU)
   98 CONTINUE
      DO 96 J=1,NGW
      UGUGS(J)=UGUGS(J)+GUGS(J)
      UUGS(J)=UUGS(J)+GS(J)
      UGWSS(J)=UGWSS(J)+GWSS(J)
      UBASQ(J)=UBASQ(J)+BASQ(J)
   96 CONTINUE
      DO 97 J=1,NRES
      URUSS(J)=URUSS(J)+RUSS(J)
      USTOGW(J)=USTOGW(J)+SSTOGW(J)
      URASQ(J)=URASQ(J)+RASQ(J)
   97 CONTINUE
C-
C---COMPUTE DAILY BASIN AVERAGES FOR COMPONENTS-
C-
C-
C---ZERO DAILY VALUES-                                                  LS0287
C-                                                                      LS0287
 9700 KNT=0                                                             LS0888
      ET=0.                                                             LS0287
      SM=0.                                                             LS0287
      PW=0.                                                             LS0287
      PAR=0.                                                            LS0287
      SML=0.                                                            LS0287
      SAR=0.                                                            LS0287
      ETA=0.                                                            LS0287
      APT=0.                                                            LS0287
      XINT=0.                                                           LS0287
       IF(INTRY.EQ.2) GO TO 200                                         LS0888
 1001 DO 1010 IRU=1,NRU
      A=DARU(IRU)
      ETA=ETA+(AET(IRU)*A)
      ET=ET+(PET(IRU)*A)
      APT=APT+(PTN(IRU)*A)
      XINT=XINT+(XINLOS(IRU)*A)
      IF(SMLT(IRU).LE.0.)GO TO 1005
      SML=SML+(SMLT(IRU)*A)
      SAR=SAR+A
 1005 SUG=SUG+(UGS(IRU)*A)
      SUS=SUS+(USS(IRU)*A)
      SM=SM+(SMAV(IRU)*A)
      IF(PWEQV(IRU).LE.0.) GO TO 1007
      PW=PW+(PWEQV(IRU)*A)
      PAR=PAR+(DARU(IRU)*ASC(IRU))
      KNT=KNT+1
 1007 CONTINUE
 1010 CONTINUE
      XINT=XINT/DAT
      ET=ET/DAT
      ETA=ETA/DAT
      SM=SM/DAT
      APT=APT/DAT
  107 KR=INT((PAR/DAT)*100.)                                            LS0486
      PW=PW/DAT
      SML=SML/DAT
      RS=SRS/DAT
      DBAS=BAS/DAT
      DRAS=RAS/DAT
      DSAS=SAS/DAT
      TRO=DBAS+DRAS+DSAS
C                                                                        1083KF
C-    - WRITE DAILY VALUES TO PLTGEN FILE                                1083KF
C-    - CONVERT RESERVOIRS FROM INCHES TO CFS                            1083KF
C                                                                        1083KF
Ckf   IF(IDOUT .NE. 3) GO TO 740                                         1083KF
Ckf     CBAS = DBAS * DAT / 23.76                                        1083KF
Ckf     CRAS = DRAS * DAT / 23.76                                        1083KF
Ckf     CSAS = DSAS * DAT / 23.76                                        1083KF
Ckf     WRITE(40,705) PLID, MYR, MO, MDY,                                1083KF
Ckf  *                PRO, ORO, PP, ET, ETA, SM,                         1083KF
Ckf  *                CBAS, CRAS, CSAS                                   1083KF
Ckf40 CONTINUE                                                           1083KF
      IF (IDOUT .EQ. 3) THEN                                             KF1191
C       save basin time series to be written to wdm file                 KF1191
        IF (DATEW(1) .EQ. 0) THEN                                        KF1191
C         set starting date                                              KF1191
          DATEW(1) = MYR                                                 KF1191
          DATEW(2) = MO                                                  KF1191
          DATEW(3) = MDY                                                 KF1191
        END IF                                                           KF1191
        IPTD = IPTD + 1                                                  KF1191
        WDV(IPTD,1) = PRO                                                KF1191
        WDV(IPTD,2) = PP                                                 KF1191
        WDV(IPTD,3) = ET                                                 KF1191
        WDV(IPTD,4) = ETA                                                KF1191
        WDV(IPTD,5) = SM                                                 KF1191
        WDV(IPTD,6) = DBAS * DAT / 23.76                                 KF1191
        WDV(IPTD,7) = DRAS * DAT / 23.76                                 KF1191
        WDV(IPTD,8) = DSAS * DAT / 23.76                                 KF1191
      END IF                                                             KF1191
C-
C---WRITE DAILY BASIN AVERAGES-
C-
      IF(IPOP1.LT.3) GO TO 220
      IF(MOPR(MO).EQ.0) GO TO 220                                       LS110483
      IF(MDY.EQ.1) WRITE(72,110)TITL
      Z=BLANK
      IF(IPSR(JLDY).EQ.1) Z=SNZ
      IF(IPSR(JLDY).EQ.2) Z=RNZ
      IF(UVWY(JWDY).NE.0) Z=UNZ
      TMXPRT=TMX(ITSOL)                                                 GL1085
      TMNPRT=TMN(ITSOL)                                                 GL1085
      WRITE(72,115)MO,MDY,MYR,TMXPRT,TMNPRT,ORAD,PP,Z,APT,XINT,ET,ETA,   GL1085
     1SM,KR,KNT,PW,SML,GWS,RS,DBAS,DRAS,DSAS,TRO,PRO,ORO                GL1085
C-
C---SUM DAILY VALUES FOR BASIN AVERAGES-
C-
  220 SSAS=SSAS+SAS
      SRAS=SRAS+RAS
      SBAS=SBAS+BAS
      SMPPT=SMPPT+PP
      SMAPT=SMAPT+APT
      SMINT=SMINT+XINT
      SMML=SMML+SML
      PRO=PROSAV
      SMRO=SMRO+PRO
      SMOA=SMOA+OROT
      SMOAIN=SMOAIN+OAIN
      SMOBIN=SMOBIN+OBIN
      SMPRIN=SMPRIN+PRIN
      SMOB=SMOB+ORO
      SMET=SMET+ET
      SMETA=SMETA+ETA
      SMIR=SMIR+DIRL
      SDBAS=SDBAS+DBAS
      SDRAS=SDRAS+DRAS
      SDSAS=SDSAS+DSAS
C-
C-
C---ZERO HRU DAILY VALUES-
C-
  200 DO 210 IRU=1,NRU
      PPTI(IRU)=0.
      EVIMP(IRU)=0.
      AET(IRU)=0.
      PET(IRU)=0.
      SMLT(IRU)=0.
      ENFIL(IRU)=0.
      SRO(IRU)=0.
      EXCS(IRU)=0.
      SX(IRU)=0.
      PTN(IRU)=0.
      NSW(IRU)=0
      PSN(IRU)=0.
      SNEV(IRU)=0.
      XINLOS(IRU)=0.
      UGS(IRU)=0.
      USS(IRU)=0.
  210 CONTINUE
C-
C---ZERO OTHER DAILY VALUES
C-
      PP=0.
      PPT=0.
      SAS=0.
      RAS=0.
      BAS=0.
      IF(INTRY.EQ.2) RETURN                                             LS0888
      DO 1200 J=1,NRES
      YRES(J)=RES(J)
 1200 CONTINUE
      DO 1210 J=1,NGW
      YGW(J)=GW(J)
      YGWSNK(J)=GWSNK(J)
 1210 CONTINUE
      IF(IDY.NE.NDY(MO).AND.(IDY.NE.IDE.OR.IMO.NE.EMO.OR.IWY.NE.EWY))
     1GO TO 216
      NDAYS=IDE-IDB+1
      DO 205 J=1,10
      SOFP(J)=SOF(J)
  205 SOF(J)=0.
      IF(IOBSW.EQ.1) GO TO 206
      IF(MO.NE.MFS) GO TO 216
      IOBSW=1
  206 DO 211 J=1,4
      OBF(J)=OBF(J)+SOFP(J)
      SSOF(J)=SSOF(J)+SOFP(J)
  211 OFSPR(J)=SSOF(J)
      IF(MO.NE.MFN) GO TO 212
      IOBSW=0
      IF(IWY.NE.EWY) GO TO 212
      DO 207 J=1,10
  207 SSOF(J)=0.
  212 IF(IWSW2.EQ.0)GO TO 215
      NOB=JWDY-NDAYS+1
      IF(NDAYS.EQ.31)GO TO 214
      NDAYS1=NDAYS+1
      DO 208 J=NDAYS1,31
  208 PWRT(J)=0.
  214 DO 209 J=1,NDAYS
  209 PWRT(J)=PRPRO(NOB-1+J)
      WRITE(9,REC=NREC)NDAYS,(PWRT(J),J=1,31)
      NREC=NREC+1
      IF(IWSW2.NE.2)GO TO 215
      DO 217 J=1,NDAYS
  217 PWRT(J)=OBSRUN(NOB-1+J)
      WRITE(9,REC=NREC)NDAYS,(PWRT(J),J=1,31)
      NREC=NREC+1
  215 NOBF=NOBF+NDAYS
      NMOBF=NMOBF+1
  216 IF(IPOP1.EQ.0.AND.IPOP2.EQ.0) GO TO 920
      IF(IDY.NE.NDY(MO).AND.IEOR.EQ.0) RETURN                           LS0884
C-
C---WRITE MONTHLY UNIT SUMMARY-
C-
      IF(NPRNT.EQ.0) GO TO 350
      IF(IPOP2 .LT. 2  .OR.  IPOP2 .GT. 3) GO TO 300                      884KF
      WRITE(43,224)
  221 DO 250 IRU=1,NRU
      K=IRD(IRU)
      IF(K .EQ. 0) THEN                                                 0285KF
        SAK = BLANK                                                     0285KF
      ELSE                                                              0285KF
        SAK = SA(K)                                                     0285KF
      ENDIF                                                             0285KF
      WRITE(43,225)IRU,MO,MYR,SAK,  ELV(IRU),UPPT(IRU),UAPT(IRU),       0285KF
     1UINT(IRU),UPET(IRU),UAET(IRU),SMAV(IRU),PWEQV(IRU),USMLT(IRU),
     2UG(IRU),US(IRU),URO(IRU)
  250 CONTINUE
      II=NRES
      IF(NGW.GT.NRES) II=NGW
      WRITE(43,91)
      DO 275 J=1,II
       R=0.
      G=0.
      IF(J.LE.NRES) R=RES(J)/ARS(J)
      IF(J.LE.NGW) G=GW(J)/ARG(J)
      WRITE(43,92)J,URUSS(J),R,URASQ(J),USTOGW(J),UGUGS(J),UGWSS(J),G,
     1UBASQ(J),UUGS(J)
  275 CONTINUE
  300 IF(IPOP1.LT.2) GO TO 320
C-
C---WRITE MONTHLY BASIN SUMMARY-
C-
      WRITE(72,310)MO,MYR,SMPPT,SMAPT,SMINT,SMET,SMETA,SMML,SDBAS,SDRAS,
     1SDSAS,SMRO,SMOB,SMPRIN,SMOBIN
C-
C---COMPUTE MEAN DAILY FLOW AND PREDICTION ERROR-
C-
      DAYS=DMON(MO)                                                     LS061884
      IF(IEOR.EQ.1) DAYS=EDY                                            LS061884
      IF(IWY.EQ.BWY.AND.IMO.EQ.BMO) DAYS=DMON(MO)-BDY+1                 LS061884
      PMN=SMRO/DAYS                                                     LS061884
  312 OMN=SMOB/DAYS                                                     LS061884
      AMN=SMOA/DAYS                                                     LS061884
      DO 313 I=1,4
  313 EAMN(I)=SOFP(I)/DAYS                                              LS061884
      PEA(1)=9999.
      PEA(2)=9999.
      IF(AMN.LE.0.) GO TO 314
      PEA(1)=(EAMN(1)/AMN)*100.
      PEA(2)=(SQRT(EAMN(2))/AMN)*100.
  314 WRITE(72,315)PMN,OMN
      WRITE(72,319)SOFP(1),SOFP(3),SOFP(2),SOFP(4),OFSPR(1),OFSPR(3),
     1OFSPR(2),OFSPR(4),EAMN(1),EAMN(3),EAMN(2),EAMN(4),PEA(1),PEA(2)
      IF(NSTOR.EQ.0) GO TO 320
      WRITE(72,317) (J,STOIN(J),J=1,NSTOR)
C-
C---COMPUTE ANNUAL UNIT SUMS-
C-
  320 IF(IPOP2 .EQ. 0  .OR.  IPOP2 .EQ. 4) GO TO 3340                     884KF
      DO 330 IRU=1,NRU
      AUPT(IRU)=AUPT(IRU)+UPPT(IRU)
  322 AUAPT(IRU)=AUAPT(IRU)+UAPT(IRU)
      AUINT(IRU)=AUINT(IRU)+UINT(IRU)
      AUPET(IRU)=AUPET(IRU)+UPET(IRU)
      AUAET(IRU)=AUAET(IRU)+UAET(IRU)
  324 AUSMLT(IRU)=AUSMLT(IRU)+USMLT(IRU)
      AURO(IRU)=AURO(IRU)+URO(IRU)
      AUG(IRU)=AUG(IRU)+UG(IRU)
      AUS(IRU)=AUS(IRU)+US(IRU)
  330 CONTINUE
      DO 333 J=1,NGW
      AGUGS(J)=AGUGS(J)+UGUGS(J)
      AUGS(J)=AUGS(J)+UUGS(J)
      AGWSS(J)=AGWSS(J)+UGWSS(J)
      ABASQ(J)=ABASQ(J)+UBASQ(J)
  333 CONTINUE
      DO 335 J=1,NRES
      ARUSS(J)=ARUSS(J)+URUSS(J)
      ARASQ(J)=ARASQ(J)+URASQ(J)
      ASTOGW(J)=ASTOGW(J)+USTOGW(J)
  335 CONTINUE
C-
C---COMPUTE ANNUAL SUMS FOR BASIN AVERAGES-
C-
 3340 CONTINUE
      ASPPT=ASPPT+SMPPT
  334 ASINT=ASINT+SMINT
      ASRO=ASRO+SMRO
      ASOA=ASOA+SMOA
      ASOB=ASOB+SMOB
  338 ASET=ASET+SMET
      ASETA=ASETA+SMETA
      ASAPT=ASAPT+SMAPT
      ASML=ASML+SMML
      ASIR=ASIR+SMIR
      ASOAIN=ASOAIN+SMOAIN
      ASOBIN=ASOBIN+SMOBIN
      ASPRIN=ASPRIN+SMPRIN
C-
C---ZERO MONTHLY SUMS-
C-
C-
  350 DO 370 IRU=1,NRU
      UPPT(IRU)=0.
      UAPT(IRU)=0.
      UINT(IRU)=0.
      UPET(IRU)=0.
      UAET(IRU)=0.
      USMLT(IRU)=0.
      URO(IRU)=0.
      UG(IRU)=0.
      US(IRU)=0.
  370 CONTINUE
      DO 353 J=1,NGW
      UGUGS(J)=0.
      UUGS(J)=0.
      UGWSS(J)=0.
      UBASQ(J)=0.
  353 CONTINUE
      DO 354 J=1,NRES
      URUSS(J)=0.
      URASQ(J)=0.
      USTOGW(J)=0.
  354 CONTINUE
      SMPPT=0.
      SMRO=0.
      SMOA=0.
      SMOB=0.
      SMET=0.
      SMML=0.
      SMETA=0.
      SMAPT=0.
      SMINT=0.
      SMOAIN=0.
      SMOBIN=0.
      SMPRIN=0.
      SDBAS=0
      SDRAS=0.
      SDSAS=0.
      DO 380 J=1,10
  380 SOF(J)=0.
      SMIR=0.
      IF(INTRY.EQ.3) RETURN                                             LS0888
C-
  499 IF(JWDY.NE.MXDY.AND.IEOR.EQ.0) RETURN                             LS061884
      IF(NPRNT.EQ.0) GO TO 600
      IF(IPOP2 .EQ. 0  .OR.  IPOP2 .EQ. 4) GO TO 500                      884KF
C-
C---WRITE YEARLY UNIT SUMS-
C-
      WRITE(43,419)
      DO 410 IRU=1,NRU
      K=IRD(IRU)
      IF(K .EQ. 0) THEN                                                 0285KF
        SAK = BLANK                                                     0285KF
      ELSE                                                              0285KF
        SAK = SA(K)                                                     0285KF
      ENDIF                                                             0285KF
      WRITE(43,225)IRU,MO,MYR,SAK,  ELV(IRU),UPPT(IRU),UAPT(IRU),       0285KF
     1AUINT(IRU),AUPET(IRU),AUAET(IRU),AUSMLT(IRU),AUG(IRU),
     2AUS(IRU),AURO(IRU)
  410 CONTINUE
      II=NRES
      IF(NGW.GT.NRES) II=NGW
      WRITE(43,91)
      DO 450 J=1,II
      R=0.
      G=0.
      IF(J.LE.NRES)R=RES(J)/ARS(J)
      IF(J.LE.NGW)G=GW(J)/ARG(J)
      WRITE(43,92)J,ARUSS(J),R,ARASQ(J),ASTOGW(J),AGUGS(J),AGWSS(J),G,
     1ABASQ(J),AUGS(J)
  450 CONTINUE
  500 IF(IPOP1.LT.1) GO TO 600
C-
C---WRITE AVERAGE ANNUAL SUMS-
C-
      IF(IPOP1.EQ.1) GO TO 1000
  550 CONTINUE                                                          LS0884
      DAYY=FLOAT(MXDY)                                                  LS0486
      IF(IEOR.EQ.1) DAYY=FLOAT(JWDY)                                    LS0486
      IF(IWY.EQ.BWY) DAYY=FLOAT(MXDY-IBDY+1)                            LS0486
      APMN=ASRO/DAYY                                                    LS061884
      AOMN=ASOB/DAYY                                                    LS061884
      AJMN=ASOA/DAYY                                                    LS061884
      WRITE(72,510)MYR,ASPPT,ASET,ASPRIN,ASOBIN,ASAPT,ASETA,ASRO,ASOB,
     1ASINT,ASML,APMN,AOMN
      SUG=SUG/DAT
      SUS=SUS/DAT
      SSGW=SSGW/DAT
      SSAS=SSAS/DAT
      SRAS=SRAS/DAT
      SBAS=SBAS/DAT
      DO 525 J=1,NGW
      TGWS=TGWS+GWSNK(J)
  525 CONTINUE
      TGWS=TGWS/DAT
      WRITE(72,530)SUG,SUS,SSGW,SSAS,SRAS,SBAS,TGWS
      IF(IPOP1.GE.2) GO TO 600
      IF(NSTOR.EQ.0) GO TO 600
      WRITE(72,317) (J,STOIN(J),J=1,NSTOR)
C-
C---ZERO ANNUAL SUMS-
C-
  600 ASPPT=0.
      ASRO=0.
      ASOA=0.
      ASOB=0.
      ASET=0.
      ASETA=0.
      ASAPT=0.
      ASML=0.
      ASINT=0.
      ASOAIN=0.
      ASOBIN=0.
      ASPRIN=0.
      DO 605 J=1,10
      SOF(J)=0.
      OFSPR(J)=0.
  605 SSOF(J)=0.
      ASIR=0.
      SUG=0.
      SSAS=0.
      SRAS=0.
      SBAS=0.
      SUS=0.
      TGWS=0.
      SSGW=0.
      DO 610 J=1,NGW
      AGUGS(J)=0.
      AUGS(J)=0.
      AGWSS(J)=0.
      ABASQ(J)=0.
  610 GWSNK(J)=0.
      DO 620 J=1,NRES
      ARUSS(J)=0.
      ARASQ(J)=0.
      ASTOGW(J)=0.
  620 CONTINUE
      DO 910 IRU=1,NRU
      AUPT(IRU)=0.
      AUAPT(IRU)=0.
      AUINT(IRU)=0.
      AUPET(IRU)=0.
      AUAET(IRU)=0.
      AUSMLT(IRU)=0.
      AURO(IRU)=0.
      AUG(IRU)=0.
      AUS(IRU)=0.
  910 CONTINUE
      IF(INTRY.EQ.4) RETURN                                             LS0888
      IEOY=1
  920 IF(JWDY.NE.MXDY.AND.IEOR.EQ.0) RETURN                             LS061884
      IF(NPRNT.EQ.0)RETURN
      IF(IPOP1.EQ.1) GO TO 2900
C-
C---WRITE YEARLY OROT-PRO SUMMARY-
C-
 1000 CONTINUE                                                          KF1191
c     write(1,8010) idout,iptd,datew
c8010 format (1x, 'idout=',i2, ' iptd=',i3, '  date:',i5,5i3 )
      IF (IDOUT .EQ. 3  .AND.  IPTD .GT. 0) THEN                        KF1191
C       save basin daily time series to wdm file                        KF1191
        DO 950 I = 1, 8                                                 KF1191
C         check each time series                                        KF1191
          IF (DSNDV(I) .GT. 0) THEN                                     KF1191
C           save this one                                               KF1191
            CALL WDTPUT ( TSF, DSNDV(I), TSSTEP, DATEW, IPTD,           KF1191
     I                    DTOVWR, QFLG, TCODE, WDV(1,I),                KF1191
     O                    RETC )                                        KF1191
            IF (RETC .NE. 0) THEN                                       KF1191
C             error writing to wdm file                                 KF1191
              WRITE (72,951) TSF, I, DSNDV(I), DATEW                     KF1191
            END IF                                                      KF1191
          END IF                                                        KF1191
 950    CONTINUE                                                        KF1191
      END IF                                                            KF1191
      IF (IPOP2 .GT. 3) THEN                                            KF0993
C       save hru and sub-basins to wdm file                             KF0993
        CALL SBWRT ( TSF, 72, DATEW )                                   KF0993
      END IF                                                            KF1191
      IF (IDOUT .EQ. 3  .OR.  IPOP2 .GT. 3) THEN                        KF1191
C       reset date and pointers                                         KF1191
        DATEW(1) = 0                                                    KF1191
        IPTH = 0                                                        KF1191
        IPTD = 0                                                        KF1191
      END IF                                                            KF1191
C
      WRITE(72,2000)MYR                                                  KF1191
      WRITE(72,2100)
      WRITE(72,2200)
      IF(MOD(MYR,4).NE.0)GO TO 2300
      IF(PMON(6).EQ.152) GO TO 2400
      DO 2210 ICY=6,13
      PMON(ICY)=PMON(ICY)+1
 2210 CONTINUE
      GO TO 2400
 2300 IF(PMON(6).EQ.151) GO TO 2400
      DO 2310 ICY=6,13
      PMON(ICY)=PMON(ICY)-1
 2310 CONTINUE
 2400 CONTINUE
      ISIX=0
 2450 DO 2600 LD=1,31
      DO 2500 LCY=1,6
      KCY=LCY+ISIX
      ICNT=PMON(KCY)+LD
      IF(ICNT.LE.PMON(KCY+1)) GO TO 2520
      PRP(LCY)=0.
      ORP(LCY)=0.
      GO TO 2500
 2520 PRP(LCY)=PRPRO(ICNT)
      ORP(LCY)=PRORO(ICNT)
 2500 CONTINUE
      WRITE(72,2550)LD,(ORP(K),PRP(K),K=1,6)
 2600 CONTINUE
      IF(ISIX.EQ.6) GO TO 2800
      WRITE(72,2000)MYR
      WRITE(72,3000)
      WRITE(72,2200)
      ISIX=6
      GO TO 2450
 2800 IF(IPOP1.NE.1) GO TO 2900
      WRITE(72,3010)
      GO TO 550
 2900 IF(IDOUT.EQ.0) GO TO 2990
      IF(IDOUT .EQ. 3) GO TO 2990                                        1083KF
      IF(IDOUT.EQ.1)GO TO 2950
      ICMO=10
      IPARM=60
      JSTAT=3
      STAIQ=STAIDC(1)(2:16)                                             LS0287
      WRITE(20,2920)SCODE,STAIQ,VNO,VNO,                                LS0888
     1IPARM,IWY,JSTAT,VNO,((DVPQ(I,J),J=1,31),I=1,12),ICMO
      DO 2930 I=1,12
      DO 2930 J=1,31
      DVPQ(I,J)=999999.
 2930 CONTINUE
      GO TO 2990
 2950 WRITE(20) MYR,(PRORO(J),J=1,366)
      WRITE(20)MYR,(PRPRO(J),J=1,366)
 2990 IF(ISTAT.EQ.0.OR.NPRNT.EQ.0)RETURN
      WRITE(30)MYR,(PRORO(J),J=1,366)
      WRITE(30)MYR,(PRPRO(J),J=1,366)
      RETURN
      END
C
C
C
      SUBROUTINE   RESVRD
C
C     + + + PURPOSE + + +
C     ?????
C
C     + + + COMMONS + + +
      INCLUDE 'call.inc'
      INCLUDE 'cbs.inc'
      INCLUDE 'csm.inc'
      INCLUDE 'cstor.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   NST1, J, K, KR, KG, N, JJ
      REAL      QRAI(11), QRAQ(11), SRQ, SSQ, GWQ, GR, DIN2, AVIN,
     #          S2O2, Q2, XKT, C2, QR, S1O1
C
C     + + + INTRINSICS + + +
      INTRINSIC   EXP
C
C     + + + END SPECIFICATIONS + + +
      NST1=NSTOR+1
      DO 10 J=1,NST1
      QRAI(J)=0.
      QRAQ(J)=0.
   10 CONTINUE
      DO 20 J=1,NRU
      K=KSTOR(J)
      IF(K.EQ.0) K=NST1
      KR=KRES(J)
      KG=KGW(J)
      SRQ=SRO(J)*DARU(J)
      SSQ=RASQ(KR)*(DARU(J)/ARS(KR))
      GWQ=BASQ(KG)*(DARU(J)/ARG(KG))
      QRAI(K)=QRAI(K)+SRQ+SSQ+GWQ
   20 CONTINUE
   25 DO 30 J=1,NST1
      QRAQ(J)=QRAI(J)*.04208754
   30 CONTINUE
      DO 100 J=1,NSTOR
      QR=0.
      K=IRUP1(J)
      IF(K.EQ.0) GO TO 50
      QR=QRO(K)
      K=IRUP2(J)
      IF(K.EQ.0) GO TO 50
      QR=QR+QRO(K)
      K=IRUP3(J)
      IF(K.EQ.0) GO TO 50
      QR=QR+QRO(K)
   50 DIN2=QR+QRAQ(J)
      AVIN=(DIN2+DIN1(J))*.5
      IF(IRTYP(J).EQ.9) GO TO 80
      S1O1=STO(J)-(QRO(J)*.5)                                           LS691
      S2O2=S1O1+AVIN                                                    LS691
C*****LINE REMOVED                                                      LS691
      N=NSOS(J)
      DO 60 JJ=2,N
      IF(S2O2.LT.WVD(J,JJ)) GO TO 70
   60 CONTINUE
      JJ=N
   70 Q2=S24(J,JJ)*S2O2+C24(J,JJ)
      IF(Q2.LT.0.) Q2=0.
      STO(J)=S2O2-(Q2*.5)
      IF(STO(J).GE.0.) GO TO 90
      Q2=S2O2+(QRO(J)*.5)
      STO(J)=0.
      GO TO 90
   80 XKT=RCS(J)
      C2=1.-EXP(-XKT)
      Q2=(AVIN*(1.-(C2/XKT)))+(STO(J)*C2)
      IF(Q2.LE.0.) Q2=0.
      STO(J)=STO(J)+AVIN-Q2
   90 QRO(J)=Q2
      DIN1(J)=DIN2
  100 CONTINUE
      QRO(NST1) = 0.                                                    LS691
      DO 110 J = 1,NSTOR                                                LS691
        IF(IDWN(J).EQ.0) QRO(NST1)=QRO(NST1)+QRO(J)                     LS691
  110 CONTINUE                                                          LS691
      QRO(NST1) = QRO(NST1) + QRAQ(NST1)                                LS691
      RETURN
      END
