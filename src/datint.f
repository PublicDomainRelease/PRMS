      SUBROUTINE   DATIN                                                MM071283
     I                 ( ERR,
     O                   ICHK )
C
C     + + + PURPOSE + + +
C     Reads data from card group 1 input file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   ERR, ICHK
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ERR    - ?????
C     ICHK   - ?????
C
C     + + + COMMONS + + +
      INCLUDE 'cdates.inc'
      INCLUDE 'cdatop.inc'
      INCLUDE 'call.inc'
      INCLUDE 'cbs.inc'
      INCLUDE 'cdpr.inc'
      INCLUDE 'cdvdt.inc'
      INCLUDE 'cet.inc'
      INCLUDE 'copt.inc'
      INCLUDE 'crd.inc'
      INCLUDE 'csize.inc'
      INCLUDE 'cstach.inc'
      INCLUDE 'csm.inc'
      INCLUDE 'csno.inc'
      INCLUDE 'csnop.inc'
      INCLUDE 'cupr.inc'
      INCLUDE 'cwx.inc'
      INCLUDE 'cplot.inc'
      INCLUDE 'cswtch.inc'
      INCLUDE 'cimprv.inc'
      INCLUDE 'cstor.inc'
      INCLUDE 'csncv.inc'
      INCLUDE 'cgeo1.inc'
      INCLUDE 'cpkadj.inc'
      INCLUDE 'csent3.inc'
      INCLUDE 'cwdsn.inc'
      INCLUDE 'cscs.inc'
      INCLUDE 'cprobn.inc'
      INCLUDE 'cjm.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   JC(45), ERRP, IC, J, I, K, KK, L, JJ, JN, IRU, M, JK
      REAL      STOIN(10), SL, DGSL, ASP, ALAT, SDARU, DATC
      CHARACTER*8 VERS                                                  LS0287
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS, ATAN, COS, MAX0
C
C     + + + EXTERNALS + + +
      EXTERNAL   CHECK, SOLTAB, SUMALL
C
C     + + + DATA INITIALIZATIONS + + +
      DATA VERS/'0888    '/                                             LS0888
      DATA  JC  / 2H01, 2H02, 2H03, 2H04, 2H05, 2H06, 2H07, 2H08, 2H09,
     *            2H10, 2H11, 2H12, 2H13, 2H14, 2H15, 2H16, 2H17, 2H18,
     *            2H19, 2H20, 2H21, 2H22, 2H23, 2H24, 2H25, 2H26, 2H27,
     *            2H28, 2H29, 2H30, 2H31, 2H32, 2H33, 2H34, 2H35, 2H36,
     *            2H37, 2H38, 2H39, 2H40, 2H01, 2H02, 2H03, 2H12, 2H13/ LS032783
C
C     + + + INPUT FORMATS + + +
  800 FORMAT( A2,8X, 5X, 14I5 )
  801 FORMAT( A2,8X, 6I5, 3X, A2, 5X,5I5)                               LS0888
  810 FORMAT( A2,8X, 70A1    )                                          LS0287
  815 FORMAT( A2,8X, 9I5,15X, F10.0 )                                   LS0885
  820 FORMAT( A2,8X, ( T11, 14I5 ) )                                     0884KF
  825 FORMAT( A2,8X, 6I5, 2F10.0 )
  826 FORMAT( A2,6X, 2( 2X, A16 ) )                                     LS0287
  827 FORMAT(A2,8X,A16,4X,2I5)                                          LS0287
  830 FORMAT( A2,6X, 4( 2X, A16 ), /,    8X, 4( 2X, A16 ) )             LS0287
  840 FORMAT( A2,2X, I2, 3X, A4, 2X, 13F5.1 )
  845 FORMAT( A2,8X, 2I5, 7F5.0 )
  850 FORMAT( A2,8X, ( T11, 14F5.2 ) )                                    884KF
  851 FORMAT(10X,I5,12F5.0)                                             GL1085
  852 FORMAT(A2,8X,14F5.0)                                              LS102783
  855 FORMAT( A2,8X,      4F5.2, 3I5, 2F5.2, 2I5 )                      GL1085
  860 FORMAT( A2,8X, I3, F5.0, F7.0, I5, 6F5.2, 2I2, I1, 3F5.2 )
  865 FORMAT( A2,8X, I5, 6F5.2, 2F5.5, 2F5.2, F5.2, 3I3 )
  870 FORMAT( A2,8X, I3, F7.0, 4F5.2 ,3I5,2F5.2)                        LS0885
  872 FORMAT(A2,8X,6I5,4F10.2)
  874 FORMAT( A2,8X, ( T11, 6F10.2 ) )                                  AL082683
  880 FORMAT( A2,8X, ( T11, 6F10.0 ) )                                    884KF
  881 FORMAT( A2,8X, 12F5.5, F5.2 )                                     LS102783
  994   FORMAT(10X,11F5.2)                                              LS0885
C
C     + + + OUTPUT FORMATS + + +
  600 FORMAT(/,5X, '***** CARD ', A2, '--OPTIMIZATION AND SENSITIVITY ',LS0287
     *              'CAN NOT BE RUN SIMULTANEOUSLY, IOPT =', I3,
     *              ',  ISEN =', I3  )
  605 FORMAT(/, 5X, '***** CARD ', A2, '--ONE OF THE FOLLOWING ',
     *              'SWITCHES IS OUT OF RANGE :',
     *       /,18X, 'VARIABLE   ISIM  IOBS  IOPT  ISEN  IDOUT  ',
     *              'IUOUT  SCODE  IPSW',                               LS0287
     *       /,18X, 'EXPECTED    0-3   0-1  -5-5   0-5    0-3  ',        1083KF
     *              '  0-4    ---   0-1',                               LS0287
     *       /,18X, 'FOUND    ', 4I6, 2I7, 5X, A2,  I6 )                LS0287
  610 FORMAT(/, 5X, '***** CARD ', A2, '--ONE OF THE FOLLOWING ',
     *              'SWITCHES IS OUT OF RANGE :',
     *       /,18X, 'VARIABLE   IPET  ISSR1  MRDC  ISUN',
     *       /,18X, 'EXPECTED    0-2    0-1   0-2   0-1',
     *       /,18X, 'FOUND  ', 2I7, 2I6 )
  615 FORMAT(/, 5X, '***** CARD ', A2, '--ONE OF THE FOLLOWING EXCEEDS ',
     *              'MODEL LIMITS :',
     *       /,18X, 'VARIABLE    NDS  NRU  NRD  NRES  NGW  NSTOR',
     *       /,18X, 'MAXIMUM       5   50   20    50   50      8',      KF0993
     *       /,18X, 'FOUND    ', 3I5, I6, I5, I7 )
  620 FORMAT(/, 5X, '***** CARD ', A2, '--ERROR IN DATES, BEGIN :',
     *              I5, 2I3, ',  END :', I5, 2I3 )
  625 FORMAT(/, 5X, '***** CARD ', A2, '--ERROR IN MONTHS,  MFS =', I3,
     *              ',  MFN =', I3 )
  630 FORMAT(/,5X, '***** CARD ', A2, '--ERROR IN PRINT OPTIONS ',
     *       /,18X,'VARIABLE  IPOP1  IPUN3  IPUN4  IPOP2  IPUN1  ',
     *             'IPUN2  ISTAT',
     *       /,18X,'EXPECTED   0-3    1-12   1-12   0-4   1-366  '      0985KF
     *             '1-366   0-1 ',
     *       /,18X,'FOUND  ',7I7)
  631 FORMAT(/, 5X, '***** CARD ', A2, '--FOUND ISTAT =', I3,
     *              '  ISTAT MUST BE ZERO WHEN IOPT OR ISEN IS ',
     *              'NON-ZERO, ISTAT = 0 WILL BE USED. ' )
  635 FORMAT(/, 5X, '***** CARD ', A2, '--ERROR IN PLOT OPTIONS ',
     *       /,18X, 'VARIABLE  IPLOT  IPLTYP  IMPLB  IDPLB  IMPLE  ',
     *              'IDPLE      PLMX             PLMN',
     *       /,18X, 'EXPECTED    0-1     0-1   1-12   1-31   1-12  ',
     *              ' 1-31   .01-1000000.    .00001-1000.',
     *       /,18X, 'FOUND ', 2I8, 4I7, F12.3, 3X, F12.5 )
  640 FORMAT(/, 5X, '***** CARD ', A2, '--LOG PLOT REQUIRES A ',
     *              'MAXIMUM = 1000*MINIMUM, PLMX,PLMN =',2F12.5)
  641 FORMAT(/, 5X, '***** CARD ', A2, '--HRU #', I3,' INPUT ERROR :',
     *       /,18X, 'VARIABLE   IRD  ICOV  ITST  ITND  ITSW   SLP',
     *       /,18X, 'EXPECTED  1-     1-3  1-12  1-12   0-1  0.-1.',
     *       /,18X, 11X, I3,
     *       /,18X, 'FOUND   ', 5I6, F6.3 )
  642 FORMAT(/, 5X, '***** CARD ', A2, '--HRU #', I3,' ONE OF THE ',
     *              'FOLLOWING EXCEEDS THE EXPECTED RANGE OF 0. - 1.',
     *       /,18X, 'VARIABLE   COVDNS  COVDNW   TRNCF',
     *       /,18X, 'FOUND    ', 3F8.3 )
  643 FORMAT(/, 5X, '***** CARD ', A2, '--HRU #', I3,' ONE OF THE ',
     *              'FOLLOWING EXCEEDS THE EXPECTED VALUE ',
     *       /,18X, 'VARIABLE    SNST  RNSTS  RNSTW ',
     *       /,18X, 'EXPECTED     10.    10.    10. ',
     *       /,18X, 'FOUND    ', 3F7.2 )
  644 FORMAT(/, 5X, '***** CARD ', A2, '--HRU #', I3, ' ONE OF THE ',
     *              'FOLLOWING IS OUT OF RANGE : ',
     *       /,18X, 'VARIABLE  ISOIL  KRES   KGW  KSTOR ',
     *       /,18X, 'EXPECTED    1-3  1-    1-     1-   ',
     *       /,18X, 15X, 2I6, I7,
     *       /,18X, 'FOUND    ', 3I6, I7 )
  645 FORMAT(/, 5X, '***** CARD ', A2, '--HRU #', I3, ' ONE OF THE ',
     *              'FOLLOWING IS OUT OF RANGE, CHECK COMPATABILITY ',
     *       /,18X, ' VARIABLE  SMAV   SMAX  RECHR   REMX  ',
     *       /,18X, 'FOUND     ', 4F7.3 )
  646 FORMAT(/, 5X, '***** CARD ', A2, '--HRU #', I3,' ONE OF THE ',
     *              'FOLLOWING EXCEEDS THE EXPECTED RANGE : ',
     *       /,18X, 'VARIABLE   SMAX   REMX  IMPERV  RETIIP    SEP  ',
     *       /,18X, 'EXPECTED  .5-20. .1-15.  0.-1.  0.-20.  0.-10. ',
     *       /,18X, 'FOUND   ', 2F7.3, F8.3, 2F7.3 )
  647 FORMAT(/, 5X, '***** CARD ', A2, '--HRU #', I3, ' ONE OF THE ',
     *              'FOLLOWING EXCEEDS THE EXPECTED VALUE WHEN ',
     *              'ISSR1 =', I3,
     *       /,18X, 'VARIABLE    SRX   SCX   SCN   SC1 ',
     *       /,18X, 'EXPECTED  .0-10. .0-1. .0-1. .0-1.',
     *       /,18X, 'FOUND    ',4F6.3 )
  648 FORMAT(/, 5X, '***** CARD ', A2, '--HRU #', I3, ' ONE OF THE ',
     *              'FOLLOWING IS OUT OF RANGE :',
     *       /,18X, 'VARIABLE    KDS        DARU   SUM OF AREA ',
     *       /,18X, 'EXPECTED    1-' I2, 13X,F12.3,
     *       /,18X, 'FOUND      ',I5,F12.3, 1X,F12.3 )
  650 FORMAT(/, 5X, '***** CARD ', A2, '--EXPECT CTW TO BE LESS THAN,',
     *              '1, FOUND CTW =', F8.3 )
  655 FORMAT(/, 5X, '***** CARD ', A2, '--WHEN IPET = 0, EXPECT ',
     *              'CTS TO BE BETWEEN 0.0 AND 1.10, FOUND ',
     *              'CTS =', F8.3 )
  660 FORMAT(/, 5X, '***** CARD ', A2, '--WHEN IPET = 1, EXPECT ',
     *              'CTS TO BE BETWEEN 0.0 AND 1.10, FOUND ',
     *              'CTS =', F8.3 )
  665 FORMAT(/, 5X, '***** CARD ', A2, '--EXPECTED RANGE OF EAIR IS ',
     *              '  0.6-1.0  , FOUND EAIR =', F8.3 )
  670 FORMAT(/, 5X, '***** CARD ', A2, '--EXPECTED RANGE OF FWCAP IS ',
     *              '0.001-1.00 , FOUND FWCAP =', F8.3 )
  675 FORMAT(/, 5X, '***** CARD ', A2, '--EXPECTED RANGE OF DENI IS ',
     *              '0.001-1.00 , FOUND DENI =', F8.3 )
  676 FORMAT(/, 5X, '***** CARD ', A2, '--EXPECTED RANGE OF DENMX IS ',
     *              ' 0.01-1.00 , FOUND DENMX =', F8.3 )
  677 FORMAT(/, 5X, '***** CARD ',A2, '--EXPECTED RANGE OF BST IS ',
     *              ' -10. TO 50.0, FOUND BST = ',F8.3 )
  680 FORMAT(/, 5X, '***** CARD ', A2, '--EXPECTED RANGE OF SETCON IS ',
     *              '.0001-1.10 , FOUND SETCON =', F8.3 )
  690 FORMAT(/, 5X, '***** GROUP 7 CARD ', A2, '--IF IOPT OR ISEN ',
     *              'GREATER THAN 2, THERE IS NO INITIALIZATION ',
     *              'PERIOD, ALL VALUES ON THIS CARD SHOULD BE ZERO.' )
  695 FORMAT(/, 5X, '***** GROUP 7 CARD ', A2, '--ERROR IN SWITCH :',
     *       /,18X, 'VARIABLE    IOBF  ITRANS',
     *       /,18X, 'EXPECTED     1-2     0-1',
     *       /,18X, 'FOUND   ', 2I8 )
  841 FORMAT( /, 5X,'***** GROUP 1 CARD ',A2,' --ONE OF THE FOLLOWING ',
     *               'OUTSIDE LIMITS:',                                 KD1291
     *         /,18X,'VARIABLE       K    SL     ASP   ALAT',           KD1291
     *         /,18X,'MINIMUM        1    0.0    0.0   0.0',            KD1291
     *         /,18X,'MAXIMUM       50   .999   360.0  90.0',           KD1291
     *         /,18X,'FOUND       ', I4, F7.3,F8.1,F6.1)
  900 FORMAT('1',70A1   )                                               LS0287
  901 FORMAT('1', 5X,'PRMS -- VERSION ', A4,                            KD1291
     *        //, 1X, 70A1    )                                         LS0287
  910 FORMAT(/,3X,'IOPT=',I5,5X,'ISIM='I5,5X,'IOBS=',I5,5X,'ISEN=',I5,  KD1291
     *5X,'PROB=',I5)                                                    KD1291
  911 FORMAT(3X,'IDOUT='I4,5X,'IUOUT=',I4,5X,'SCODE=',A2                KD1291
     1,5X,'IPSW=',I5)                                                   KD1291
  912 FORMAT(/,3X,'IPET=',I5,5X,'ISSR1=',I4,5X,'MRDC=',I5,5X,'ISUN=',I5,KD1291
     15X,'ILPS=',I5)                                                    KD1291
  915 FORMAT(3X,'NYR=',I6,5X,'NDS=',I6,5X,'NRU=',I6,5X,                 KD1291
     1'NRD=',I6,5X,'NRES=',I4,4X,'NGW=',I4,4X,'NSTOR=',I4,4X,'DAT=',    KD1291
     2F13.2,/,3X,'NTS=',I6,5X,'NPLW=',I5,'NDC=',I5)                     KD1291
  920 FORMAT(3X,'BYR/BMO/BDY=',I5,'/',I2,'/',I2,5X,'EYR/EMO/EDY=',      KD1291
     1I5,'/',I2,'/',I2,/)                                               KD1291
  921 FORMAT(3X,'INIT-BYR/BMO/BDY=',I5,'/',I2,'/',I2,5X,'EYR/EMO/EDY='  KD1291
     1,I5,'/',I2,'/',I2,3X,'OPT -BYR/BMO/BDY=',I5,'/',I2,'/',I2,5X,     KD1291
     2'EYR/EMO/EDY=',I5,'/',I2,'/',I2,/,3X,'IOBF=',I5,5X,'ITRANS=',I5)  KD1291
  923 FORMAT(3X,'MFS=',I3,5X,'MFN=',I3)                                 KD1291
  924 FORMAT(/,4X,'DATA TYPE     PARAMETER STATISTIC',                  KD1291
     *         9X,'STATION ID',                                         KF0595
     *       /,4X,'                 CODE     CODE     DSN' )            KD1291
  926 FORMAT(' DAILY DISCHARGE',3X,3(I5,4X),A10)                        KD1291
  927 FORMAT(' DAILY EVAP',8X,3(I5,4X),A16)                             KD1291
  928 FORMAT(' DAILY MAX TEMP',4X,3(I5,4X),A16,2X,                      KD1291
     1/,(37X,I5,4X,A16,2X/))                                            KD1291
  929 FORMAT(' DAILY MIN TEMP',4X,3(I5,4X),A16,2X,                      KD1291
     1/,(37X,I5,4X,A16,2X/))                                            KD1291
  930 FORMAT(' DAILY SOLAR RAD',3X,3(I5,4X),A16)                        KD1291
  931 FORMAT(' SNOW PILLOW',7X,3(I5,4X),A16,2X,                         KD1291
     1/,(37X,I5,4X,A16,2X/))                                            KD1291
  932 FORMAT(' USER VARIABLE 2',3X,3(I5,4X),A16)                        KD1291
  933 FORMAT(' UNIT DISCHARGE',4X,3(I5,4X),A16)                         KD1291
  934 FORMAT(' DAILY PRECIP',6X,2(I5,4X), ( T38, I5, 4X, A16 ) )        KD1291
  935 FORMAT(' UNIT PRECIP',7X,2(I5,4X), ( T38, I5, 4X, A16 ) )         KD1291
  940 FORMAT(//,3X,'POT SOLAR RADIATION')                               KD1291
  945 FORMAT(3X,I5,5X,A4,5X,13F8.1)
  946 FORMAT(//,3X,'RDM(1-12)=',12F6.2,/,3X,'RDC(1-12)=',               KD1291
     112F6.2,/)
  947 FORMAT(3X,'MRDC=',I3,2X,' PARS=',F6.2,2X,' PARW=',F6.2,2X,        KD1291
     1' RDB=',F6.2,2X,' RDP=',F6.2,2X,' RDMX=',F6.2,2X,' RTB=',         KD1291
     2F6.2,2X,' RTC=',F6.2,2X,' ITSOL=',I3)                             KD1291
  948 FORMAT(//,3X,'SUNLIGHT HOURS/12')                                 KD1291
  950 FORMAT(//,3X,'IRU ',2X,'IRD',4X,'ITST',3X,'ITSW',3X,              KD1291
     1'TXAJ',3X,'RNSTS',3X,'SNST ',3X,'COVDS',3X,'ICOV',4X,'SMAX',3X,   KD1291
     2'REMX',6X,'SCN',5X,'SRX  ',2X,'RETIP',3X,' SEP',4X,'KRES',/,9X,   KD1291
     34('----',3X),4('-----',3X),'----',3X,'-----',5X,'---',5X,'---',4X KD1291
     4,2('-----',3X),'----',/,9X,'ELEV',3X,'ITND',3X,'CTX',4X,'TNAJ',   KD1291
     53X,'RNSTW',3X,'TRNCF',3X,'COVDW',3X,'ISOIL',3X,'SMAV',3X,'RECHR', KD1291
     65X,'SC1',5X,'SCX',4X,'IMPRV',3X,'KSTOR',3X,'KGW',/)               KD1291
  955 FORMAT(3X,I2,5X,I2,2I7,4F8.2,I6,F9.2,F8.2,F9.5,2F8.2,F8.2,I7,
     2/,7X,F7.0,I6,F7.2,F7.2,3F8.2,I6,F9.2,F8.2,F9.5,2F8.2,I7,I8)
  960 FORMAT(///42X,'PERV',7X,'IMPERV',/3X,'IRU',5X,'IDS',6X,'SLOPE',   KD1291
     16X,'AREA',7X,'AREA',8X,'AREA',7X,'UPCOR',4X,'DRCOR',4X,'DSCOR',   KD1291
     25X,'TST',4X,'KTS',3X,'KSP',3X,'KDC',2X,'AIMX',2X,'PKFAC')         KD1291
  965 FORMAT(3X,I3,5X,I3,6X,F5.2,2(2X,F9.1),3X,F9.1,3F9.2,F9.1,3I6,     LS0885
     12F7.2)                                                            LS0885
  966 FORMAT(31X,'----',/,3X,'TOTAL',19X,F9.1,2X,F9.1,3X,F9.1)          KD1291
  968 FORMAT(/,13X,'RMXA=',F5.2,5X,                                     KD1291
     1'RMXM=',F5.2,5X,'MTSS=',I5,5X,'MTSE=',I5,5X,'ARSA=',F5.2,         KD1291
     25X,'ARSM=',F5.2)                                                  KD1291
  969 FORMAT(/,3X,'CSEL(1-5)=',5F10.0)                                  KD1291
  970 FORMAT(/,3X,'MPCS=',I4,5X,                                        KD1291
     15X,'MPCN=',I4,5X,'MPC1=',I4,5X,'PCONR=',F6.2,4X,'PCONS=',F6.2)    KD1291
  971 FORMAT(/3X,'ISP1=',I3,5X,'ISP2=',I3,5X,'EAIR=',F6.3,5X,           KD1291
     1'FWCAP=',F5.2,5X,'DENI=',F5.2,5X,'DENMX=',F5.2,5X,                KD1291
     2'SETCON=',F5.2,5X,'BST=',F6.2,/)                                  KD1291
  972 FORMAT(/,3X,'TLX(1-12)=',12F8.2,/,3X,'TLN(1-12)=',12F8.2)         KD1291
  973 FORMAT(/,3X,'EVC(1-12)=',12F8.3)                                  KD1291
  974 FORMAT(/, 3X, '  #       RES      RSEP       RESMX      ',         0884KF
     *              'REXP     KRSP     RCF         RCP             ',    0884KF
     *              '  #       GW       GSNK      RCB',                 KD1291
     *       /, 3X, ' --      ----      ----      ------      ',         0884KF
     *              '----     ----  ---------   ---------          ',    0884KF
     *              ' --     -----     -----    ------' )                0884KF
  975 FORMAT(/,3X,'PAT(1-12)=',12F8.2/3X,'AJMX(1-12)=',12F8.2)          KD1291
  976 FORMAT(   3X, I3, 2(2X,F8.3), 2(2X,F10.4), I6, 2(2X,F10.7),        0884KF
     *              10X, I3, 2(2X,F8.3), 2X,F8.4 )                       0884KF
  977 FORMAT(/,3X,'CECN(1-12)=',12F8.2)                                 KD1291
  978 FORMAT(  80X, 10X, I3, 2(2X,F8.3), 2X,F8.4 )                       0884KF
  981 FORMAT(/, 5X,'***** CARD ', A2, '--EXPECTED RANGE OF GSNK(', I2,
     *             ') IS:  0.0 TO 1.0, FOUND ', F6.2)
  983 FORMAT(/, 5X,'***** CARD ', A2, '--EXPECTED RANGE OF RCB(', I2,
     *             ') IS:  0.0 TO 1.0, FOUND ', F6.2)
  984 FORMAT(/,3X,'CTS(1-12)=',12F9.6,/,3X,'CTW=',F8.2)                 KD1291
  986 FORMAT(/,3X,'STORAGE RESERVOIR',/,3X,'RES',2X,'TYPE',2X,'NSOS',   KD1291
     12X,'IRUP1',2X,'IRUP2',2X,'IRUP3',7X,'RCS',2X,'STO CFSD',3X,       KD1291
     2'OUTFLOW',4X,'INFLOW',4X,'STO IN',/)                              KD1291
  987 FORMAT(3X,I3,2I6,3I7,F10.4,4F10.2)
  988 FORMAT(/,3X,'O2S2 TABLES')                                        KD1291
  989 FORMAT(/,3X,I5,5(2X,2F8.2,3X),/,8X,5(2X,2F8.2,3X))
  991 FORMAT(/, 5X,'***** CARD ', A2, '--EXPECTED RANGE OF RCF(', I2,
     *             ') IS:  0.0 TO 1.0, FOUND ', F6.2)
  992 FORMAT(/, 5X,'***** CARD ', A2, '--EXPECTED RANGE OF RCP(', I2,
     *             ') IS:  0.0 TO 1.0, FOUND ', F6.2)
  995   FORMAT(/,1X,'SNOW DEPLETION CURVES')                            LS0885
  996   FORMAT(1X,'CURVE#  0.0  0.1  0.2  0.3  0.4  0.5  0.6  0.7  0.8  LS0885
     10.9  1.0',/)                                                      LS0885
  997   FORMAT(1X,I5,11F5.2)                                            LS0885
 9471 FORMAT(/,3X,'TSOLX(1-12) -',12F8.1)                               KD1291
 9700 FORMAT(/,3X,'PCR(1-NRU) -',(T20,10F7.2))                          KD1291
 9701 FORMAT(/,3X,'PCS(1-NRU) -',(T20,10F7.2))                          KD1291
C
C     + + + END SPECIFICATIONS + + +
C
      READ(71,801) IC, ISIM,IOBS,IOPT,ISEN,IDOUT,IUOUT,SCODE,IPSW,       LS0287
     1            ICHK,IPKAD,PROB,DEBUG                                 LS0888
      IF(IC .NE. JC(1)) CALL CHECK( 1, IC, JC(1) )
      IF(IOPT .EQ. 0  .OR.  ISEN .EQ. 0) GO TO 700
        WRITE(72,600) IC, IOPT, ISEN
        ERR = 1
  700 CONTINUE
      IF( ISIM .GE. 0  .AND.  ISIM .LE. 3   .AND.
     *    IOBS .GE. 0  .AND.  IOBS .LE. 1   .AND.
     *    IOPT .GE.-5  .AND.  IOPT .LE. 5   .AND.
     *    ISEN .GE. 0  .AND.  ISEN .LE. 5   .AND.
     *   IDOUT .GE. 0  .AND. IDOUT .LE. 3   .AND.                        1083KF
     *   IUOUT .GE. 0  .AND. IUOUT .LE. 4   .AND.                        0312AL
     *    PROB .GE. 0  .AND.  PROB .LE. 1   .AND.                       LS0888
     *    IPSW .GE. 0  .AND.  IPSW .LE. 1 ) GO TO 705
        WRITE(72,605) IC,ISIM,IOBS,IOPT,ISEN,IDOUT,IUOUT,SCODE,IPSW
        ERR = 1
  705 CONTINUE
      READ(71,800) IC, IPET,ISSR1,MRDC,ISUN,ILPS                         GL081584
      IF(IC .NE. JC(2)) CALL CHECK( 1, IC, JC(2) )
      IF( IPET .GE. 0  .AND.  IPET .LE. 2   .AND.
     *   ISSR1 .GE. 0  .AND. ISSR1 .LE. 1   .AND.
     *    MRDC .GE. 0  .AND.  MRDC .LE. 2   .AND.
     *    ISUN .GE. 0  .AND.  ISUN .LE. 1 ) GO TO 710
        WRITE(72,610) IC, IPET, ISSR1, MRDC, ISUN
        ERR = 1
  710 CONTINUE
      READ(71,810) IC, TITL                                              LS0287
      IF(IC .NE. JC(3)) CALL CHECK( 1, IC, JC(3) )
      READ(71,815) IC, NDS,NRU,NRD,NRES,NGW,NSTOR,NTS,NPLW,NDC,DAT       LS0885
      IF(IC .NE. JC(4)) CALL CHECK( 1, IC, JC(4) )
      IF( NDS .LE. 5   .AND.  NRU .LE. 50   .AND.
     *    NRD .LE. 20  .AND.  NRES .LE. 50   .AND.                       KF0993
     *    NGW .LE. 50  .AND.  NSTOR .LE. 8 ) GO TO 715
        WRITE(72,615) IC, NDS, NRU, NRD, NRES, NGW, NSTOR
        ERR = 1
  715 CONTINUE
      WRITE(72,901) VERS, TITL                                           LS0287
      WRITE(72,910) IOPT,ISIM,IOBS,ISEN,PROB                             LS0888
      WRITE(72,911)IDOUT,IUOUT,SCODE,IPSW                                LS0287
      WRITE(72,912)IPET,ISSR1,MRDC,ISUN,ILPS                             GL081584
      READ(71,820) IC, BYR,BMO,BDY,EYR,EMO,EDY,NMO
      IF(IC .NE. JC(5)) CALL CHECK( 1, IC, JC(5) )
      IF( BYR .LE. EYR  .AND.
     *    BMO .GE. 1  .AND.  BMO .LE. 12   .AND.
     *    BDY .GE. 1  .AND.  BDY .LE. 31   .AND.
     *    EMO .GE. 1  .AND.  EMO .LE. 12   .AND.
     *    EDY .GE. 1  .AND.  EDY .LE. 31 ) GO TO 720
        WRITE(72,620) IC, BYR, BMO, BDY, EYR, EMO, EDY
  720 CONTINUE
      MYR=BYR
      MO=BMO
      MDY=BDY
      BWY=BYR
      IF(BMO.GE.10) BWY=BWY+1
      EWY=EYR
      IF(EMO.GE.10) EWY=EWY+1
      NYR=EWY-BWY+1
      WRITE(72,915) NYR,NDS,NRU,NRD,NRES,NGW,NSTOR,DAT,NTS,NPLW,NDC      LS0885
      WRITE(72,920) BYR,BMO,BDY,EYR,EMO,EDY
      IF(IOPT.EQ.0.AND.ISEN.EQ.0)GO TO 816                              LS0888
      READ(37,820) IC, BYRIN,BMOIN,BDYIN,EYRIN,EMOIN,EDYIN
      IF(IC .NE. JC(41)) CALL CHECK( 7, IC, JC(41) )                    LS102783
      IF(  ABS(IOPT) .LT. 3  .AND.    ISEN .LT. 3)  GO TO 790
      IF( BYRIN .EQ. 0  .AND.  BMOIN .EQ. 0   .AND.
     *    BDYIN .EQ. 0  .AND.  EYRIN .EQ. 0   .AND.
     *    EMOIN .EQ. 0  .AND.  EDYIN .EQ. 0 ) GO TO 790
        WRITE(72,690) IC
        ERR = 1
  790 CONTINUE
      READ(37,820) IC, BYROP,BMOOP,BDYOP,EYROP,EMOOP,EDYOP
      IF(IC .NE. JC(42)) CALL CHECK( 7, IC, JC(42) )                    LS102783
      READ(37,820) IC, IOBF,ITRANS
      IF(IC .NE. JC(43)) CALL CHECK( 7, IC, JC(43) )                    LS102783
      IF( IOBF .GE. 1  .AND.  IOBF .LE. 2   .AND.
     *   ITRANS.GE. 0  .AND. ITRANS.LE. 1 ) GO TO 795
        WRITE(72,695) IC, IOBF, ITRANS
        ERR = 1
  795 CONTINUE
      WRITE(72,921) BYRIN,BMOIN,BDYIN,EYRIN,EMOIN,EDYIN,BYROP,BMOOP,
     1BDYOP,EYROP,EMOOP,EDYOP,IOBF,ITRANS
  816 IF(PROB.EQ.0) GO TO 817                                           JKM10/85
C                                                                       JKM10/85
C     READ IN INITIALIZTION FOR PROBABILITY ANALYSIS                    JKM10/85
C                                                                       JKM10/85
      READ(55,820) IC, BYRFC,BMOFC,BDYFC,EYRFC,EMOFC,EDYFC              JKM10/85
      IF(IC .NE. JC(41)) CALL CHECK( 9, IC, JC(41) )                    JKM10/85
C                                                                       JKM10/85
C     READ IN PROBABILITY ANALYSIS PERIOD                               JKM10/85
C                                                                       JKM10/85
      READ(55,820) IC, BYRPR,BMOPR,BDYPR,EYRPR,EMOPR,EDYPR              JKM10/85
      IF(IC .NE. JC(42)) CALL CHECK( 9, IC, JC(42) )                    JKM10/85
C                                                                       JKM10/85
  817 READ(71,820) IC, MFS, MFN
      IF(IC .NE. JC(6)) CALL CHECK( 1, IC, JC(6) )
      IF(MFS .GE. 1  .AND.  MFS .LE. 12   .AND.
     *   MFN .GE. 1  .AND.  MFN .LE. 12 ) GO TO 725
        WRITE(72,625) IC, MFS, MFN
        ERR = 1
  725 CONTINUE
      WRITE(72,923) MFS,MFN
      READ(71,820) IC, IPOP1,IPUN3,IPUN4,IPOP2,IPUN1,IPUN2,ISTAT
      IF(IC .NE. JC(7)) CALL CHECK( 1, IC, JC(7) )
      ERRP = 0                                                          0985KF
      IF (IPOP1 .LT. 0  .OR.  IPOP1 .GT. 3) THEN                        0286KF
        ERRP = 1                                                        0286KF
      ELSE IF (IPOP1 .EQ. 2  .OR.  IPOP1 .EQ. 3) THEN                   0286KF
        IF (IPUN3 .LT. 1  .OR.  IPUN3 .GT. 12  .OR.                     0286KF
     *      IPUN4 .LT. 1  .OR.  IPUN4 .GT. 12) ERRP = 1                 0286KF
      END IF                                                            0286KF
      IF (IPOP2 .LT. 0  .OR.  IPOP2 .GT. 4) THEN                        0286KF
        ERRP = 1                                                        0286KF
      ELSE IF (IPOP2 .GE. 2  .AND. IPOP2 .LE. 4) THEN                   0286KF
        IF(IPUN1 .LT. 1  .OR.  IPUN1 .GT. 366  .OR.                     0286KF
     *     IPUN2 .LT. 1  .OR.  IPUN2 .GT. 366) ERRP = 1                 0286KF
      END IF                                                            0985KF
      IF (ISTAT .LT. 0  .OR.  ISTAT .GT. 1) ERRP = 1                    0985KF
      IF (ERRP .EQ. 1) THEN                                             0985KF
        ERR = 1                                                         0985KF
        WRITE(72,630)IC, IPOP1, IPUN3, IPUN4, IPOP2, IPUN1, IPUN2, ISTAT0985KF
      END IF                                                            0985KF
      IF( IOPT .EQ. 0  .AND.  ISEN .EQ. 0 ) GO TO 731
      IF( ISTAT .EQ. 0 ) GO TO 731
      WRITE(72,631) IC, ISTAT
      ISTAT = 0
  731 CONTINUE
      READ(71,825) IC, IPLOT,IPLTYP,IMPLB,IDPLB,IMPLE,IDPLE,PLMX,PLMN
      IF(IC .NE. JC(8)) CALL CHECK( 1, IC, JC(8) )
      IF(IPLOT .EQ. 0) GO TO 740
        ERRP = 0                                                        0985KF
        IF (IPLOT .EQ. 1) THEN                                          0985KF
C         CHECK DATES                                                   0985KF
          IF (IMPLB .LT. 1  .OR.  IMPLB .GT. 12  .OR.                   0985KF
     *        IDPLB .LT. 1  .OR.  IDPLB .GT. 31  .OR.                   0985KF
     *        IMPLE .LT. 1  .OR.  IMPLE .GT. 12  .OR.                   0985KF
     *        IDPLE .LT. 1  .OR.  IDPLE .GT. 31) ERRP = 1               0985KF
C         CHECK PLOT TYPE AND LIMITS                                    0985KF
          IF (IPLTYP .LT. 0  .OR.  IPLTYP .GT. 1) ERRP = 1              0985KF
          IF (IPLTYP .EQ. 0  .AND.                                      0985KF
     *       (  PLMN .LT. -.00001  .OR.  PLMN .GT. 1.0E3  .OR.          0985KF
     *          PLMX .LT. .01      .OR.  PLMX .GT. 1.0E6 )) ERRP = 1    0985KF
          IF (IPLTYP .EQ. 1) THEN                                       0985KF
            IF (PLMN .LT. .00001  .OR. PLMN .GT. 1.0E3  .OR.            0985KF
     *          PLMX .LT. .01     .OR. PLMX .GT. 1.0E6) ERRP = 1        0985KF
            IF (PLMN*1000.-PLMX .GT. .01) THEN                          0985KF
              PLMN = PLMX / 10000.                                      0985KF
              WRITE(72,640) IC, PLMX, PLMN                               0985KF
            END IF                                                      0985KF
          END IF                                                        0985KF
        ELSE                                                            0985KF
          ERRP = 1                                                      0985KF
        END IF                                                          0985KF
        IF (ERRP .EQ. 1) THEN                                           0985KF
          ERR = 1                                                       0985KF
          WRITE(72,635) IC, IPLOT, IPLTYP, IMPLB, IDPLB, IMPLE, IDPLE,   0985KF
     1                 PLMX, PLMN                                       0985KF
        END IF                                                          0985KF
  740 CONTINUE
      READ(71,820) IC, NDTY,(IDUS(J),J=1,10)
      IF(IC .NE. JC(9)) CALL CHECK( 1, IC, JC(9) )
      READ(71,820) IC, (PARMCA(I),I=1,10)
      IF(IC .NE. JC(10)) CALL CHECK( 1, IC, JC(10) )
      READ(71,820) IC, (STATCA(I),I=1,10)
      IF(IC .NE. JC(11)) CALL CHECK( 1, IC, JC(11) )
      READ(71,830) IC, ( STAIDC(K), K = 1, 8  )                          LS0287
      IF(IC .NE. JC(12)) CALL CHECK( 1, IC, JC(12) )
      READ(71,820) IC, ( DSNC(I), I = 1, 8 )                             LS0287
      IF(IC .NE. JC(44)) CALL CHECK( 1, IC, JC(44) )                    LS102783
      DO 25 IDS=1,NDS
      READ(71,826) IC, ( STAIDP(K,IDS), K = 1, 2 )                       LS0287
      IF(IC .NE. JC(13)) CALL CHECK( 1, IC, JC(13) )
      READ(71,820) IC, ( DSNP(K,IDS), K = 1, 2 )                         XXXXXXXX
      IF(IC .NE. JC(45)) CALL CHECK( 1, IC, JC(45) )                    LS102783
   25 CONTINUE                                                          XXXXXXXX
      DO 27 J=1,NTS                                                     GL081584
      READ(71,827) IC,STAIDT(J),(DSNT(KK,J),KK=1,2)                      LS0287
   27 CONTINUE                                                          GL081584
      IF(NPLW.EQ.0) GO TO 29                                            GL081584
      DO 28 J=1,NPLW                                                    GL081584
      READ(71,827) IC,STAIDS(J),DSNS(J)                                  LS0287
   28 CONTINUE                                                          GL081584
   29 WRITE(72,924)                                                       0884KF
      WRITE(72,926)PARMCA(1),STATCA(1),DSNC(1),STAIDC(1)                 LS0287
      WRITE(72,927)PARMCA(2),STATCA(2),DSNC(2),STAIDC(2)                 LS0287
      WRITE(72,928)PARMCA(3),STATCA(3),(DSNT(1,J),STAIDT(J),J=1,NTS)     LS0287
      WRITE(72,929)PARMCA(4),STATCA(4),(DSNT(2,J),STAIDT(J),J=1,NTS)     LS0287
      WRITE(72,930)PARMCA(5),STATCA(5),DSNC(5),STAIDC(5)                 LS0287
      WRITE(72,931)PARMCA(6),STATCA(6),(DSNS(J),STAIDS(J),J=1,NPLW)      LS0287
      WRITE(72,932)PARMCA(7),STATCA(7),DSNC(7),STAIDC(7)                 LS0287
      WRITE(72,933)PARMCA(8),STATCA(8),DSNC(8),STAIDC(8)                 LS0287
      WRITE(72,934)PARMCA(9),STATCA(9),(DSNP(1,J),STAIDP(1,J),J=1,NDS)   LS0287
      WRITE(72,935)PARMCA(10),STATCA(10),(DSNP(2,J),STAIDP(2,J),J=1,NDS) LS0287
      IF(MRDC.EQ.0.AND.IPET.NE.1) GO TO 31                              XXXXXXXX
      DO 30 J=1,NRD
      READ(71,840) IC, K,SA(J),(RAD(J,L),L=1,13)
      IF(IC .NE. JC(14)) CALL CHECK( 1, IC, JC(14) )
      IF ( K  .GT.  0   .AND.   K  .LT.  51   .AND.
     *     RAD(J,2) .GE. 0.0  .AND.  RAD(J,2) .LT. 1.0  .AND.
     *     RAD(J,3) .GE. 0.0  .AND.  RAD(J,3) .LE. 360.0  .AND.
     *     RAD(J,4) .GE. 0.0  .AND.  RAD(J,4) .LE. 90.0  )  GO TO 941
      WRITE (72,841) IC, K, (RAD(J,L),L=2,4)
  941 CONTINUE
      IF(RAD(J,1).GT.0.) GO TO 30                                       LS0287
      SL=RAD(J,2)
      DGSL=ATAN(SL)                                                     LS102783
      COSSL(J)=COS(DGSL)                                                LS102783
      ASP=RAD(J,3)
      ALAT=RAD(J,4)
      CALL SOLTAB(K,SL,ASP,ALAT)
   30 CONTINUE
   31 CONTINUE                                                          KF0391
      IF(MRDC.EQ.0) THEN                                                KF0391
C       default ITSOL to first temperature station                      KF0391
        ITSOL = 1                                                       KF0391
      ELSE                                                              KF0391
        READ(71,850) IC, (RDM(J),J=1,12)
        IF(IC .NE. JC(15)) CALL CHECK( 1, IC, JC(15) )
        READ(71,850) IC, (RDC(J),J=1,12)
        IF(IC .NE. JC(16)) CALL CHECK( 1, IC, JC(16) )
        READ(71,850) IC, PARS,PARW,RDB,RDP,RDMX,RTB,RTC                  GL1085
        IF(IC .NE. JC(17)) CALL CHECK( 1, IC, JC(17) )
        READ(71,851) ITSOL,(TSOLX(J),J=1,12)                             GL1085
        WRITE(72,940)
        DO 70 J=1,NRD
   70   WRITE(72,945) J,SA(J),(RAD(J,L),L=1,13)
        WRITE(72,946) (RDM(J),J=1,12),(RDC(K),K=1,12)
        WRITE(72,947)MRDC,PARS,PARW,RDB,RDP,RDMX,RTB,RTC,ITSOL           GL1085
        WRITE(72,9471) (TSOLX(J),J=1,12)                                 GL1085
      END IF                                                            KF0391
      IF(IPET.NE.1) GO TO 75
      WRITE(72,948)
      DO 949 J=1,NRD
      WRITE(72,945) J,SA(J),(SSH(J,L),L=1,13)
  949 CONTINUE
   75 WRITE(72,900) TITL                                                 LS0287
      READ(71,855)IC,ARSA,ARSM,RMXA,RMXM,MPCS,MPCN,MPC1,PCONR,PCONS,     GL1085
     1MTSS,MTSE                                                         GL1085
      IF(IC .NE. JC(18)) CALL CHECK( 1, IC, JC(18) )
      IF(PCONR.LE.0.) PCONR=1.                                          LS0287
      IF(PCONS.LE.0.) PCONS=1.                                          LS0287
      READ(71,880) IC,(CSEL(J),J=1,NTS)                                  GL081584
      READ(71,850)  IC,(PCR(J),J=1,NRU)                                  GL1085
      READ(71,850) IC,(PCS(J),J=1,NRU)                                   GL1085
      WRITE(72,968) RMXA,RMXM,MTSS,MTSE,ARSA,ARSM                        GL1085
      WRITE(72,969) (CSEL(J),J=1,NTS)                                    GL081584
      WRITE(72,970) MPCS,MPCN,MPC1,PCONR,PCONS
      WRITE(72,9700) (PCR(J),J=1,NRU)                                    GL1085
      WRITE(72,9701) (PCS(J),J=1,NRU)                                    GL1085
      READ(71,881) IC, (CTS(J),J=1,12),CTW                               LS102783
      IF(IC .NE. JC(19)) CALL CHECK( 1, IC, JC(19) )
      IF(CTW .GT. 1.00) WRITE(72,650) IC, CTW
      DO 8810 JJ=1,12                                                   LS102783
      IF(IPET.EQ.0 .AND. ( CTS(JJ) .LT. 0.0 .OR. CTS(JJ) .GT. 1.0 ) )   LS102783
     *   WRITE(72,655) IC, CTS(JJ)                                       LS102783
      IF(IPET .EQ. 1 .AND. ( CTS(JJ) .LT. 0.0 .OR. CTS(JJ) .GT. 1.0 ) ) LS102783
     *   WRITE(72,660) IC, CTS(JJ)                                       LS102783
 8810 CONTINUE                                                          LS102783
      WRITE(72,984) (CTS(J),J=1,12),CTW                                  LS102783
      READ(71,850) IC, (PAT(J),J=1,12)
      IF(IC .NE. JC(20)) CALL CHECK( 1, IC, JC(20) )
      READ(71,850) IC, (AJMX(J),J=1,12)
      IF(IC .NE. JC(21)) CALL CHECK( 1, IC, JC(21) )
      WRITE(72,975)(PAT(J),J=1,12),(AJMX(J),J=1,12)
      READ(71,850) IC, (TLX(J),J=1,12)
      IF(IC .NE. JC(22)) CALL CHECK( 1, IC, JC(22) )
      READ(71,850) IC, (TLN(J),J=1,12)
      IF(IC .NE. JC(23)) CALL CHECK( 1, IC, JC(23) )
      WRITE(72,972) (TLX(J),J=1,12),(TLN(K),K=1,12)
      READ(71,850) IC, (EVC(J),J=1,12)
      IF(IC .NE. JC(24)) CALL CHECK( 1, IC, JC(24) )
      WRITE(72,973)(EVC(J),J=1,12)
      READ(71,845) IC,ISP1,ISP2,EAIR,FWCAP,DENI,DENMX,SETCON,BST
      IF(IC .NE. JC(25)) CALL CHECK( 1, IC, JC(25) )
      IF(EAIR .LT. .6  .OR.  EAIR .GT. 1.) WRITE(72,665) IC, EAIR
      IF(FWCAP .LT. .001  .OR. FWCAP .GT. 1.010) WRITE(72,670) IC, FWCAP
      IF(DENI .LT. .001   .OR.  DENI .GT. 1.100) WRITE(72,675) IC, DENI
      IF(DENMX .LT. .01  .OR. DENMX .GT. 1.1000) WRITE(72,676) IC, DENMX
      IF(SETCON .LT. .0001  .OR.  SETCON .GT. 1.100)
     *   WRITE(72,680) IC, SETCON
      IF ( BST .LT. -10.  .OR.  BST .GT. 50. ) WRITE(72,677) IC,BST
      WRITE(72,971) ISP1,ISP2,EAIR,FWCAP,DENI,DENMX,SETCON,BST
      READ(71,852)IC,(CECN(J),J=1,12)                                    LS102783
      IF(IC .NE. JC(26)) CALL CHECK(1, IC, JC(26) )                     LS102783
      READ(71,820) IC                                                    LS0885
      IF(IC .NE. JC(27)) CALL CHECK( 1, IC, JC(27) )                    LS102783
      READ(71,850) IC, (RES(J),J=1,NRES)
      IF(IC .NE. JC(28)) CALL CHECK( 1, IC, JC(28) )                    LS102783
      READ(71,850) IC, (GW(J),J=1,NGW)
      IF(IC .NE. JC(29)) CALL CHECK( 1, IC, JC(29) )                    LS102783
      READ(71,820) IC, (KRSP(J),J=1,NRES)
      IF(IC .NE. JC(30)) CALL CHECK( 1, IC, JC(30))                     LS102783
      READ(71,850) IC, (RESMX(J),REXP(J),J=1,NRES)
      IF(IC .NE. JC(31)) CALL CHECK( 1, IC, JC(31) )                    LS102783
      READ(71,850) IC, (RSEP(J),J=1,NRES)
      IF(IC .NE. JC(32)) CALL CHECK( 1, IC, JC(32) )                    LS102783
      READ(71,850) IC, (GSNK(J),J=1,NGW)
      IF(IC .NE. JC(33)) CALL CHECK( 1, IC, JC(33) )                    LS102783
      DO 736 J=1,NGW
        IF (GSNK(J) .GE. 0.0  .AND.  GSNK(J) .LE. 1.0) GO TO 736
        WRITE (72,981) IC, J, GSNK(J)
        ERR = 1
  736 CONTINUE
      READ(71,850) IC, (RCB(J),J=1,NGW)
      IF(IC .NE. JC(34)) CALL CHECK( 1, IC, JC(34) )                    LS102783
      DO 737 J=1,NGW
        IF (RCB(J) .GE. 0.0  .AND.  RCB(J) .LE. 1.0) GO TO 737
        WRITE (72,983) IC, J, RCB(J)
        ERR = 1
  737 CONTINUE
      READ(71,880) IC, (RCF(J),RCP(J),J=1,NRES)
      IF(IC .NE. JC(35)) CALL CHECK( 1, IC, JC(35) )                    LS102783
      DO 739 J=1,NRES
        IF (RCF(J) .GE. 0.0  .AND.  RCF(J) .LE. 1.0) GO TO 738
          WRITE (72,991) IC, J, RCF(J)
          ERR = 1
  738   CONTINUE
        IF (RCP(J) .GE. 0.0  .AND.  RCP(J) .LE. 1.0) GO TO 739
          WRITE (72,992) IC, J, RCP(J)
          ERR = 1
  739 CONTINUE
      WRITE(72,977)(CECN(J),J=1,12)                                      LS102783
      JN = MAX0( NRES, NGW )                                             0884KF
      WRITE(72,974)                                                       0884KF
      DO 750 J = 1, JN                                                   0884KF
         IF(J .LE. NGW  .AND.  J .LE. NRES)                              0884KF
     *     THEN                                                          0884KF
             WRITE(72,976) J, RES(J), RSEP(J), RESMX(J), REXP(J),         0884KF
     *                    KRSP(J), RCF(J), RCP(J),                       0884KF
     *                    J, GW(J), GSNK(J), RCB(J)                      0884KF
           ELSE                                                          0884KF
             IF(J .GT. NGW) WRITE(72,976) J, RES(J), RSEP(J), RESMX(J),   0884KF
     *                      REXP(J), KRSP(J), RCF(J), RCP(J)             0884KF
             IF(J .GT. NRES) WRITE(72,978) J, GW(J), GSNK(J), RCB(J)      0884KF
           ENDIF                                                         0884KF
  750    CONTINUE                                                        0884KF
      SDARU = 0.0
      WRITE(72,900) TITL                                                 LS0287
      WRITE(72,950)
      DO 40 J=1,NRU
      READ(71,860) IC,IRD(J),SLP(J),ELV(J),ICOV(J),COVDNS(J),COVDNW(J),
     1TRNCF(J),SNST(J),RNSTS(J),RNSTW(J),ITST(J),ITND(J),ITSW(J),CTX(J),
     2TXAJ(J),TNAJ(J)
      IF(  IRD(J) .LE. NRD  .AND.
     *     SLP(J) .GE. 0.0  .AND.  SLP(J) .LT. 1.0  .AND.
     *    ICOV(J) .GE. 0    .AND.  ICOV(J) .LE. 3   .AND.
     *    ITST(J) .GE. 0    .AND.  ITST(J) .LE. 12  .AND.
     *    ITND(J) .GE. 0    .AND.  ITND(J) .LE. 12  .AND.
     *    ITSW(J) .GE. 0    .AND.  ITSW(J) .LE. 1 ) GO TO 741
        WRITE(72,641) IC, J, NRD, IRD(J), ICOV(J), ITST(J),
     *               ITND(J), ITSW(J), SLP(J)
        ERR = 1
  741 CONTINUE
      IF( COVDNS(J) .GE. 0.  .AND.  COVDNS(J) .LE. 1.   .AND.
     *    COVDNW(J) .GE. 0.  .AND.  COVDNW(J) .LE. 1.   .AND.
     *     TRNCF(J) .GE. 0.  .AND.   TRNCF(J) .LE. 1. ) GO TO 742
        WRITE(72,642) IC, J, COVDNS(J), COVDNW(J), TRNCF(J)
  742 CONTINUE
      IF( SNST(J) .GE. 0.  .AND.  SNST(J) .LE. 10.   .AND.
     *   RNSTS(J) .GE. 0.  .AND. RNSTS(J) .LE. 10.   .AND.
     *   RNSTW(J) .GE. 0.  .AND. RNSTW(J) .LE. 10. ) GO TO 743
        WRITE(72,643) IC, J, SNST, RNSTS, RNSTW
  743 CONTINUE
      READ(71,865) IC,ISOIL(J),SMAX(J),SMAV(J),REMX(J),RECHR(J),SRX(J),
     3SCX(J),SCN(J),SC1(J),IMPERV(J),RETIP(J),SEP(J),KRES(J),KGW(J),
     4KSTOR(J)
      IF(ISOIL(J) .GE. 1     .AND.  ISOIL(J) .LE. 3   .AND.
     *    KRES(J) .LE. NRES  .AND.    KGW(J) .LE. NGW .AND.
     *   KSTOR(J) .LE. NSTOR ) GO TO 744
      WRITE(72,644) IC, J, NRES, NGW, NSTOR, ISOIL(J), KRES(J),
     *             KGW(J), KSTOR(J)
      ERR = 1
  744 CONTINUE
      IF( SMAV(J) .LE. SMAX(J)  .AND. RECHR(J) .LE. REMX(J)   .AND.
     *    REMX(J) .LE. SMAX(J) ) GO TO 745
        WRITE(72,645) IC, J, SMAV(J), SMAX(J), RECHR(J), REMX(J)
        ERR = 1
  745 CONTINUE
      IF( SMAX(J) .GE. 0.  .AND.  SMAX(J) .LE. 20.   .AND.
     *    REMX(J) .GE. 0.  .AND.  REMX(J) .LE. 15.   .AND.
     *   IMPERV(J).GE. 0.  .AND. IMPERV(J).LE. 1.    .AND.
     *   RETIP(J) .GE. 0.  .AND. RETIP(J) .LE. 20.   .AND.
     *     SEP(J) .GE. 0.  .AND.   SEP(J) .LE. 10. ) GO TO 746
      WRITE(72,646) IC, J, SMAX(J), REMX(J), IMPERV(J), RETIP(J),
     *             SEP(J)
  746 CONTINUE
      IF(  SRX(J) .GE. 0.  .AND.  SRX(J) .LE. 10.   .AND.
     *     SCX(J) .GE. 0.  .AND.  SCX(J) .LE. 1.    .AND.
     *   ( ( ISSR1 .EQ. 0  .AND.  SCN(J) .GE. 0. .AND. SCN(J) .LE. 1. )
     *.OR. ( ISSR1 .EQ. 1  .AND.  SCN(J) .GE. 0. .AND. SCN(J) .LE. 1. )
     *                     .AND.  SC1(J) .GE. 0. .AND. SC1(J) .LE. 1. ))
     *   GO TO 747
        WRITE(72,647) IC, J, ISSR1, SRX(J), SCX(J), SCN(J), SC1(J)
  747 CONTINUE
      READ(71,870) IC, KDS(J),DARU(J),UPCOR(J),DRCOR(J),DSCOR(J),TST(J), GL081584
     1KTS(J),KSP(J),KSDC(J),AIMX(J),PKFAC(J)                            LS0885
      SDARU = SDARU + DARU(J)
      IF( KDS(J) .LE. NDS  .AND.  SDARU   .LE. DAT*1.05 ) GO TO 748     AL080983
        WRITE(72,648) IC, J, NDS, DAT, KDS(J), DARU(J), SDARU
      ERR = 1
  748 CONTINUE
      WRITE(72,955) J,IRD(J),ITST(J),ITSW(J),TXAJ(J),RNSTS(J),SNST(J),
     1COVDNS(J),ICOV(J),SMAX(J),REMX(J),SCN(J),SRX(J),RETIP(J),SEP(J),
     2KRES(J),ELV(J),ITND(J),CTX(J),TNAJ(J),RNSTW(J),TRNCF(J),COVDNW(J),
     3ISOIL(J),SMAV(J),RECHR(J),SC1(J),SCX(J),IMPERV(J),KSTOR(J),KGW(J)
      IT(J)=ITSW(J)
      PERV(J)=1.-IMPERV(J)
      DARIP(J)=DARU(J)*IMPERV(J)
      DARP(J)=DARU(J)*PERV(J)
   40 CONTINUE
      WRITE(72,960)
      DO 90 J=1,NRU
      L=KDS(J)
   90 WRITE(72,965)J,L,SLP(J),DARU(J),DARP(J),DARIP(J),UPCOR(J),DRCOR(J)
     1,DSCOR(J),TST(J),KTS(J),KSP(J),KSDC(J),AIMX(J),PKFAC(J)           LS0885
      DATC=0.
      DATIP=0.
      DATP=0.
      DO 11 IDS=1,NDS
   11 PPAR(IDS)=0.
      DO 42 J=1,NGW
   42 ARG(J)=0.
      DO 44 J=1,NRES
   44 ARS(J)=0.
      DO 10 IRU=1,NRU
      IDS=KDS(IRU)
      L=KGW(IRU)
      M=KRES(IRU)
      ARG(L)=ARG(L)+DARU(IRU)
      ARS(M)=ARS(M)+DARU(IRU)
      PPAR(IDS)=PPAR(IDS)+DARU(IRU)
      DATC=DATC+DARU(IRU)
      DATIP=DATIP+DARIP(IRU)
      DATP=DATP+DARP(IRU)
   10 CONTINUE
      WRITE(72,966) DATC,DATP,DATIP
      IF(NSTOR.EQ.0) GO TO 48
      DO 60 JK=1,NSTOR
      READ(71,872) IC,J,IRTYP(J),NSOS(J),IRUP1(J),IRUP2(J),IRUP3(J),
     1  RCS(J),STO(J),QRO(J),DIN1(J)
      IF(IC .NE. JC(39)) CALL CHECK( 1, IC, JC(39))                     LS102783
      IF(IRUP1(J).NE.0) IDWN(IRUP1(J)) = 1                              LS691
      IF(IRUP2(J).NE.0) IDWN(IRUP2(J)) = 1                              LS691
      IF(IRUP3(J).NE.0) IDWN(IRUP3(J)) = 1                              LS691
      STOIN(J)=(STO(J)*23.76)/DAT
      IF(IRTYP(J).EQ.9) GO TO 60
      KK=NSOS(J)
      READ(71,874) IC, (O2(J,K),S2(J,K),K=1,KK)
      IF(IC .NE. JC(40)) CALL CHECK( 1, IC, JC(40) )                    LS102783
      DO 50 L=1,KK
      WVD(J,L)=S2(J,L)+(O2(J,L)*.5)
   50 CONTINUE
      DO 52 L=2,KK
      S24(J,L)=(O2(J,L)-O2(J,L-1))/(WVD(J,L)-WVD(J,L-1))
      C24(J,L)=O2(J,L)-(S24(J,L)*WVD(J,L))
   52 CONTINUE
      WRITE(72,986)
      WRITE(72,987)J,IRTYP(J),NSOS(J),IRUP1(J),IRUP2(J),IRUP3(J),RCS(J),
     1STO(J),QRO(J),DIN1(J),STOIN(J)
  110 CONTINUE
      WRITE(72,988)
      IF(IRTYP(J).EQ.9) GO TO 120
      L=NSOS(J)
      WRITE(72,989) J,(O2(J,K),S2(J,K),K=1,L)
  120 CONTINUE
   60 CONTINUE
   48 CONTINUE
      IF(NDC.GT.0) THEN                                                 LS0885
        WRITE(72,995)                                                    LS0885
        WRITE(72,996)                                                    LS0885
        DO 990 J=1,NDC                                                  LS0885
        READ(71,994)(SCA(J,K),K=1,11)                                    LS0885
  990   WRITE(72,997)J,(SCA(J,K),K=1,11)                                 LS0885
      END IF                                                            LS0885
      IDY=1
      JWDY=1
      MXDY=366
      DO 225 J=1,NRES
  225 RES(J)=RES(J)*ARS(J)
      DO 230 J=1,NGW
  230 GW(J)=GW(J)*ARG(J)
      IF(PARMCA(4).NE.20) GO TO 200
      BST=(BST*1.8)+32.
      DO 1300 J=1,12
 1300 PAT(J)=(PAT(J)*1.8)+32.
C     DO 1310 J=1,NRU                                                   LS102783
C1310 TST(J)=(TST(J)*1.8)+32.                                           LS102783
  200 CALL SUMALL(4)
      CALL SUMALL(3)
      CALL SUMALL(2)
      RETURN
      END
C
C
C
      SUBROUTINE   CHECK
     I                  ( IGRP, IFND, IEXP )
C
C     + + + PURPOSE + + +
C     Prints message about error reading input file.  STOPs program.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IGRP
      INTEGER   IFND, IEXP
C
C     + + + ARGUMENT DEFINTIONS + + +
C     IGRP   - expected card group
C     IFND   - identifier of record found
C     IEXP   - identifier of expected record
C
C     + + + OUTPUT FORMATS + + +
    1 FORMAT(/, 5X, '***** EXPECTED CARD NOT FOUND ',
     *       /, 5X, '***** LOOKING FOR GROUP', I3, '  CARD ', A2,
     *       /, 5X, '***** FOUND CARD ', A2,
     *       /, 5X, '***** NO MORE INPUT WILL BE READ.  MUST STOP' )
C
C     + + + END SPECIFICATIONS + + +
C
      WRITE(72,1) IGRP, IEXP, IFND
C
      STOP
      END
C
C
C
      SUBROUTINE   SOLTAB
     ?                   (K,I,A,L0)
C
C     + + + PURPOSE + + +
C     Determine solar radiation
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   K
      REAL      I, A, L0
C
C     + + + ARGUMENT DEFINITIONS + + +
C     K      - ?????
C     I      - ?????
C     A      - ?????
C     L0     - ?????
C
C     + + + COMMONS + + +
      INCLUDE 'cet.inc'
      INCLUDE 'crd.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   JDAY(13), IS
      REAL      E(13), DIM(13), L1, L2, R0, R1, D, D1, DAY,
     #          T, T0, T1, T2, T3, T6, T7, T8, T9, TX,
     #          V, W, X, Y
C
C     + + + FUNCTIONS + + +
      REAL   FUNC3
C
C     + + + INTRINSICS + + +
      INTRINSIC   SIN, COS, ATAN, ASIN, TAN, ABS, ACOS
C
C     + + + DATA INITIALIZATIONS + + +
      DATA E/2.06699,2.06317,2.05582,2.04520,2.03243,2.01706,2.00080,
     11.98553,1.96990,1.95714,1.94689,1.94005,1.93616/
      DATA DIM/-.410152,-.383391,-.337430,-.27198,-.190532,-.09832,0.,
     1.09832,.190532,.27198,.33743,.383391,.410152/
      DATA JDAY/356,10,23,38,51,66,80,94,109,123,138,152,173/
C
C     + + + END SPECIFICATIONS + + +
C
      FUNC3 (V,W,X,Y)=R1*(SIN(D)*SIN(W)*(X-Y)*3.8197+COS(D)*COS(W)*
     1(SIN(X+V)-SIN(Y+V))*12./3.14159)
      I=ATAN(I)
      A=A/57.2958
      L0=L0/57.2958
      R0=2.0
      L1=ASIN(COS(I)*SIN(L0)+SIN(I)*COS(L0)*COS(A))
      D1=COS(I)*COS(L0)-SIN(I)*SIN(L0)*COS(A)
      IF(D1.EQ.0.) D1=.0000000001
      L2=ATAN(SIN(I)*SIN(A)/D1)
      IF(D1.LT.0.) L2=L2+3.14159
      DO 200 IS=1,13
      DAY=JDAY(IS)
      D=DIM(IS)
      R1=60.*E(IS)
      T=0.
      TX=-TAN(L1)*TAN(D)
      IF(TX.LT.-1.0) T=3.14159
      IF(TX.GT.1.0)  T=0.
      IF(ABS(TX).LE.1.) T=ACOS(TX)
      T7=T-L2
      T6=-T-L2
      T=0.
      TX=-TAN(L0)*TAN(D)
      IF(TX.LT.-1.0) T=3.14159
      IF(TX.GT.1.0) T=0.
      IF(ABS(TX).LE.1.) T=ACOS(TX)
      T1=T
      T0=-T
      T3=T7
      IF(T7.GT.T1) T3=T1
      T2=T6
      IF(T6.LT.T0) T2=T0
      IF(I.NE.0) GO TO 150
      RAD(K,IS)=FUNC3 (0.,L0,T1,T0)
      SSH(K,IS)=(T1-T0)*3.8197/12.
      GO TO 200
  150 IF(T3.GE.T2) GO TO 160
      T2=0.
      T3=0.
  160 T6=T6+6.28318
      IF(T6.LT.T1) GO TO 190
      T7=T7-6.28318
      IF(T7.GT.T0) GO TO 180
      RAD(K,IS)=FUNC3 (L2,L1,T3,T2)
      SSH(K,IS)=(T3-T2)*3.8197/12.
      GO TO 200
  180 T8=T0
      T9=T7
      GO TO 195
  190 T8=T6
      T9=T1
  195 RAD(K,IS)=FUNC3 (L2,L1,T3,T2) + FUNC3 (L2,L1,T9,T8)
      SSH(K,IS)=(T3-T2+T9-T8)*3.8197/12.
  200 CONTINUE
      RETURN
      END
C
C
C
      SUBROUTINE PKADJ
C
C     + + + PURPOSE + + +
C     Adjust snow pack
C
C     + + + COMMONS + + +
      INCLUDE 'call.inc'
      INCLUDE 'csno.inc'
      INCLUDE 'copt.inc'
      INCLUDE 'csnop.inc'
      INCLUDE 'copsno.inc'
      INCLUDE 'cpkadj.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   J, IRU
      REAL      PAD, DIF, PCT
C
C     + + + OUTPUT FORMATS + + +
  910 FORMAT(2X,'PKADJ',3X,I2,'/',I2,'/',I4)                            KD1291
  920 FORMAT(2X,'B4  ',15F8.2/)                                         KD1291
  930 FORMAT(2X,'AFT ',15F8.2/)                                         KD1291
  940 FORMAT(2X,'ADDED PPT = ',F5.2/)                                   KD1291
C
C     + + + END SPECIFICATIONS + + +
      PAD=0.
      IF(NPRNT.EQ.0) GO TO 10
      WRITE(72,910) MO,MDY,MYR
      WRITE(72,920) (PWEQV(J),J=1,NRU)
   10 DO 20 IRU=1,NRU
      IF(PKAD(IRU).LT.0.) GO TO 20
      IF(PKAD(IRU).GT.0.) GO TO 12
      SMLT(IRU)=PWEQV(IRU)
      PWEQV(IRU)=0.
      PICE(IRU)=0.
      FREWT(IRU)=0.
      PSS(IRU)=0.
      DPT(IRU)=0.
      GO TO 20
   12 DIF=PKAD(IRU)-PWEQV(IRU)
      IF(DIF) 14,20,16
   14 IF(FREWT(IRU).LE.0.) GO TO 16
      PCT=FREWT(IRU)/PICE(IRU)
      PWEQV(IRU)=PKAD(IRU)
      FREWT(IRU)=(PCT/(1.+PCT))*PWEQV(IRU)
      PICE(IRU)=PWEQV(IRU)-FREWT(IRU)
      GO TO 18
   16 IF(PWEQV(IRU).GT.0.) GO TO 17
      DEN(IRU)=.3
      PSS(IRU)=DIF
      DPT(IRU)=DIF/.3
   17 PICE(IRU)=PICE(IRU)+DIF
      PWEQV(IRU)=PKAD(IRU)
      PKDEF(IRU)=PWEQV(IRU)*PACT(IRU)*(-1.27)
   18 PAD=PAD+(DARU(IRU)*DIF)
   20 CONTINUE
      PAD=PAD/DAT
      IF(NPRNT.EQ.0) GO TO 30
C               OUTPUT ADJUSTMENTS AND TOTAL ADDITION TO BASIN
      WRITE(72,930) (PWEQV(J),J=1,NRU)
      WRITE(72,940) PAD
   30 RETURN
      END
