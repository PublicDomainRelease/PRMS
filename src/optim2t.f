C
C
C
      SUBROUTINE   MATIND
     I                   ( S,
     O                     T,
     I                     NMAT )
C
C     + + + PURPOSE + + +
C     ?????
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NMAT
      DOUBLE PRECISION S(20,20), T(20,20)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     S      - ?????
C     T      - ?????
C     NMAT   - ?????
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I, I1, J, J2, JJ1, JJ, J1, K, K1, L, IT
      REAL      Z
      DOUBLE PRECISION SP(20,20), TS(20,20)
C
C     + + + OUTPUT FORMATS + + +
9700  FORMAT(' TERMINATE IN MATINV')                                    KD1291
C
C     + + + END SPECIFICATIONS + + +
C
 7001 DO 7000 I=1,NMAT
      DO 7000 J=1,NMAT
      T(I,J)=0.0D0
      TS(I,J)=0.0D0
 7000 CONTINUE
      DO 7100 K=1,NMAT
      K1=K-1
      DO 7300 I=K,NMAT
      TS(I,K)=S(I,K)
      IF(K1.EQ.0)GO TO 7300
      DO 7200 L=1,K1
 7200 TS(I,K)=TS(I,K)-TS(I,L)*T(L,K)
 7300 CONTINUE
      DO 7400 J=K,NMAT
      IF(J.GT.K)GO TO 7700
      T(K,J)=1.0D0
      GO TO 7400
 7700 T(K,J)=S(K,J)
      IF(K1.EQ.0)GO TO 7500
      DO 7600 L=1,K1
 7600 T(K,J)=T(K,J)-TS(K,L)*T(L,J)
 7500 T(K,J)=T(K,J)/TS(K,K)
 7400 CONTINUE
 7100 CONTINUE
      IT=1
 9000 DO 9300 I1=1,NMAT
      I=NMAT-I1+1
      SP(I,I)=1.0D0/T(I,I)
      IF(I.EQ.NMAT)GO TO 9200
      J1=I+1
      DO 9100 J=J1,NMAT
      SP(I,J)=0.0D0
      DO 9100 JJ=J1,J
      SP(I,J)=SP(I,J)-T(I,JJ)*SP(JJ,J)/T(I,I)
9100  CONTINUE
9200  IF(I.EQ.1)GO TO 9300
      J2=I-1
      DO 9250 J=1,J2
      SP(I,J)=0.0D0
9250  CONTINUE
9300  CONTINUE
      IF(IT.EQ.2)GO TO 6100
      DO 6000 I=1,NMAT
      DO 6000 J=1,NMAT
      T(I,J)=TS(J,I)
      TS(J,I)=SP(J,I)
 6000 CONTINUE
      IT=2
      GO TO 9000
 6100 DO 6200 I=1,NMAT
      DO 6200 J=1,NMAT
      T(I,J)=0.0D0
      DO 6300 K=1,NMAT
 6300 T(I,J)=T(I,J)+TS(I,K)*SP(J,K)
 6200 CONTINUE
      RETURN
9600  WRITE(72,9700)
      STOP
      END
C
C
C
      SUBROUTINE   MATINV
     I                   ( S,
     O                     T,
     I                     NMAT,
     O                     IER )
C
C     + + + PURPOSE + + +
C     ?????
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NMAT, IER
      REAL      S(20,20), T(20,20)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     S
C     T
C     NMAT
C     IER
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I, I1, J, J2, JJ1, JJ, J1, K
      REAL      SP(20,20), Z
C
C     + + + INTRINSICS + + +
      INTRINSIC   SQRT
C
C     + + + END SPECIFICATIONS + + +
C
      DO 8500 I=1,NMAT
      Z=0.
      IF(I.EQ.1)GO TO 8200
      I1=I-1
      DO 8100 J=1,I1
8100  Z=Z+T(J,I)*T(J,I)
8200  IF(S(I,I)-Z.LE.0.)GO TO 9600
      T(I,I)=SQRT(S(I,I)-Z)
      IF(I.EQ.NMAT)GO TO 8500
      J2=I+1
      DO 8450 J=J2,NMAT
      Z=0.
      IF(I.EQ.1)GO TO 8400
      JJ1=I-1
      DO 8300 JJ=1,JJ1
8300  Z=Z+T(JJ,I)*T(JJ,J)
8400  T(I,J)=(S(I,J)-Z)/T(I,I)
8450  CONTINUE
8500  CONTINUE
      DO 9300 I1=1,NMAT
      I=NMAT-I1+1
      SP(I,I)=1./T(I,I)
      IF(I.EQ.NMAT)GO TO 9200
      J1=I+1
      DO 9100 J=J1,NMAT
      SP(I,J)=0.
      DO 9100 JJ=J1,J
      SP(I,J)=SP(I,J)-T(I,JJ)*SP(JJ,J)/T(I,I)
9100  CONTINUE
9200  IF(I.EQ.1)GO TO 9300
      J2=I-1
      DO 9250 J=1,J2
      SP(I,J)=0.
9250  CONTINUE
9300  CONTINUE
      DO 9400 I=1,NMAT
      DO 9400 J=1,I
      T(I,J)=0.
      DO 9500 K=1,NMAT
9500  T(I,J)=T(I,J)+SP(I,K)*SP(J,K)
      T(J,I)=T(I,J)
9400  CONTINUE
      RETURN
9600  IER=1
      RETURN
      END
C
C
C
      SUBROUTINE   SENMAT
C
C     + + + PURPOSE + + +
C     ?????
C     + + + COMMONS + + +
      INCLUDE 'copt.inc'
      INCLUDE 'csent.inc'
      INCLUDE 'csent3.inc'
      INCLUDE 'csent1.inc'
      INCLUDE 'cvol.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   NSWIT, I, II, IIS, JJ, K, IVAL, JK, J, KL, NDAY, NDAYP
      REAL      XINC(20), XDEL(20), XTEMP
C
C     + + + INTRINSICS + + +
      INTRINSIC   ALOG, EXP
C
C     + + + DATA INITIALIZATIONS + + +
      DATA NSWIT/1/                                                     0000000
C
C     + + + OUTPUT FORMATS + + +
  243 FORMAT(I5,F12.4)                                                  0000000
C2141 FORMAT(A29,4I3,5F6.2)
 7015 FORMAT(//,2X,'LOP',6X,'VALUE')                                    KD1291
C
C     + + + END SPECIFICATIONS + + +
      GO TO (27,7025) ,NSWIT                                            0000000
   27 DO 7010 I=1,NV                                                    0000000
      XOPT(I)=XX(I)                                                     0000000
      IF(ILOPL(I).EQ.1)XINC(I)=ALOG(1.+PINC)                            0000000
      IF(ILOPL(I).EQ.0)XINC(I)=XOPT(I)*PINC                             0000000
      XDEL(I)=XOPT(I)+XINC(I)                                           0000000
7010  CONTINUE                                                          0000000
      NSWIT=2                                                           0000000
      IF(IOPT.EQ.0)WRITE(72,7015)                                        0000000
      II=0                                                              0000000
7025  II=II+1                                                           0000000
      IF(II.GT.NV)GO TO 28                                              0000000
      XTEMP=XOPT(II)                                                    0000000
      IF(ILOPL(II).EQ.1)XTEMP=EXP(-20.+XTEMP)                           0000000
      IF(IOPT.EQ.0)WRITE(72,243)LOP(II),XTEMP                            0000000
      IF(II.EQ.1)GO TO 7030                                             0000000
      IIS=II-1                                                          0000000
      XX(IIS)=XOPT(IIS)                                                 0000000
 7030 XX(II)=XDEL(II)                                                   0000000
      RETURN                                                            0000000
   28 XX(NV)=XOPT(NV)                                                   0000000
      IF(IOPT.NE.0)GO TO 62
      DO 40 I=1,NV
      IF(ILOPL(I).EQ.0)GO TO 40
      XOPT(I)=EXP(-20.+XOPT(I))
      XINC(I)=PINC*XOPT(I)
   40 CONTINUE
   62 IF(ISEN.LT.3) GO TO 30                                            0000000
      REWIND 8
      READ(8) (SMVOPT(JJ),JJ=1,NST)                                     0000000
      DO 33 K=1,NV                                                      0000000
   33 READ(8) (ASENS(IVAL,K),IVAL=1,NST)                                0000000
      JK=0                                                              0000000
      DO 32 J=1,NST                                                     0000000
      IF(TESTNO(J).EQ.0) GO TO 32                                       0000000
      JK=JK+1                                                           0000000
      IF(ITRANS.EQ.0) SMVOPT(JK)=SMVOPT(J)                                000000
      IF(ITRANS.EQ.1) SMVOPT(JK)=ALOG(SMVOPT(J))                          000000
      DO 35 K=1,NV                                                      0000000
      IF(ITRANS.EQ.0) ASENS(JK,K)=ASENS(J,K)                              000000
      IF(ITRANS.EQ.1) ASENS(JK,K)=ALOG(ASENS(J,K))                        000000
   35 CONTINUE                                                          0000000
   32 CONTINUE                                                          0000000
      NOBF=JK                                                           0000000
      DO 7026 II=1,NV                                                   0000000
      DO 7026 IVAL=1,NOBF                                               0000000
      ASENS(IVAL,II)=(ASENS(IVAL,II)-SMVOPT(IVAL))/XINC(II)             0000000
7026  CONTINUE                                                          0000000
      GO TO 7027
   30 REWIND 8
      DO 54 KL=1,NMOBF
      NREC=2*KL
      READ(9,REC=NREC)NDAY,(SMVOPT(J),J=1,31)
      NREC=NREC+1
      READ(9,REC=NREC)NDAY,(OBSRUN(J),J=1,31)
C     WRITE(72,2141)'SENMA1,KL,NREC,IWSW2,NDY,OBSR',KL,NREC,IWSW2,NDP,
C    *(OBSRUN(J),J=1,5)
C     WRITE(72,2141)'SENMA1,KL,NREC,IWSW2,NDY,SMVO',KL,NREC,IWSW2,NDP,
C    *(SMVOPT(J),J=1,5)
      NREC=NREC+1
      DO 51 K=1,NV
      NREC=KL+1+NMOBF*(K+1)
      READ(9,REC=NREC)NDAYP,(ASENS(JJ,K),JJ=1,31)
      NREC=NREC+1
   51 CONTINUE
      IF(ITRANS.EQ.0)GO TO 55
      DO 70 J=1,NDAY
      SMVOPT(J)=ALOG(SMVOPT(J)+1.)
      OBSRUN(J)=ALOG(OBSRUN(J)+1.)
      DO 70 K=1,NV
      ASENS(J,K)=ALOG(ASENS(J,K)+1.)
   70 CONTINUE
   55 DO 52 II=1,NV
      DO 52 IVAL=1,NDAY
      ASENS(IVAL,II)=(ASENS(IVAL,II)-SMVOPT(IVAL))/XINC(II)
   52 CONTINUE
      WRITE(8)NDAY
      WRITE(8)((ASENS(IVAL,II),II=1,NV),IVAL=1,NDAY),
     1(OBSRUN(IVAL),IVAL=1,NDAY),(SMVOPT(IVAL),IVAL=1,NDAY)
C     WRITE(72,2141)'SENMA2,YR,MO,IWSW2,NDY,ASEN',IWY,KL,IWSW2,NDAY,
C    *(ASENS(IVAL,1),IVAL=1,5)
C     WRITE(72,2141)'SENMA2,YR,MO,IWSW2,NDY,OBSR',IWY,KL,IWSW2,NDAY,
C    *(OBSRUN(II),II=1,5)
C     WRITE(72,2141)'SENMA2,YR,MO,IWSW2,NDY,SMVO',IWY,KL,IWSW2,NDAY,
C    *(SMVOPT(II),II=1,5)
   54 CONTINUE
      ENDFILE 8
 7027 NDSN=1
      NSWIT=1
      RETURN                                                            0000000
      END                                                               0000000
C
C
C
      SUBROUTINE   SNORT
     I                  ( SIZE, NV,
     M                    ISTORE,
     O                    IMEX )
C
C     + + + PURPOSE + + +
C     Sorts out which of the new directions is most parallel to
C     each of the old directions so that function evaluations
C     are not wasted adjusting step size.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NV, ISTORE(20), IMEX
      REAL      SIZE(20)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SIZE   - Cosines of angles between one of the old directions
C              and all the remaining new directions to which a step
C              size has not yet been assigned
C     NV     - Number of parameters
C     ISTORE - Equals zero if Ith new direction has not yet been
C              assigned a step size
C     IMEX   - Index of the new direction to which the old direction
C              step size is most appropriate
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   J, JJ
      REAL      HOLD
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS
C
C     + + + END SPECIFICATIONS + + +
      J = 0
    1 J = J + 1
      IF(ISTORE(J).EQ.J) GO TO 1
      HOLD = ABS(SIZE(J))
      IMEX = J
      IF(J.GE.NV) GO TO 4
      JJ = J + 1
      IF(JJ.EQ.ISTORE(JJ)) GO TO 2
    3 IF(HOLD.GE.ABS(SIZE(JJ))) GO TO 2
      HOLD = ABS(SIZE(JJ))
      IMEX = JJ
    2 JJ = JJ + 1
      IF(JJ.GT.NV) GO TO 4
      IF(JJ.EQ.ISTORE(JJ)) GO TO 2
      GO TO 3
    4 ISTORE(IMEX) = IMEX
      RETURN
      END
C
C
C
      SUBROUTINE   SUB1
     I                 ( NV,
     M                   X,
     ?                   NC, G, H,
     I                   B4,
     O                   WITE, NFCC,
     M                   NOEND, U,
     ?                   N,
     I                   NTRY, AP,
     O                   NDOP, SDX,
     I                   PHI, IOPT, IR )
C
C     + + + PURPOSE + + +
C     Controls the main strategy of the Rosenbrock optimization
C     procedure.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NV, NC, NFCC, NOEND, NTRY, NDOP, IOPT, IR
      REAL      X(20), G(60), H(60), B4, WITE, U(2), AP(20),
     #          SDX(20), PHI(10)
C
C     + + + ARGUMENT DEFINTIONS + + +
C     NV     - Number of variables
C     X      - Parameter values
C     NC     - Number of constraints
C     G      - Lower constraint values (1 to NC)
C              inside value of lower constraint (NC+1 to 2NC)
C              penalty value of obj function for each constraing(2NC+1to3NC)
C     H      - Same as G for upper constraint value
C     B4     - Obj function switch  +1=maximize  -1=minimize
C     WITE   - ?????
C     NFCC   - ?????
C     NOEND  - ?????
C     U      - (1) stores best value of obj function
C              (2) stores obj function value assoc. with current param values
C     N      - Switch to control U array
C     NTRY   - Max no. of iterations of param set to be run
C     AP     - ?????
C     NDOP   - End opt swithc  0=no end   1=end
C     SDX    - Unscaled parameter values returned to model for eval
C     PHI    - ?????
C     IOPT   - ?????
C     IR     - ?????
C
C     + + + SAVES + + +
      INTEGER   NE(20), NF(20), IFTS, JSW, NROUND, ISW, I, NUTOLD,
     #          NTRYCT, NSTAGE, J, L, NUIT, K, IST, IPC, N
      REAL      SSX(20), A(420), B(2), D(20), SX(20), STORX(20),
     #          UREV, UKEEP, ELTA, BD
      SAVE
C
C     + + + DEFINTIONS + + +
C     NE     - Array of failure counts
C     A      - Parameter step sizes used in opt (1 to NV)
C              (NV+1 to NV*NV+NV) stores the orthonormal search directions
C              as the NV components of the Ith search direction appear
C              in locations A(NV*J+I) ,J runs from 1 to NV
C     B      - (1) modulus of progress along all search directions
C              (2) modulus of the ratio of progress along all search
C              directions to progress along all but first direction
C     D      - Stores progress along each search direction
C     ISW    - Controls input data about constraints
C     JSW    - Step size and direction switch  1=halve step,reverse direction
C              2=halve step, same direction
C     NF     - Array of success counts
C     NROUND - Counter for no. of times coordinate axes searched
C     NSTAGE - Counter for number of reorthogonalizations
C     NTRYCT - Counter for no. of iterations thru param set
C     NUIT   - Switch that indicates conditions found in boundary
C              0=no parameter value in constraint zone
C              1=parameter values altered, new obj function eval needed
C              2=penalty function makes current obj function less opt than
C                previous best value, withdraw this step in SUB1
C              -2= boundary zone has been entered
C     NUTOLD - Stores previous value of NUIT
C     SSX    - Stores best parameter values in unscaled form
C     SX     - Array of scaling factors
C     UKEEP  - Stores start of round obj function for test of convergence
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL   TCALE, BDRY, UNSCAL, SUB3
C
C     + + + DATA INITIALIZATIONS + + +
      DATA IFTS/1/
C
C     + + + OUTPUT FORMATS + + +
   77 FORMAT('0CONVERGENCE HAS OCCURRED',/,' ------------------------') KD1291
  112 FORMAT(' PARAMETERS :',2F10.3)                                    KD1291
  113 FORMAT(13X,2F10.3)                                                0000000
  114 FORMAT(/,' OBJ. FNC. :',1X,2F10.3)                                KD1291
  115 FORMAT(/,' AUTOREGRESSIVE, MOVING AVE. PARAMETERS:',/,10F10.3)    KD1291
  117 FORMAT('1')                                                       KD1291
  120 FORMAT(////,' RESULTS FOR INITIAL PARAMETER VALUES',/)            KD1291
  131 FORMAT(////,' RESULTS FOR ADJUSTMENT',I3,', PARAMETER',I3         KD1291
     1,//,19X,'OLD',7X,'NEW')                                           KD1291
  300 FORMAT(21X,'AFTER',I2,' ROUNDS ,',I3,' STAGES ,',I6,' FUNCTION E',KD1291
     1'VALUATIONS ',I6,' TRIALS AND ',              /21X,'F =',D15.8,   KD1291
     210X,'B(1) =',E15.8,10X,'B(2) =',E15.8/(5X,9E14.6))                KD1291
  900 FORMAT(/,1X,2I4,2E15.8,6E15.8,/,4(39X,6E15.8,/))                  0000000
  910 FORMAT(/,' INIT',4X,2E15.8,6E15.8,/39X,6E15.8)                    KD1291
C
C     + + + END SPECIFICATIONS + + +
C
      IF(IFTS.EQ.0) GO TO 100                                           0000000
      IFTS=0                                                            0000000
      JSW=1                                                             0000000
      NROUND = 1                                                        0000000
      ISW = 2                                                           0000000
      NFCC = 0                                                          0000000
      U(1) = U(1) * B4                                                  0000000
      UREV=-U(1)                                                        0000000
      IF(IOPT.EQ.2)GO TO 110                                            0000000
      WRITE(72,910) UREV,U(2),(X(I),I=1,NV)                              0000000
      GO TO 111                                                         0000000
  110 WRITE(72,120)                                                      0000000
      DO 116 I=1,NV                                                     0000000
  116 STORX(I)=X(I)                                                     0000000
      DO 122 I=1,NV                                                     0000000
      IF(I.EQ.1)WRITE(72,112)X(I)                                        0000000
      IF(I.GT.1)WRITE(72,113)X(I)                                        0000000
  122 CONTINUE                                                          0000000
      WRITE(72,114)UREV                                                  0000000
      WRITE(72,115)(PHI(I),I=1,IR)                                       0000000
      WRITE(72,117)                                                      0000000
  111 UKEEP = U(1)                                                      0000000
      WITE=U(1)                                                         0000000
   26 CONTINUE                                                          0000000
      CALL TCALE(NV,X,SX,G,H,NC)                                        0000000
      NUTOLD = 0                                                        0000000
      NTRYCT = 0                                                        0000000
      NSTAGE = 0                                                        0000000
      B(1) = 0.                                                         0000000
      B(2) = 0.                                                         0000000
      N = 1                                                             0000000
      DO 20 J =1,NV                                                     0000000
      A(J)=AP(J)                                                        0000000
      NE(J) = 0                                                         0000000
      D(J)=0.                                                           0000000
      NF(J)=0                                                           0000000
      DO 2 I = 1,NV                                                     0000000
      L= NV*I + J                                                       0000000
      IF(I-J) 3,4,3                                                     0000000
    3 A(L) = 0.                                                         0000000
      GO TO 2                                                           0000000
    4 A(L) = 1.                                                         0000000
    2 CONTINUE                                                          0000000
   20 CONTINUE                                                          0000000
      GO TO 261                                                         0000000
   18 DO 5 I = 1,NC                                                     0000000
      IF((X(I).GT.G(I)).AND.(X(I).LT.H(I))) GO TO 5                     0000000
      JSW=2                                                             0000000
      GO TO 6                                                           0000000
    5 CONTINUE                                                          0000000
    7 U(N) = 0.0                                                        0000000
      DO 22 I=1,NV                                                      0000000
   22 SDX(I) = X(I) * SX(I)                                             0000000
      RETURN                                                            0000000
  100 UREV=-U(1)                                                        0000000
      IF(IOPT.EQ.2) GO TO 130                                           0000000
      WRITE(72,900) NTRYCT,J,UREV,U(2),(SDX(I),I=1,NV)                   0000000
      GO TO 404                                                         0000000
  130 WRITE(72,131)NTRYCT,J                                              0000000
      DO 132 I=1,NV                                                     0000000
      IF(I.EQ.1)WRITE(72,112)STORX(I),SDX(I)                             0000000
      IF(I.GT.1)WRITE(72,113)STORX(I),SDX(I)                             0000000
  132 CONTINUE                                                          0000000
      WRITE(72,114)UREV,U(2)                                             0000000
      WRITE(72,115)(PHI(I),I=1,IR)                                       0000000
      WRITE(72,117)                                                      0000000
      IF(U(2).GT.UREV)GO TO 404                                         0000000
      DO 119 I=1,NV                                                     0000000
  119 STORX(I)=SDX(I)                                                   0000000
  404 CONTINUE                                                          0000000
   24 U(N) = U(N)*B4                                                    0000000
      IF(N.NE.2) GO TO 261                                              0000000
      IF(U(1).GT.U(2)) GO TO 6                                          0000000
  261 CONTINUE                                                          0000000
      NUIT = 0                                                          0000000
      CALL BDRY(X,NUIT,G,H,NC,N,U)                                      LS0287
      IF(NUIT-1) 8,7,6                                                  0000000
    8 IF(N.EQ.1) GO TO 13                                               0000000
      IF(U(1).LT.U(2)) GO TO 505                                        0000000
      IF(NUTOLD.EQ.0) GO TO 6                                           0000000
  505 NUTOLD = NUIT                                                     0000000
   89 CONTINUE                                                          0000000
      NE(J) = 0                                                         0000000
      NF(J) = 1                                                         0000000
    9 D(J) = D(J) + A(J)                                                0000000
      U(1) = U(2)                                                       0000000
      A(J) = 3.*A(J)                                                    0000000
      GO TO 10                                                          0000000
    6 DO 11 I= 1,NV                                                     0000000
   11 X(I) = SSX(I)                                                     0000000
      IF(JSW.EQ.1) GO TO 154                                            0000000
  150 A(J)=.5*A(J)                                                      0000000
      JSW=1                                                             0000000
      GO TO 14                                                          0000000
  154 A(J) = -0.5 *A(J)                                                 0000000
      NE(J) = NE(J) + 1                                                 0000000
   10 CONTINUE                                                          0000000
   19 IF(J.GE.NV) GO TO 70                                              0000000
      J = J + 1                                                         0000000
      GO TO 14                                                          0000000
   70 IF(NV.EQ.1) GO TO 13                                              0000000
      DO 12 K = 1,NV                                                    0000000
      IF((NE(K).GT.1).AND.(NF(K).NE.0)) GO TO 12                        0000000
      IF((NE(K).GT.4).AND.(NF(K).EQ.0)) GO TO 73                        0000000
      GO TO 13                                                          0000000
   12 CONTINUE                                                          0000000
      GO TO 15                                                          0000000
   73 IST = 0                                                           0000000
      DO 75 I = 1,NV                                                    0000000
      IF(NC.LE.0) GO TO 271                                             0000000
      ELTA = 0.0001*(H(I)-G(I))                                         0000000
      GO TO 272                                                         0000000
  271 ELTA = 0.0001                                                     0000000
  272 CONTINUE                                                          0000000
      IF(ABS(D(I)).LT.ELTA) D(I) = ELTA                                 0000000
      IF(NE(I).LE.4) GO TO 75                                           0000000
      IST = IST + 1                                                     0000000
   75 CONTINUE                                                          0000000
      IF(IST.LT.NV) GO TO 15                                            0000000
      IF(ABS(U(1)-UKEEP).LT.1.E-8*ABS(U(1))) GO TO 405                  0000000
      IF(NSTAGE.EQ.0) GO TO 405                                         0000000
      NROUND = NROUND + 1                                               0000000
      UKEEP = U(1)                                                      0000000
      CALL UNSCAL(NV,X,SX,G,H,NC)                                       0000000
      GO TO 26                                                          0000000
   13 J=1                                                               0000000
      NTRYCT = NTRYCT + 1                                               0000000
      IF(NTRYCT.GT.NTRY) GO TO 88                                       0000000
   14 DO 16 I = 1,NV                                                    0000000
      L=NV*I+J                                                          0000000
      SSX(I) = X(I)                                                     0000000
   16 X(I) = A(L)*A(J) + X(I)                                           0000000
      N = 2                                                             0000000
      GO TO 18                                                          0000000
   15 CALL SUB3(NV,X,A,B,NSTAGE,D)                                      LS0287
      BD=B4*U(1)                                                        0000000
  260 WRITE(72,300) NROUND,NSTAGE,NFCC,NTRYCT,BD,B(1),B(2),(X(I),I=1,NV) 0000000
      IF(NOEND.EQ.2) GO TO 88                                           0000000
      DO 270 IPC = 1,NV                                                 0000000
      NE(IPC) = 0                                                       0000000
      NF(IPC)=0                                                         0000000
      D(IPC)=0.                                                         0000000
  270 CONTINUE                                                          0000000
      GO TO 13                                                          0000000
  405 CONTINUE                                                          0000000
      WRITE(72,77)                                                       0000000
      NOEND = 2                                                         0000000
   88 CONTINUE                                                          0000000
      NDOP=1                                                            0000000
      IFTS=1
      CALL UNSCAL(NV,X,SX,G,H,NC)                                       0000000
      DO 600 I=1,NV                                                     0000000
  600 SDX(I)=X(I)                                                       0000000
   86 CONTINUE                                                          0000000
      RETURN                                                            0000000
      END                                                               0000000
C
C
C
      SUBROUTINE   SUB3
     I                 ( NV, X,
     M                   A,
     O                   B,
     M                   NSTAGE,
     I                   D )
C
C     + + + PURPOSE + + +
C     Carries out the Gram-Schmidt orthogonalization to set up
C     new orthonormal search directions.
C
C     + + + DUMMY ARUGMENTS + + +
      INTEGER   NV, NSTAGE
      REAL      X(20), A(420), B(2), D(20)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NV     - Number of parameters
C     A      - Parameter step sizes used in opt (1 TO NV)
C              A(NV+1 TO NV*NV+NV) stores orthonormal search direction
C              as the NV components of the Ith search direction appear
C              in locations A(NV*J+I), J runs from 1 to NV
C     B      - (1) modulus of progress along all the search directions
C     B      - (2) modulus of the ratio of progress along all search
C              directions to progress along all but first direction
C     NSTAGE - Number of reorthogonalizations
C     D      - Array of progress along each search direction
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   ISTORE(20), I, IPHI, L, NNV, J, KNV, K, LL, LK, IMEX
      REAL      HOLDA(420), SIZE(20), BD
C
C     + + + DEFINITIONS + + +
C     HOLDA  - Array of old step sizes and directions
C     IMEX   - Index of the new direction to which the old direction
C              step size is most appropriate
C     SIZE   - Array of cosine angles between new and old directions
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS, SQRT
C
C     + + + EXTERNALS + + +
      EXTERNAL    SNORT
C
C     + + + OUTPUT FORMATS + + +
    5 FORMAT('0PROGRESS IN VARIABLE',I2,' NOT SATISFACTORY,',' A(I)=',  KD1291
     1E20.8,' ,D(I)=',E20.8)                                            KD1291
C
C     + + + END SPECIFICATIONS + + +
C
      DO 1 I = 1,NV
    2 IF(ABS(A(I))*1.E8 -ABS(X(I))) 3,1,1
    3 WRITE(72,5) I,A(I),D(I)
      A(I)=A(I)*1.E+4
      GO TO 2
    1 CONTINUE
      IPHI = NV*(NV+1)
      DO 40 I = 1,IPHI
   40 HOLDA(I) = A(I)
      DO 41 I = 1,NV
   41 ISTORE(I) = 0
      DO 4 I = 1,NV
      L = NV*I +NV
      A(L) = D(NV)*A(L)
      NNV = NV-1
      DO 6 J = 1,NNV
      KNV = NV-J
      L = NV*I+KNV
    6 A(L) = D(KNV)*A(L) + A(L+1)
    4 CONTINUE
      DO 7 J = 1,2
      B(J) = 0.
      DO 8 I = 1,NV
      L = NV*I + J
    8 B(J)=A(L)*A(L) + B(J)
    7 B(J)=SQRT(B(J))
      B(2)=B(2)/B(1)
      J=1
    9 K=1
   12 BD=0.
      IF(K.EQ.J) GO TO 13
      DO 10 I = 1,NV
      L = NV*I +J
      LL = L - K
   10 BD = A(L)*A(LL)+BD
      DO 11 I= 1,NV
      L = NV*I+J
      LL = L - K
   11 A(L)=-A(LL)*BD +A(L)
      K=K+1
      GO TO 12
   13 DO 14 I = 1,NV
      L=NV*I+J
   14 BD=A(L)*A(L)+BD
      BD=SQRT(BD)
      DO 15 I=1,NV
      L=NV*I+J
   15 A(L)=A(L)/BD
      J=J+1
      IF(J.LE.NV)GO TO 9
      DO 42 J = 1,NV
      DO 43 K = 1,NV
      IF(K.EQ.ISTORE(K)) GO TO 43
      SIZE(K) = 0.
      DO 44 I = 1,NV
      LK = NV*I + K
      LL = NV*I + J
      SIZE(K) = SIZE(K) + HOLDA(LL)*A(LK)
   44 CONTINUE
   43 CONTINUE
      CALL SNORT(SIZE,NV,ISTORE,IMEX)
      A(IMEX) = HOLDA(J)
   42 CONTINUE
      NSTAGE = NSTAGE + 1
      RETURN
      END
C
C
C
      SUBROUTINE   TCALE
     I                  ( NV,
     M                    X,
     O                    SX,
     M                    G, H,
     I                    NC )
C
C     + + + PURPOSE + + +
C     Sets up scaled parameter and constraint values and
C     unscales parameter and constraint values.
C
C     + + + DUMMY ARUGMENTS + + +
      INTEGER   NV, NC
      REAL      X(20), SX(20), G(60), H(60)
C
C     + + + ARGUMENT DEFINTIONS + + +
C     NV     - Number of parameters
C     X      - Parameter values(scale entry, unscale exit)
C              Scaled parameter values(scale exit, unscale entry)
C     SX     - Array of scaling vactors
C     G      - Lower constraint values (1 TO NC)
C              inside value of lower constraint (NC+1 TO 2NC)
C              penalty value of obj function for each constraint(2NC+1TO3NC)
C     H      - Same as G for upper constraint values
C     NC     - Number of constraints
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I, J
      REAL      A
C
C     + + + END SPECIFICATIONS + + +
C
      DO 1 I = 1,NV
      SX(I) = X(I)
    1 X(I) = 1.0
      DO 2 I = 1,NC
      IF(I.GT.NV) RETURN
      J = NC + I
      A = 1.0/SX(I)
      G(I) = G(I) * A
      H(I) = H(I) * A
      G(J) = G(J) * A
    2 H(J) = H(J) * A
      RETURN
      END
C
C
C
      SUBROUTINE   UNSCAL
     I                   ( NV,
     M                     X,
     I                     SX,
     M                     G, H,
     I                     NC )
C
C     + + + PURPOSE + + +
C     ?????
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NV, NC
      REAL      X(20), SX(20), G(60), H(60)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NV     - ?????
C     X      - ?????
C     SX     - ?????
C     G      - ?????
C     H      - ?????
C     NC     - ?????
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I, J
C
C     + + + END SPECIFICATIONS + + +
C
      DO 4 I=1,NC
      IF(I.GT.NV) GO TO 3
      G(I) = G(I) * SX(I)
      H(I) = H(I) * SX(I)
      J = NC + I
      G(J) = G(J) * SX(I)
    4 H(J) = H(J) * SX(I)
    3 CONTINUE
      DO 6 I=1,NV
    6 X(I) = X(I) *SX(I)
      RETURN
      END
C
C
C
      SUBROUTINE   BDRY
     I                 ( X,
     O                   NUIT,
     M                   G, H,
     I                   NC, N,
     M                   U )
C
C     + + + PURPOSE + + +
C     Checks to see if any parameters lie close to their
C     boundaries. Penalizes the objective function value if any
C     of the paramerer values lie too close to the constraint
C     boundaries.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NUIT, NC, N
      REAL      X(20), G(60), H(60), U(2)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     X      - Parameter values
C     NUIT   - Switch that indicates condition found by boundary
C              0=no parameter value in constraint zone
C              1=param value altered, new obj function eval needed
C              2=penalty function makes current obj function less opt
C                than previous best value, withdraw this step in SUB1
C              -2=boundary zone has been entered
C     G      - Lower constraint values (1 TO NC)
C              inside value of lower constraint (NC+1 TO 2NC)
C              penalty value of obj function for each constraint(2NC+1TO3NC)
C     H      - Same as G for upper constraint values
C     N      - Switch to control U array
C     NC     - Number of constraints
C     U     - (1) Stores best value of obj function
C     U      - (2) Stores obj function assoc. with current parameters
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I, K, IST
      REAL      GD, HD, BD
C
C     + + + OUTPUT FORMATS + + +
    2 FORMAT('0PARAMETER VALUE RAISED TO BOUNDARY EDGE FOR I =',I2)     KD1291
    5 FORMAT('0PARAMETER VALUE LOWERED TO BOUNDARY EDGE FOR I =',I2)    KD1291
C
C     + + + END SPECIFICATIONS + + +
C
      DO 1 I = 1,NC                                                     OP
      IF(U(1).LE.U(N)) GO TO 10                                         OP
      NUIT = 2                                                          OP
      RETURN                                                            OP
   10 K = NC+I                                                          OP
      IF(X(I).GE.G(K)) GO TO 4                                          OP
      IF(N.EQ.2) GO TO 7                                                OP
      IST = -I                                                          OP
      WRITE(72,2)I                                                       OP
    3 NUIT = 1                                                          OP
      GO TO 1                                                           OP
    4 IF(X(I).LE.H(K)) GO TO 6                                          OP
      IF(N.EQ.2) GO TO 8                                                OP
      WRITE(72,5)I                                                       OP
      IST = +I                                                          OP
      GO TO 3                                                           OP
    6 K = 2*NC + I                                                      OP
      G(K) = U(1)                                                       OP
      H(K) = U(1)                                                       OP
      GO TO 1                                                           OP
    7 GD = G(K) - X(I)                                                  OP
      HD = G(K) - G(I)                                                  OP
      GD = GD/HD                                                        OP
      K = K + NC                                                        OP
      HD = U(N) - G(K)                                                  OP
      GO TO 9                                                           OP
    8 GD = X(I) - H(K)                                                  OP
      HD = H(I) - H(K)                                                  OP
      GD = GD/HD                                                        OP
      K = K + NC                                                        OP
      HD = U(N) - H(K)                                                  OP
   9  BD = -2.*GD + 4.                                                  OP
      BD = BD*GD - 3.                                                   OP
      U(N) = BD*GD*HD + U(N)                                            OP
      NUIT = -2                                                         OP
    1 CONTINUE                                                          OP
      IF(U(1).GT.U(N)) NUIT = 2                                         OP
      RETURN                                                            OP
      END                                                               OP
