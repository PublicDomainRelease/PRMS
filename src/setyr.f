C
C
C
      SUBROUTINE   SETYR
     ?                  (NDOP)
C
C     + + + PURPOSE + + +
C     This subroutine sets the next year in a series of years
C     for probability analysis
C     Last year of previous is set to first year of next run
C     This suborutine built by James K. Marron 10/8/85
C
C     + + + DUMMY ARUGMENTS + + +
      INTEGER   NDOP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NDOP   - ????
C
C     + + + COMMONS + + +
      INCLUDE 'cdates.inc'
      INCLUDE 'cprobn.inc'
      INCLUDE 'csize.inc'
      INCLUDE 'cjm.inc'
      INCLUDE 'cscs.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   MYR, NYRI, MDIFF
C
C     + + + END SPECIFICATIONS + + +
C
      BYR=EYR
      IF(BMO.LT.10) BYR=BYR+1
      EYR=EYR+1
C
C     CHECK AND ADJUST YEARS
C
      MYR=BYR
      BWY=BYR
      IF(BMO.GE.10) BWY=BWY+1
      EWY=EYR
      IF(EMO.GE.10) EWY=EWY+1
      NYRI=NYR
      IF(BYRFC.EQ.0.OR.NYRI.GT.1) GO TO 50
      MDIFF=ME-MB+1
      IF(MDIFF.LT.12)NYRI=0
   50 NYR=EWY-BWY+1
C
C     IF LAST SIMULATION SET INDEX
C
      IF(EYR.EQ.EYRPR) NDOP=1
      IF (DEBUG.EQ.0) GO TO 3000
      PRINT *,' IN SETYR BYR EYR BWY EWY ',BYR,EYR,BWY,EWY
 3000 RETURN
      END
