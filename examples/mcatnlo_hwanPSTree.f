C     CALLS FUNCTIONS FROM INTERFACE.CC TO PASS EVENT INFO TO
C     C++ CLASS, WHICH OUTPUTS THE ROOT NTUPLE
C
C----------------------------------------------------------------------
      SUBROUTINE HWABEG
C     CALLED BEFORE FIRST EVENT ANALYZED
C----------------------------------------------------------------------
C
      CALL INITROOT("../herwig.cfg")
C
      END

C----------------------------------------------------------------------
      SUBROUTINE HWAEND
C     CALLED BY HWDRIVER EVERY NSTEP EVENTS
C     UNUSED SINCE ROOT MANAGES BUFFER WELL
C----------------------------------------------------------------------
C
      END


C----------------------------------------------------------------------
      SUBROUTINE HWANAL
C     ADDS EVENT WEIGHT AND PDF INFO TO C++ CLASS
C----------------------------------------------------------------------
      INCLUDE 'HERWIG65.INC'
      INTEGER MAXNUP
      PARAMETER (MAXNUP=500)
      INTEGER NUP,IDPRUP,IDUP,ISTUP,MOTHUP,ICOLUP
      DOUBLE PRECISION XWGTUP,SCALUP,AQEDUP,AQCDUP,PUP,VTIMUP,SPINUP,
     & XMP2,XMA2,XMB2,BETA,VA,VB,SIGMA,DELTA,S2,XKA,XKB,PTF,E,PL,
     & XSCALE,XEPHO
      COMMON/HEPEUP/NUP,IDPRUP,XWGTUP,SCALUP,AQEDUP,AQCDUP,
     &              IDUP(MAXNUP),ISTUP(MAXNUP),MOTHUP(2,MAXNUP),
     &              ICOLUP(2,MAXNUP),PUP(5,MAXNUP),VTIMUP(MAXNUP),
     &              SPINUP(MAXNUP)
      DOUBLE PRECISION UX1,UX2,UQ2
      COMMON/CPDFRWGT/UX1,UX2,UQ2
      DOUBLE PRECISION WGT,Q2REN,Q2FAC,X1,X2
      INTEGER FL1,FL2
      COMMON/EVENT/WGT,Q2REN,Q2FAC,X1,X2,FL1,FL2
      INTEGER I
C     
      IF(IERROR.NE.0) RETURN
C
C--- LOOP OVER PARTICLES
      DO I=1,NHEP
c--- ADD FINAL STATE PARTICLES
         IF(ISTHEP(I).EQ.1)THEN
            CALL HWAADD(I)
c--- FIND INTIAL PARTON FLAVOURS
         ELSE IF(ISTHEP(I).EQ.121)THEN
            FL1=IDHEP(I)
         ELSE IF(ISTHEP(I).EQ.122)THEN
            FL2=IDHEP(I)
         ENDIF
      ENDDO
c
C--- EXTRACT EVENT INFORMATION (WEIGHT, SCALES, MOMENTUM FRACTIONS)
      WGT=EVWGT
      Q2REN=SCALUP*SCALUP
      Q2FAC=UQ2
      X1=UX1
      X2=UX2
C
      CALL FILLROOT()
C
      END


C----------------------------------------------------------------------
      SUBROUTINE HWAADD(I)
C     ADDS A PARTICLE 4-MOMENTUM AND PDG CODE TO C++ CLASS
C----------------------------------------------------------------------
      INCLUDE 'HERWIG65.INC'
      INTEGER I
      DOUBLE PRECISION PX,PY,PZ,E
      INTEGER ID
      COMMON/PARTICLE/PX,PY,PZ,E,ID
C
      ID=IDHEP(I)
      PX=PHEP(1,I)
      PY=PHEP(2,I)
      PZ=PHEP(3,I)
      E=PHEP(4,I)
C
      CALL ADDPARTICLE()
C
      END
