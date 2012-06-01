C     CALLS FUNCTIONS FROM INTERFACE.CC TO PASS EVENT INFO TO
C     C++ CLASS, WHICH OUTPUTS THE ROOT NTUPLE
C
C----------------------------------------------------------------------
      SUBROUTINE PYABEG
C     CALLED BEFORE FIRST EVENT ANALYZED
C----------------------------------------------------------------------
C
      CALL INITROOT("pythia.cfg")
C
      END

C----------------------------------------------------------------------
      SUBROUTINE PYAEND
C     CALLED BY PYTHIA EVERY NSTEP EVENTS
C     UNUSED SINCE ROOT MANAGES BUFFER WELL
C----------------------------------------------------------------------
C
      END


C----------------------------------------------------------------------
      SUBROUTINE PYANAL
C     ADDS EVENT WEIGHT AND PDF INFO TO C++ CLASS
C----------------------------------------------------------------------
      INCLUDE 'hepevt.h'
      DOUBLE PRECISION WGT,X1,X2,Q2
      DOUBLE PRECISION UX1,UX2,UQ2
      INTEGER FL1,FL2
      INTEGER MINT
      DOUBLE PRECISION VINT
      COMMON/PYINT1/MINT(400),VINT(400)
      COMMON/EVENT/WGT,X1,X2,Q2,FL1,FL2
      INTEGER I
C     
      IF(MINT(51).NE.0) RETURN
C
C--- LOOP OVER PARTICLES
      DO I=1,NHEP
c--- ADD FINAL STATE PARTICLES
         IF(ISTHEP(I).EQ.1)THEN
            CALL PYAADD(I)
c--- FIND INTIAL PARTON FLAVOURS
         ELSE IF(ISTHEP(I).EQ.121)THEN
            FL1=IDHEP(I)
         ELSE IF(ISTHEP(I).EQ.122)THEN
            FL2=IDHEP(I)
         ENDIF
      ENDDO
c
      WGT=VINT(97)
      X1=VINT(41)
      X2=VINT(42)
      Q2=VINT(53)
      CALL FILLROOT()
C
      END


C----------------------------------------------------------------------
      SUBROUTINE PYAADD(I)
C     ADDS A PARTICLE 4-MOMENTUM AND PDG CODE TO C++ CLASS
C----------------------------------------------------------------------
      INCLUDE 'hepevt.h'
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
      CALL ADDPARTICLE()
C
      END
