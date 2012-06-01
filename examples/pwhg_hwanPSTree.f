C     CALLS FUNCTIONS FROM INTERFACE.CC TO PASS EVENT INFO TO
C     C++ CLASS, WHICH OUTPUTS THE ROOT NTUPLE
C
C----------------------------------------------------------------------
      SUBROUTINE HWABEG
C     CALLED BEFORE FIRST EVENT ANALYZED
C----------------------------------------------------------------------
C
      CALL INITROOT("herwig.cfg")
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
      DOUBLE PRECISION WGT,X1,X2,Q2
      DOUBLE PRECISION UX1,UX2,UQ2
      INTEGER FL1,FL2
      COMMON/EVENT/WGT,X1,X2,Q2,FL1,FL2
      COMMON/CPDFRWGT/UX1,UX2,UQ2
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
      WGT=EVWGT
      X1=UX1
      X2=UX2
      Q2=UQ2
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
      CALL ADDPARTICLE()
C
      END
