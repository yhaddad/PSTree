C  Event common block contains:
C  - event weight
C  - renormalisation scale
C  - factorisation sclae
C  - Bjorken x of incoming partons
C  - flavours of incoming partons
C 
      DOUBLE PRECISION WGT,Q2REN,Q2FAC,X1,X2
      INTEGER FL1,FL2
      COMMON/EVENT/WGT,Q2REN,Q2FAC,X1,X2,FL1,FL2

C  Particle common block contains:
C  - four-momentum
C  - PDG code
C
      DOUBLE PRECISION PX,PY,PZ,E
      INTEGER ID
      COMMON/PARTICLE/PX,PY,PZ,E,ID

