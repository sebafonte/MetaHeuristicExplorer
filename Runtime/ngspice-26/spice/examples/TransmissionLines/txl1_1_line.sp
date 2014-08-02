MOSdriver -- lossy line TXL model -- C load 
m5     0     168    2     0  mn0p9  w = 18.0u l=0.9u
m6     1     168    2     1  mp1p0  w = 36.0u l=1.0u
CN2  2   0  0.025398e-12
CN3  3   0  0.007398e-12
y1  2 0 3 0 ymod
vdd    1    0   dc 	5.0
VS 168  0  PULSE (0 5 15.9NS 0.2NS 0.2NS 15.8NS 32NS )
.control
TRAN 0.2N 47N 0 0.1N
plot v(2) v(3) ylimit -0.5 5
.endc
.MODEL mn0p9 NMOS VTO=0.8 KP=48U GAMMA=0.30 PHI=0.55
+LAMBDA=0.00 CGSO=0 CGDO=0 CJ=0 CJSW=0 TOX=18000N LD=0.0U
.MODEL mp1p0 PMOS VTO=-0.8 KP=21U GAMMA=0.45 PHI=0.61
+LAMBDA=0.00 CGSO=0 CGDO=0 CJ=0 CJSW=0 TOX=18000N LD=0.0U
.MODEL ymod txl R=12.45 L=8.972e-9 G=0 C=0.468e-12 length=16
.end
