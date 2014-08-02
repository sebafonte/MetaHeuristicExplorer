Code Model Test - Transient: gain, summer, mult, divide, pwl
*
*
*** analysis type ***
.control
tran .1s 10s
plot v(1) v(10) v(20) v(30) v(40) v(50)
.endc
*
*** input sources ***
*
v1 1 0 DC PWL(0 0 10 10) 
*
v2 2 0 DC 2
*
*** gain block ***
a1 1 10 gain1 
.model gain1 gain (in_offset=0.0 gain=2.0 out_offset=0.0)
*
*
*** summer block ***
a2 [1 2] 20 summer1
.model summer1 summer (in_offset=[0.0 0.0] in_gain=[1.0 1.0] 
+                      out_gain=1.0  out_offset=0.0)
*
*
*** mult block ***
a3 [1 1] 30 mult1
.model mult1 mult (in_offset=[0.0 0.0] in_gain=[1.0 1.0] 
+                      out_gain=0.1  out_offset=0.0)
*
*
*** divider block ***
a4 2 1 40 divide1
.model divide1 divide (num_offset=0.0 num_gain=1.0 den_offset=0.0 den_gain=1.0 
+                      den_lower_limit=0.1 den_domain=1.0e-16 
+                      fraction=false out_gain=1.0 out_offset=0.0)
*
*
*** pwl block ***
a5 1 50 pwl1
.model pwl1 pwl (x_array=[-1.0 0.0 1.0 2.0 3.0 4.0 5.0]
+                y_array=[ 0.0 0.0 1.0 4.0 4.5 5.0 5.0]
+                input_domain=0.01 fraction=TRUE)
*
*
*** resistors to ground ***
r1 1 0 1k
r2 2 0 1k
r3 3 0 1k
*
r10 10 0 1k
r20 20 0 1k
r30 30 0 1k
r40 40 0 1k
r50 50 0 1k
*
*
.end






