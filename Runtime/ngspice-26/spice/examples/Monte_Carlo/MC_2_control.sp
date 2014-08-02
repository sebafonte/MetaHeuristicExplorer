* Perform Monte Carlo simulation in ngspice
* script for use with 25 stage Ring-Osc. BSIM3
* circuit is in MC_2_circ.sp
* edit 'set sourcepath' for your path to circuit file
* start script by 'ngspice -o MC_2_control.log MC_2_control.sp'
*
.control
  save buf                     $ we just need output vector buf, save memory by more than 10x
  let mc_runs = 100            $ number of runs for monte carlo
  let run = 1                  $ number of the actual run

* Where to find the circuit netlist file MC_2_circ.sp
  set sourcepath = ( D:\Spice_general\ngspice\examples\Monte_Carlo )

* create file for frequency information
  echo Monte Carlo, frequency of R.O. > MC_frequ.log

* run the simulation loop
  dowhile run <= mc_runs
    * without the reset switch there is some strange drift
    * towards lower and lower frequencies
    reset
    set run ="$&run"              $ create a variable from the vector
    set rndseed = $run            $ set the rnd seed value to the loop index
    source MC_2_circ.sp           $ load the circuit, including model data
    tran 15p 200n 0
    write mc_ring{$run}.out buf   $ write each sim output to its own rawfile
    linearize buf                 $ lienarize buf to allow fft
    fft buf                       $ run fft on vector buf
    let buf2=db(mag(buf))
    * find the frequency where buf has its maximum of the fft signal
    meas sp fft_max MAX_AT buf2 from=0.1G to=0.7G
    print fft_max >> MC_frequ.log $ print frequency to file
    destroy all                   $ delete all output vectors
    remcirc                       $ delete circuit
    let run = run + 1             $ increase loop counter
  end

  quit

.endc

.end
