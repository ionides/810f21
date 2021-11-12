# Here's a variation on the example in hw11 which suggests that passing the
# 10^8 numbers back to the master process after they are calculated is a
# significant cost. 

library(doParallel)
  registerDoParallel()

  sim1 <- function(n){
    rnorm(n)
    return()
  }

  sim2 <- function(n){
    rnorm(n)
  }

  system.time(
   sim1(10^8)
  ) -> time0

  system.time(
    foreach(i=1:10) %dopar% sim1(10^7)
  ) -> time1

  system.time(
    foreach(i=1:10^2) %dopar% sim1(10^6)
  ) -> time2

  system.time(
    foreach(i=1:10^3) %dopar% sim1(10^5)
  ) -> time3

   system.time(
    foreach(i=1:10^4) %dopar% sim1(10^4)
  ) -> time4
  
  times1 <- rbind(time0,time1,time2,time3,time4)

  system.time(
   sim2(10^8)
  ) -> time0

  system.time(
    foreach(i=1:10) %dopar% sim2(10^7)
  ) -> time1

  system.time(
    foreach(i=1:10^2) %dopar% sim2(10^6)
  ) -> time2

  system.time(
    foreach(i=1:10^3) %dopar% sim2(10^5)
  ) -> time3

   system.time(
    foreach(i=1:10^4) %dopar% sim2(10^4)
  ) -> time4
  
  times2 <- rbind(time0,time1,time2,time3,time4)

# > times1
#       user.self sys.self elapsed user.child sys.child
# time0     5.372    0.201   5.598      0.000     0.000
# time1     0.005    0.006   1.876      2.473     0.126
# time2     0.015    0.008   1.494      0.002     0.003
# time3     0.093    0.019   1.604      5.668     0.308
# time4     0.878    0.019   2.470      5.818     0.441
# > times2
#       user.self sys.self elapsed user.child sys.child
# time0     5.400    0.202   5.614      0.000     0.000
# time1     0.298    0.584   2.774      4.294     0.649
# time2     0.298    0.600   3.703      0.002     0.007
# time3     0.374    0.598   2.868      6.200     0.971
# time4     1.305    0.907   4.942      6.437     1.549
