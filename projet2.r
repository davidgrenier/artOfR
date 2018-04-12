c_p <- 4185.5
r <- sqrt(18500/pi)
C <- function (p) p*(r^2)*c_p
sprintf("C1 = %.4g", C(2))
sprintf("C2 = %.4g", C(7))
sprintf("Q1 = %.4g", -4.02*C(2))
sprintf("Q2 = %.4g", -4.02*C(7))
