c_p <- 4185.5
deltaT <- -4.02
r <- sqrt(18500/pi)
m <- function (p) pi*p*r^2
C <- function (p) m(p)*c_p
Q <- function (p) deltaT*C(p)
A <- function (p) 2*pi*r*p + pi*r^2
(A(7)/(pi*r^2))/(A(2)/(pi*r^2))
sprintf("r = %.4g", r)
sprintf("m1 = %.4g", m(2))
sprintf("m2 = %.4g", m(7))
sprintf("C1 = %.4g", C(2))
sprintf("C2 = %.4g", C(7))
sprintf("Q1 = %.4g", Q(2))
sprintf("Q2 = %.4g", Q(7))
sprintf("A1 = %.4g", A(2))
sprintf("A2 = %.4g", A(7))
