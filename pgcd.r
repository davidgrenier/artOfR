pgcd <- function (p, q) {
    gcd <- function (a, b, c, d, n, m) {
        q <- n %/% m
        r <- n - q*m
        if (r == 0) c(m, c, d)
        else gcd(c, d, a-q*c, b-q*d, m, r)
    }
    gcd(0, 1, 1, 0, p, q)
}
pgcd(831, 40)
