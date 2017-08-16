txt <- scan("data/nyt.txt","")
wl <- split(seq(txt),txt)
wl[order(sapply(wl,length))]
