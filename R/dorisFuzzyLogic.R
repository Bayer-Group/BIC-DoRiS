lip <- function(x,x1,y1,x2,y2){
###
### linear interpolation
###
    if (x1 < x2)
        (y1*(x2-x)+y2*(x-x1))/(x2-x1)
    else
        NA
}



gt <- function(x,d){
###
### gt (greater than zero) calculates the truth value of each element in
### vector x, based on the linear truth function defined by skalar d.
###
###   1 +           /-----
###     |          /
###     |         /
###     |        /
###     |       /
###     |      /
###   0 +-----/.....+.....
###           0     d
###
    if (d > 0)
        ifelse(x <= 0, 0,
        ifelse(x <  d, lip(x,0,0,d,1),
                       1))
    else
        NA
}



lt <- function(x,d){
###
### lt (less than zero) calculates the truth value of each element in vector x,
### based on the linear truth function defined by skalar d.
###
###   1 +-----\
###     |      \
###     |       \
###     |        \
###     |         \
###     |          \
###   0 +...........\-----
###           d     0
###
    if (d < 0)
        ifelse(x <= d, 1,
        ifelse(x <  0, lip(x,d,1,0,0),
                       0))
    else
        NA
}



eq <- function(x,dn,dp){
###
### eq (equal to zero) calculates the truth value of each element in vector x,
### based on the piecewise linear truth function defined by skalars dn and dp.
###
###   1 +           /\
###     |          /  \
###     |         /    \
###     |        /      \
###     |       /        \
###     |      /          \
###   0 +-----/......+.....\-----
###          dn      0      dp
###
    if (dn < 0 & dp > 0)
        ifelse(x <=  dn, 0,
        ifelse(x <    0, lip(x,dn,0,0,1),
        ifelse(x ==   0, 1,
        ifelse(x <   dp, lip(x,0,1,dp,0),
                         0))))
    else
        NA
}



truthValues <- function(x,p,d){
###
### x is vector with values to be compared to zero
### p is pattern of "<", "=", or ">"
### d is a vector with fuzzy logic parameters delta,
### corresponding with the dose levels/pattern values
###

    n=length(x)
    ##
    p <- unlist(strsplit(p, split=""))

    if (n == length(p) &
        n == length(d) & all(d > 0)){
        unlist(
            mapply(
                function(x,p,d) {
                    ifelse(p == "<", lt(x,-d),
                    ifelse(p == "=", eq(x,-d,d),
                    ifelse(p == ">", gt(x,d),
                           NA)))
                },
                x, p, d
            )
        )
    }
    else
        rep(NA,n)
}


