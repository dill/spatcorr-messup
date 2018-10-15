# analyse the stomach cancer data

# load the data so we don't have to run that weird file
load("dat.RData")

# construct data.frame
dat <- data.frame(observed = O,
                  expected = E,
                  socioc   = SEc,
                  mun      = as.factor(1:194),
                  x        = X1,
                  y        = X2)

# get the neighbourhood structure in the format that mrf wants it
source("omgpolys.R")
names(nb) <- 1:194


library(mgcv)

# fit the spatial model as in the paper
b <- gam(observed~offset(log(expected)) + socioc +
                  s(mun, bs="mrf", xt=list(nb=nb), k=192),
         data=dat, method="REML", family=poisson)

print(summary(b))

# git the model without spatial term
b_sec <- gam(observed~offset(log(expected)) + socioc,
         data=dat, method="REML", family=poisson)

print(summary(b_sec))

# ^^^^
# as in the paper, without spatial term, we have b_socioc = -0.13683
#  when we include spatial term, b_socioc = -0.01997


# now try a 2d spline
b_xy <- gam(observed~offset(log(expected)) + socioc +
                     s(x, y),
            data=dat, method="REML", family=poisson)

print(summary(b_xy))

# ^^^^
# as in paper, we have the same issue with the socioc effect!


# what if we smooth socioc?
# with space
b_sms <- gam(observed~offset(log(expected)) + s(socioc, k=5) +
                  s(mun, bs="mrf", xt=list(nb=nb), k=60),
         data=dat, method="REML", family=poisson)

print(summary(b_sms))


# no space
b_sm <- gam(observed~offset(log(expected)) + s(socioc, k=5),
         data=dat, method="REML", family=poisson)

print(summary(b_sm))


par(mfrow=c(1,2))

plot(b_sm, ylim=c(-0.3, 0.3), main="no spatial term")
plot(b_sms, ylim=c(-0.3, 0.3), main="with spatial term")




