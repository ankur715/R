# Non-Life Insurance Pricing with GLM

################
### Example 1.2
# We grab the data for Table 1.2 from the book's web site and store it as an R object with lots of good meta information.
con <- url("http://www2.math.su.se/~esbj/GLMbook/moppe.sas")
con
data <- readLines(con, n = 200L, warn = FALSE, encoding = "unknown")
close(con)
## Find the data range
data
data.start <- grep("^cards;", data) + 1L
data.end   <- grep("^;", data[data.start:999L]) + data.start - 2L
table.1.2  <- read.table(text = data[data.start:data.end],
                         header = FALSE, sep = "", quote = "",
                         col.names = c("premiekl", "moptva", "zon", "dur",
                                       "medskad", "antskad", "riskpre", "helpre", "cell"),
                         na.strings = NULL,
                         colClasses = c(rep("factor", 3), "numeric",
                                        rep("integer", 4), "NULL"), comment.char = "")
rm(con, data, data.start, data.end)     # Cleanup
comment(table.1.2) <-
  c("Title: Partial casco moped insurance from Wasa insurance, 1994--1999",
    "Source: http://www2.math.su.se/~esbj/GLMbook/moppe.sas",
    "Copyright: http://www2.math.su.se/~esbj/GLMbook/")
comment(table.1.2)
table.1.2
################
### Example 1.3
# Here we are concerned with replicating Table 1.4. We do it slowly, step-by-step, for pedagogical reasons.
if (!exists("table.1.2"))
  load("table.1.2.RData")
## We calculate each of the columns individually and slowly here
## to show each step
## First we have simply the labels of the table
rating.factor <-
  with(table.1.2,
       c(rep("Vehicle class", nlevels(premiekl)),
         rep("Vehicle age", nlevels(moptva)),
         rep("Zone", nlevels(zon))))
rating.factor
## The Class column
class.num <- with(table.1.2, c(levels(premiekl), levels(moptva), levels(zon)))
class.num
## The Duration is the sum of durations within each class
duration.total <-
  c(with(table.1.2, tapply(dur, premiekl, sum)),
    with(table.1.2, tapply(dur, moptva, sum)),
    with(table.1.2, tapply(dur, zon, sum)))
duration.total
## Calculate relativities in the tariff
## The denominator of the fraction is the class with the highest exposure
## (i.e. the maximum total duration): we make that explicit with the
## which.max() construct.  We also set the contrasts to use this as the base,
## which will be useful for the glm() model later.
class.base <- which.max(duration.total[1:2])
class.base
age.base  <- which.max(duration.total[3:4])
age.base
zone.base <- which.max(duration.total[5:11])
zone.base
rt.class <- with(table.1.2, tapply(helpre, premiekl, sum))
rt.class <- rt.class / rt.class[class.base]
rt.age   <- with(table.1.2, tapply(helpre, moptva, sum))
rt.age   <- rt.age / rt.age[age.base]
rt.zone  <- with(table.1.2, tapply(helpre, zon, sum))
rt.zone  <- rt.zone / rt.zone[zone.base]

contrasts(table.1.2$premiekl) <-
  contr.treatment(nlevels(table.1.2$premiekl))[rank(-duration.total[1:2],
                                                    ties.method = "first"), ]
contrasts(table.1.2$moptva) <-
  contr.treatment(nlevels(table.1.2$moptva))[rank(-duration.total[3:4],
                                                  ties.method = "first"), ]
contrasts(table.1.2$zon) <-
  contr.treatment(nlevels(table.1.2$zon))[rank(-duration.total[5:11],
                                               ties.method = "first"), ]
###
# An alternative approach using direct optimization is outlined in Exercise 1.3 below.
## Relativities of MMT; we use the glm approach here as per the book's
## SAS code at http://www2.math.su.se/~esbj/GLMbook/moppe.sas
m <- glm(riskpre ~ premiekl + moptva + zon, data = table.1.2,
         family = poisson("log"), weights = dur)
## If the next line is a mystery then you need to
## (1) read up on contrasts or
## (2) remember that the link function is log() which is why we use exp here
rels <- exp( coef(m)[1] + coef(m)[-1] ) / exp(coef(m)[1])
rels
rm.class <- c(1, rels[1])               # See rm.zone below for the
rm.age   <- c(rels[2], 1)               # general approach
rm.zone  <- c(1, rels[3:8])[rank(-duration.total[5:11], ties.method = "first")]
## Create and save the data frame
table.1.4 <-
  data.frame(Rating.factor = rating.factor, Class = class.num,
             Duration = duration.total,
             Rel.tariff = c(rt.class, rt.age, rt.zone),
             Rel.MMT    = c(rm.class, rm.age, rm.zone))
table.1.4
save(table.1.4, file = "table.1.4.RData")
setwd('C:/Users/ankur/Documents/Data Science/Projects/GNY/etc/Non-Life Insurance Pricing')
write.csv(table.1.4, "table.1.4.csv")
print(table.1.4, digits = 3)
rm(rating.factor, class.num, duration.total, class.base, age.base, zone.base,
   rt.class, rt.age, rt.zone, rm.class, rm.age, rm.zone, m, rels)
################
################
## Exercise 1.3
## The values from the book
g0  <- 0.03305
g12 <- 2.01231
g22 <- 0.74288
dim.names <- list(Milage = c("Low", "High"),
                  Age = c("New", "Old"))
pyears <- matrix(c(47039, 56455, 190513, 28612), nrow = 2,
                 dimnames = dim.names)
claims <- matrix(c(0.033, 0.067, 0.025, 0.049), nrow = 2,
                 dimnames = dim.names)

GvalsError <- function (gvals) {
  ## The current estimates
  g0  <- gvals[1]
  g12 <- gvals[2]
  g22 <- gvals[3]
  ## The current estimates in convenient matrix form
  G  <- matrix(c(1, 1, g12, g22), nrow = 2)
  G1 <- matrix(c(1, g12), nrow = 2, ncol = 2)
  G2 <- matrix(c(1, g22), nrow = 2, ncol = 2, byrow = TRUE)
  ## The calculated values
  G0  <- addmargins(claims * pyears)["Sum", "Sum"] / ( sum(pyears * G1 * G2) )
  G12 <- addmargins(claims * pyears)["High", "Sum"] /
    ( g0 * addmargins(pyears * G2)["High", "Sum"] )
  G22 <- addmargins(claims * pyears)["Sum", "Old"] /
    ( g0 * addmargins(pyears * G1)["Sum", "Old"] )
  ## The sum of squared errors
  error <- (g0 - G0)^2 + (g12 - G12)^2 + (g22 - G22)^2
  return(error)
}
## Minimize the error function to obtain our estimate
gamma <- optim(c(g0, g12, g22), GvalsError)
gamma
stopifnot(gamma$convergence == 0)
gamma <- gamma$par
gamma
values <- data.frame(legend = c("Our calculation", "Book value"),
                     g0  = c(gamma[1], g0),
                     g12 = c(gamma[2], g12),
                     g22 = c(gamma[3], g22),
                     row.names = "legend")
print(values, digits = 4)
## Close, but not the same.
rm(g0, g12, g22, dim.names, pyears, claims, gamma, values)
################

