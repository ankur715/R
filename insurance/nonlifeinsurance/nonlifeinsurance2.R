install.packages(c("data.table", "foreach", "ggplot2"), dependencies = TRUE)
# The goal is to reproduce Table 2.7 so we start building that as a data frame after loading the data.
## Load the data from last.
if (!exists("table.1.2"))
  load("table.1.2.RData")
library("foreach")
## We are looking to reproduce table 2.7 which we start building here,
## adding columns as we go.
table.2.7 <-
  data.frame(rating.factor =
               c(rep("Vehicle class", nlevels(table.1.2$premiekl)),
                 rep("Vehicle age",   nlevels(table.1.2$moptva)),
                 rep("Zone",          nlevels(table.1.2$zon))),
             class =
               c(levels(table.1.2$premiekl),
                 levels(table.1.2$moptva),
                 levels(table.1.2$zon)),
             stringsAsFactors = FALSE) 
table.2.7
## Calculate duration per rating factor level and also set the
## contrasts (using the same idiom as in the code for the previous
## chapter). We use foreach here to execute the loop both for its
## side-effect (setting the contrasts) and to accumulate the sums.
new.cols <-
  foreach (rating.factor = c("premiekl", "moptva", "zon"),
           .combine = rbind) %do%
  {
    nclaims <- tapply(table.1.2$antskad, table.1.2[[rating.factor]], sum)
    sums <- tapply(table.1.2$dur, table.1.2[[rating.factor]], sum)
    n.levels <- nlevels(table.1.2[[rating.factor]])
    contrasts(table.1.2[[rating.factor]]) <-
      contr.treatment(n.levels)[rank(-sums, ties.method = "first"), ]
    data.frame(duration = sums, n.claims = nclaims)
  }
table.2.7 <- cbind(table.2.7, new.cols)
rm(new.cols)
table.2.7
### frequency model
model.frequency <-
  glm(antskad ~ premiekl + moptva + zon + offset(log(dur)),
      data = table.1.2, family = poisson)
model.frequency
rels <- coef( model.frequency )
rels <- exp( rels[1] + rels[-1] ) / exp( rels[1] )
table.2.7$rels.frequency <-
  c(c(1, rels[1])[rank(-table.2.7$duration[1:2], ties.method = "first")],
    c(1, rels[2])[rank(-table.2.7$duration[3:4], ties.method = "first")],
    c(1, rels[3:8])[rank(-table.2.7$duration[5:11], ties.method = "first")])
table.2.7
### Severity model
model.severity <-
  glm(medskad ~ premiekl + moptva + zon,
      data = table.1.2[table.1.2$medskad > 0, ],
      family = Gamma("log"), weights = antskad)
rels <- coef( model.severity )
rels
rels <- exp( rels[1] + rels[-1] ) / exp( rels[1] )
rels
## Aside: For the canonical link function use
## rels <- rels[1] / (rels[1] + rels[-1])
table.2.7$rels.severity <-
  c(c(1, rels[1])[rank(-table.2.7$duration[1:2], ties.method = "first")],
    c(1, rels[2])[rank(-table.2.7$duration[3:4], ties.method = "first")],
    c(1, rels[3:8])[rank(-table.2.7$duration[5:11], ties.method = "first")])
### Now it is trivial to combine and display the results.
table.2.7$rels.pure.premium <- with(table.2.7, rels.frequency * rels.severity)
print(table.2.7, digits = 2)
###### Case Study: Motorcycle Insurance
columns <- c(agarald = 2L, kon = 1L, zon = 1L, mcklass = 1L, fordald = 2L,
             bonuskl = 1L, duration = 8L, antskad = 4L, skadkost = 8L)
columns.classes <- c("integer", rep("factor", 3), "integer",
                    "factor", "numeric", rep("integer", 2))
columns
columns.classes
stopifnot(length(columns) == length(columns.classes))
con <- url("http://www2.math.su.se/~esbj/GLMbook/mccase.txt")
con
mccase <- read.fwf(con, widths = columns, header = FALSE,
                   col.names = names(columns),
                   colClasses = columns.classes,
                   na.strings = NULL, comment.char = "")
try(close(con), silent = TRUE)
rm(columns, column.classes, con)
mccase$mcklass <- ordered(mccase$mcklass)
mccase$bonuskl <- ordered(mccase$bonuskl)
### Adding meta-data information
comment(mccase) <-
  c("Title: Partial casco insurance for motorcycles from Wasa, 1994--1998",
    "Source: http://www2.math.su.se/~esbj/GLMbook/mccase.txt",
    "Copyright: http://www2.math.su.se/~esbj/GLMbook/")
comment(mccase$agarald) <-
  c("The owner's age, between 0 and 99",
    "Name: Age of Owner")
comment(mccase$kon) <-
  c("Name: Gender of Owner",
    "Code: M=Male",
    "Code: K=Female")
comment(mccase$zon) <-
  c("Name: Geographic Zone",
    "Code: 1=Central and semi-central parts of Sweden's three largest cities",
    "Code: 2=suburbs and middle-sized towns",
    "Code: 3=Lesser towns, except those in 5 or 7",
    "Code: 4=Small towns and countryside, except 5--7",
    "Code: 5=Northern towns",
    "Code: 6=Northern countryside",
    "Code: 7=Gotland (Sweden's largest island)")
comment(mccase$mcklass) <-
  c("Name: MC Class",
    "Description: A classification by the EV ratio, defined as (engine power in kW × 100) / (vehicle weight in kg + 75), rounded to the nearest lower integer.",
    "Code: 1=EV ratio <= 5",
    "Code: 2=EV ratio 6--8",
    "Code: 3=EV ratio 9--12",
    "Code: 4=EV ratio 13--15",
    "Code: 5=EV ratio 16--19",
    "Code: 6=EV ratio 20--24",
    "Code: 7=EV ratio >= 25")
comment(mccase$fordald) <-
  c("Vehicle age, between 0 and 99",
    "Name: Vehicle Age")
comment(mccase$bonuskl) <-
  c("Name: Bonus Class",
    "Description: A driver starts with bonus class 1; for each claim-free year the bonus class is increased by 1. After the first claim the bonus is decreased by 2; the driver cannot return to class 7 with less than 6 consecutive claim free years.")
comment(mccase$duration) <-
  c("Name: Duration",
    "Comment: The number of policy years",
    "Unit: year")
comment(mccase$antskad) <-
  c("Name: Number of Claims")
comment(mccase$skadkost) <-
  c("Name: Cost of Claims",
    "Unit: SEK")
### Rating factors
mccase$rating.1 <- mccase$zon
mccase$rating.2 <- mccase$mcklass
mccase$rating.3 <-
  cut(mccase$fordald, breaks = c(0, 1, 4, 99),
      labels = as.character(1:3), include.lowest = TRUE,
      ordered_result = TRUE)
mccase$rating.4 <- ordered(mccase$bonuskl) # Drop comments
levels(mccase$rating.4) <-                 # Combine levels
  c("1", "1", "2", "2", rep("3", 3))
####### save the data
save(mccase, file = "mccase.RData")
#### Problem 1: Aggregate to cells of current tariff
### Compute the empirical claim frequency and severity at this level
if (!exists("mccase"))
  load("mccase.RData")
## Conver to data.table
library("data.table")
mccase <- data.table(mccase, key = paste("rating", 1:4, sep = "."))

## Aggregate to levels of current rating factors
mccase.current <-
  mccase[,
         list(duration = sum(duration),
              antskad = sum(antskad),
              skadkost = sum(skadkost),
              num.policies = .N),
         by = key(mccase)]             
## Claim frequency and severity. Change NaN to NA.
mccase.current$claim.freq <-
  with(mccase.current, ifelse(duration != 0, antskad / duration, NA_real_))
mccase.current$severity <-
  with(mccase.current, ifelse(antskad != 0, skadkost / antskad, NA_real_))
## Save
save(mccase.current, file = "mccase.current.RData")
#### Problem 2: Determine how the duration and number of claims is distributed
## Load data if needed
library("data.table")
if (!exists("mccase"))
  load("mccase.RData")
if (!exists("mccase.current"))
  load("mccase.current.RData")
if (!is(mccase, "data.table"))
  mccase <- data.table(mccase, key = paste("rating", 1:4, sep = "."))
library("grid")
library("ggplot2")
### 1. Number of claims (antskad)
plot.titles <- c("Geo. zone", "MC class", "Vehicle age", "Bonus class")
plots <-
  lapply(1:4,
         function(i)
           ggplot(mccase, aes(antskad))
         + geom_histogram(aes(weight = duration))
         + scale_x_discrete(limits = c(0, 2))
         + scale_y_log10()
         + facet_grid(paste("rating.", i, " ~ .", sep = ""),
                      scales = "fixed")
         ## We drop the axis titles to make more room for the data
         + opts(axis.title.x = theme_blank(), axis.title.y = theme_blank(),
                axis.text.x = theme_blank(),  axis.text.y = theme_blank(),
                axis.title.y = theme_blank(), axis.ticks = theme_blank(),
                title = plot.titles[i])
  )

grid.newpage()
pushViewport(viewport(layout = grid.layout(nrow = 1, ncol = 4)))
## We can ignore the warnings from displaying the plots for now
for (i in 1:4)
  print(plots[[i]], vp = viewport(layout.pos.col = i))
data <- mccase[order(antskad), list(N = .N, w = sum(duration)), by = antskad]
M <- glm(N ~ antskad, family = poisson(), weights = w, data = data)
data$predicted <- round(predict(M, data[, list(antskad)], type = "response"))
print(data[, list(antskad, N, predicted)], digits = 1)
high.limit <- 7L                 # Only display up to this many claims
plots <-
  lapply(1:4,
         function(i)
           ggplot(mccase.current, aes(antskad))
         + geom_histogram(breaks = 0:high.limit, aes(weight = duration))
         + scale_x_discrete(limits = c(0, high.limit), breaks = 0:high.limit)
         ## Following xlim() needed for scale_x_discrete only; see
         ## https://groups.google.com/d/msg/ggplot2/wLWGCUz8K6k/DeVudyfXyKgJ
         + xlim(0, high.limit)
         + scale_y_log10()
         + facet_grid(paste("rating.", i, " ~ .", sep = ""),
                      scales = "fixed")
         ## We drop the axis titles to make more room for the data
         + opts(axis.title.x = theme_blank(), axis.title.y = theme_blank(),
                axis.text.x = theme_blank(),  axis.text.y = theme_blank(),
                axis.title.y = theme_blank(), axis.ticks = theme_blank(),
                title = plot.titles[i])
  )

grid.newpage()
pushViewport(viewport(layout = grid.layout(nrow = 1, ncol = 4)))
for (i in 1:4)
  print(plots[[i]], vp = viewport(layout.pos.col = i))
ggplot(mccase.current, aes(antskad)) +
  geom_histogram(breaks = 0:high.limit, aes(weight = duration)) +
  scale_x_discrete(limits = c(0, high.limit), breaks = 0:high.limit) +
  xlim(0, high.limit) +
  scale_y_log10() +
  facet_grid(rating.4 ~ rating.3, scales = "fixed", labeller = "label_both")
## rating.3 is the vehicle age.
data <-
  mccase.current[order(rating.3, antskad),
                 list(N = .N, w = sum(duration)),
                 by = list(rating.3, antskad)]
data$predicted <-
  unlist(lapply(levels(data$rating.3),
                function (l) round(predictionPoisson(data[rating.3 == l]))))
## Show the fit
print(data[antskad <= high.limit,
           list(rating.3, antskad, N, predicted)], digits = 1)
## Show simplistic residuals per level of the rating factor
print(data[antskad <= high.limit,
           list(res = sum(abs(predicted - N))),
           by = rating.3])

## rating.4 is the bonus class
data <-
  mccase.current[order(rating.4, antskad),
                 list(N = .N, w = sum(duration)),
                 by = list(rating.4, antskad)]
data$predicted <-
  unlist(lapply(levels(data$rating.4),
                function (l) round(predictionPoisson(data[rating.4 == l]))))
## Show the fit
print(data[antskad <= high.limit,
           list(rating.4, antskad, N, predicted)], digits = 1)
## Show simplistic residuals per rating factor
print(data[antskad <= high.limit,
           list(res = sum(abs(predicted - N))),
           by = rating.4])
ggplot(data, aes(x = antskad, y = N)) +
  geom_bar(breaks = 0L:high.limit, stat = "identity") +
  facet_wrap( ~ rating.4) +
  geom_line(aes(y = predicted), data = data, colour = "red", size = 1) +
  xlim(-0.5, high.limit + 0.5)
plots <-
  lapply(1:4,
         function(i)
           ggplot(mccase.current, aes(duration))
         + geom_histogram(binwidth = 40)
         + xlim(0, 440)
         + scale_y_log10()
         + facet_grid(paste("rating.", i, " ~ .", sep = ""),
                      scales = "fixed")
         ## We drop the axis titles to make more room for the data
         + opts(axis.title.x = theme_blank(), axis.title.y = theme_blank(),
                axis.text.x = theme_blank(),  axis.text.y = theme_blank(),
                axis.title.y = theme_blank(), axis.ticks = theme_blank(),
                title = plot.titles[i])
  )

grid.newpage()
pushViewport(viewport(layout = grid.layout(nrow = 1, ncol = 4)))
## We can ignore the warnings from displaying the plots for now
for (i in 1:4)
  print(plots[[i]], vp = viewport(layout.pos.col = i))
ggplot(mccase, aes(duration)) +
  geom_histogram(binwidth = 0.05) +
  xlim(0, 3) +
  opts(title = "Duration of motorcycle policies") +
  annotate("text", 3.0, 1e4, label = "Histogram bin width = 0.05",
           size = 3, hjust = 1, vjust = 1)
library("data.table")
if (!exists("mccase.current"))
  load("mccase.current.RData")

case.2.4 <-
  data.frame(rating.factor =
               c(rep("Zone",        nlevels(mccase.current$rating.1)),
                 rep("MC class",    nlevels(mccase.current$rating.2)),
                 rep("Vehicle age", nlevels(mccase.current$rating.3)),
                 rep("Bonus class", nlevels(mccase.current$rating.4))),
             class =
               with(mccase.current,
                    c(levels(rating.1), levels(rating.2),
                      levels(rating.3), levels(rating.4))),
             ## These are the values from Table 2.8 in the book:
             relativity =
               c(7.678, 4.227, 1.336, 1.000, 1.734, 1.402, 1.402,
                 0.625, 0.769, 1.000, 1.406, 1.875, 4.062, 6.873,
                 2.000, 1.200, 1.000,
                 1.250, 1.125, 1.000),
             stringsAsFactors = FALSE)
print(case.2.4, digits = 3)
library("foreach")
new.cols <-
  foreach (rating.factor = paste("rating", 1:4, sep = "."),
           .combine = rbind) %do%
  {
    totals <- mccase.current[, list(D = sum(duration),
                                    N = sum(antskad),
                                    C = sum(skadkost)),
                             by = rating.factor]
    n.levels <- nlevels(mccase.current[[rating.factor]])
    contrasts(mccase.current[[rating.factor]]) <-
      contr.treatment(n.levels)[rank(-totals[["D"]], ties.method = "first"), ]
    data.frame(duration = totals[["D"]],
               n.claims = totals[["N"]],
               skadkost = totals[["C"]])
  }
case.2.4 <- cbind(case.2.4, new.cols)
rm(new.cols)
## Model the frequency

model.frequency <-
  glm(antskad ~
        rating.1 + rating.2 + rating.3 + rating.4 + offset(log(duration)),
      data = mccase.current[duration > 0], family = poisson)
## Res. dev. 360 on 389 dof

rels <- coef( model.frequency )
rels <- exp( rels[1] + rels[-1] ) / exp( rels[1] )
case.2.4$rels.frequency <-
  c(c(1, rels[1:6])[rank(-case.2.4$duration[1:7], ties.method = "first")],
    c(1, rels[7:12])[rank(-case.2.4$duration[8:14], ties.method = "first")],
    c(1, rels[13:14])[rank(-case.2.4$duration[15:17], ties.method = "first")],
    c(1, rels[15:16])[rank(-case.2.4$duration[18:20], ties.method = "first")])

## Model the severity. We stick with the non-canonical link function
## for the time being.

model.severity <-
  glm(skadkost ~ rating.1 + rating.2 + rating.3 + rating.4,
      data = mccase.current[skadkost > 0,],
      family = Gamma("log"), weights = antskad)
## Res.dev. 516 on 164 dof

rels <- coef( model.severity )
rels <- exp( rels[1] + rels[-1] ) / exp( rels[1] )
case.2.4$rels.severity <-
  c(c(1, rels[1:6])[rank(-case.2.4$duration[1:7], ties.method = "first")],
    c(1, rels[7:12])[rank(-case.2.4$duration[8:14], ties.method = "first")],
    c(1, rels[13:14])[rank(-case.2.4$duration[15:17], ties.method = "first")],
    c(1, rels[15:16])[rank(-case.2.4$duration[18:20], ties.method = "first")])

## Combine the frequency and severity
case.2.4$rels.pure.prem <- with(case.2.4, rels.frequency * rels.severity)
## Convert to data.table
library("data.table")
case.2.4 <- data.table(case.2.4)

## Save
save(case.2.4, file = "case.2.4.RData")

## Compare with current values
print(case.2.4[,
               list(rating.factor, class, duration, n.claims,
                    skadkostK = round(skadkost / 1e3),
                    relativity, rels.pure.prem)],
      digits = 3)
#### Problem 4: discussions
with(case.2.4, max(rels.pure.prem) / min(rels.pure.prem)) # 3552
with(case.2.4, max(relativity) / min(relativity))    