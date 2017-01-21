# Chapter 4 - EDA

library(aplpack)
library(RSADBE)
library(lattice)
library(LearnEDA)

data("TheWALL")
str(TheWALL)


# Summary Statistics

sort(TheWALL$Score)
quantile((TheWALL$Score))
diff(quantile((TheWALL$Score)))

# precentiles
quantile(TheWALL$Score, seq(0,1,.01))

# Home and Away games
table(TheWALL$HA_Ind)
boxplot(Score~HA_Ind, data = TheWALL)

boxplot(Score~HA_Ind, subset = (Score < 200), data = TheWALL)

# Five Numbers - Hinges and quantiles differ
fivenum(TheWALL$Score)
quantile((TheWALL$Score))

# range and IQR
range(TheWALL$Score)
diff(range(TheWALL$Score))
IQR(TheWALL$Score)
IQR(TheWALL$Score[TheWALL$HA_Ind == "Away"])
IQR(TheWALL$Score[TheWALL$HA_Ind == "Home"])

# Stem-and-Leaf Plot

x <- c(12,22,42,13,27,46,25,52)
stem(x)

data(octane)
stem(octane$Method_1, scale = 2)
stem(octane$Method_2, scale = 2)
stem.leaf.backback(octane$Method_1, octane$Method_2,
                   back.to.back = FALSE, m = 5)
stem.leaf.backback(octane$Method_1, octane$Method_2,
                   back.to.back = TRUE, m = 5)
# compare with similar histograms
par(mfrow=c(1,2))
hist(octane$Method_1, xlab = "Ratings under method I",
     breaks = 16,
     main = "Histogram of Ocatne Ratings for method I",
     col = "mistyrose")
hist(octane$Method_2, xlab = "Ratings under method II",
     breaks = 16,
     main = "Histogram of Ocatne Ratings for method II",
     col = "cornsilk")

# Letter values
data(octane)
lval(octane$Method_1)
lval(octane$Method_2)

# Data re-expression
hydroelectric <- c(14,18,28,26,36,30,30,34,30,43,45,54,52,60,68, + 68,61,75,76,70,76,86,90,96,100,100,100,100,100,100,110,112,
                   + 118,110,124,130,135,135,130,175,165,140,250,280,204,200,270,
                   + 40,320,330,468,400,518,540,595,600,810,810,1728,1400,1743,2700)
sort(hydroelectric)
summary(hydroelectric)
hist(hydroelectric)
hist(hydroelectric[hydroelectric<800], breaks = 13)
stem.leaf(hydroelectric)
stem.leaf(hydroelectric, unit = 1)

max(hydroelectric) / min(hydroelectric)
sort(round(log(hydroelectric,10),2))
stem.leaf(round(log(hydroelectric,10),2), unit = 0.01)
hist(round(log(hydroelectric,10),2), breaks = 20)
histogram(round(log(hydroelectric,10),2), breaks = 20)

# Bagplot
data("Gasoline")
panel.bagplot <- function(x,y) {
  require(aplpack)
  bagplot(x,y, verbose = FALSE,  create.plot = TRUE, add = TRUE)
}
pairs(Gasoline[-19, -c(1,4,5,13)],
      upper.panel = panel.bagplot)
