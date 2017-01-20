# Chapter 4 - EDA

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

library(aplpack)
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


