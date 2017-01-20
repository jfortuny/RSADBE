# Chapter 3
library(RSADBE)
library(lattice)
data("Severity_Counts")
str(Severity_Counts)
names(Severity_Counts)
barchart(Severity_Counts, xlab="Bug Count", xlim=c(0,12000), col=rep(c(2,3),5))
barplot(Severity_Counts, xlab="Bug Count", xlim=c(0,12000), horiz=TRUE, col=rep(c(2,3),5))


data("Bug_Metrics_Software")
str(Bug_Metrics_Software)

# Bar Charts
par(mfrow=c(1,2))
barplot(Bug_Metrics_Software[,,1], beside = TRUE,
        col = c("lightblue", "mistyrose", "lightcyan", "lavender", "cornsilk"),
        legend = c("JDT", "PDE", "Equinox", "Lucene", "Mylyn"))
title(main = "Before Release Bug Frequency", font.main = 4)

barplot(Bug_Metrics_Software[,,2], beside = TRUE,
        col = c("lightblue", "mistyrose", "lightcyan", "lavender", "cornsilk"),
        legend = c("JDT", "PDE", "Equinox", "Lucene", "Mylyn"))
title(main = "After Release Bug Frequency", font.main = 4)
par(mfrow=c(1,1))

# both graphs into one graph
barchart(Software~Freq|Bugs, groups = BA_Ind,
         data = as.data.frame(Bug_Metrics_Software),
         col = c(2,3))

# Dot Charts
dotchart(Severity_Counts,
         col = 41:42,
         lcolor = "black",
         pch = 15:16,
         labels = names(Severity_Counts),
         main = "Dot Plot for the Before and After Release Bug Frequency",
         cex = 1.5)

bms <- data.frame(Bug_Metrics_Software)
par(mfrow = c(1,2))
dotchart(Bug_Metrics_Software[,,1],
         gcolor = 1:5,
         color = 6:10,
         lcolor = "black",
         pch = 15:19,
         labels = names(bms$Bugs),
         main = "Before Release Bug Frequency",
         xlab = "Frequency Count")
dotchart(Bug_Metrics_Software[,,2],
         gcolor = 1:5,
         color = 6:10,
         lcolor = "black",
         pch = 15:19,
         labels = names(bms$Bugs),
         main = "After Release Bug Frequency",
         xlab = "Frequency Count")

# Spine plot
ShiftOperator <- matrix(c(40,35,28,26,40,22,52,46,49),
                        nrow = 3,
                        byrow = TRUE,
                        dimnames = list(c("Shift 1", "Shift 2", "Shift 3"),
                                        c("Operator 1", "Operator 2", "Operator 3")))
colSums(ShiftOperator)
rowSums(ShiftOperator)

spineplot(ShiftOperator)
abline(h=0.33, lwd=3, col="red")
abline(h=0.67, lwd=3, col="red")
abline(v=0.33, lwd=3, col="green")
abline(v=0.67, lwd=3, col="green")

# Mosaic Plot
data("Titanic")
str(Titanic)
head(Titanic)
class(Titanic)
mode(Titanic)

xtabs(Freq~Class, data=Titanic)
prop.table(xtabs(Freq~Class+Survived, data=Titanic), margin = 1)
prop.table(xtabs(Freq~Sex+Survived, data=Titanic), margin = 1)
prop.table(xtabs(Freq~Age+Survived, data=Titanic), margin = 1)
mosaicplot(Titanic, color = c("Red", "green"))

# Continuous variables

# Boxplot
data("resistivity")
summary(resistivity)

boxplot(resistivity, range = 0)
boxplot(resistivity, range = 0, notch = TRUE)

resistivity2 <- data.frame(rep(names(resistivity), each=8),
                           c(resistivity[,1], resistivity[,2]))
names(resistivity2) <- c("Process", "Resistivity")
bwplot(Resistivity~Process, data = resistivity2, notch = TRUE)

boxplot(Speed~Expt, data = morley,
        main = "Whiskers at Lowe- and Upper- Confidence Limits")
abline(h=792.458, lty=3)

# Histograms
data("galton")
names(galton)
dim(galton)
head(galton)
sapply(galton, range)
summary(galton)

par(mfrow=c(2,2))
hist(galton$parent, breaks = "FD", xlab = "Height of Parent",
     main = "Histogram for Parent Height with Freedman-Diaconis breaks",
     xlim = c(60,75))
hist(galton$parent, xlab = "Height of Parent",
     main = "Histogram for Parent Height with Sturges breaks",
     xlim = c(60,75))
hist(galton$child, breaks = "FD", xlab = "Height of Child",
     main = "Histogram for Child Height with Freedman-Diaconis breaks",
     xlim = c(60,75))
hist(galton$child, xlab = "Height of Child",
     main = "Histogram for Child Height with Sturges breaks",
     xlim = c(60,75))

data(octane)
par(mfrow=c(2,2))
hist(octane$Method_1, xlab = "Ratings under method I",
     main = "Histogram of Ocatne Ratings for method I",
     col = "mistyrose")
hist(octane$Method_2, xlab = "Ratings under method II",
     main = "Histogram of Ocatne Ratings for method II",
     col = "cornsilk")
data(Samplez)
hist(Samplez$Sample_1, xlab = "Sample 1",
     main = "Histogram: Sample 1", col = "magenta")
hist(Samplez$Sample_2, xlab = "Sample 2",
     main = "Histogram: Sample 2", col = "snow")

# Scatter plots
data("Gasoline")
pairs(Gasoline)

data(DCD)
summary(DCD)
plot(DCD$Drain_Current, DCD$GTS_Voltage, type = "b",
     xlim = c(1,2.2), ylim = c(0.6, 2.4),
     xlab = "Current Drain", ylab = "Voltage")
points(DCD$Drain_Current, DCD$GTS_Voltage/1.15, type = "b",
       col = "green")

# utility functions
panel.hist<- function(x, ...)
{
  usr<- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks<- h$breaks; nB<- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}
panel.cor<- function(x, y, digits=2, prefix="", cex.cor, ...) {
  usr<- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x,y,use="complete.obs"))
  txt<- format(c(r, 0.123456789), digits=digits)[1]
  txt<- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor<- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

pairs(Gasoline,
      diag.panel = panel.hist,
      lower.panel = panel.smooth,
      upper.panel = panel.cor)

# Pareto Charts
Cause_Freq <- c(5,23,7,41,19,4,3,4,2,1)
names(Cause_Freq) <-paste("C", 1:10, sep="")
Cause_Freq_Dec <- sort(Cause_Freq, dec=TRUE)
Cause_Freq_CUMSUM <- cumsum(Cause_Freq_Dec)
Cause_Freq_PCT <- Cause_Freq_CUMSUM/sum(Cause_Freq)
cbind(Cause_Freq_Dec, Cause_Freq_CUMSUM, Cause_Freq_PCT)

library(qcc)
Reject_Freq <- c(9,22,15,40,8)
names(Reject_Freq) = c("No Addr.", "Illegible", "Curr. Customer", "No Sign.", "Other")
Reject_Freq
options(digits = 2)
pareto.chart(Reject_Freq)
