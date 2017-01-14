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
