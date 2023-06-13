## Management of  2001-2005 Colony Size and Land Use Data
#
#GOALS
# A.Correlation between Simpsons Index and Size
# B. AvgNests, top5LU, AvgLUSize
# C. Calculate Simpsons Index
#


# linear regression of colony size and Simpson's Index

install.packages("ggplot2")
install.packages("dplyr")
install.packages("broom")
install.packages("ggpubr")

library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)

#read in file
library(readxl)
2001 <- read_excel("F:/nlcd_land_cover_l48_20210604/nlcd_land_cover_2001/x2001_nlcd_LUSummary.xlsb.xlsx", 
                          sheet = "colonynames_analysis")

summary(X2001_nlcd_LUSummary)
Size = X2001_nlcd_LUSummary$SIZE01
SimpsonsIndex = X2001_nlcd_LUSummary$D

#Plot Index vs. Size
plot(Size ~ SimpsonsIndex, data = X2001_nlcd_LUSummary)


#distribution of dependent variable, size
hist(X2001_nlcd_LUSummary$SIZE01)

#distribution of independent variable, Simpsons Diversity Index
hist(X2001_nlcd_LUSummary$D)



#Simple regression
size.LUdiversity.lm <- lm(Size ~ SimpsonsIndex, data = X2001_nlcd_LUSummary)

summary(size.LUdiversity.lm)

#Simple regression - PLOTS

size.graph <- ggplot(X2001_nlcd_LUSummary, aes(x=SimpsonsIndex, y=Size))+
  geom_point()
size.graph
#add line
size.graph <- size.graph + geom_smooth(method="lm", col="black")

size.graph
#LABEL GRAPH
sizeLine.graph <- size.graph + stat_regline_equation(label.x = 0.2, label.y = 500)

sizeLine.graph

#pearsons correlation - Using this bc variable is interval, means not robust w/outliers
# cor(x, y, method="pearson") - correlation coefficient. Doesnt give p-value

# cor.test(x,y, method="pearson") - test for association btw paired samples
res = cor.test(X2001_nlcd_LUSummary$SIZE01, X2001_nlcd_LUSummary$D, method = "pearson")
print(res)

 #spearman correlation - Not using this bc not ordinal, BUT use it if not linear incr/decr
#cor.test(X2001_nlcd_LUSummary$SIZE01, X2001_nlcd_LUSummary$D, method = "spearman",
#         alternative = "less")


############################
# Seperated out n=125 colonies with at least one active year. Repeating correlation analysis
# ACTIVE VS NONACTIVE COLONIES
# FILE - X2001_nlcd_LUSummary_AvNA  - Sheets: ACTIVE, NONACTIVE

###########################
# ACTIVE COLONIES
#read in file
summary(a_2001_nlcd_LUSummary_AvNA_xlsb)
Size = a_2001_nlcd_LUSummary_AvNA_xlsb$SIZE01
SimpsonsIndex = a_2001_nlcd_LUSummary_AvNA_xlsb$D

#Plot Index vs. Size
plot(Size ~ SimpsonsIndex, data = a_2001_nlcd_LUSummary_AvNA_xlsb)


#distribution of dependent variable, size
hist(a_2001_nlcd_LUSummary_AvNA_xlsb$SIZE01)

#distribution of independent variable, Simpsons Diversity Index
hist(a_2001_nlcd_LUSummary_AvNA_xlsb$D)



#Simple regression
size.LUdiversity.lm <- lm(Size ~ SimpsonsIndex, data = a_2001_nlcd_LUSummary_AvNA_xlsb)

summary(size.LUdiversity.lm)

#Simple regression - PLOTS

size.graph <- ggplot(a_2001_nlcd_LUSummary_AvNA_xlsb, aes(x=SimpsonsIndex, y=Size))+
  geom_point()
size.graph
#add line
size.graph <- size.graph + geom_smooth(method="lm", col="black")

size.graph
#LABEL GRAPH
sizeLine.graph <- size.graph + stat_regline_equation(label.x = 0.2, label.y = 500)

sizeLine.graph

#pearsons correlation - Using this bc variable is interval, means not robust w/outliers
# cor(x, y, method="pearson") - correlation coefficient. Doesnt give p-value

# cor.test(x,y, method="pearson") - test for association btw paired samples
res = cor.test(a_2001_nlcd_LUSummary_AvNA_xlsb$SIZE01, a_2001_nlcd_LUSummary_AvNA_xlsb$D, method = "pearson")
print(res)


############################
# Seperated out n=125 colonies . Repeating correlation analysis
# ACTIVE VS NONACTIVE COLONIES
# FILE - X2001_nlcd_LUSummary_AvNA  - Sheets: ACTIVE, NONACTIVE

###########################
#read in file
summary(na_2001_nlcd_LUSummary_AvNA_xlsb)
Size = na_2001_nlcd_LUSummary_AvNA_xlsb$SIZE01
SimpsonsIndex = na_2001_nlcd_LUSummary_AvNA_xlsb$D

#Plot Index vs. Size
plot(Size ~ SimpsonsIndex, data = na_2001_nlcd_LUSummary_AvNA_xlsb)

#distribution of dependent variable, size
hist(na_2001_nlcd_LUSummary_AvNA_xlsb$SIZE01)

#distribution of independent variable, Simpsons Diversity Index
hist(na_2001_nlcd_LUSummary_AvNA_xlsb$D)



#Simple regression
size.LUdiversity.lm <- lm(Size ~ SimpsonsIndex, data = na_2001_nlcd_LUSummary_AvNA_xlsb)

summary(size.LUdiversity.lm)

#Simple regression - PLOTS

size.graph <- ggplot(na_2001_nlcd_LUSummary_AvNA_xlsb, aes(x=SimpsonsIndex, y=Size))+
  geom_point()

size.graph
#add line
size.graph <- size.graph + geom_smooth(method="lm", col="black")


size.graph
#LABEL GRAPH
sizeLine.graph <- size.graph + stat_regline_equation(label.x = 0.2, label.y = 1)

sizeLine.graph

 #pearsons correlation - Using this bc variable is interval, means not robust w/outliers
# cor(x, y, method="pearson") - correlation coefficient. Doesnt give p-value

# cor.test(x,y, method="pearson") - test for association btw paired samples
res = cor.test(na_2001_nlcd_LUSummary_AvNA_xlsb$SIZE01, na_2001_nlcd_LUSummary_AvNA_xlsb$D, method = "pearson")
print(res)
