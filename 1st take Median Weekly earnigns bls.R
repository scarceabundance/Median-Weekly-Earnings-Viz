install.packages("lattice")

library(lattice)
library(latticeExtra)
library(readxl)

BLSHA_Regress <- read_excel("C:/Users/AGabrielClay/Desktop/personal projects/Unemployment Charawcteristic tables/(Occupation) Median Weekly earnings + certification or liscencing.xlsx", 
                            sheet = "AllYears1")
BLSHA_Regress
View(BLSHA_Regress)

library(lattice);
data("BLSHA_Regress");
attach(BLSHA_Regress)
panel.lm = function(x,y) { 
  panel.xyplot(x,y) 
  (panel.abline(y,x))
}
xyplot(Workers ~ MEW_CL | Year,panel= panel.lm,data=BLSHA_Regress)
# too fw points in some of the neighborhoods, lets combine 

nbd = as.numeric(cut(neighborrhood,c(0,2,3,4),labels=c(1,2,3)))
table(nbd)

xyplot(Workers ~ MEW_CL | Year, panel= panel.lm,layout=c(4,1),
       main = "ChangeS in Workforce Participation and Percent Change in Earnings",
       sub = "(2016-2017)",
       par.settings = list(par.main.text = list(cex = 1.4, col = "black"), 
                           par.sub.text = list(cex = 1.4, col = "black"), 
                           par.xlab.text = list(cex = 1.2, col = "steelblue"), 
                           par.ylab.text = list(cex = 1.0, col = "steelblue"),
                           col = c("green", "orange","brown", "red")),
       auto.key = list(space = "right", cex = 0.7, size = 1.7, points = TRUE)
)
  # HERE IS WHERE YOU MADE CHANGE TO TEST UNDERSTANDING OF GIT PROCESSES 
  # whwat is git id/ SHA ????
  
?xyplot

library(lattice)

View(melanoma)

life <-  (c(1,2,3,4,5))
happiness <-  (c("a","b","c","d","e"))
table(life, happiness)

