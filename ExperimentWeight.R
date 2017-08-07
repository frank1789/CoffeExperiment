# Clean work space and screen options
rm(list = ls())
close.screen(all = TRUE)

#get and set working directory
getwd()
setwd('/Users/francescoargentieri/ProjectR')


library(lubridate)
times <- c("00:00", "05:10", "07:15", "09:00", "11:30")
ms(times)
## [1] "0S"      "5M 10S"  "7M 15S"  "9M 0S"   "11M 30S"
You can then use as.period() to convert to the time unit you desire:
  
  as.period(ms(times), unit = "sec")
## [1] "0S"   "310S" "435S" "540S" "690S"
If you convert to numeric now, you will get the seconds as numbers:
  
  seconds <- as.numeric(as.period(ms(times), unit = "sec"))
seconds
## [1]   0 310 435 540 690


df <- read.table("DesignMatrix-ResultWeight.dat", header = T)



# define level
lvl = c(-1, +1)
# define five factor
factors = list(
  A = lvl,
 B = lvl,
  C = lvl,
  D = lvl,
  E = lvl
)

# Definisco la design matrix
df  <- expand.grid(A = lvl, B = lvl, C = lvl, D = lvl, E = lvl)
df
#data <- df$Yield

YieldWeight <- c(NA,NA,NA,NA,NA,NA,12,46,NA,NA,NA,NA,100,84,NA,NA,NA,132,106,NA,NA,NA,NA,NA,NA,NA,NA,134,NA,NA,NA,106) 

(df <- data.frame(df, Yield = YieldWeight))
# Metodo di Daniels

df.lm   <- lm(Yield~A*B*C*D*E, d = df)
qqnorm(df.lm$res)
anova(df.lm)
# Estraggo gli effetti (escludendo l'intercetta)
effects <- as.vector(df.lm$effects)[2:length(df.lm$effects)]
qn      <- qqnorm(effects, datax = T, ylab = "Effects quantiles",
                  main = "Normal probability plot")
text(qn$x, qn$y, lab = names(df.lm$effects)[2:length(df.lm$effects)], pos = 4 )
qqline(effects, datax = T)
grid()

df.lm2 <- lm(Yield~A*C + D +E, data = df)
qqnorm(df.lm2$res)
anova(df.lm2)


#df.lm2 <- lm(Yield~A+D*E, d = df1)
#anova(df.lm2)

# Guardo se il modello ? adeguato con qqplot


qqnorm(df.lm2$residuals, datax = T, main = "QQ-plot residuals")
qqline(df.lm2$residuals, datax = T)

plot(df.lm2$fitted.values, df.lm2$residuals , ylab = "Residuals" , xlab = "Fitted",
     main = "Fitted values pattern")

hist(df.lm2$residuals, xlab = "Residuals", main = "Histogram of residuals")
close.screen(all.screens = T)
anova(df.lm2)
