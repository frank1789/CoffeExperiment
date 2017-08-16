# Clean work space and screen options
rm(list = ls())
close.screen(all = TRUE)

# get and set working directory
getwd()
setwd('/Users/francescoargentieri/ProjectR')

# load library
library(lubridate)
library(MASS)

# read dataframe from file with result
df <- read.table("DesignMatrix-ResultTime.dat", header = T)

# convert string time [mm:ss] -> time in numeric second [s]
df$Yield <- as.numeric(as.period(ms(df$Yield), unit = "sec"))

df$WaterType <-
  factor(
    df$WaterType,
    levels = c('Levissima', 'SanBenedetto'),
    labels = c('-', '+')
  )
df$Pressing <-
  factor(df$Pressing,
         levels = c('No', 'Yes'),
         labels = c('-', '+'))
df$Heat <- factor(df$Heat,
                  levels = c('Low', 'High'),
                  labels = c('-', '+'))

names(df)[3] <- "A"
names(df)[4] <- "B"
names(df)[5] <- "C"
names(df)[6] <- "D"
names(df)[7] <- "E"

df

# analisys of linear model
df.lm<- lm(Yield ~ A * B * C * D * E, data = df)
anova(df.lm)
# the residual is equal to 0 then apply the Daniels method

# Daniels method
# Extract the effects by discarding the "Intercept" values
effects <- as.vector(df.lm$effects)[2:length(df.lm$effects)]
qn      <- qqnorm(effects,
                  datax = T,
                  ylab = "Effects quantiles",
                  main = "Normal probability plot")
text(qn$x, qn$y, lab = names(df.lm$effects)[2:length(df.lm$effects)], pos = 4 )
qqline(effects, datax = T, col = "dodgerblue")

# modify the linear model
df.lm2 <- lm(Yield ~ A * B * D + C * E, data = df)
anova(df.lm2)

# MAC - model accuracy  
qqnorm(df.lm2$residuals, datax = T, main = "QQ-plot residuals")
qqline(df.lm2$residuals, datax = T, col = "red")
plot(df.lm2$fitted.values, df.lm2$residuals, ylab = "Residuals" , xlab = "Fitted values",
     main = "Fitted values pattern")
hist(df.lm2$residuals, xlab = "Residuals", main = "Histogram of residuals")
plot(df$RunOrder, df.lm2$residuals, xlab="Actual Run Order", ylab="Residual",
     main="Run Order Plot")
boxplot(df.lm2$residuals, horizontal = T)

# linear model 3 - without interaction
df.lm3 <- lm(Yield ~ A + C + E, data = df)
anova(df.lm3)

# check MAC
qqnorm(df.lm3$residuals, datax = T, main = "QQ-plot residuals")
qqline(df.lm3$residuals, datax = T, col = "red")
plot( df.lm3$fitted.values, df.lm3$residuals, ylab = "Residuals" , xlab = "Fitted values",
      main = "Fitted values pattern")
hist(df.lm3$residuals, xlab = "Residuals", main = "Histogram of residuals")
plot(df$RunOrder, df.lm3$residuals, xlab="Actual Run Order", ylab="Residual",
     main="Run Order Plot")
boxplot(df.lm3$residuals, horizontal = T)

# inteaction plot
interaction.plot(df$A,df$B,df$Yield, xlab = "WaterLevel", ylab = "Yield")
interaction.plot(df$A,df$C,df$Yield, xlab = "WaterLevel", ylab = "Yield")
interaction.plot(df$A,df$D,df$Yield, xlab = "WaterLevel", ylab = "Yield")
interaction.plot(df$A,df$E,df$Yield, xlab = "WaterLevel", ylab = "Yield")
interaction.plot(df$B,df$C,df$Yield, xlab = "WaterType", ylab = "Yield")
interaction.plot(df$B,df$D,df$Yield, xlab = "WaterType", ylab = "Yield")
interaction.plot(df$B,df$E,df$Yield, xlab = "WaterType", ylab = "Yield")
interaction.plot(df$C,df$E,df$Yield, xlab = "CoffeLoad", ylab = "Yield")
interaction.plot(df$D,df$E,df$Yield, xlab = "Pressing", ylab = "Yield", trace.label = "Heat", col = 3:1, legend = T)

shapiro.test(df.lm2$residuals)
shapiro.test(df.lm3$residuals)

bc = boxcox(df.lm2, data = df)
title("Box-Cox Transformation")
lambda = bc$x[which.max(bc$y)]
lambda
boxcox(df.lm2,plotit=T,data= df)
#boxcox(df.lm2,lambda=seq(0.0,1.0,by=0.05),plotit=T,data = df)
abline(v=c(lambda), col = "green")

bc = boxcox(df.lm3, data = df)
title("Box-Cox Transformation")
lambda = bc$x[which.max(bc$y)]
lambda

