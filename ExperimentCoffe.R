#Coffe experiment
# Clean work space and screen options
rm(list = ls())
close.screen(all = TRUE)

#get and set working directory
getwd()
setwd('/Users/francescoargentieri/ProjectR')

# function to generate the design matrix, with the measured time
prepare <- function(factors = list(), namefile = "", runorder = TRUE) {
    df.g <- expand.grid(factors)
    n    = nrow(df.g)
    key  = data.frame(r = runif(n, 0, 1), s = 1:n)
    key  = key[order(key$r), ]$s
    df.h = data.frame(RunOrder  = key,
                      StdOrder  = 1:n)
    df.t = data.frame(Yield=rep(NA, n))
    df = cbind(df.h, df.g, df.t)
    if (runorder) {
      df = df[order(df$RunOrder),]
    }
  
    # input name file
    if (namefile != "") {
      write.table(
        df,
        namefile,
        col.names = T,
        row.names = F,
        quote = F,
        sep = "\t"
      )
    }
    return(df)
}

#Generate design matrix

# define level
lvl = c("-", "+")

# define five factor
factors = list(
  WaterLvl = lvl,
  WaterType = c("Levissima","SanBenedetto"),
  CoffeLoad = lvl,
  Pressing = c("No","Yes"),
  Heat = c("Low","High")
)

# generate design matrix
df <- prepare(factors, runorder = F, "DesignMatrix.dat")
#End Generate matrix

#Experiment 1 - weigth 
df <- read.table("DesignMatrix-ResultWeight.dat", header = T)

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
df.lm   <- lm(Yield ~ A * B * C * D * E, d = df)
anova(df.lm)
# the residual is equal to 0 then apply the Daniels method

# Daniels method
# Extract the effects by discarding the "Intercept" values
effects <- as.vector(df.lm$effects)[2:length(df.lm$effects)]
qn      <- qqnorm(effects, datax = T, ylab = "Effects Quantiles", main = "Normal probability plot")
text(qn$x, qn$y, lab = names(df.lm$effects)[2:length(df.lm$effects)], pos = 4 )
qqline(effects,  datax = T, col = "dodgerblue")

# modify the linear model
df.lm2 <- lm(Yield ~ A * C * D + B * C * E , data = df)
anova(df.lm2)

# MAC lm2
qqnorm(df.lm2$residuals)
qqline(df.lm2$residuals, col = "red")
plot(df.lm2$fit, df.lm2$res, xlab = "Fitted values", ylab = "Residuals")
hist(df.lm2$residuals, xlab = "Residuals", main = "Histogram of residuals")
boxplot(df.lm2$residuals, horizontal = T)

# linear model 3
df.lm3 <- lm(Yield ~ A * C + B * E , data = df)
anova(df.lm3)

# permorm MAC
qqnorm(df.lm3$residuals, datax = T, main = "QQ-plot residuals")
qqline(df.lm3$residuals, datax = T, col = "red")
plot(df.lm3$fitted.values, df.lm3$residuals , ylab = "Residuals" , xlab = "Fitted",
     main = "Fitted values pattern")

hist(df.lm3$residuals, xlab = "Residuals", main = "Histogram of residuals")
boxplot(df.lm3$residuals, horizontal = T)

#Interaction plot
interaction.plot(df$A,df$B,df$Yield, xlab = "WaterLevel", ylab = "Yield", trace.label = "WaterType" )
interaction.plot(df$A,df$C,df$Yield, xlab = "WaterLevel", ylab = "Yield", trace.label = "CoffeLoad")
interaction.plot(df$A,df$D,df$Yield, xlab = "WaterLevel", ylab = "Yield", trace.label = "Pressing")
interaction.plot(df$A,df$E,df$Yield, xlab = "WaterLevel", ylab = "Yield", trace.label = "Heat")
interaction.plot(df$B,df$A,df$Yield, xlab = "WaterType", ylab = "Yield", trace.label = "CoffeLoad")
interaction.plot(df$B,df$C,df$Yield, xlab = "WaterType", ylab = "Yield", trace.label = "CoffeLoad")
interaction.plot(df$B,df$D,df$Yield, xlab = "WaterType", ylab = "Yield", trace.label = "Pressing")
interaction.plot(df$B,df$E,df$Yield, xlab = "WaterType", ylab = "Yield", trace.label = "Heat")
interaction.plot(df$C,df$D,df$Yield, xlab = "CoffeLoad", ylab = "Yield", trace.label = "Pressing")
interaction.plot(df$C,df$E,df$Yield, xlab = "CoffeLoad", ylab = "Yield", trace.label = "Heat")
interaction.plot(df$D,df$E,df$Yield, xlab = "Pressing", ylab = "Yield", trace.label = "Heat")

#Fractional Factorial Plane
lvl <- c(-1, +1)
df2 <- expand.grid(A=lvl, B=lvl, C=lvl, D=lvl)
df2
attach(df2)
df2$E <- A*B*C*D
detach(df2)
df2
df2$A <- factor(df2$A,levels = lvl ,labels = c('-', '+'))
df2$B <- factor(df2$B,levels = lvl,labels = c('-', '+'))
df2$C <- factor(df2$C,levels = lvl,labels = c('-', '+'))
df2$D <- factor(df2$D,levels = lvl,labels = c('-', '+'))
df2$E <- factor(df2$E,levels = lvl,labels = c('-', '+'))
df2

df.ffp <- read.table("ffpWeight.dat", header = T)

#analysis effect - Daniel method
df.ffp.lm <- lm(Yield ~ A * B * C * D * E, data = df.ffp)

n <- length(df.ffp.lm$effects)
effects <- as.vector(df.ffp.lm$effects[2:n])
qn <- qqnorm(effects, datax=T)
text(qn$x, qn$y, lab=names(df.ffp.lm$effects)[2:n], pos=4)
qqline(effects, datax=T)

#Linear moder & mac
df.ffp.lm2 <- lm(log(Yield) ~ A * C + D, data = df.ffp)
anova(df.ffp.lm2)

#MAC
qqnorm(df.ffp.lm2$residuals)
qqline(df.ffp.lm2$residuals)
hist(df.ffp.lm2$residuals)
plot(df.ffp.lm2$fitted.values, df.ffp.lm2$residuals , ylab = "Residuals" , xlab = "Fitted",
     main = "Fitted values pattern")
shapiro.test(df.ffp.lm2$residuals)

#End Experiment 1


#Experiment 2 - Time
# load library
if (!require(lubridate)) {
  install.packages("lubridate")
  library(lubridate)
}

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
df.lm <- lm(Yield ~ A * B * C * D * E, data = df)
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

#Interaction plot
interaction.plot(df$A,df$B,df$Yield, xlab = "WaterLevel", ylab = "Yield", trace.label = "WaterType" )
interaction.plot(df$A,df$C,df$Yield, xlab = "WaterLevel", ylab = "Yield", trace.label = "CoffeLoad")
interaction.plot(df$A,df$D,df$Yield, xlab = "WaterLevel", ylab = "Yield", trace.label = "Pressing")
interaction.plot(df$A,df$E,df$Yield, xlab = "WaterLevel", ylab = "Yield", trace.label = "Heat")
interaction.plot(df$B,df$A,df$Yield, xlab = "WaterType", ylab = "Yield", trace.label = "CoffeLoad")
interaction.plot(df$B,df$C,df$Yield, xlab = "WaterType", ylab = "Yield", trace.label = "CoffeLoad")
interaction.plot(df$B,df$D,df$Yield, xlab = "WaterType", ylab = "Yield", trace.label = "Pressing")
interaction.plot(df$B,df$E,df$Yield, xlab = "WaterType", ylab = "Yield", trace.label = "Heat")
interaction.plot(df$C,df$D,df$Yield, xlab = "CoffeLoad", ylab = "Yield", trace.label = "Pressing")
interaction.plot(df$C,df$E,df$Yield, xlab = "CoffeLoad", ylab = "Yield", trace.label = "Heat")
interaction.plot(df$D,df$E,df$Yield, xlab = "Pressing", ylab = "Yield", trace.label = "Heat")

#Fraction Factorial Plane
lvl <- c(-1, +1)
df2 <- expand.grid(A=lvl, B=lvl, C=lvl, D=lvl)
df2
attach(df2)
df2$E <- A*B*C*D
detach(df2)
df2
df2$A <- factor(df2$A,levels = lvl ,labels = c('-', '+'))
df2$B <- factor(df2$B,levels = lvl,labels = c('-', '+'))
df2$C <- factor(df2$C,levels = lvl,labels = c('-', '+'))
df2$D <- factor(df2$D,levels = lvl,labels = c('-', '+'))
df2$E <- factor(df2$E,levels = lvl,labels = c('-', '+'))
df2

(df.ffp <- read.table("ffpTime.dat", header = T))
df.ffp.lm <- lm(Yield ~ A * B * C * D * E, data = df.ffp)
anova(df.ffp.lm)

#Daniel Method - verify effect
n <- length(df.ffp.lm$effects)
effects <- as.vector(df.ffp.lm$effects[2:n])
qn <- qqnorm(effects,datax = T)
text(qn$x, qn$y, lab=names(df.ffp.lm$effects)[2:n], pos=1)
qqline(effects, datax = T, col= "dodgerblue ")

#Linear model test anova and MAC
df.ffp.lm2 <- lm(Yield ~ A + C * E, data = df.ffp)
anova(df.ffp.lm2)

#MAC
qqnorm(df.ffp.lm2$residuals)
qqline(df.ffp.lm2$residuals, col = "red")
hist(df.ffp.lm2$residuals)
plot(df.ffp.lm2$fitted.values, df.ffp.lm2$residuals , ylab = "Residuals" , xlab = "Fitted",
     main = "Fitted values pattern")
shapiro.test(df.ffp.lm2$residuals)
