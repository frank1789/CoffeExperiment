# Clean work space and screen options
rm(list = ls())
close.screen(all = TRUE)
library(MASS)
#get and set working directory
getwd()
setwd('/Users/francescoargentieri/ProjectR')

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
qn      <- qqnorm(effects,
                 datax = T, 
                  ylab = "Effects Quantiles",
                  main = "Normal probability plot")
text(qn$x, qn$y, lab = names(df.lm$effects)[2:length(df.lm$effects)], pos = 4 )
qqline(effects,  datax = T, col = "dodgerblue")

# modify the linear model
df.lm2 <- lm(Yield ~ A*C*D + B * E , data = df)
anova(df.lm2)
shapiro.test(df.lm2$residuals)

# MAC lm2
qqnorm(df.lm2$residuals)
qqline(df.lm2$residuals, col = "red")
plot(df.lm2$fit, df.lm2$res, xlab = "Fitted values", ylab = "Residuals")
hist(df.lm2$residuals, xlab = "Residuals", main = "Histogram of residuals")

# linear model 3
df.lm3 <- lm(Yield ~ A + B * E *C , data = df)
anova(df.lm3)

# permorm MAC
qqnorm(df.lm3$residuals, datax = T, main = "QQ-plot residuals")
qqline(df.lm3$residuals, datax = T, col = "red")
plot(df.lm3$fitted.values, df.lm3$residuals , ylab = "Residuals" , xlab = "Fitted",
     main = "Fitted values pattern")

hist(df.lm3$residuals, xlab = "Residuals", main = "Histogram of residuals")
shapiro.test(df.lm3$residuals)
#######################################

# linear model 4
df.lm4 <- lm(Yield ~ C+A, data = df)
anova(df.lm4)
qqnorm(df.lm4$residuals, main = "QQ-plot residuals")
qqline(df.lm4$residuals, col="red" )

plot(df.lm4$fitted.values, df.lm3$residuals , ylab = "Residuals" , xlab = "Fitted",
     main = "Fitted values pattern")

hist(df.lm4$residuals, xlab = "Residuals", main = "Histogram of residuals")


interaction.plot(df$A,df$B,df$Yield, xlab = "A", ylab = "B")
interaction.plot(df$A,df$C,df$Yield, xlab = "A", ylab = "B")
interaction.plot(df$A,df$D,df$Yield, xlab = "A", ylab = "B")
interaction.plot(df$B,df$C,df$Yield, xlab = "A", ylab = "B")
interaction.plot(df$B,df$D,df$Yield, xlab = "A", ylab = "B")
interaction.plot(df$C,df$E,df$Yield, xlab = "A", ylab = "B")
interaction.plot(df$D,df$E,df$Yield, xlab = "A", ylab = "B")
interaction.plot(df$B,df$E,df$Yield, xlab = "A", ylab = "B")

boxcox(Yield~A+C+E,data=df)
abline(v=c(1.5), col="green")
