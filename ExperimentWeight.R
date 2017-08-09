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
qqline(effects,  datax = T, col = "red")
################################# fin qui ok
# modify the linear model
df.lm2 <- lm(Yield ~ C , data = df)
qqnorm(df.lm2$residuals)
qqline(df.lm2$residuals)
anova(df.lm2)
summary(df.lm2)
plot(df.lm2$fit, df.lm2$res, xlab = "residuals", ylab = "fittedvalue")
#df.lm2 <- lm(Yield~A+D*E, d = df1)
#anova(df.lm2)

# Guardo se il modello ? adeguato con qqplot


qqnorm(df.lm2$residuals, main = "QQ-plot residuals")
qqline(df.lm2$residuals, )

plot(df.lm2$fitted.values, df.lm2$residuals , ylab = "Residuals" , xlab = "Fitted",
     main = "Fitted values pattern")

hist(df.lm2$residuals, xlab = "Residuals", main = "Histogram of residuals")
close.screen(all.screens = T)
anova(df.lm2)
shapiro.test(df.lm2$residuals)
qqnorm(df.lm2$residuals, datax = T, main = "QQ-plot residuals")
qqline(df.lm2$residuals, datax = T)
interaction.plot(df$E,df$A,df$Yield, xlab = "A", ylab = "B")


boxcox((Yield)~C,data=df) 
abline(v=c(0.5), col="green")


df.lm3 <- lm(sqrt(Yield)~C+B*D,data=df)
anova(df.lm3)

