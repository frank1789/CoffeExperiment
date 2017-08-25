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
df.lm2 <- lm(Yield ~ A*C*D + B *C* E , data = df)
anova(df.lm2)
shapiro.test(df.lm2$residuals)

# MAC lm2
qqnorm(df.lm2$residuals)
qqline(df.lm2$residuals, col = "red")
plot(df.lm2$fit, df.lm2$res, xlab = "Fitted values", ylab = "Residuals")
hist(df.lm2$residuals, xlab = "Residuals", main = "Histogram of residuals")
boxplot(df.lm2$residuals, horizontal = T)

# linear model 3
df.lm3 <- lm(log(Yield) ~ A*C + B * E , data = df)
anova(df.lm3)

# permorm MAC
qqnorm(df.lm3$residuals, datax = T, main = "QQ-plot residuals")
qqline(df.lm3$residuals, datax = T, col = "red")
plot(df.lm3$fitted.values, df.lm3$residuals , ylab = "Residuals" , xlab = "Fitted",
     main = "Fitted values pattern")

hist(df.lm3$residuals, xlab = "Residuals", main = "Histogram of residuals")
boxplot(df.lm3$residuals, horizontal = T)
shapiro.test(df.lm3$residuals)
#######################################

# linear model 4
df.lm4 <- lm(1/Yield ~ A*C + B*E+D, data = df)
anova(df.lm4)
qqnorm(df.lm4$residuals, main = "QQ-plot residuals")
qqline(df.lm4$residuals, col="red" )

plot(df.lm4$fitted.values, df.lm4$residuals , ylab = "Residuals" , xlab = "Fitted",
     main = "Fitted values pattern")

hist(df.lm4$residuals, xlab = "Residuals", main = "Histogram of residuals")
shapiro.test(df.lm4$residuals)

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
interaction.plot(df$E,df$D,df$Yield, xlab = "Pressing", ylab = "Yield", trace.label = "Heat")
interaction.plot(df$E,df$C,df$Yield, xlab = "Pressing", ylab = "Yield", trace.label = "Heat")
interaction.plot(df$E,df$B,df$Yield, xlab = "Pressing", ylab = "Yield", trace.label = "Heat")
interaction.plot(df$E,df$A,df$Yield, xlab = "Pressing", ylab = "Yield", trace.label = "Heat")
y <- df$Yield
bc = boxcox(df.lm2, data = df)
title("Box-Cox Transformation")
lambda = bc$x[which.max(bc$y)]
lambda

##############################
bc = boxcox((Yield)^3 ~ A*C*D + B * E, data = df)
title("Box-Cox Transformation")
lambda = bc$x[which.max(bc$y)]
lambda


df.anova <- anova(df.lm3)
df.anova
(length(df.anova$'Mean Sq'))
stdres <- df.lm3$residuals/sqrt(df.anova$`Mean Sq`[1])
close.screen(all = T)
plot(stdres, ylim = c(-3,3), main = "Standarized residuals")
abline(h=1, col="green")
abline(h=2, col="orange")
abline(h=3, col="red")
abline(h=-1, col="green")
abline(h=-2, col="orange")
abline(h=-3, col="red")

floor(length(df$Yield)*c(1-0.68,1-0.95,1-0.997))

#########################################


df








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
dfrow <- df[3:7]
dfrow


library("dplyr")


a <- df
b <- df2


df.ffp<-dplyr::semi_join(a, b, by = c("A","B","C","D","E"))

write.table(df.ffp[3:8],"ffpWeight.dat", col.names = T,
            row.names = F,
            quote = F,
            sep = "\t")
df.ffp <- df.ffp[3:8]
sum(df.ffp$Yield) # => 485

df.ffp.lm <- lm(Yield~A*B*C*D*E, data=df.ffp)

n <- length(df.ffp.lm$effects)
effects <- as.vector(df.ffp.lm$effects[2:n])
qn <- qqnorm(effects, datax=T)
text(qn$x, qn$y, lab=names(df.ffp.lm$effects)[2:n], pos=4)
qqline(effects, datax=T)

df.ffp.lm <- lm(log(Yield)~A*C+D, data=df.ffp)
anova(df.ffp.lm)
qqnorm(df.ffp.lm$residuals)
qqline(df.ffp.lm$residuals)
hist(df.ffp.lm$residuals)
plot(df.ffp.lm$fitted.values, df.ffp.lm$residuals , ylab = "Residuals" , xlab = "Fitted",
     main = "Fitted values pattern")

