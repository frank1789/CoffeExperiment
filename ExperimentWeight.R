# Clean work space and screen options
rm(list = ls())
close.screen(all = TRUE)

#get and set working directory
getwd()
setwd('/Users/francescoargentieri/ProjectR')

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
df  <- expand.grid(
  A = lvl,
  B = lvl,
  C = lvl,
  D = lvl,
  E = lvl
)

# result yield experimet
YieldWeight <-
  c(
    NA,
    NA,
    NA,
    NA,
    NA,
    NA,
    12,
    46,
    NA,
    NA,
    NA,
    NA,
    100,
    84,
    NA,
    NA,
    NA,
    132,
    106,
    NA,
    NA,
    NA,
    NA,
    NA,
    NA,
    NA,
    NA,
    134,
    NA,
    NA,
    NA,
    106
  )

# complte the dataframe
(df <- data.frame(df, Yield = YieldWeight))

# analisys of linear model
df.lm   <- lm(Yield ~ A * B * C * D * E, d = df)
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
qqline(effects, datax = T, col = "red")

# modify the linear model
df.lm2 <- lm(Yield ~ A * C + B + D + E, data = df)
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
