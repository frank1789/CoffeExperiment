# Clean work space and screen options
rm(list = ls())
close.screen(all = TRUE)

#get and set working directory
getwd()
setwd('/Users/francescoargentieri/ProjectR')

# Load library
source("PrepareFunction.R")

#definisco il mio esperimento, vorrei misurare la quantità e il tempo di risalita del caffè dalla caldaia
#                                                    +----------> sotto la valvola (g)
#                                                    |
#fattore 1 ----------------> livelli:3 acqua caldaia +----------> alla valvola (g)
#                                                    |
#                                                    +---------> sopra valvola (g)
#                                                    +----------> alto resiudo fisso 
#                                                    |
#fattore 2 ---------------> livelli:3 qualità acqua  +----------> rubinetto
#                                                    |
#                                                    +---------> basso residuo fisso
#                                                    +----------> sotto raso (g)
#                                                    |
#fattore 3 ---------------> livelli:3 caffè filtro   +----------> raso (g)
#                                                    |
#                                                    +---------> sopra raso (g)
#                                                    +----------> alta(+)
#                                                    |
#fattore 4 ---------------> livelli:2 fiamma         +
#                                                    |
#                                                    +---------> bassa(-)
#                                                    +----------> sotto raso
#                                                    |
#fattore 5 ---------------> livelli:3 caffè filtro   +----------> raso
#                                                    |
#                                                    +---------> sopra raso

#                       +----------> quantità (g)
#                       |
#Resa --------------->  +
#                       |
#                       +----------> tempo (s)

# define level
lvl = c("-", "+")
# define five factor
factors = list(
  WaterLvl = lvl,
  WaterType = c("Levissima","SanBenedetto"),
  CoffeLoad = lvl,
  Pressing = lvl,
  Heat = lvl
)

# generate design matrix
df <- prepare(factors, runorder = F, "DesignMatrix.dat")

df <- read.table("DesignMatrix.dat", header = T)
# preview of design matrix
#sink("NoResult.txt")
#print(df)
#sink()

# perform the experiment and store result in vector
#result <- runif(32, 1.5, 100.5)
#data <- df$Yield
#df$Yield <- result
# print preview 
df
#attach(df)
df.lm <- lm(Yield ~  as.factor(WaterLvl) * WaterType * as.factor(CoffeLoad) * Pressing *Heat, data = df)
anova(df.lm)
hist(df.lm$res, xlab = "Residuals", main = "Histogram of Residuals")
qqnorm(df.lm$residuals, ylab = "Residuals")
qqline(df.lm$residuals, col = "red" )
plot(df.lm$fitted.values, df.lm$residuals , ylab = "Residuals" , xlab = "Fitted",
     main = "Fitted values pattern")
plot(df$RunOrder, df.lm$residuals, ylab = "Residuals", main = "RunOrder pattern")
plot(df$RunOrder, df.lm$residuals, ylab = "Resiudals", main = "Run")


#hist(df.lm$effects, xlab = "Residuals", main = "Histogram of Residuals")
qn <- qqnorm(df.lm$effects, ylab = "Residuals")
text(qn$x, qn$y, labels = names(df.lm$eff)[2:18], pos = 4)
qqline(df.lm$effects, col = "red")
#plot(df.lm$fitted.values, df.lm$effects , ylab = "Residuals" , xlab = "Fitted",
#     main = "Fitted values pattern")
#plot(df$RunOrder, df.lm$effects, ylab = "Residuals", main = "RunOrder pattern")
#plot(df$RunOrder, df.lm$effects, ylab = "Resiudals", main = "Run")
detach(df)
###############################################################################################

  #   a <- nrow(factors)
  #   b <- nrow(factors)
  #   key <- data.frame(r = runif(a * b * k, 0, 1), s = c(1:(a * b * k)))
  #   key <- key[order(key$r), ]
  #   df <- data.frame(RunOrder = key$s,
  #       StandardOrder = c(1:(a * b * k)),
  #       A = as.factor(gl(a, 1, a * k * b, lab = factors[[1]])),
  #       B = as.factor(gl(b, a, a * b * k, lab = factors[[2]])),
  #       Repeat = gl(k, a * b),
  #       Response = rep(NA, a * b * k))
  #   names(df)[3:4] <- names(factors)
  #   if (runorder) {
  #     df <- df[order(df$RunOrder),]
  #     }
  #   write.table(df, name, col.names = T, quote = F, sep = "\t", fileEncoding = "UTF8")
  #   return(df)
  # }

m <- data.frame(Temperature = c(15, 70, 125), Material = c("A", "B", "C"))
k <- 4

df <- prepare(m, k, "battery.dat", run = FALSE)
df <- read.table("DesignMatrix-ResultWeight.dat", header = TRUE)
df$Temperature <- as.factor(df$Temperature)
df$Response <- 130
attach(df)
df[1:10, ]

df.lm <- lm(Response ~ Material * Temperature, data = df)
detach(df)

df2 <- read.table("testcatapult.dat", h = T,stringsAsFactors = default.stringsAsFactors())
df2

df2 <- df
df2.lm <- lm(df)
# 4 figures arranged in 2 rows and 2 columns
#par(mar= c(5,4,0,2) +0.1)
split.screen(c(2,2))
screen(1)
boxplot(df2$Heat, horizontal = T)
screen(2)
qqnorm(df2$Yield, ylab = "residuals")
qqline(df2$Yield, col = "dodgerblue" )
screen(3)
hist(df2$distance)
screen(4)
plot(df2$order,df2$distance, ylab = "Residuals")
close.screen(all=TRUE)

hist(df2.lm$residuals , xlab="Residuals", main ="Histogram of residuals")

par(mar= c(5,4,0,2) +0.1)
split.screen(c(2,2))
screen(1)
boxplot(df2.lm$residual, horizontal = T)
screen(3)
qqnorm(df2.lm$residuals, ylab = "residuals")
qqline(df2.lm$residuals)
screen(4)
plot(df2$order, df2.lm$res)
anova(df2.lm)

df2
write.table(df2, "testtab.txt", col.names = T, quote = F, row.names = F, sep = "\t", qmethod = c("escape", "double"))
df2


if(!require(AlgDesign)){
  +     install.packages("AlgDesign")
  +     library(AlgDesign)
}

gen.factorial(c(2,2,2,2,2),5, varNames = c("fact1", "fact2", "fact3", "fact4", "fact5"))
