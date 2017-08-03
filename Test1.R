# Clean work space and screen options
rm(list = ls())
close.screen(all = TRUE)

# Load library
source("PrepareFunction.R")

#library(gdata)

#get and set working directory
getwd()
setwd('/Users/francescoargentieri/ProjectR')

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

# function to generate the design matrix, with the measured time
prepare <-
  function(factors = list(
    fact1 = c(1:2),
    fact2 = c(1:2),
    fact3 = c(1:2),
    fact4 = c(1:2)
  ),
  namefile = "",
  runorder = TRUE) {
    df.g <- expand.grid(factors)
    df.g
    n    = nrow(df.g)
    key  = data.frame(r = runif(n, 0, 1), s = 1:n)
    key  = key[order(key$r), ]$s
    df.h = data.frame(RunOrder  = key,
                      StdOrder  = 1:n)
    df.t = data.frame(Yield = rep(NA, n))
    df = cbind(df.h, df.g, df.t)
    if (runorder) {
      df = df[order(df$RunOrder), ]
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

# generate design matrix
df <- prepare(
  factors = list(
    WaterLvl = c(-1, +1),
    WaterType = c(-1, +1),
    CoffeLoad = c(-1, +1),
    Heat = c(-1, +1)
  ),
  runorder = F,
  "prova.txt"
)

# preview of design matrix
print(df)

# write result in colunm
result <- c(1:16)
print(result)
df$Yield <- result

# print preview 
df

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
df <- read.table("battery.dat", header = TRUE)
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
