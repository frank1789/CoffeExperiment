#clean work space
rm(list = ls())

#get and set working directory
getwd()
setwd('/Users/francescoargentieri/ProjectR')

print('test1')

#prepare function
prepare <- function(factors = data.frame(A = c(1:3), B = c(1:3)),
           k = 4,
           name,
           runorder = TRUE) {
    a <- nrow(factors)
    b <- nrow(factors)
    key <- data.frame(r = runif(a * b * k, 0, 1), s = c(1:(a * b * k)))
    key <- key[order(key$r), ]
    df <- data.frame(RunOrder = key$s,
        StandardOrder = c(1:(a * b * k)),
        A = as.factor(gl(a, 1, a * k * b, lab = factors[[1]])),
        B = as.factor(gl(b, a, a * b * k, lab = factors[[2]])),
        Repeat = gl(k, a * b),
        Response = rep(NA, a * b * k))
    names(df)[3:4] <- names(factors)
    if (runorder) {
      df <- df[order(df$RunOrder),]
      }
    
    write.table(df, name, col.names = T, row.names = F, quote = F, sep = "\t")
    return(df)
  }

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

