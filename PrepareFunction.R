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