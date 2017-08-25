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

