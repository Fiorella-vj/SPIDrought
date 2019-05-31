#Use of SPIDrought package
#libraries
ifelse(!require(SCI),install.packages("SCI"),library(SCI))
ifelse(!require(xlsx),install.packages("xlsx"),library(xlsx))
ifelse(!require(zoo),install.packages("zoo"),library(zoo))

# Define working directory -----------------------------------------------------
dir=getwd()
setwd(dir)

# Loading package and example data ----------------------------------------------------
devtools::install_github("Fiorella-vj/SPIDrought",force=T)
data("datapp", package="SPIDrought")
plot(datapp[,2])


# Aplying function --------------------------------------------------------

SPIF(data=datapp,scale=3,ini.clim=NULL,fin.clim=NULL,threshold=NULL,distr="gamma")

#The result is saved in your working directory


