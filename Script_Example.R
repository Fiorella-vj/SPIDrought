#Use of SPIDrought package
#
# Define working directory -----------------------------------------------------
dir=getwd()
setwd(dir)

# Loading example data ----------------------------------------------------
data("datapp", package="SPIDrought")
plot(datapp[,2])


# Aplying function --------------------------------------------------------

SPIF(data=datapp,scale=3,ini.clim=NULL,fin.clim=NULL,threshold=NULL,distr="gamma")

#The result is saved in your working directory


