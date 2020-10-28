library(jpeg)
# library(phenora)
library(gridExtra)
# library(HIEv)
library(raster)
library(readr)

source('r/function_get_gcc.R')


# # ## check plot if needed
# fn = 'download/irrigated/irrigated_20191204_0900.jpg'
# see = readJPEG(fn)
# plot(raster(see[,,1]))
# polygon(c(0.0,0.6,0.6,0.0),c(0.1,0.1,0.9,0.9))


path.nm = 'download/control/'
gcc.control.df = get_gcc_func(path.nm,c(0.3,0.8,0.1,0.9))
write.csv(gcc.control.df,'control.gcc.csv',row.names = F)

path.nm = 'download/irrigated/'
gcc.irrigated.df= get_gcc_func(path.nm,c(0,0.6,0.1,0.9))



# read additional photos for irigated plots
# not that functions has been changed to read photo creationdate
i.g.p1.df=read.csv('irrigated.gcc..part1.csv') 
list.files(path.nm)
path.nm = 'download/irrigated/'
i.g.p2.df = get_gcc_func(path.nm,c(0,0.6,0.1,0.9))

write.csv(gcc.irrigated.df,'irrigated.gcc.csv',row.names = F)