library(imager)
# read in image
fn <- 'PUT YOU IMAGE PATH AND NAME HERE'#e.g 'download/WSCT7366.jpg'
photo.ls <- load.image(fn)

# define coords
# here I'm using propotional values
# you can replace these with absoulte values
x.coords <- c(0.0,0.5,0.5,0.0) * nrow(t1)
y.coords <- c(0.1,0.1,0.9,0.9) * ncol(t1)

# make plot
plot(photo.ls,ann=F,axes=F)
polygon(x.coords,y.coords,border = 'red')
