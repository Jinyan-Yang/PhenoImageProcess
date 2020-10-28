source('r/functions.R')
# modifed from Remko's function

# fn <- 'pic/ng/9/WSCT1262.jpg'

processImage.new <- function(fn, ROI=NULL){
  
  phot <- read_and_crop(fn,NULL)
  if(is.null(phot)){
    return(data.frame(filename=NA, GCC=NA, RCC=NA, BCC=NA, RGBtot=NA))
  }
  
  if(!is.null(ROI)){
    xmin <- ceiling(max(phot$x) * ROI[1])
    xmax <- ceiling((max(phot$x) * ROI[2]))
    
    ymin <- ceiling(max(phot$y) * ROI[3])
    ymax <- ceiling(max(phot$y) * ROI[4])
    
    phot <- phot[phot$x >= xmin & phot$x <= xmax &
                   phot$y >= ymin & phot$y <= ymax,]
  }
  library(raster)
  
  RDN <- mean(phot$R)
  GDN <- mean(phot$G)
  BDN <- mean(phot$B)
  bn <- RDN+GDN+BDN
  
  RI <- RDN/bn
  GI <- GDN/bn
  BI <- BDN/bn
  
  #GEI <- 2*GDN - (RDN + BDN)
  
  # Convention
  rgbtot <- bn * 255
  
  return(data.frame(filename=fn, GCC=GI, RCC=RI, BCC=BI, RGBtot=rgbtot))
}

get_gcc_func <- function(path.vec, ROI=NULL){
  # path.vec='pic/ng/5/'
  
  # read picture names
  nm.photo <- list.files(path.vec)

  # # remove files that are too dark
  # for(i in seq_along(nm.photo)){
  #   fn <- file.path(path.vec,nm.photo[i])
  #   
  #   tmp <- try(readJPEG(fn))
  #   
  #   # # remove dark photos and those cannot be read in
  #   # if(is.array(tmp)){
  #   #   # if(mean(tmp)<0.29){
  #   #   #   unlink(fn)
  #   #   #   print(fn)
  #   #   # }
  #   # }else{
  #   #   unlink(fn)
  #   #   print(fn)
  #   # }
  #   
  # 
  # }

  # reread picture names
  nm.photo <- list.files(path.vec,pattern = '.JPG')
  
  # fn = c('download/irrigated/WSCT7365.JPG','download/irrigated/WSCT7366.JPG')
  
  # get the date
  # date.vec <- file.info(file.path(path.vec,nm.photo))$ctime
  # date.vec <- parse_number(nm.photo)
  fn = file.path(path.vec,nm.photo)
  # fn=fn[1:10]
  date.vec <- read_exif(fn)$CreateDate

  # set path for the processImages function
  # myimgs <- setImages(path.vec)
  # myimgs[[1]] <- myimgs[[1]][!duplicated(myimgs[[1]])]
  
  # read and calculate gcc
  temp.ls <- list()
  for(i in seq_along(fn)){
    temp.ls[[i]] <- processImage.new(fn[i], ROI=ROI)
  }
  
  # put gcc into a data frame
  gcc.day.df <- do.call(rbind,temp.ls)
  
  # gcc.day.df$DateTime <- as.Date(as.character(date.vec),'%Y%m%d')
  gcc.day.df$DateTime <- date.vec
  gcc.day.df$Date <-  as.Date(date.vec,'%Y:%m:%d')

  return(gcc.day.df)
  
}


get.smooth.gcc.func = function(Date.vec,gcc.vec){
  library(mgcv)
  library(lubridate)
  gam.frdm = round(length(Date.vec)/3)
  
  gam.in.df = data.frame(x = as.numeric(Date.vec),
                         y = gcc.vec)
  fit.gam <- gam(y~s(x,k = gam.frdm),data = gam.in.df)
  
  out.df = predict(fit.gam,gam.in.df)
  return(out.df)
}

