source('r/functions.R')
# modifed from Remko's function
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
  # read picture names
  nm.photo <- list.files(path.vec)

  # remove files that are too dark
  for(i in seq_along(nm.photo)){
    fn <- file.path(path.vec,nm.photo[i])
    
    tmp <- try(readJPEG(fn))
    
    # remove dark photos and those cannot be read in
    if(is.array(tmp)){
      if(mean(tmp)<0.29){
        unlink(fn)
        print(fn)
      }
    }else{
      unlink(fn)
      print(fn)
    }
    
  
  }

  # reread picture names
  nm.photo <- list.files(path.vec,pattern = '.JPG')
  
  # fn = 'download/irrigated/WSCT7365.JPG'
  # library(exif)
  # read_exif(fn)$origin_timestamp
  
  # get the date
  date.vec <- file.info(file.path(path.vec,nm.photo))$ctime
  # date.vec <- parse_number(nm.photo)

  # set path for the processImages function
  myimgs <- setImages(path.vec)
  
  # read and calculate gcc
  temp.ls <- list()
  for(i in seq_along(myimgs[[1]])){
    temp.ls[[i]] <- processImage.new(myimgs[[1]][i], ROI=ROI)
  }
  
  # put gcc into a data frame
  gcc.day.df <- do.call(rbind,temp.ls)
  
  # gcc.day.df$DateTime <- as.Date(as.character(date.vec),'%Y%m%d')
  gcc.day.df$DateTime <- date.vec
  gcc.day.df$Date <-  as.Date(date.vec)
  
  return(gcc.day.df)
  
}

