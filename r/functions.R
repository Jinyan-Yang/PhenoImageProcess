fast_load <- function(fn){
  print(fn)
  
  jpg <- try(jpeg::readJPEG(fn))
  jpg <- aperm(jpg, c(2,1,3))
  
  img_out <- array(dim = c(dim(jpg)[1:2], 1, 3))
  img_out[,,1,] <- jpg
  
  return(imager::cimg(img_out))
}

subsetROI <- function(img, ROI){
  
  ROI <- convert_ROI(ROI)
  
  d <- asdataframeImg(img)
  d$proi <- point.in.polygon(d$x, d$y, ROI$x, ROI$y)
  
  d[d$proi == 1,]
}

convert_ROI <- function(ROI){
  
  if(is.list(ROI)){
    return(ROI[c("x","y")])
  } else if(is.numeric(ROI) && length(ROI) == 4){
    return(list(x=ROI[1:2], y=ROI[3:4]))
  }
  stop("The ROI is malformed: either vector (x0,x1,y0,y1) or list with components x and y.")
  
}

asdataframeImg <- function(img){
  d <- as.data.frame(img, wide="c")
  names(d)[3:5] <- c("R","G","B")  
  d
}

load_img_fromstack <- function(imgs, nr=NULL){
  if(!inherits(imgs, "phenofiles"))
    stop("Set list of images with 'setImages' first.", call.=FALSE)
  
  nimg <- length(imgs$filenames)
  if(nr > nimg)stop("Only ", nimg," in image stack.")
  
  if(is.null(nr))stop("Provide index nr of image stack.", call.=FALSE)
  
  img <- load.image(imgs$filenames[nr])
  return(img)
}

imgSize <- function(x){ 
  dim(x)[1:2]
}  

dectime <- function(DateTime){
  hour(DateTime) + minute(DateTime)/60
}

read_and_crop <- function(fn,ROI){
  
  hasSize <- file.info(fn)["size"] > 0
  if(!hasSize)
    return(NULL)
  
  # fast_load, see utils.R. Works for jpeg only, but 2x fast.
  phot <- try(fast_load(fn))
  if(inherits(phot, "try-error"))
    return(NULL)
  
  if(!is.null(ROI)){
    phot <- subsetROI(phot,ROI)
  } else {
    phot <- asdataframeImg(phot)
  }
  
  return(phot)
}
