library(jpeg)
library(phenora)
library(gridExtra)
library(exifr)
library(doBy)
# get ng####
source('r/function_get_gcc.R')
folder.nm <- list.files('pic/ng/')

for(i in seq_along(folder.nm)){
  
  fn.out <- sprintf('cache/ng/gcc_ng_%s.rds',folder.nm[i])
  
  if(!file.exists(fn.out)){

      gcc.df <- (get_gcc_func(sprintf('pic/ng/%s',folder.nm[i]),
                              ROI=c(0.2,.8,0.2,.8)))

    saveRDS(gcc.df,fn.out)
  }
}

# make plot####
folder.nm <- list.files('pic/ng/')

pdf('ng.gcc.pdf',width = 10,height = 10 * 0.618)

par(mfrow=c(3,3))
treat.vec <- c('c','-',#'c',
               '-','+','-',
               '+','c','+','landscape')
for(i in seq_along(folder.nm)){
  
  fn.out <- sprintf('cache/ng/gcc_ng_%s.rds',folder.nm[i])
  
  if(file.exists(fn.out)){
    
    gcc.df <- readRDS(fn.out)
    gcc.df <- gcc.df[gcc.df$RGBtot > 200,]
    
    # change date
    if(folder.nm[i] %in%c(4,5)){
      gcc.df$Date[gcc.df$Date<as.Date('2019-1-1')] <- gcc.df$Date[gcc.df$Date<as.Date('2019-1-1')] + 1168
    }
    
    # 
    gcc.df <- summaryBy(.~Date,data = gcc.df,FUN = mean,na.rm=T,keep.names = T)
    
    gcc.df$gcc.smooth <- get.smooth.gcc.func(Date.vec = gcc.df$Date,gcc.vec = gcc.df$GCC)

    # hist(gcc.df$RGBtot)
    plot(GCC~Date,data = gcc.df,main = folder.nm[i],
         pch=16,ylim=c(0.25,0.4),xlab='')
    
    points(gcc.smooth~Date,data = gcc.df,type='l',
        col='grey',lwd=2)
    legend('topleft',legend =treat.vec[i],bty='n')
    abline(h=0.3,col='salmon')

  }
}
dev.off()

