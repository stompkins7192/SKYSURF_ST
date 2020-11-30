library(ProFound)

setwd("~/R/x86_64-pc-linux-gnu-library/4.0/ProFound/extdata/images_crs/")
pics <- list.files(all.files=TRUE, full.names=FALSE );

#print(pics)


for (i in 1:144) {
  filename = paste('images_crs/image_', i, '.fits', sep="")
  print(filename)
  image=readFITS(system.file("extdata", filename, package="ProFound"))
  sink(file = 'AWorking.txt', append = TRUE)
  # cat(paste("\n","Starting iteration",i,"\n"))
  # profoundSkySplitFFT
  # profoundMAkeSkyBlur
  # profoundChisel
  # skytype --> mean
  mask <- image$imDat > 50
  SKY = profoundProFound(image$imDat, verbose=FALSE, skycut = 1.5, pixcut = 4, 
                         SBdilate = 2, doclip = TRUE, redosky= TRUE, redoskysize= 13, 
                         type = 'bicubic', skytype='median', redosegim=TRUE, boxiters=6, mask=mask, 
                         box=c(200,200), grid=c(50,50), keepim=TRUE)
  
  #cat(paste(mean(SKY$sky, na.rm=TRUE)),"\n")
  a <- (SKY$image / SKY$sky)
  a <- a[ a != 0 ]
  
  cat(paste(mean(abs(log10(a)))), "\n")
  

  sink()
  
}


