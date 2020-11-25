library(ProFound)

setwd("~/R/x86_64-pc-linux-gnu-library/4.0/ProFound/extdata/images_crs/")
pics <- list.files(all.files=TRUE, full.names=FALSE );

#print(pics)
sink(file = 'all_in_one.txt', append = TRUE)
cat(paste("MEASURED","\n"))

for (i in 1:144) {
  filename = paste('images_crs/image_', i, '.fits', sep="")
  #print(filename)
  image=readFITS(system.file("extdata", filename, package="ProFound"))
  sink(file = 'all_in_one.txt', append = TRUE)

# profoundSkySplitFFT
# profoundMAkeSkyBlur
# profoundChisel
# skytype --> mean
  mask <- image$imDat > 50
  SKY = profoundProFound(image$imDat, verbose=FALSE, skycut = 1.5, pixcut = 4, 
                         SBdilate = 2, doclip = TRUE, redosky= TRUE, redoskysize= 13, 
                         type = 'bicubic', skytype='median', redosegim=TRUE, boxiters=6, mask=mask, 
                         box=c(200,200), grid=c(50,50))
  cat(paste(mean(SKY$sky, na.rm=TRUE)),"\n")
  
  sink()
}
setwd("~/Sky_Estimates_R")
#MOVE DATA TO CORRECT LOCATION
file.copy(from = "~/R/x86_64-pc-linux-gnu-library/4.0/ProFound/extdata/images_crs/all_in_one.txt",
          to   = "~/Sky_Estimates_R")
#GET RID OF OLD DATA
file.remove("~/R/x86_64-pc-linux-gnu-library/4.0/ProFound/extdata/images_crs/all_in_one.txt")
#EXTRACT NECESSARY DATA
input <- read.table("final_images_data.txt", header= TRUE)
x <- (input$SKY)
RMS <- (input$RMS)
measured <- read.table("all_in_one.txt", header= TRUE)
y <- (measured$MEASURED)
#CREATE ERROR LIST
error <- ((x-y)*100/x)

a <- error
cat(paste(a))

png(filename="/home/scott/Sky_Estimates_R/test.png",width=20.0,height=12.0,units="cm",res=240)


magplot(RMS, error, xlab='Input Sky RMS', ylab='((Input Sky - Measured Sky)/Input Sky)
        *100', main='Percent Error in Sky Measurements')
dev.off()