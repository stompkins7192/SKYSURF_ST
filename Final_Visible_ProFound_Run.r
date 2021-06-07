###IMPORTANT!--> THERE ARE TWO DIFFERENT FITS FILE STRUCTURES FOR 
#WFC3IR VS WFC3 UVIS and ACSWFC IMAGES, MAKE SURE IR IMAGES AND VISIBLE IMAGES ARE IN
#DIFFERENT DIRECTORIES OR ELSE THE SCRIPT WILL FAIL, THERE IS AN IDENTICAL COPY WHICH
#DEALS WITH THE IR IMAGES

#THIS IS RUN ON WFC3UVIS AND ACS IMAGES

##Import Necessary Libraries, Give R More Cores to Run (Will Change Depending On Where
#You Run It)
start_time = Sys.time()
#library(doParallel)   #Ignore This library and line below for now, may work 
#registerDoParallel(cores=9)  #in the future.
library(ProFound)
library(data.table)
library(Rfits)
library(RColorBrewer)

#ASGR: General comment, rather than have things like SKY and SKY2, you should use SKY1$ and SKY2 (so name the first frame also). Makes it more obvious what is going on I think.
#ASGR: General comment, try to stick to either using "<-" or "=". For these purposes they functionally do the same thing, but using both will confuse non-R users (making them think there is something special going on. Probably stick to "=".

###True/False Toggles For Different Functions

#Toggling These On And Off Can Help With The Speed Of Bugfixing
#write_file works and is by far the slowest step

write_file = FALSE #Do You Want The ProFound Output FITS File? 
#WARNING OVERWRITES PREVIOUS  #DOES NOT WORK ON IR IMAGES #TAKES A LOT OF TIME
write_9panel_png= FALSE #Do You Want A Diagnostic Grid For The Image Saved As A PNG? 
#WARNING OVERWRITES PREVIOUS #WORKS ON IR IMAGES
write_CSV = FALSE #Do You Want A CSV Of Useful Data Output? 
#WARNING OVERWRITES PREVIOUS CSV WHEN RUN AGAIN #WORKS ON IR IMAGES
write_skymap_png = FALSE #Do You Want A SkyMap PNG With The Object and Bad Pixel Maps? 
#WARNING OVERWRITES PREVIOUS #WORKS ON IR IMAGES

#Set Working Directory - Change To Where Your Raw FITS Files Are Located

stub = "~/Desktop/ProFound_Visible_Outputs/"
setwd(paste0(stub))

##WORKS WITH #Debug Information In Case You Want To Change These Flags, Should Work 
#With All Possible Combinations
##MASK NO HEADER YES
##NO MASK NO HEADER YES
## MASK AND HEADER YES
## HEADER NO MASK YES

#Obtain A List of FITS Files In Your Specified Working Directory
files = dir(pattern="*.fits")
lst = vector("list", length(files))
obj = names(files)
lst = files

#Create Table for CSV Output File
df = data.frame(matrix(ncol = 17, nrow = length(lst)))
#x =  c("Instrument", 'Filter', "Image_Name", "Sky_Mean_PF", "Sky_SD", "Percent_Sky", 'Sky_RMS', 
#        'RMS_SD', 'Sky_Chi-Squared', 'Sky_LL', 'Sky_Stat_Corr', 'N_Objects', 'Image_Area', 
#        'EXPTIME', 'Expected_RMS', 'RMS_Error_%', 'BUNITS')
#colnames(df) = x


#Start A Loop To Read Each FITS File In the List
#foreach(i=1:length(lst), .combine=cbind) %dopar% {
#Leave this line above, may work in the future^.
for(i in 1:length(lst)){
  filename = paste( lst[i], sep="")
  print(filename)
  file_image = paste( lst[i], sep="")
  image1 = Rfits_read_image(filename = file_image, header = TRUE, ext = 2)
  image2 = Rfits_read_image(filename = file_image, header = TRUE, ext = 5)
  
  ###########
  ###Add Quadrant Offsets Here########
  ###########
  

  RF_image1 = Rfits_read_header(file_image, ext = 1)
  ext2_header = Rfits_read_header(file_image, ext = 2)
  ext3_header = Rfits_read_header(file_image, ext = 3)
  ext4_header = Rfits_read_header(file_image, ext = 4)
  ext5_header = Rfits_read_header(file_image, ext = 5)
  ext6_header= Rfits_read_header(file_image, ext = 6)
  ext7_header = Rfits_read_header(file_image, ext = 7)
  
  rootnames = list()
  rootnames = append(rootnames, list(RF_image1$keyvalues$ROOTNAME))

  maskimage = Rfits_read_image(filename = file_image, header = TRUE, ext = 4)
  maskimage2 = Rfits_read_image(filename = file_image, header = TRUE, ext = 7)

  mask = maskimage$imDat > 0
  mask2 = maskimage2$imDat > 0  #Mask All Bad Pixels, Works
  
  #Do Things With or Without Dilation Below, Without Is Bad!
  #ASGR: not sure you need the image, skycut or pixcut arguments. Pretty sure they won't be used for anything. Also, maybe as well just pass $objects into newmask and use this (makes more sense than extracting the segmentation part, which you do later)
  newmask = profoundMakeSegimDilate(segim = mask,  size = 3, shape = "disc")
  newmask2 = profoundMakeSegimDilate(segim = mask2,  size = 3, shape = "disc")

  ##SIZE OF DILATION ABOVE ^^^ (size =  N) MUST BE ODD AND SMALL, OR ELSE IT DOES NOT WORK

  #ASGR: a lot of these options are just the defaults. Pros and cons to listing them, but fine with you putting it in to make it all clearer.
  #ASGR: It will work like this, but really box and grid should be integer inputs, so wrap the inputs in "ceiling".
  
  ###CREATE A PROFOUND DATA OUTPUT STRUCTURE FOR EACH CHIP ON ACS OR WFC3UVIS
  SKY1 = profoundProFound(image1$imDat, verbose=FALSE, skycut = 1.5, pixcut = 4, 
                         SBdilate = 2, doclip = TRUE, redosky= TRUE, redoskysize= 13, 
                         type = 'bicubic', skytype='median', redosegim=TRUE, boxiters=6, mask=newmask$objects, 
                         box=c(ceiling(image1$keyvalues$NAXIS1 / 3), ceiling(image1$keyvalues$NAXIS2 / 3)), 
                         grid=c(ceiling(image1$keyvalues$NAXIS1 / 3), ceiling(image1$keyvalues$NAXIS2 / 3)),
                         header = NULL)
  
  SKY2 = profoundProFound(image2$imDat, verbose=FALSE, skycut = 1.5, pixcut = 4, 
                          SBdilate = 2, doclip = TRUE, redosky= TRUE, redoskysize= 13, 
                          type = 'bicubic', skytype='median', redosegim=TRUE, boxiters=6, mask=newmask2$objects, 
                          box=c(ceiling(image2$keyvalues$NAXIS1 / 3), ceiling(image2$keyvalues$NAXIS2 / 3)), 
                          grid=c(ceiling(image2$keyvalues$NAXIS1 / 3), ceiling(image2$keyvalues$NAXIS2 / 3)),
                          header = NULL)
  

  #ASGR: you can remove the numeric='32' from the integer images.
  
  if(isTRUE(write_file)){
    ##Write SkyMap 1 and Global Header as Extension
    Rfits_write_header(filename = paste0(stub,"FITS_Maps/",RF_image1$keyvalues$ROOTNAME,'_flc_profound.fits'), 
                       keyvalues = RF_image1$keyvalues, keycomments = RF_image1$keycomments, ext = 1,
                       create_file = TRUE,overwrite_file=TRUE)
    Rfits_write_key(filename = paste0(stub,"FITS_Maps/",RF_image1$keyvalues$ROOTNAME,'_flc_profound.fits'), keyname = 'EXTNAME', keyvalue = 'GLOBAL_HEADER', ext = 1)
    
    ###Write SkyMap 1 as Extension 2
    Rfits_write_image(SKY1$sky, filename = paste0(stub,"FITS_Maps/",RF_image1$keyvalues$ROOTNAME,'_flc_profound.fits'), 
                      compress = TRUE, create_ext = TRUE, create_file = FALSE, overwrite_file = FALSE)
    #Write the Header for this Extension (Copy of the Original)
    Rfits_write_header(filename = paste0(stub,"FITS_Maps/",RF_image1$keyvalues$ROOTNAME,'_flc_profound.fits'), 
                       keyvalues = ext2_header$keyvalues, ext = 2, create_ext = FALSE)
    
    #Properly Name the Extension
    Rfits_write_key(filename = paste0(stub,"FITS_Maps/",RF_image1$keyvalues$ROOTNAME,'_flc_profound.fits'), keyname = 'EXTNAME', keyvalue = 'SKY_MAP_1', ext = 2)
    
    ####Write SkyRMSMap 1 as Extension 3
    Rfits_write_image(SKY1$skyRMS, filename = paste0(stub,"FITS_Maps/",RF_image1$keyvalues$ROOTNAME,'_flc_profound.fits'), 
                       compress = TRUE, create_ext = TRUE, create_file = FALSE, overwrite_file = FALSE)
    ###Write the Header for this Extension (Copy of the Original)
    Rfits_write_header(filename = paste0(stub,"FITS_Maps/",RF_image1$keyvalues$ROOTNAME,'_flc_profound.fits'), 
                       keyvalues = ext2_header$keyvalues, ext = 3, create_ext = FALSE)
    
    #Properly Name the Extension
    Rfits_write_key(filename = paste0(stub,"FITS_Maps/",RF_image1$keyvalues$ROOTNAME,'_flc_profound.fits'), keyname = 'EXTNAME', keyvalue = 'SKYRMS_MAP_1', ext = 3)

    ###Write the Bad Pixel Map as Extension 4 (MAKE INTEGERS 16 BIT TO SAVE SPACE, IT IS A BINARY ARRAY)
    Rfits_write_image(SKY1$mask, filename = paste0(stub,"FITS_Maps/",RF_image1$keyvalues$ROOTNAME,'_flc_profound.fits'), 
                      compress = FALSE, create_ext = TRUE, create_file = FALSE, overwrite_file = FALSE,
                      integer = '16')
    ##Write the Header for this Extension (Copy of the Original )
    Rfits_write_header(filename = paste0(stub,"FITS_Maps/",RF_image1$keyvalues$ROOTNAME,'_flc_profound.fits'), 
                       keyvalues = ext2_header$keyvalues, ext = 4, create_ext = FALSE)
    
    #Properly Name the Extension
    Rfits_write_key(filename = paste0(stub,"FITS_Maps/",RF_image1$keyvalues$ROOTNAME,'_flc_profound.fits'), keyname = 'EXTNAME', keyvalue = 'BAD_PIXEL_MAP_1', ext = 4)
    
    
    ###Write the First Object Map as Extension 5 (MAKE INTEGERS 16 BIT TO SAVE SPACE, IT IS A BINARY ARRAY)
    Rfits_write_image(SKY1$objects_redo, filename = paste0(stub,"FITS_Maps/",RF_image1$keyvalues$ROOTNAME,'_flc_profound.fits'), 
                      compress = FALSE, create_ext = TRUE, create_file = FALSE, overwrite_file = FALSE,
                      integer = '16')
    ##Write the Header for this Extension (Copy of the Original )
    Rfits_write_header(filename = paste0(stub,"FITS_Maps/",RF_image1$keyvalues$ROOTNAME,'_flc_profound.fits'), 
                       keyvalues = ext2_header$keyvalues, ext = 5, create_ext = FALSE)
    
    #Properly Name the Extension
    Rfits_write_key(filename = paste0(stub,"FITS_Maps/",RF_image1$keyvalues$ROOTNAME,'_flc_profound.fits'), keyname = 'EXTNAME', keyvalue = 'OBJECTS_REDO_1', ext = 5)
    
    ###Repeat All for Second ACS Chip
  
    ###Write SkyMap 2 as Extension 6
    Rfits_write_image(SKY2$sky, filename = paste0(stub,"FITS_Maps/",RF_image1$keyvalues$ROOTNAME,'_flc_profound.fits'), 
                      compress = TRUE, create_ext = TRUE, create_file = FALSE, overwrite_file = FALSE)
    #Write the Header for this Extension (Copy of the Original)
    Rfits_write_header(filename = paste0(stub,"FITS_Maps/",RF_image1$keyvalues$ROOTNAME,'_flc_profound.fits'), 
                       keyvalues = ext5_header$keyvalues, ext = 6, create_ext = FALSE)
    
    #Properly Name the Extension
    Rfits_write_key(filename = paste0(stub,"FITS_Maps/",RF_image1$keyvalues$ROOTNAME,'_flc_profound.fits'), keyname = 'EXTNAME', keyvalue = 'SKY_MAP_2', ext = 6)
    
    ####Write SkyRMSMap 1 as Extension 7
    Rfits_write_image(SKY2$skyRMS, filename = paste0(stub,"FITS_Maps/",RF_image1$keyvalues$ROOTNAME,'_flc_profound.fits'), 
                      compress = TRUE, create_ext = TRUE, create_file = FALSE, overwrite_file = FALSE)
    ###Write the Header for this Extension (Copy of the Original)
    Rfits_write_header(filename = paste0(stub,"FITS_Maps/",RF_image1$keyvalues$ROOTNAME,'_flc_profound.fits'), 
                       keyvalues = ext5_header$keyvalues, ext = 7, create_ext = FALSE)
    
    #Properly Name the Extension
    Rfits_write_key(filename = paste0(stub,"FITS_Maps/",RF_image1$keyvalues$ROOTNAME,'_flc_profound.fits'), keyname = 'EXTNAME', keyvalue = 'SKYRMS_MAP_2', ext = 7)
    
    ###Write the Bad Pixel Map 2 as Extension 8 (MAKE INTEGERS 16 BIT TO SAVE SPACE, IT IS A BINARY ARRAY)
    Rfits_write_image(SKY2$mask, filename = paste0(stub,"FITS_Maps/",RF_image1$keyvalues$ROOTNAME,'_flc_profound.fits'), 
                      compress = FALSE, create_ext = TRUE, create_file = FALSE, overwrite_file = FALSE,
                      integer = '16')
    ##Write the Header for this Extension (Copy of the Original )
    Rfits_write_header(filename = paste0(stub,"FITS_Maps/",RF_image1$keyvalues$ROOTNAME,'_flc_profound.fits'), 
                       keyvalues = ext5_header$keyvalues, ext = 8, create_ext = FALSE)
    
    #Properly Name the Extension
    Rfits_write_key(filename = paste0(stub,"FITS_Maps/",RF_image1$keyvalues$ROOTNAME,'_flc_profound.fits'), keyname = 'EXTNAME', keyvalue = 'BAD_PIXEL_MAP_2', ext = 8)
    
    

    ###Write the Second Chip Object Map as Extension 9 (MAKE INTEGERS 16 BIT TO SAVE SPACE, IT IS A BINARY ARRAY)
    Rfits_write_image(SKY2$objects_redo, filename = paste0(stub,"FITS_Maps/",RF_image1$keyvalues$ROOTNAME,'_flc_profound.fits'), 
                      compress = FALSE, create_ext = TRUE, create_file = FALSE, overwrite_file = FALSE,
                      integer = '16')
    ##Write the Header for this Extension (Copy of the Original )
    Rfits_write_header(filename = paste0(stub,"FITS_Maps/",RF_image1$keyvalues$ROOTNAME,'_flc_profound.fits'), 
                       keyvalues = ext5_header$keyvalues, ext = 9, create_ext = FALSE)
    
    #Properly Name the Extension
    Rfits_write_key(filename = paste0(stub,"FITS_Maps/",RF_image1$keyvalues$ROOTNAME,'_flc_profound.fits'), keyname = 'EXTNAME', keyvalue = 'OBJECTS_REDO_2', ext = 9)

  }
    #END OF OUTPUT FITS FILE PRODUCTION CODE
  
  
  ##WRITE A CSV
  
  #ASGR: Not sure you need to round these numbers just for writing to a CSV. I only do it so it displays in a more readable way on the plots.
  #ASGR: general comment, as long as you variable is boolean (TRUE/FALSE) you don't also need to wrap it in a "isTRUE:
  if(isTRUE(write_CSV)){
  
  #Calculate All Values Going Into CSV
  stat_mean = signif(mean(SKY1$sky[SKY1$mask==0], na.rm=TRUE),8)
  
  stat_sd = signif(sd(SKY1$sky[SKY1$mask==0], na.rm=TRUE),8)
  
  statrms_mean = signif(mean(SKY1$skyRMS[SKY1$mask==0], na.rm=TRUE),8)
  statrms_sd = signif(sd(SKY1$skyRMS[SKY1$mask==0], na.rm=TRUE),8)
  stat_cor = signif(cor(as.numeric(SKY1$sky[SKY1$mask==0]), as.numeric(SKY1$skyRMS[SKY1$mask==0])^2, use="pairwise.complete.obs"),8)
  not_sky =   (SKY1$mask)+(SKY1$objects_redo)
  maxvalue = 1.01
  not_sky[not_sky > maxvalue] = 1
  good_pix = length(image1$imDat) - (sum(not_sky))
  
  rni = c(RF_image1$keyvalues$READNSEC, RF_image1$keyvalues$READNSED)
  readnoise = mean(rni, na.rm = TRUE)

  sky_background = SKY1$sky
  fake_rms = profoundMakeSigma(image = sky_background,
                               exptime = RF_image1$keyvalues$EXPTIME, image_units = 'elec',
                               sky_units = 'elec', readRMS = readnoise, output_units = 'elec', skycut = 1.5)
  expected_RMS = mean(fake_rms, na.rm = TRUE)
  
  RMS_erorr = 100 * mean(abs(log10(SKY1$skyRMS/fake_rms) * log(10)), na.rm = TRUE)
  percent_sky = (good_pix / length(image1$imDat))*100 #Calculate the Percentage of Sky Pixels
  percent_sky = signif(percent_sky, digits = 4) 

  

  
 # ("Filter", 'Instrument',"Image_Name", "Sky_Mean_PF", "Sky_SD", "Percent_Sky", 'Sky_RMS', 
  #  'RMS_SD', 'Sky_Chi-Squared', 'Sky_LL', 'Sky_Stat_Corr', 'N_Objects', 'Image_Area', 
 #   'EXPTIME', 'Expected_RMS', 'RMS_Error_%', 'BUNITS')
  #  df = data.frame(image1$keyvalues$ROOTNAME, stat_mean, stat_sd, percent_sky)

  df[i , 1] = RF_image1$keyvalues$INSTRUME #File Name
  if(RF_image1$keyvalues$INSTRUME == "WFC3"){   #KeyValue Names Are Slightly Different For ACS vs WFC3UVIS
    df[i , 2] = RF_image1$keyvalues$FILTER}
  if(RF_image1$keyvalues$INSTRUME == "ACS"){
    df[i , 2] = RF_image1$keyvalues$FILTER1}
  df[i , 3] = image1$keyvalues$ROOTNAME #File Name
  df[i , 4] = stat_mean  #Mean Sky Value
  df[i , 5] = stat_sd #Sky SD
  df[i , 6] = percent_sky   #Percent of Sky Pixels, Not Objects or Bad Pixels
  df[i , 7]  =  statrms_mean #Mean RMS Value 
  df[i , 8]  =  statrms_sd  #RMS SD
  df[i , 9]  =  SKY1$skyChiSq  #Sky Fit Chi-Squared Value
  df[i , 10]  =  SKY1$skyLL   #Sky Fit Log-Likelyhood
  df[i , 11]  =  stat_cor   #Correlation Stat Between RMS and Sky Maps
  df[i , 12]  =  length(SKY1$segstats$Nobject)  #Number Of Objects Detected In Image
  df[i , 13]  =  SKY1$imarea  #Image Area (In Square Degrees, Requires Header = TRUE)
  df[i , 14]  =  RF_image1$keyvalues$EXPTIME #Image Exposure Time
  df[i , 15]  =   expected_RMS  #RMS Value Expected Based On Known Info (Not 100% Correct Yet)
  df[i , 16]  =   RMS_erorr #% Error For Expected Versus Calculated RMS
  df[i , 17]  =   image1$keyvalues$BUNIT #Brightness Units

  write.table(df,file = paste0(stub,"Image_Info/Image_Info_Chip1.csv"), 
              row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE, sep = ",") #Write DF to CSV (Goes To Path_Out Directory By Default)
  #CSV IS CONSTRUCTED IN THE ORDER THE FITS FILES ARE READ
  
  ###END OF WRITE CSV CODE
  }
  
  ##BEGINNING OF MAKE SKYMAP CODE
  
  if(isTRUE(write_skymap_png)){


  ##Define Needed Statistics
  not_sky =   (SKY1$mask) + (SKY1$objects_redo)
  maxvalue = 1.01
  not_sky[not_sky > maxvalue] = 1
  good_pix = length(image1$imDat) - (sum(not_sky))
  
  percent_sky = (good_pix / length(image1$imDat))*100 #Calculate the Percentage of Sky Pixels
  percent_sky = signif(percent_sky, digits = 4) 

  png(filename = paste0(stub,"SkyMap_PNGs/SkyMap_", 
                        RF_image1$keyvalues$ROOTNAME , ".png"), width = 1280, 
      height = 1280, units = "px")

    #Does Not Take Header Into Account! Mapped Pixel By Pixel
  if(!is.null(SKY1$mask)){

    magimage(SKY1$sky-median(SKY1$sky,na.rm=TRUE), qdiff=TRUE,  cex.lab=2.5, cex.axis=1.5, side = 1:4)
    #Optional Toggle (Show Objects_Redo Map)
    magimage(SKY1$objects_redo!=0, col=c(NA,rgb(0/255, 0/255, 0/255, alpha = 0.5)), add=TRUE, magmap=FALSE, zlim=c(0,1), cex.lab=2.5, cex.axis=1.5, side = 1:4)
    
    #Mask (Keep)
    magimage(SKY1$mask!=0, col=c(NA,rgb(204/255, 0/255, 0/255, alpha = 0.5)), add=TRUE, magmap=FALSE, zlim=c(0,1), cex.lab=2.5, cex.axis=1.5, side = 1:4)
    
    suppressWarnings({
      stat_mean = signif(mean(SKY1$sky[SKY1$mask==0], na.rm=TRUE),8)
      stat_sd = signif(sd(SKY1$sky[SKY1$mask==0], na.rm=TRUE),8)
      stat_cor = signif(cor(as.numeric(SKY1$sky[SKY1$mask==0]), as.numeric(SKY1$skyRMS[SKY1$mask==0])^2, use="pairwise.complete.obs"),8)
      legend('topleft',legend=c('Sky',paste0('Mean: ',stat_mean),paste0('SD: ',stat_sd),paste0('% Of Sky Pixels: ',percent_sky)),bg='white',cex=1.5)
      })
      #END OF SKYMAP PNG PRODUCTION CODE
    dev.off(2)
  }
  
    if(isTRUE(write_9panel_png)){
   
      png(filename = paste0(stub,"9_Panel_PNGs/D_Grid_", 
                            RF_image1$keyvalues$ROOTNAME , ".png"), width = 1280, 
          height = 1280, units = "px")
      plot(SKY1)
      dev.off(2)
      }
  }
  ###REPEAT LAST 3 OPERATIONS FOR CHIP 2
  
  if(isTRUE(write_CSV)){
    
    #Calculate All Values Going Into CSV
    stat_mean = signif(mean(SKY2$sky[SKY2$mask==0], na.rm=TRUE),8)
    
    stat_sd = signif(sd(SKY2$sky[SKY2$mask==0], na.rm=TRUE),8)
    
    statrms_mean = signif(mean(SKY2$skyRMS[SKY2$mask==0], na.rm=TRUE),8)
    statrms_sd = signif(sd(SKY2$skyRMS[SKY2$mask==0], na.rm=TRUE),8)
    stat_cor = signif(cor(as.numeric(SKY2$sky[SKY2$mask==0]), as.numeric(SKY2$skyRMS[SKY2$mask==0])^2, use="pairwise.complete.obs"),8)
    not_sky =   (SKY2$mask) + (SKY2$objects_redo)
    maxvalue = 1.01
    not_sky[not_sky > maxvalue] = 1
    good_pix = length(image2$imDat) - (sum(not_sky))
    
    readnoise2 = mean(c(RF_image1$keyvalues$READNSEA, RF_image1$keyvalues$READNSEB), na.rm = TRUE)
    
    sky_background2 = SKY2$sky
    fake_rms2 = profoundMakeSigma(image = sky_background2,
                                 exptime = RF_image1$keyvalues$EXPTIME, image_units = 'elec',
                                 sky_units = 'elec', readRMS = readnoise2, output_units = 'elec', skycut = 1.5)
    expected_RMS2 = mean(fake_rms2, na.rm = TRUE)
    
    RMS_erorr2 = 100 * mean(abs(log10(SKY2$skyRMS/fake_rms2) * log(10)), na.rm = TRUE)
    
    percent_sky = (good_pix / length(image2$imDat))*100 #Calculate the Percentage of Sky Pixels
    percent_sky = signif(percent_sky, digits = 8) 
    

    
    # ("Filter", 'Instrument',"Image_Name", "Sky_Mean_PF", "Sky_SD", "Percent_Sky", 'Sky_RMS', 
    #  'RMS_SD', 'Sky_Chi-Squared', 'Sky_LL', 'Sky_Stat_Corr', 'N_Objects', 'Image_Area', 
    #   'EXPTIME', 'Expected_RMS', 'RM2S_Error_%', 'BUNITS')
    #  df = data.frame(image1$keyvalues$ROOTNAME, stat_mean, stat_sd, percent_sky)
    
    df[i , 1] = RF_image1$keyvalues$INSTRUME #File Name
    if(RF_image1$keyvalues$INSTRUME == "WFC3"){   #KeyValue Names Are Slightly Different For ACS vs WFC3UVIS
      df[i , 2] = RF_image1$keyvalues$FILTER}
    if(RF_image1$keyvalues$INSTRUME == "ACS"){
      df[i , 2] = RF_image1$keyvalues$FILTER1}
    df[i , 3] = image2$keyvalues$ROOTNAME #File Name
    df[i , 4] = stat_mean  #Mean Sky Value
    df[i , 5] = stat_sd #Sky SD
    df[i , 6] = percent_sky   #Percent of Sky Pixels, Not Objects or Bad Pixels
    df[i , 7]  =  statrms_mean #Mean RMS Value 
    df[i , 8]  =  statrms_sd  #RMS SD
    df[i , 9]  =  SKY2$skyChiSq  #Sky Fit Chi-Squared Value
    df[i , 10]  =  SKY2$skyLL   #Sky Fit Log-Likelyhood
    df[i , 11]  =  stat_cor   #Correlation Stat Between RMS and Sky Maps
    df[i , 12]  =  length(SKY2$segstats$Nobject)  #Number Of Objects Detected In Image
    df[i , 13]  =  SKY2$imarea  #Image Area (In Square Degrees, Requires Header = TRUE)
    df[i , 14]  =  RF_image1$keyvalues$EXPTIME #Image Exposure Time
    df[i , 15]  =   expected_RMS2  #RMS Value Expected Based On Known Info (Not 100% Correct Yet)
    df[i , 16]  =   RMS_erorr2 #% Error For Expected Versus Calculated RMS
    df[i , 17]  =   image2$keyvalues$BUNIT #Brightness Units
    write.table(df,file = paste0(stub,"Image_Info/Image_Info_Chip2.csv"), 
                row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE, sep = ",") #Write DF to CSV (Goes To Path_Out Directory By Default)
    #CSV IS CONSTRUCTED IN THE ORDER THE FITS FILES ARE READ
    
    ###END OF WRITE CSV CODE
  }
  
  ##BEGINNING OF MAKE SKYMAP CODE
  
  if(isTRUE(write_skymap_png)){
    
    
    ##Define Needed Statistics
    not_sky =   (SKY2$mask) + (SKY2$objects_redo)
    maxvalue = 1.01
    not_sky[not_sky > maxvalue] = 1
    good_pix = length(image2$imDat) - (sum(not_sky))
    
    percent_sky = (good_pix / length(image2$imDat))*100 #Calculate the Percentage of Sky Pixels
    percent_sky = signif(percent_sky, digits = 4) 

    png(filename = paste0(stub,"SkyMap_PNGs/SkyMapChip2_", 
                          RF_image1$keyvalues$ROOTNAME , ".png"), width = 1280, 
        height = 1280, units = "px")
    
    #Does Not Take Header Into Account! Mapped Pixel By Pixel
    if(!is.null(SKY2$mask)){
      
      magimage(SKY2$sky-median(SKY2$sky,na.rm=TRUE), qdiff=TRUE,  cex.lab=2.5, cex.axis=1.5, side = 1:4)
      #Optional Toggle (Show Objects_Redo Map)
      magimage(SKY2$objects_redo!=0, col=c(NA,rgb(0/255, 0/255, 0/255, alpha = 0.5)), add=TRUE, magmap=FALSE, zlim=c(0,1), cex.lab=2.5, cex.axis=1.5, side = 1:4)
      
      #Mask (Keep)
      magimage(SKY2$mask!=0, col=c(NA,rgb(204/255, 0/255, 0/255, alpha = 0.5)), add=TRUE, magmap=FALSE, zlim=c(0,1), cex.lab=2.5, cex.axis=1.5, side = 1:4)
      
      suppressWarnings({
        stat_mean = signif(mean(SKY2$sky[SKY2$mask==0], na.rm=TRUE),4)
        stat_sd = signif(sd(SKY2$sky[SKY2$mask==0], na.rm=TRUE),4)
        stat_cor = signif(cor(as.numeric(SKY2$sky[SKY2$mask==0]), as.numeric(SKY2$skyRMS[SKY2$mask==0])^2, use="pairwise.complete.obs"),4)
        legend('topleft',legend=c('Sky',paste0('Mean: ',stat_mean),paste0('SD: ',stat_sd),paste0('% Of Sky Pixels: ',percent_sky)),bg='white',cex=1.5)
      })
      #END OF SKYMAP PNG PRODUCTION CODE
      dev.off(2)
    }
    
    if(isTRUE(write_9panel_png)){
      
      png(filename = paste0(stub,"9_Panel_PNGs/D_Grid_Chip2", 
                            RF_image1$keyvalues$ROOTNAME , ".png"), width = 1280, 
          height = 1280, units = "px")
      plot(SKY2)
      dev.off(2)
    }
  }
}
end_time = Sys.time()
runtime = end_time - start_time
cat(paste0("Runtime In Seconds_" ,runtime, sep="\n"))
dev.off(2) ##Turn Off Plot Saving

###IF THE CODE ISINT SAVING PLOTS, TYPE
#dev.list() IN THE CONSOLE AND DO
#dev.off(N) FOR ALL N SHOWN
