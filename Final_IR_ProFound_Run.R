###IMPORTANT!--> THERE ARE TWO DIFFERENT FITS FILE STRUCTURES FOR 
#WFC3IR VS WFC3 UVIS and ACSWFC IMAGES, MAKE SURE IR IMAGES AND VISIBLE IMAGES ARE IN
#DIFFERENT DIRECTORIES OR ELSE THE SCRIPT WILL FAIL, THERE IS AN IDENTICAL COPY WHICH
#DEALS WITH THE VISIBLE (ACS AND WFC3) IMAGES


#THIS IS RUN ON WFC3IR IMAGES

##Import Necessary Libraries, Give R More Cores to Run (Will Change Depending On Where
#You Run It)
#library(doParallel) #Ignore This library and line below for now, may work 
#registerDoParallel(cores=6) #in the future.
library(ProFound)
library(data.table)
library(Rfits)
library(RColorBrewer)
start_time = Sys.time()

#ASGR: General comment, try to stick to either using "=" or "=". 
#For these purposes they functionally do the same thing, but using both will confuse non-R users (making them think there is something special going on. Probably stick to "=".

###True/False Toggles For Different Functions 

#Toggling These On And Off Can Help With The Speed Of BugFixing
#write_file works and is by far the slowest step

write_file = TRUE #Do You Want The ProFound Output FITS File? 
#WARNING OVERWRITES PREVIOUS  #DOES NOT WORK ON VISIBLE IMAGES #TAKES A LOT OF TIME

write_9panel_png = TRUE #Do You Want A Diagnostic Grid For The Image Saved As A PNG? 
#WARNING OVERWRITES PREVIOUS #WORKS ON IR IMAGES

write_CSV = TRUE #Do You Want A CSV Of Useful Data Output? 
#WARNING OVERWRITES PREVIOUS #WORKS ON IR IMAGES

write_skymap_png = TRUE #Do You Want A SkyMap PNG With The Object and Bad Pixel Maps? 
#WARNING OVERWRITES PREVIOUS #WORKS ON IR IMAGES


#Set Working Directory - Change To Where Your Raw FITS Files Are Located (temp_unzip)
stub = "~/Desktop/ProFound_IR_Outputs/"
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
x =  c("Instrument", 'Filter', "Image_Name", "Sky_Mean_PF", "Sky_SD", "Percent_Sky", 'Sky_RMS', 
        'RMS_SD', 'Sky_Chi-Squared', 'Sky_LL', 'Sky_Stat_Corr', 'N_Objects', 'Image_Area', 
        'EXPTIME', 'Expected_RMS', 'RMS_Error_%', 'BUNITS')
colnames(df) = x


#Star#t A Loop To Read Each FITS File In the List
for(i in 1:length(lst)){
#foreach(i=1:length(lst), .combine=cbind) %dopar% { 
  #Leave This Commented Out Code Line (Might Work
  #In The Future)

  filename = paste( lst[i], sep="")
  print(filename)

  image = Rfits_read_array(filename = filename, header = TRUE, ext = 2)
  
  ###########
  ###Add Quadrant Offsets Here########
  ###########


  RF_image = Rfits_read_header(filename, ext = 1)
  ext2_header = Rfits_read_header(filename, ext = 2)
  ext3_header = Rfits_read_header(filename, ext = 3)
  ext4_header = Rfits_read_header(filename, ext = 4)
  
  
  rootnames = list()
  rootnames = append(rootnames, list(RF_image$keyvalues$ROOTNAME))

  maskimage = Rfits_read_image(lst[i], ext = 4, header = TRUE)

  mask = maskimage$imDat > 0

  
  #Do Things With or Without Dilation Below, Without Is Bad!
  #ASGR: not sure you need the image, skycut or pixcut arguments. 
  #Pretty sure they won't be used for anything. Also, maybe as well just pass 
  #$objects into newmask and use this
  #(makes more sense than extracting the segmentation part, which you do later)
  newmask = profoundMakeSegimDilate(segim = mask,  size = 3, shape = "disc")

  ##SIZE OF DILATION ABOVE ^^^ (size =  N) MUST BE ODD AND SMALL, OR ELSE IT DOES NOT WORK
  

  # mask = NULL
  
  #ASGR: a lot of these options are just the defaults. Pros and cons to listing them, but fine with you putting it in to make it all clearer.
  
  SKY = profoundProFound(image$imDat, verbose=FALSE, skycut = 1.5, pixcut = 4, 
                         SBdilate = 2, doclip = TRUE, redosky= TRUE, redoskysize= 13, 
                         type = 'bicubic', skytype='median', redosegim=TRUE, boxiters=6, mask=newmask$segim, 
                         box=c(image$keyvalues$NAXIS1 / 3,image$keyvalues$NAXIS2 / 3), 
                         grid=c(image$keyvalues$NAXIS1 / 3,image$keyvalues$NAXIS2 / 3),
                         header = NULL)

  
  
  
  ##PLOTTING
  
  ##WCS PLOTS WORK WITH HEADER
  

  
  #ABOVE WORKS
 
  #ASGR: Is it always the case that objects_redo cannot overlap with mask? To be safe you could do good_pix = length(image$imDat) - length(which(SKY$mask > 0 | SKY$objects_redo > 0))

  good_pix = length(image$imDat) - length(which(SKY$mask > 0 | SKY$objects_redo > 0))
  percent_sky= (good_pix / length(image$imDat))*100 #Calculate the Percentage of Sky Pixels
  percent_sky = signif(percent_sky, digits = 4) 
  ###WRITE FITS SKYMAP (OPTIONAL)
 
  ####
  
  #Extension Headers
  
  #ASGR: you can remove the numeric='32' from the integer images.
  
  #WRITE EXTENSIONS
  if(isTRUE(write_file)){
   
    #Write Copy Of FITS File

    
    ##Write SkyMap 1 and Global Header as Extension
    Rfits_write_header(filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_flt_profound.fits'), 
                       keyvalues = RF_image$keyvalues, keycomments = RF_image$keycomments, ext = 1,
                       create_file = TRUE)
    Rfits_write_key(filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_flt_profound.fits'), keyname = 'EXTNAME', keyvalue = 'GLOBAL_HEADER', ext = 1)
    
    ###Write SkyMap 1 as Extension 2
    Rfits_write_image(SKY$sky, filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_flt_profound.fits'), 
                      compress = TRUE, create_ext = TRUE, create_file = FALSE, overwrite_file = FALSE)
    #Write the Header for this Extension (Copy of the Original)
    Rfits_write_header(filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_flt_profound.fits'), 
                       keyvalues = ext2_header$keyvalues, ext = 2, create_ext = FALSE)
    
    #Properly Name the Extension
    Rfits_write_key(filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_flt_profound.fits'), keyname = 'EXTNAME', keyvalue = 'SKY_MAP_1', ext = 2)
    
    ####Write SkyRMSMap 1 as Extension 3
    Rfits_write_image(SKY$skyRMS, filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_flt_profound.fits'), 
                      compress = TRUE, create_ext = TRUE, create_file = FALSE, overwrite_file = FALSE)
    ###Write the Header for this Extension (Copy of the Original)
    Rfits_write_header(filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_flt_profound.fits'), 
                       keyvalues = ext2_header$keyvalues, ext = 3, create_ext = FALSE)
    
    #Properly Name the Extension
    Rfits_write_key(filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_flt_profound.fits'), keyname = 'EXTNAME', keyvalue = 'SKYRMS_MAP_1', ext = 3)
    
    
    ###Write the Bad Pixel Map as Extension 4 (MAKE INTEGERS 16 BIT TO SAVE SPACE, IT IS A BINARY ARRAY)
    Rfits_write_image(SKY$mask, filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_flt_profound.fits'), 
                      compress = FALSE, create_ext = TRUE, create_file = FALSE, overwrite_file = FALSE,
                      integer = '16')
    ##Write the Header for this Extension (Copy of the Original )
    Rfits_write_header(filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_flt_profound.fits'), 
                       keyvalues = ext2_headerr$keyvalues, ext = 4, create_ext = FALSE)
    
    #Properly Name the Extension
    Rfits_write_key(filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_flt_profound.fits'), keyname = 'EXTNAME', keyvalue = 'BAD_PIXEL_MAP_1', ext = 4)
    
    ###Write the Object Map as Extension 5 (MAKE INTEGERS 16 BIT TO SAVE SPACE, IT IS A BINARY ARRAY)
    Rfits_write_image(SKY$objects_redo, filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_flt_profound.fits'), 
                      compress = FALSE, create_ext = TRUE, create_file = FALSE, overwrite_file = FALSE,
                      integer = '16')
    ##Write the Header for this Extension (Copy of the Original )
    Rfits_write_header(filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_flt_profound.fits'), 
                       keyvalues = ext2_header$keyvalues, ext = 5, create_ext = FALSE)
    
    #Properly Name the Extension
    Rfits_write_key(filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_flt_profound.fits'), keyname = 'EXTNAME', keyvalue = 'OBJECTS_REDO_1', ext = 5)
    
   
  }
  #END OF OUTPUT FITS FILE PRODUCTION CODE
  
  
  ##WRITE A CSV
  
  #ASGR: Not sure you need to round these numbers just for writing to a CSV. I only do it so it displays in a more readable way on the plots.
  #ASGR: general comment, as long as you variable is boolean (TRUE/FALSE) you don't also need to wrap it in a "isTRUE:
  if(isTRUE(write_CSV)){
    
    #Calculate All Values Going Into CSV
    stat_mean = signif(mean(SKY$sky[SKY$mask==0], na.rm=TRUE),8)
    
    stat_sd = signif(sd(SKY$sky[SKY$mask==0], na.rm=TRUE),8)
    
    statrms_mean = signif(mean(SKY$skyRMS[SKY$mask==0], na.rm=TRUE),8)
    statrms_sd = signif(sd(SKY$skyRMS[SKY$mask==0], na.rm=TRUE),8)
    stat_cor = signif(cor(as.numeric(SKY$sky[SKY$mask==0]), as.numeric(SKY$skyRMS[SKY$mask==0])^2, use="pairwise.complete.obs"),8)
    not_sky =   (SKY$mask)+(SKY$objects_redo)
    maxvalue = 1.01
    not_sky[not_sky > maxvalue] = 1
    good_pix = length(image$imDat) - (sum(not_sky))
    
    readnoise = mean(c(RF_image$keyvalues$READNSEA, RF_image$keyvalues$READNSEB, 
                     RF_image$keyvalues$READNSEC, RF_image$keyvalues$READNSED), na.rm = TRUE)

    
    sky_background = SKY$sky
    fake_rms = profoundMakeSigma(image = sky_background,
                                 exptime = RF_image$keyvalues$EXPTIME, image_units = 'elec/s',
                                 readRMS = readnoise, output_units = 'elec/s', skycut = 1.5, read_units = 'elec')
                                 
                       
    
    expected_RMS = mean(fake_rms, na.rm = TRUE)

    RMS_erorr = 100 * mean(abs(log10(SKY$skyRMS/fake_rms) * log(10)), na.rm = TRUE)
    percent_sky = (good_pix / length(image$imDat))*100 #Calculate the Percentage of Sky Pixels
    percent_sky = signif(percent_sky, digits = 8) 
    

    # ("Filter", 'Instrument',"Image_Name", "Sky_Mean_PF", "Sky_SD", "Percent_Sky", 'Sky_RMS', 
    #  'RMS_SD', 'Sky_Chi-Squared', 'Sky_LL', 'Sky_Stat_Corr', 'N_Objects', 'Image_Area', 
    #   'EXPTIME', 'Expected_RMS', 'RMS_Error_%', "BUNITS")

    df[i , 1] = RF_image$keyvalues$INSTRUME #File Name
    if(RF_image$keyvalues$INSTRUME == "WFC3"){   #KeyValue Names Are Slightly Different For ACS vs WFC3UVIS
      df[i , 2] = RF_image$keyvalues$FILTER}
    df[i , 3] = image$keyvalues$ROOTNAME #File Name
    df[i , 4] = stat_mean  #Mean Sky Value
    df[i , 5] = stat_sd #Sky SD
    df[i , 6] = percent_sky   #Percent of Sky Pixels, Not Objects or Bad Pixels
    df[i , 7]  =  statrms_mean #Mean RMS Value 
    df[i , 8]  =  statrms_sd  #RMS SD
    df[i , 9]  =  SKY$skyChiSq  #Sky Fit Chi-Squared Value
    df[i , 10]  =  SKY$skyLL   #Sky Fit Log-Likelyhood
    df[i , 11]  =  stat_cor   #Correlation Stat Between RMS and Sky Maps
    df[i , 12]  =  length(SKY$segstats$Nobject)  #Number Of Objects Detected In Image
    df[i , 13]  =  SKY$imarea  #Image Area (In Square Degrees, Requires Header = TRUE)
    df[i , 14]  =  RF_image$keyvalues$EXPTIME #Image Exposure Time
    df[i , 15]  =   expected_RMS  #RMS Value Expected Based On Known Info (Not 100% Correct Yet)
    df[i , 16]  =   RMS_erorr #% Error For Expected Versus Calculated RMS
    df[i , 17]  =   image$keyvalues$BUNIT #Brightness Units

    write.table(df,file = paste0(stub,"Image_Info/Image_Info.csv"), 
              row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE, sep = ",") #Write DF to CSV
    
    #CSV IS CONSTRUCTED IN THE ORDER THE FITS FILES ARE READ
    
    ###END OF WRITE CSV CODE
  }
  
  ##BEGINNING OF MAKE SKYMAP CODE
  
  if(isTRUE(write_skymap_png)){
    
    
    ##Define Needed Statistics
    not_sky =   (SKY$mask)+(SKY$objects_redo)
    maxvalue = 1.01
    not_sky[not_sky > maxvalue] = 1
    good_pix = length(image$imDat) - (sum(not_sky))
    
    percent_sky= (good_pix / length(image$imDat))*100 #Calculate the Percentage of Sky Pixels
    percent_sky = signif(percent_sky, digits = 4) 
    
    ####NEED TO CHANGE PILE OUTPUT LOCATION TO YOUR CHOSEN PATH
    
    
    png(filename = paste0(stub,"SkyMap_PNGs/SkyMap_", 
                          RF_image$keyvalues$ROOTNAME , ".png"), width = 1280, 
        height = 1280, units = "px")
    
    
    
    #Does Not Take Header Into Account! Mapped Pixel By Pixel
    if(!is.null(SKY$mask)){
      
      magimage(SKY$sky-median(SKY$sky,na.rm=TRUE), qdiff=TRUE,  cex.lab=2.5, cex.axis=1.5, side = 1:4)
      #Optional Toggle (Show Objects_Redo Map)
      magimage(SKY$objects_redo!=0, col=c(NA,rgb(0/255, 0/255, 0/255, alpha = 0.5)), add=TRUE, magmap=FALSE, zlim=c(0,1), cex.lab=2.5, cex.axis=1.5, side = 1:4)
      
      #Mask (Keep)
      magimage(SKY$mask!=0, col=c(NA,rgb(204/255, 0/255, 0/255, alpha = 0.5)), add=TRUE, magmap=FALSE, zlim=c(0,1), cex.lab=2.5, cex.axis=1.5, side = 1:4)
      
      suppressWarnings({
        stat_mean = signif(mean(SKY$sky[SKY$mask==0], na.rm=TRUE),4)
        stat_sd = signif(sd(SKY$sky[SKY$mask==0], na.rm=TRUE),4)
        stat_cor = signif(cor(as.numeric(SKY$sky[SKY$mask==0]), as.numeric(SKY$skyRMS[SKY$mask==0])^2, use="pairwise.complete.obs"),4)
        legend('topleft',legend=c('Sky',paste0('Mean: ',stat_mean),paste0('SD: ',stat_sd),paste0('% Of Sky Pixels: ',percent_sky)),bg='white',cex=1.5)
      })
      #END OF SKYMAP PNG PRODUCTION CODE
      dev.off(2)
    }
    
    if(isTRUE(write_9panel_png)){
      
      png(filename = paste0(stub,"9_Panel_PNGs/D_Grid_", 
                            RF_image$keyvalues$ROOTNAME , ".png"), width = 1280, 
          height = 1280, units = "px")
      plot(SKY)
      dev.off(2)
    }
  }
}

dev.off(2) ##Turn Off Plot Saving
end_time = Sys.time()
runtime = end_time - start_time
cat(paste0("Runtime In Seconds_" ,runtime, sep="\n"))
###IF THE CODE ISINT SAVING PLOTS, TYPE
#dev.list() IN THE CONSOLE AND DO
#dev.off(N) FOR ALL N SHOWN
