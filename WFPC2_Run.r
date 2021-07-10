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

write_file = FALSE #Do You Want The ProFound Output FITS File? 
#WARNING OVERWRITES PREVIOUS  #DOES NOT WORK ON OTHER IMAGES #TAKES A LOT OF SPACE

write_CSV = FALSE #Do You Want A CSV Of Useful Data Output? 
#WARNING APPENDS TO PREVIOUS

write_skymap_png = TRUE #Do You Want A SkyMap PNG With The Object and Bad Pixel Maps? 
#WARNING OVERWRITES PREVIOUS 

write_9panel_png = TRUE #Do You Want A Diagnostic Grid For The Image Saved As A PNG? 
#WARNING OVERWRITES PREVIOUS

#Set Working Directory - Change To Where Your Raw FITS Files Are Located (temp_unzip)
stub = "~/Desktop/WFPC2_Outputs/"
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




#Star#t A Loop To Read Each FITS File In the List
for(i in 1:length(lst)){
  #foreach(i=1:length(lst), .combine=cbind) %dopar% { 
  #Leave This Commented Out Code Line (Might Work
  #In The Future)
  
  filename = paste( lst[i], sep="")
  print(filename)
  
  image1 = Rfits_read_array(filename = filename, header = TRUE, ext = 2)
  image2 = Rfits_read_array(filename = filename, header = TRUE, ext = 3)
  image3 = Rfits_read_array(filename = filename, header = TRUE, ext = 4)
  image4 = Rfits_read_array(filename = filename, header = TRUE, ext = 5)
  
  ###########
  ###Add Quadrant Offsets Here########
  ###########
  
  
  RF_image = Rfits_read_header(filename, ext = 1)
  ext2_header = Rfits_read_header(filename, ext = 2)
  ext3_header = Rfits_read_header(filename, ext = 3)
  ext4_header = Rfits_read_header(filename, ext = 4)
  ext5_header = Rfits_read_header(filename, ext = 5)

  rootnames = list()
  rootnames = append(rootnames, list(RF_image$keyvalues$ROOTNAME))

  

  
  
  #Do Things With or Without Dilation Below, Without Is Bad!
  #ASGR: not sure you need the image, skycut or pixcut arguments. 
  #Pretty sure they won't be used for anything. Also, maybe as well just pass 
  #$objects into newmask and use this
  #(makes more sense than extracting the segmentation part, which you do later)

  
  ##SIZE OF DILATION ABOVE ^^^ (size =  N) MUST BE ODD AND SMALL, OR ELSE IT DOES NOT WORK
  
  
  # mask = NULL
  
  #ASGR: a lot of these options are just the defaults. Pros and cons to listing them, but fine with you putting it in to make it all clearer.
  
  
  mask1 = profoundMakeSegimDilate(segim = image1$imDat > 50, size = 3)
  mask1$objects[1:88,] = 1L
  mask1$objects[,1:96] = 1L

  mask2 = profoundMakeSegimDilate(segim = image2$imDat > 50, size = 3)
  mask2$objects[1:66,] = 1L
  mask2$objects[,1:46] = 1L
  
  mask3 = profoundMakeSegimDilate(segim = image3$imDat > 50, size = 3)
  mask3$objects[1:50,] = 1L
  mask3$objects[,1:67] = 1L
  
  mask4 = profoundMakeSegimDilate(segim = image4$imDat > 50, size = 3)
  mask4$objects[1:63,] = 1L
  mask4$objects[,1:64] = 1L
  
 
  
  SKY1 = profoundProFound(image1$imDat, verbose=FALSE, skycut = 1.5, pixcut = 4, 
                         SBdilate = 2, doclip = TRUE, redosky= TRUE, redoskysize= 13, 
                         type = 'bicubic', skytype='median', redosegim=TRUE, boxiters=6, mask=mask1$objects, 
                         box=c(image1$keyvalues$NAXIS1 / 3,image1$keyvalues$NAXIS2 / 3), 
                         grid=c(image1$keyvalues$NAXIS1 / 3,image1$keyvalues$NAXIS2 / 3),
                         header = NULL)
  SKY2 = profoundProFound(image2$imDat, verbose=FALSE, skycut = 1.5, pixcut = 4, 
                          SBdilate = 2, doclip = TRUE, redosky= TRUE, redoskysize= 13, 
                          type = 'bicubic', skytype='median', redosegim=TRUE, boxiters=6, mask=mask2$objects, 
                          box=c(image2$keyvalues$NAXIS1 / 3,image2$keyvalues$NAXIS2 / 3), 
                          grid=c(image2$keyvalues$NAXIS1 / 3,image2$keyvalues$NAXIS2 / 3),
                          header = NULL)
  SKY3 = profoundProFound(image3$imDat, vHeadererbose=FALSE, skycut = 1.5, pixcut = 4, 
                          SBdilate = 2, doclip = TRUE, redosky= TRUE, redoskysize= 13, 
                          type = 'bicubic', skytype='median', redosegim=TRUE, boxiters=6, mask=mask3$objects, 
                          box=c(image3$keyvalues$NAXIS1 / 3,image3$keyvalues$NAXIS2 / 3), 
                          grid=c(image3$keyvalues$NAXIS1 / 3,image3$keyvalues$NAXIS2 / 3),
                          header = NULL)
  SKY4 = profoundProFound(image4$imDat, verbose=FALSE, skycut = 1.5, pixcut = 4, 
                          SBdilate = 2, doclip = TRUE, redosky= TRUE, redoskysize= 13, 
                          type = 'bicubic', skytype='median', redosegim=TRUE, boxiters=6, mask=mask4$objects, 
                          box=c(image4$keyvalues$NAXIS1 / 3,image4$keyvalues$NAXIS2 / 3), 
                          grid=c(image4$keyvalues$NAXIS1 / 3,image4$keyvalues$NAXIS2 / 3),
                          header = NULL)
  
  

  ##PLOTTING
  
  ##WCS PLOTS WORK WITH HEADER
  
  
  
  #ABOVE WORKS
  
  #ASGR: Is it always the case that objects_redo cannot overlap with mask? To be safe you could do good_pix = length(image$imDat) - length(which(SKY$mask > 0 | SKY$objects_redo > 0))
  
 # good_pix = length(image$imDat) - length(which(SKY1$mask > 0 | SKY1$objects_redo > 0))
 # percent_sky= (good_pix / length(image1$imDat))*100 #Calculate the Percentage of Sky Pixels
 # percent_sky = signif(percent_sky, digits = 4) 
  ###WRITE FITS SKYMAP (OPTIONAL)
  
  ####
  
  #Extension Headers
  
  #ASGR: you can remove the numeric='32' from the integer images.
  
  #WRITE EXTENSIONS
  if(isTRUE(write_file)){
    
    #Write Copy Of FITS File
    
    
    ##Write SkyMap 1 and Global Header as Extension
    Rfits_write_header(filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), 
                       keyvalues = RF_image$keyvalues, keycomments = RF_image$keycomments, ext = 1,
                       create_file = TRUE)
    Rfits_write_key(filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), keyname = 'EXTNAME', keyvalue = 'GLOBAL_HEADER', ext = 1)
    
    ###Write SkyMap 1 as Extension 2
    Rfits_write_image(SKY1$sky, filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), 
                      compress = TRUE, create_ext = TRUE, create_file = FALSE, overwrite_file = FALSE)
    #Write the Header for this Extension (Copy of the Original)
    Rfits_write_header(filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), 
                       keyvalues = ext2_header$keyvalues, ext = 2, create_ext = FALSE)
    
    #Properly Name the Extension
    Rfits_write_key(filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), keyname = 'EXTNAME', keyvalue = 'SKY_MAP_1', ext = 2)
    
    ####Write SkyRMSMap 1 as Extension 3
    Rfits_write_image(SKY1$skyRMS, filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), 
                      compress = TRUE, create_ext = TRUE, create_file = FALSE, overwrite_file = FALSE)
    ###Write the Header for this Extension (Copy of the Original)
    Rfits_write_header(filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), 
                       keyvalues = ext2_header$keyvalues, ext = 3, create_ext = FALSE)
    
    #Properly Name the Extension
    Rfits_write_key(filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), keyname = 'EXTNAME', keyvalue = 'SKYRMS_MAP_1', ext = 3)
    
    #Will be changed once a mask is availible
    ###Write the Bad Pixel Map as Extension 4 (MAKE INTEGERS 16 BIT TO SAVE SPACE, IT IS A BINARY ARRAY)
      Rfits_write_image(SKY1$mask, filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), 
                        compress = FALSE, create_ext = TRUE, create_file = FALSE, overwrite_file = FALSE,
                        integer = '16')
    ##Write the Header for this Extension (Copy of the Original )
    Rfits_write_header(filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), 
                       keyvalues = ext2_header$keyvalues, ext = 4, create_ext = FALSE)
    
    #Properly Name the Extension
    Rfits_write_key(filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), keyname = 'EXTNAME', keyvalue = 'BAD_PIXEL_MAP_1', ext = 4)
    
    ###Write the Object Map as Extension 5 (MAKE INTEGERS 16 BIT TO SAVE SPACE, IT IS A BINARY ARRAY)
    Rfits_write_image(SKY1$objects_redo, filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), 
                      compress = FALSE, create_ext = TRUE, create_file = FALSE, overwrite_file = FALSE,
                      integer = '16')
    ##Write the Header for this Extension (Copy of the Original )
    Rfits_write_header(filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), 
                       keyvalues = ext2_header$keyvalues, ext = 5, create_ext = FALSE)
    
    #Properly Name the Extension
    Rfits_write_key(filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), keyname = 'EXTNAME', keyvalue = 'Object_Map_1', ext = 5)
    
    ########################################
    ########################################
    ########################################
    #Write the Second Set Of Extensions
    
    ##Write SKY2 Info
    
    ###Write SkyMap 2 as Extension 6
    Rfits_write_image(SKY2$sky, filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), 
                      compress = TRUE, create_ext = TRUE, create_file = FALSE, overwrite_file = FALSE)
    #Write the Header for this Extension (Copy of the Original)
    Rfits_write_header(filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), 
                       keyvalues = ext3_header$keyvalues, ext = 6, create_ext = FALSE)
    
    #Properly Name the Extension
    Rfits_write_key(filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), keyname = 'EXTNAME', keyvalue = 'SKY_MAP_2', ext = 6)
    
    ####Write SkyRMSMap 1 as Extension 3
    Rfits_write_image(SKY2$skyRMS, filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), 
                      compress = TRUE, create_ext = TRUE, create_file = FALSE, overwrite_file = FALSE)
    ###Write the Header for this Extension (Copy of the Original)
    Rfits_write_header(filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), 
                       keyvalues = ext3_header$keyvalues, ext = 7, create_ext = FALSE)
    
    #Properly Name the Extension
    Rfits_write_key(filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), keyname = 'EXTNAME', keyvalue = 'SKYRMS_MAP_2', ext = 7)
    
    
    ###Write the Bad Pixel Map as Extension 4 (MAKE INTEGERS 16 BIT TO SAVE SPACE, IT IS A BINARY ARRAY)
      Rfits_write_image(SKY2$mask, filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), 
                        compress = FALSE, create_ext = TRUE, create_file = FALSE, overwrite_file = FALSE,
                        integer = '16')
    ##Write the Header for this Extension (Copy of the Original )
    Rfits_write_header(filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), 
                       keyvalues = ext3_header$keyvalues, ext = 8, create_ext = FALSE)
    
    #  #Properly Name the Extension
    Rfits_write_key(filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), keyname = 'EXTNAME', keyvalue = 'BAD_PIXEL_MAP_2', ext = 8)
    
    ###Write the Object Map as Extension 5 (MAKE INTEGERS 16 BIT TO SAVE SPACE, IT IS A BINARY ARRAY)
    Rfits_write_image(SKY2$objects_redo, filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), 
                      compress = FALSE, create_ext = TRUE, create_file = FALSE, overwrite_file = FALSE,
                      integer = '16')
    ##Write the Header for this Extension (Copy of the Original )
    Rfits_write_header(filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), 
                       keyvalues = ext3_header$keyvalues, ext = 9, create_ext = FALSE)
    
    #Properly Name the Extension
    Rfits_write_key(filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), keyname = 'EXTNAME', keyvalue = 'Object_Map_2', ext = 9)
    
    ########################################
    ########################################
    ########################################
    #Write the Third Set Of Extensions
    
    ##Write SKY3 Info
    
    ###Write SkyMap 3 as Extension 10
    Rfits_write_image(SKY3$sky, filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), 
                      compress = TRUE, create_ext = TRUE, create_file = FALSE, overwrite_file = FALSE)
    #Write the Header for this Extension (Copy of the Original)
    Rfits_write_header(filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), 
                       keyvalues = ext4_header$keyvalues, ext = 10, create_ext = FALSE)
    
    #Properly Name the Extension
    Rfits_write_key(filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), keyname = 'EXTNAME', keyvalue = 'SKY_MAP_3', ext = 10)
    
    ####Write SkyRMSMap 1 as Extension 3
    Rfits_write_image(SKY3$skyRMS, filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), 
                      compress = TRUE, create_ext = TRUE, create_file = FALSE, overwrite_file = FALSE)
    ###Write the Header for this Extension (Copy of the Original)
    Rfits_write_header(filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), 
                       keyvalues = ext4_header$keyvalues, ext = 11, create_ext = FALSE)
    
    #Properly Name the Extension
    Rfits_write_key(filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), keyname = 'EXTNAME', keyvalue = 'SKYRMS_MAP_3', ext = 11)
    
    
    ###Write the Bad Pixel Map as Extension 4 (MAKE INTEGERS 16 BIT TO SAVE SPACE, IT IS A BINARY ARRAY)
      Rfits_write_image(SKY3$mask, filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), 
                        compress = FALSE, create_ext = TRUE, create_file = FALSE, overwrite_file = FALSE,
                        integer = '16')
    ##Write the Header for this Extension (Copy of the Original )
    Rfits_write_header(filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), 
                       keyvalues = ext4_header$keyvalues, ext = 12, create_ext = FALSE)
    
    #  #Properly Name the Extension
    Rfits_write_key(filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), keyname = 'EXTNAME', keyvalue = 'BAD_PIXEL_MAP_3', ext = 12)
    
    ###Write the Object Map as Extension 5 (MAKE INTEGERS 16 BIT TO SAVE SPACE, IT IS A BINARY ARRAY)
    Rfits_write_image(SKY3$objects_redo, filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), 
                      compress = FALSE, create_ext = TRUE, create_file = FALSE, overwrite_file = FALSE,
                      integer = '16')
    ##Write the Header for this Extension (Copy of the Original )
    Rfits_write_header(filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), 
                       keyvalues = ext4_header$keyvalues, ext = 13, create_ext = FALSE)
    
    #Properly Name the Extension
    Rfits_write_key(filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), keyname = 'EXTNAME', keyvalue = 'Object_Map_3', ext = 13)
    
    ########################################
    ########################################
    ########################################
    #Write the Fourth Set Of Extensions
    
    ##Write SKY4 Info
    
    ###Write SkyMap 4 as Extension 14
    Rfits_write_image(SKY4$sky, filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), 
                      compress = TRUE, create_ext = TRUE, create_file = FALSE, overwrite_file = FALSE)
    #Write the Header for this Extension (Copy of the Original)
    Rfits_write_header(filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), 
                       keyvalues = ext5_header$keyvalues, ext = 14, create_ext = FALSE)
    
    #Properly Name the Extension
    Rfits_write_key(filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), keyname = 'EXTNAME', keyvalue = 'SKY_MAP_4', ext = 14)
    
    ####Write SkyRMSMap 4 as Extension 15
    Rfits_write_image(SKY4$skyRMS, filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), 
                      compress = TRUE, create_ext = TRUE, create_file = FALSE, overwrite_file = FALSE)
    ###Write the Header for this Extension (Copy of the Original)
    Rfits_write_header(filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), 
                       keyvalues = ext5_header$keyvalues, ext = 15, create_ext = FALSE)
    
    #Properly Name the Extension
    Rfits_write_key(filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), keyname = 'EXTNAME', keyvalue = 'SKYRMS_MAP_4', ext = 15)
    
    
    ###Write the 4th Bad Pixel Map as Extension 16 (MAKE INTEGERS 16 BIT TO SAVE SPACE, IT IS A BINARY ARRAY)
      Rfits_write_image(SKY4$mask, filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), 
                        compress = FALSE, create_ext = TRUE, create_file = FALSE, overwrite_file = FALSE,
                        integer = '16')
    ##Write the Header for this Extension (Copy of the Original )
    Rfits_write_header(filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), 
                       keyvalues = ext5_header$keyvalues, ext = 16, create_ext = FALSE)
    
    #  #Properly Name the Extension
    Rfits_write_key(filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), keyname = 'EXTNAME', keyvalue = 'BAD_PIXEL_MAP_4', ext = 16)
    
    ###Write the 4th Object Map as Extension 17 (MAKE INTEGERS 16 BIT TO SAVE SPACE, IT IS A BINARY ARRAY)
    Rfits_write_image(SKY4$objects_redo, filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), 
                      compress = FALSE, create_ext = TRUE, create_file = FALSE, overwrite_file = FALSE,
                      integer = '16')
    ##Write the Header for this Extension (Copy of the Original )
    Rfits_write_header(filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), 
                       keyvalues = ext5_header$keyvalues, ext = 17, create_ext = FALSE)
    
    #Properly Name the Extension
    Rfits_write_key(filename = paste0(stub,"FITS_Maps/",RF_image$keyvalues$ROOTNAME,'_c0m_profound.fits'), keyname = 'EXTNAME', keyvalue = 'Object_Map_4', ext = 17)
    
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
    not_sky =   (SKY1$objects_redo)
    maxvalue = 1.01
    not_sky[not_sky > maxvalue] = 1
    good_pix = length(image1$imDat) - (sum(not_sky))
    
    
    ###Chip 1 / Planetary Camera
    if(RF_image$keyvalues$ATODGAIN == 7){
      readnoise1 = 5.24
      gain1 = 7.12
    }
    if(RF_image$keyvalues$ATODGAIN == 15){
      readnoise1 = 7.02
      gain1 = 13.99
    }

    sky_background = SKY1$sky * gain1
    fake_rms = profoundMakeSigma(image = sky_background,
                                 exptime = RF_image$keyvalues$EXPTIME, image_units = 'elec',
                                 readRMS = readnoise1, output_units = 'elec', skycut = 1.5, read_units = 'elec')
    
    
    
    expected_RMS = mean(fake_rms, na.rm = TRUE)
    
    RMS_erorr = 100 * mean(abs(log10(SKY1$skyRMS*gain1/fake_rms) * log(10)), na.rm = TRUE)
    percent_sky = (good_pix / length(image1$imDat))*100 #Calculate the Percentage of Sky Pixels
    percent_sky = signif(percent_sky, digits = 8) 
    
    
    # ("Filter", 'Instrument',"Image_Name", "Sky_Mean_PF", "Sky_SD", "Percent_Sky", 'Sky_RMS', 
    #  'RMS_SD', 'Sky_Chi-Squared', 'Sky_LL', 'Sky_Stat_Corr', 'N_Objects', 'Image_Area', 
    #   'EXPTIME', 'Expected_RMS', 'RMS_Error_%', "BUNITS")
    
    df[i , 1] = RF_image$keyvalues$INSTRUME #File Name
    if(RF_image$keyvalues$INSTRUME == "WFPC2"){   #KeyValue Names Are Slightly Different For ACS vs WFC3UVIS
      df[i , 2] =  RF_image$keyvalues$FILTNAM1}
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
    df[i , 14]  =  RF_image$keyvalues$EXPTIME #Image Exposure Time
    df[i , 15]  =   expected_RMS / gain1  #RMS Value Expected Based On Known Info (Not 100% Correct Yet)
    df[i , 16]  =   RMS_erorr #% Error For Expected Versus Calculated RMS
    df[i , 17]  =   image1$keyvalues$BUNIT #Brightness Units
    
    write.table(df,file = paste0(stub,"Image_Info/Image_Info_PC.csv"), 
                row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE, sep = ",") #Write DF to CSV
    
    #CSV IS CONSTRUCTED IN THE ORDER THE FITS FILES ARE READ
    
    ###END OF WRITE CSV CODE
  }
  
  ##BEGINNING OF MAKE SKYMAP CODE
  
  if(isTRUE(write_skymap_png)){
    
    
    ##Define Needed Statistics
    not_sky =   (SKY1$objects_redo)
    maxvalue = 1.01
    not_sky[not_sky > maxvalue] = 1
    good_pix = length(image1$imDat) - (sum(not_sky))
    
    percent_sky= (good_pix / length(image1$imDat))*100 #Calculate the Percentage of Sky Pixels
    percent_sky = signif(percent_sky, digits = 4) 
    
    ####NEED TO CHANGE PILE OUTPUT LOCATION TO YOUR CHOSEN PATH
    
    
    png(filename = paste0(stub,"SkyMap_PNGs/Chip_PC/SkyMap_", 
                          RF_image$keyvalues$ROOTNAME , ".png"), width = 1280, 
        height = 1280, units = "px")
    
    
    
    #Does Not Take Header Into Account! Mapped Pixel By Pixel
    if(!is.null(SKY1$mask)){
      
      magimage(SKY1$sky-median(SKY1$sky,na.rm=TRUE), qdiff=TRUE,  cex.lab=2.5, cex.axis=1.5, side = 1:4)
      #Optional Toggle (Show Objects_Redo Map)
      magimage(SKY1$objects_redo!=0, col=c(NA,rgb(0/255, 0/255, 0/255, alpha = 0.5)), add=TRUE, magmap=FALSE, zlim=c(0,1), cex.lab=2.5, cex.axis=1.5, side = 1:4)
      
      #Mask (Keep)
      magimage(SKY1$mask!=0, col=c(NA,rgb(204/255, 0/255, 0/255, alpha = 0.5)), add=TRUE, magmap=FALSE, zlim=c(0,1), cex.lab=2.5, cex.axis=1.5, side = 1:4)
      
      suppressWarnings({
        stat_mean = signif(mean(SKY1$sky, na.rm=TRUE),4)
        stat_sd = signif(sd(SKY1$sky, na.rm=TRUE),4)
        stat_cor = signif(cor(as.numeric(SKY1$sky), as.numeric(SKY1$skyRMS)^2, use="pairwise.complete.obs"),4)
        legend('topleft',legend=c('Sky',paste0('Mean: ',stat_mean),paste0('SD: ',stat_sd),paste0('% Of Sky Pixels: ',percent_sky)),bg='white',cex=1.5)
      })
      #END OF SKYMAP PNG PRODUCTION CODE
      dev.off(2)
    }
    
    if(isTRUE(write_9panel_png)){
      
      png(filename = paste0(stub,"9_Panel_PNGs/Chip_PC/D_Grid_", 
                            RF_image$keyvalues$ROOTNAME , ".png"), width = 1280, 
          height = 1280, units = "px")
      plot(SKY1)
      dev.off(2)
    }
  }


#################
###REPEAT LAST 3 OPERATIONS FOR WF2 CHIP
#################


if(isTRUE(write_CSV)){
  
  #Calculate All Values Going Into CSV
    stat_mean = signif(mean(SKY2$sky[SKY2$mask==0], na.rm=TRUE),8)

    stat_sd = signif(sd(SKY2$sky[SKY2$mask==0], na.rm=TRUE),8)
 
   statrms_mean = signif(mean(SKY2$skyRMS[SKY2$mask==0], na.rm=TRUE),8)
 
   statrms_sd = signif(sd(SKY2$skyRMS[SKY2$mask==0], na.rm=TRUE),8)

  stat_cor = signif(cor(as.numeric(SKY2$sky[SKY2$mask==0]), as.numeric(SKY2$skyRMS[SKY2$mask==0])^2, use="pairwise.complete.obs"),8)

  not_sky =   (SKY2$mask)+(SKY2$objects_redo)
  not_sky =   (SKY2$objects_redo)
  maxvalue = 1.01
  not_sky[not_sky > maxvalue] = 1
  good_pix = length(image2$imDat) - (sum(not_sky))
  
  #For Chip 2 / WF2 
  if(RF_image$keyvalues$ATODGAIN == 7){
    readnoise1 = 5.51
    gain1 = 7.12
  }
  if(RF_image$keyvalues$ATODGAIN == 15){
    readnoise1 = 7.84
    gain1 = 14.50
  }
  
  sky_background = SKY2$sky * gain1
  fake_rms = profoundMakeSigma(image = sky_background,
                               exptime = RF_image$keyvalues$EXPTIME, image_units = 'elec',
                               readRMS = readnoise1, output_units = 'elec', skycut = 1.5, read_units = 'elec')
  
  
  
  expected_RMS = mean(fake_rms, na.rm = TRUE)
  
  RMS_erorr = 100 * mean(abs(log10(SKY2$skyRMS*gain1/fake_rms) * log(10)), na.rm = TRUE)
  percent_sky = (good_pix / length(image2$imDat))*100 #Calculate the Percentage of Sky Pixels
  percent_sky = signif(percent_sky, digits = 8) 
  
  
  # ("Filter", 'Instrument',"Image_Name", "Sky_Mean_PF", "Sky_SD", "Percent_Sky", 'Sky_RMS', 
  #  'RMS_SD', 'Sky_Chi-Squared', 'Sky_LL', 'Sky_Stat_Corr', 'N_Objects', 'Image_Area', 
  #   'EXPTIME', 'Expected_RMS', 'RMS_Error_%', "BUNITS")
  
  df[i , 1] = RF_image$keyvalues$INSTRUME #File Name
  if(RF_image$keyvalues$INSTRUME == "WFPC2"){   #KeyValue Names Are Slightly Different For ACS vs WFC3UVIS
    df[i , 2] =  RF_image$keyvalues$FILTNAM1}
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
  df[i , 14]  =  RF_image$keyvalues$EXPTIME #Image Exposure Time
  df[i , 15]  =   expected_RMS / gain1  #RMS Value Expected Based On Known Info (Not 100% Correct Yet)
  df[i , 16]  =   RMS_erorr #% Error For Expected Versus Calculated RMS
  df[i , 17]  =   image2$keyvalues$BUNIT #Brightness Units
  
  write.table(df,file = paste0(stub,"Image_Info/Image_Info_Chip2.csv"), 
              row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE, sep = ",") #Write DF to CSV
  
  #CSV IS CONSTRUCTED IN THE ORDER THE FITS FILES ARE READ
  
  ###END OF WRITE CSV CODE
}

##BEGINNING OF MAKE SKYMAP CODE

if(isTRUE(write_skymap_png)){
  
  
  ##Define Needed Statistics
  not_sky =   (SKY2$objects_redo)
  maxvalue = 1.01
  not_sky[not_sky > maxvalue] = 1
  good_pix = length(image2$imDat) - (sum(not_sky))
  
  percent_sky= (good_pix / length(image2$imDat))*100 #Calculate the Percentage of Sky Pixels
  percent_sky = signif(percent_sky, digits = 4) 
  
  ####NEED TO CHANGE PILE OUTPUT LOCATION TO YOUR CHOSEN PATH
  
  
  png(filename = paste0(stub,"SkyMap_PNGs/Chip_2/SkyMap_Chip2_", 
                        RF_image$keyvalues$ROOTNAME , ".png"), width = 1280, 
      height = 1280, units = "px")
  
  
  
  #Does Not Take Header Into Account! Mapped Pixel By Pixel
  if(!is.null(SKY2$mask)){
    
    magimage(SKY2$sky-median(SKY2$sky,na.rm=TRUE), qdiff=TRUE,  cex.lab=2.5, cex.axis=1.5, side = 1:4)
    #Optional Toggle (Show Objects_Redo Map)
    magimage(SKY2$objects_redo!=0, col=c(NA,rgb(0/255, 0/255, 0/255, alpha = 0.5)), add=TRUE, magmap=FALSE, zlim=c(0,1), cex.lab=2.5, cex.axis=1.5, side = 1:4)
    
    #Mask (Keep)
     magimage(SKY2$mask!=0, col=c(NA,rgb(204/255, 0/255, 0/255, alpha = 0.5)), add=TRUE, magmap=FALSE, zlim=c(0,1), cex.lab=2.5, cex.axis=1.5, side = 1:4)
    
    suppressWarnings({
      stat_mean = signif(mean(SKY2$sky, na.rm=TRUE),4)
      stat_sd = signif(sd(SKY2$sky, na.rm=TRUE),4)
      stat_cor = signif(cor(as.numeric(SKY2$sky), as.numeric(SKY2$skyRMS)^2, use="pairwise.complete.obs"),4)
      legend('topleft',legend=c('Sky',paste0('Mean: ',stat_mean),paste0('SD: ',stat_sd),paste0('% Of Sky Pixels: ',percent_sky)),bg='white',cex=1.5)
    })
    #END OF SKYMAP PNG PRODUCTION CODE
    dev.off(2)
  }
  
  if(isTRUE(write_9panel_png)){
    
    png(filename = paste0(stub,"9_Panel_PNGs/Chip_2/D_Grid_Chip2_", 
                          RF_image$keyvalues$ROOTNAME , ".png"), width = 1280, 
        height = 1280, units = "px")
    plot(SKY2)
    dev.off(2)
  }
}
  
  
  #################
  ###REPEAT LAST 3 OPERATIONS FOR WF3 CHIP
  #################
  
  
  if(isTRUE(write_CSV)){
    
    #Calculate All Values Going Into CSV
      stat_mean = signif(mean(SKY3$sky[SKY3$mask==0], na.rm=TRUE),8)

      stat_sd = signif(sd(SKY3$sky[SKY3$mask==0], na.rm=TRUE),8)

     statrms_mean = signif(mean(SKY3$skyRMS[SKY3$mask==0], na.rm=TRUE),8)

     statrms_sd = signif(sd(SKY3$skyRMS[SKY3$mask==0], na.rm=TRUE),8)
  
    stat_cor = signif(cor(as.numeric(SKY3$sky[SKY3$mask==0]), as.numeric(SKY3$skyRMS[SKY3$mask==0])^2, use="pairwise.complete.obs"),8)
   
     not_sky =   (SKY3$mask)+(SKY3$objects_redo)

    maxvalue = 1.01
    not_sky[not_sky > maxvalue] = 1
    good_pix = length(image3$imDat) - (sum(not_sky))
    
    #For Chip 2 / WF2 
    if(RF_image$keyvalues$ATODGAIN == 7){
      readnoise1 = 5.22
      gain1 = 6.90
    }
    if(RF_image$keyvalues$ATODGAIN == 15){
      readnoise1 = 6.99
      gain1 = 13.95
    }
    
    sky_background = SKY3$sky * gain1
    fake_rms = profoundMakeSigma(image = sky_background,
                                 exptime = RF_image$keyvalues$EXPTIME, image_units = 'elec',
                                 readRMS = readnoise1, output_units = 'elec', skycut = 1.5, read_units = 'elec')
    
    
    
    expected_RMS = mean(fake_rms, na.rm = TRUE)
    
    RMS_erorr = 100 * mean(abs(log10(SKY3$skyRMS*gain1/fake_rms) * log(10)), na.rm = TRUE)
    percent_sky = (good_pix / length(image3$imDat))*100 #Calculate the Percentage of Sky Pixels
    percent_sky = signif(percent_sky, digits = 8) 
    
    
    # ("Filter", 'Instrument',"Image_Name", "Sky_Mean_PF", "Sky_SD", "Percent_Sky", 'Sky_RMS', 
    #  'RMS_SD', 'Sky_Chi-Squared', 'Sky_LL', 'Sky_Stat_Corr', 'N_Objects', 'Image_Area', 
    #   'EXPTIME', 'Expected_RMS', 'RMS_Error_%', "BUNITS")
    
    df[i , 1] = RF_image$keyvalues$INSTRUME #File Name
    if(RF_image$keyvalues$INSTRUME == "WFPC2"){   #KeyValue Names Are Slightly Different For ACS vs WFC3UVIS
      df[i , 2] =  RF_image$keyvalues$FILTNAM1}
    df[i , 3] = image3$keyvalues$ROOTNAME #File Name
    df[i , 4] = stat_mean  #Mean Sky Value
    df[i , 5] = stat_sd #Sky SD
    df[i , 6] = percent_sky   #Percent of Sky Pixels, Not Objects or Bad Pixels
    df[i , 7]  =  statrms_mean #Mean RMS Value 
    df[i , 8]  =  statrms_sd  #RMS SD
    df[i , 9]  =  SKY3$skyChiSq  #Sky Fit Chi-Squared Value
    df[i , 10]  =  SKY3$skyLL   #Sky Fit Log-Likelyhood
    df[i , 11]  =  stat_cor   #Correlation Stat Between RMS and Sky Maps
    df[i , 12]  =  length(SKY3$segstats$Nobject)  #Number Of Objects Detected In Image
    df[i , 13]  =  SKY3$imarea  #Image Area (In Square Degrees, Requires Header = TRUE)
    df[i , 14]  =  RF_image$keyvalues$EXPTIME #Image Exposure Time
    df[i , 15]  =   expected_RMS / gain1  #RMS Value Expected Based On Known Info (Not 100% Correct Yet)
    df[i , 16]  =   RMS_erorr #% Error For Expected Versus Calculated RMS
    df[i , 17]  =   image3$keyvalues$BUNIT #Brightness Units
    
    write.table(df,file = paste0(stub,"Image_Info/Image_Info_Chip3.csv"), 
                row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE, sep = ",") #Write DF to CSV
    
    #CSV IS CONSTRUCTED IN THE ORDER THE FITS FILES ARE READ
    
    ###END OF WRITE CSV CODE
  }
  
  ##BEGINNING OF MAKE SKYMAP CODE
  
  if(isTRUE(write_skymap_png)){
    
    
    ##Define Needed Statistics
    not_sky =   (SKY3$objects_redo)
    maxvalue = 1.01
    not_sky[not_sky > maxvalue] = 1
    good_pix = length(image3$imDat) - (sum(not_sky))
    
    percent_sky= (good_pix / length(image3$imDat))*100 #Calculate the Percentage of Sky Pixels
    percent_sky = signif(percent_sky, digits = 4) 
    
    ####NEED TO CHANGE PILE OUTPUT LOCATION TO YOUR CHOSEN PATH
    
    
    png(filename = paste0(stub,"SkyMap_PNGs/Chip_3/SkyMap_Chip3_", 
                          RF_image$keyvalues$ROOTNAME , ".png"), width = 1280, 
        height = 1280, units = "px")
    
    
    
    #Does Not Take Header Into Account! Mapped Pixel By Pixel
    if(!is.null(SKY3$mask)){
      
      magimage(SKY3$sky-median(SKY1$sky,na.rm=TRUE), qdiff=TRUE,  cex.lab=2.5, cex.axis=1.5, side = 1:4)
      #Optional Toggle (Show Objects_Redo Map)
      magimage(SKY3$objects_redo!=0, col=c(NA,rgb(0/255, 0/255, 0/255, alpha = 0.5)), add=TRUE, magmap=FALSE, zlim=c(0,1), cex.lab=2.5, cex.axis=1.5, side = 1:4)
      
      #Mask (Keep)
       magimage(SKY3$mask!=0, col=c(NA,rgb(204/255, 0/255, 0/255, alpha = 0.5)), add=TRUE, magmap=FALSE, zlim=c(0,1), cex.lab=2.5, cex.axis=1.5, side = 1:4)
      
      suppressWarnings({
        stat_mean = signif(mean(SKY3$sky, na.rm=TRUE),4)
        stat_sd = signif(sd(SKY3$sky, na.rm=TRUE),4)
        stat_cor = signif(cor(as.numeric(SKY3$sky), as.numeric(SKY3$skyRMS)^2, use="pairwise.complete.obs"),4)
        legend('topleft',legend=c('Sky',paste0('Mean: ',stat_mean),paste0('SD: ',stat_sd),paste0('% Of Sky Pixels: ',percent_sky)),bg='white',cex=1.5)
      })
      #END OF SKYMAP PNG PRODUCTION CODE
      dev.off(2)
    }
    
    if(isTRUE(write_9panel_png)){
      
      png(filename = paste0(stub,"9_Panel_PNGs/Chip_3/D_Grid_Chip3_", 
                            RF_image$keyvalues$ROOTNAME , ".png"), width = 1280, 
          height = 1280, units = "px")
      plot(SKY3)
      dev.off(2)
    }
  }
  
  
  #################
  ###REPEAT LAST 3 OPERATIONS FOR WF4 CHIP
  #################
  
  
  if(isTRUE(write_CSV)){
    
    #Calculate All Values Going Into CSV
      stat_mean = signif(mean(SKY4$sky[SKY4$mask==0], na.rm=TRUE),8)

      stat_sd = signif(sd(SKY4$sky[SKY4$mask==0], na.rm=TRUE),8)

     statrms_mean = signif(mean(SKY4$skyRMS[SKY4$mask==0], na.rm=TRUE),8)

     statrms_sd = signif(sd(SKY4$skyRMS[SKY4$mask==0], na.rm=TRUE),8)
 
    stat_cor = signif(cor(as.numeric(SKY4$sky[SKY4$mask==0]), as.numeric(SKY4$skyRMS[SKY4$mask==0])^2, use="pairwise.complete.obs"),8)
    
     not_sky =   (SKY4$mask)+(SKY4$objects_redo)

    maxvalue = 1.01
    not_sky[not_sky > maxvalue] = 1
    good_pix = length(image4$imDat) - (sum(not_sky))
    
    #For Chip 2 / WF2 
    if(RF_image$keyvalues$ATODGAIN == 7){
      readnoise1 = 5.19
      gain1 = 7.10
    }
    if(RF_image$keyvalues$ATODGAIN == 15){
      readnoise1 = 8.32
      gain1 = 13.95
    }
    
    sky_background = SKY4$sky * gain1
    fake_rms = profoundMakeSigma(image = sky_background,
                                 exptime = RF_image$keyvalues$EXPTIME, image_units = 'elec',
                                 readRMS = readnoise1, output_units = 'elec', skycut = 1.5, read_units = 'elec')
    
    
    
    expected_RMS = mean(fake_rms, na.rm = TRUE)
    
    RMS_erorr = 100 * mean(abs(log10(SKY4$skyRMS*gain1/fake_rms) * log(10)), na.rm = TRUE)
    percent_sky = (good_pix / length(image4$imDat))*100 #Calculate the Percentage of Sky Pixels
    percent_sky = signif(percent_sky, digits = 8) 
    
    
    # ("Filter", 'Instrument',"Image_Name", "Sky_Mean_PF", "Sky_SD", "Percent_Sky", 'Sky_RMS', 
    #  'RMS_SD', 'Sky_Chi-Squared', 'Sky_LL', 'Sky_Stat_Corr', 'N_Objects', 'Image_Area', 
    #   'EXPTIME', 'Expected_RMS', 'RMS_Error_%', "BUNITS")
    
    df[i , 1] = RF_image$keyvalues$INSTRUME #File Name
    if(RF_image$keyvalues$INSTRUME == "WFPC2"){   #KeyValue Names Are Slightly Different For ACS vs WFC3UVIS
      df[i , 2] =  RF_image$keyvalues$FILTNAM1}
    df[i , 3] = image4$keyvalues$ROOTNAME #File Name
    df[i , 4] = stat_mean  #Mean Sky Value
    df[i , 5] = stat_sd #Sky SD
    df[i , 6] = percent_sky   #Percent of Sky Pixels, Not Objects or Bad Pixels
    df[i , 7]  =  statrms_mean #Mean RMS Value 
    df[i , 8]  =  statrms_sd  #RMS SD
    df[i , 9]  =  SKY4$skyChiSq  #Sky Fit Chi-Squared Value
    df[i , 10]  =  SKY4$skyLL   #Sky Fit Log-Likelyhood
    df[i , 11]  =  stat_cor   #Correlation Stat Between RMS and Sky Maps
    df[i , 12]  =  length(SKY4$segstats$Nobject)  #Number Of Objects Detected In Image
    df[i , 13]  =  SKY4$imarea  #Image Area (In Square Degrees, Requires Header = TRUE)
    df[i , 14]  =  RF_image$keyvalues$EXPTIME #Image Exposure Time
    df[i , 15]  =   expected_RMS / gain1  #RMS Value Expected Based On Known Info (Not 100% Correct Yet)
    df[i , 16]  =   RMS_erorr #% Error For Expected Versus Calculated RMS
    df[i , 17]  =   image4$keyvalues$BUNIT #Brightness Units
    
    write.table(df,file = paste0(stub,"Image_Info/Image_Info_Chip4.csv"), 
                row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE, sep = ",") #Write DF to CSV
    
    #CSV IS CONSTRUCTED IN THE ORDER THE FITS FILES ARE READ
    
    ###END OF WRITE CSV CODE
  }
  
  ##BEGINNING OF MAKE SKYMAP CODE
  
  if(isTRUE(write_skymap_png)){
    
    
    ##Define Needed Statistics
    not_sky =   (SKY4$objects_redo)
    maxvalue = 1.01
    not_sky[not_sky > maxvalue] = 1
    good_pix = length(image4$imDat) - (sum(not_sky))
    
    percent_sky= (good_pix / length(image4$imDat))*100 #Calculate the Percentage of Sky Pixels
    percent_sky = signif(percent_sky, digits = 4) 
    
    ####NEED TO CHANGE PILE OUTPUT LOCATION TO YOUR CHOSEN PATH
    
    
    png(filename = paste0(stub,"SkyMap_PNGs/Chip_4/SkyMap_Chip4_", 
                          RF_image$keyvalues$ROOTNAME , ".png"), width = 1280, 
        height = 1280, units = "px")
    
    
    
    #Does Not Take Header Into Account! Mapped Pixel By Pixel
    if(!is.null(SKY4$mask)){
      
      magimage(SKY4$sky-median(SKY1$sky,na.rm=TRUE), qdiff=TRUE,  cex.lab=2.5, cex.axis=1.5, side = 1:4)
      #Optional Toggle (Show Objects_Redo Map)
      magimage(SKY4$objects_redo!=0, col=c(NA,rgb(0/255, 0/255, 0/255, alpha = 0.5)), add=TRUE, magmap=FALSE, zlim=c(0,1), cex.lab=2.5, cex.axis=1.5, side = 1:4)
      
      #Mask (Keep)
       magimage(SKY4$mask!=0, col=c(NA,rgb(204/255, 0/255, 0/255, alpha = 0.5)), add=TRUE, magmap=FALSE, zlim=c(0,1), cex.lab=2.5, cex.axis=1.5, side = 1:4)
      
      suppressWarnings({
        stat_mean = signif(mean(SKY4$sky, na.rm=TRUE),4)
        stat_sd = signif(sd(SKY4$sky, na.rm=TRUE),4)
        stat_cor = signif(cor(as.numeric(SKY4$sky), as.numeric(SKY4$skyRMS)^2, use="pairwise.complete.obs"),4)
        legend('topleft',legend=c('Sky',paste0('Mean: ',stat_mean),paste0('SD: ',stat_sd),paste0('% Of Sky Pixels: ',percent_sky)),bg='white',cex=1.5)
      })
      #END OF SKYMAP PNG PRODUCTION CODE
      dev.off(2)
    }
    
    if(isTRUE(write_9panel_png)){
      
      png(filename = paste0(stub,"9_Panel_PNGs/Chip_4/D_Grid_Chip4_", 
                            RF_image$keyvalues$ROOTNAME , ".png"), width = 1280, 
          height = 1280, units = "px")
      plot(SKY4)
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
