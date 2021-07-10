#mkdir -p ~/Desktop/ProFound_IR_Output/SkyMap_PNGs  ##You might have to remove the -p option on Mac, it is required for linux.
#mkdir -p ~/Desktop/ProFound_IR_Output/9_Panel_PNGs
#mkdir -p ~/Desktop/ProFound_IR_Output/FITS_Maps
#mkdir -p ~/Desktop/ProFound_IR_Output/Image_Info
##Leave these commented out for test runs.

#for file in ~/R/x86_64-pc-linux-gnu-library/4.0/ProFound/extdata/hst_sample_images/Files_To_Loop/IR_test/temp_zip/*fits.gz
for file in ~/Desktop/HST_Sample_Images/F160W_Driz_test2/*fits.gz
do 
	#echo $file;date  #I had to comment this out, might be required for Mac.
	gunzip -kdc "$file" > ~/Desktop/ProFound_IR_Outputs/temp.fits
	#Rscript ~/R/x86_64-pc-linux-gnu-library/4.0/ProFound/extdata/hst_sample_images/Files_To_Loop/IR_test/IR_ProFound_Run_1.0.R
	Rscript ~/Desktop/ProFound_IR_Outputs/Final_IR_ProFound_Run.R
	sleep 1s
	rm ~/Desktop/ProFound_IR_Outputs/temp.fits
done
	


