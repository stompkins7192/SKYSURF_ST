# SKYSURF_ST
Various Code, Files, and Etc. Related to my SKYSURF Project Work
The following files are part of my preliminary skysurf work including data, scripts, and plotting code in python. It is far from complete and is not "pretty"
as far as code or plots go, but for constantly re-analyzing the same set of images, it works just fine.



For the all_in_one.R script, this script reads a set of consitently named .FTIS files from a specified directory path, and then puts the resulting sky measurements into a text file with a header. It can be modified to output other values. After this, it takes the sky values, and then uses the provided final_images_data.txt and the R load table function to read the input sky measurements and add them to a list. It then calculates a percent error measurement from the measured sky values containend in all_in_one.txt and the given values in final_images_data.txt and plots it versus the input noise (sky RMS). The plot is automatically generated and saved as a .png file.


The SkyEstimateFinal.ipnyb is a jupyter notebook that does the same thing, but makes a slightly more informative plot, albiet not autmatically. So long as the called upon .txt files are in the directory where the notebook is run, then it is simply matter of executing the code blocks from top to bottom. It will generate, and if you uncomment the plt.savefig call in the plotting cell, save, the same measured sky error versus sky RMS plot. It compares the (current as of 11/24) best sky measurement to the original "best" sky measument, and plots a horizontal line at the mean value of the current measurement, allowing you to analyze how the data points fall around the mean error, and around zero with another horizontal line. Cells output various useful information such as the mean error, the position in the array of the minimum and maximum errors, and the lengths of the lists. To run, the notebook requries python and all of the packages called in the first block.

Currently, the second portion of the code which repeats the above analysis for the gradient images, is non funcitonal unless you add your own datafiles for it to read. Since we are going to discuss and refine our techniques for measuring and analyzing the gradient images, I have not invested into reproducing a simple percent error measurement plot.

