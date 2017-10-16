# SERRF - sytematical error removal using random forest.
a QC-based normalization method for large-scale untargeted metabolomics.

## online
* **address**: http://serrf.fiehnlab.ucdavis.edu
* **functionality**: 
1. use quality control (QC) samples to build SERRF to normalize biology samples and validate samples.
2. calculate relative standard deviation (RSD) on the validate samples to access performance quantitatively.
3. perform principal component analysis (PCA) to access performance visually.

more tutorial and explanation available online.

## local
* **download:**
1. R and RStudio
2. the above LOCAL folder
* **steps:**
1. open RStudio
2. open the *LOCAL --> run me.R*.
3. change the path where the *run me.R* is. If you are using Windows, you can right click the *run me.R* and copy the *location* and then paste it to the second line of *run me.R*. For example, your *run me.R location* is *C:\Users\Sili Fan\Documents\GitHub\SERRFweb\LOCAL*, paste this to the second line of *run me.R*, which then will be `workingdirectory = "C:\Users\Sili Fan\Documents\GitHub\SERRFweb\LOCAL" `. Then chagne all the `\` to `\\`, meaning the second line will be `workingdirectory = "C:\\Users\\Sili Fan\\Documents\\GitHub\\SERRFweb\\LOCAL" `
4. put your data in a right format (as example.xlsx) and put it the same folder as *run me.R*. 
5. change the 4th line of *run me.R* to the name of you file.
6. finally, you can run the *run me.R* in RStudio by *Ctrl + a* and *Ctrl + r* (for Windows). (sorry, I am not familiar with other system hot keys)
7. all the results will be automatically saved in the same folder of *run me.R*
* **functionality**:
1. perform at most 15 normalization methods (some use QC, some don't).
2. calculate cross-validated RSD of QC (if it is a QC-based normalization method) and RSD of validate samples (if you have validate samples) as quantitative performance measurement.
3. perform PCA as performance visualization.

Both online and local version have only be tested in a limited number of data sets. **If any error/suggestion, please feel free to contact me at slfan@ucdavis.edu.** I'll try my best to answer your question within 24 hours. 

#### citation (being submitted)
Sili Fan, Tomas Cajka, Stanley L. Hazen, W.H. Wilson T ang, Dinesh K. Barupal, and Oliver Fiehn. Systematical Error Removal using Random Forest (SERRF) for large-scale untargeted metabolomics
