Natural History and Survival Extrapolation of Different Cancers
========================================================================================================================================================

## Introduction
This study is focussing on determining the usefulness of real world data to determine and predict cancer survival for specific cancers. We will focus on the following cancers:
* Breast
* Colorectal
* Lung
* Liver
* Stomach
* Head/neck
* Prostate
* Pancreas

## Running the analysis
1) Download this entire repository (you can download as a zip folder using Code -> Download ZIP, or you can use GitHub Desktop). 
2) Open the project <i>CancerSurvivalExtrapolation.Rproj</i> in RStudio (when inside the project, you will see its name on the top-right of your RStudio session)
3) Open and work though the <i>CodeToRun.R</i> file which should be the only file that you need to interact with. Run the lines in the file, adding your database specific information and so on (see comments within the file for instructions). The last line of this file will run the study <i>(source(here("RunStudy.R"))</i>.     
4) After running you should then have a zip folder with results to share in your output folder.

## Important information!
1) Minimum cell counts for this study is 10 all counts less than this are obscured.
2) You need to set your study start date in the CodeToRun.R if you only have useable data from 2000. So some examples.. your database starts in 1973 then you leave the start date as "2000-01-01". Your database started in 2006 so you would put "2006.01.01".

Link to the shiny app for the papers: 
1) Survival results https://dpa-pde-oxford.shinyapps.io/EHDENCancerSurvivalStudyShiny/
2) Extrapolation results https://dpa-pde-oxford.shinyapps.io/EHDENCancerExtrapStudyShiny/

Any problems with the repo or shiny apps contact danielle.newby@ndorms.ox.ac.uk
