# uschinadiss
This repository contains the scripts and data related to the study of American doctoral dissertations in Chinese history. The paper "" is available on the PEERS platform [https://www.peers.press/p/TMRTRl7sIUqpr2hQ74hj] as an online paper and on the HAL-SHS repository [https://shs.hal.science/halshs-04269261] as a downlable pdf file.
The markdown script encapsulates the methodological backbone of the study of American dissertations on Chinese history. Utilizing a suite of R packages such as tidyverse for data manipulation, ggplot2 for visualization, stm and stminsights for structural topic modeling, ggraph for graph plotting, reshape for data restructuring, kableExtra for advanced table generation, RColorBrewer and pals for color palette, this workflow underpins the data processing, analysis, and presentation of findings. In this Markdown script, we perform a complete chain of operations from basic statistical computing, to mapping, and to topic modeling
Scripts: 
1. The reference script is UShistdiss.R. This is the script that served to process the data
2. The UShistdiss script is the script which I used to clean and shape the data. It is not a clean script because it contains various iterations of data cleaning. Yet it provides useful chunks of script for data processing.
3. The uschinadiss.Rmd file is the markdown script produced on the basis of the original UShistdiss.R script
4. The uschinadiss.html file is the html version of the Markdown script
Data files:
1. The usdiss4 csv file contains the data that I used in my study.
2. The usdiss4LocUSA csv file contains the list of cities and states in the United States with the geocoordinates (latitude, longitude)
3. The US_universities_LocCoord csv file contains the list of American universities and their geocoordinates.
