#These were the USGS written user codes explained durring the R-course
#The code below will download the programs and create the neccessary paths to
#access the data linked to them

install.packages(c("dplyr","dataRetrieval","ggplot2","tidyr","RColorBrewer","EGRET"))

rprofile_path = file.path(Sys.getenv("HOME"), ".Rprofile")
write('\noptions(repos=c(getOption(\'repos\'), 
      USGS=\'http://owi.usgs.gov/R\'))\n',
      rprofile_path, 
      append =  TRUE)

install.packages(c("gsIntroR","smwrBase","smwrGraphs"))

#Here is a link to the course webpage with additional info
shell.exec("http://usgs-r.github.io/introR/")

