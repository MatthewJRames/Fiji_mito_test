#test Zenodo Upload 7/29/2023
{
  library(dplyr)
  library(ggplot2)
  library(tidyverse)
  library(ggpubr)
  library(readxl)
  library(data.table)
  library(cowplot)
  library(ggpmisc)
}


setwd("/Users/rames/Documents/test_R_code_fiji")

getwd()

#======================================================================
#Making this import multiple dataframes
# ROI_1 <- read_xlsx(("Results_0.xlsx"), sheet = 1)
#below is the manual way
{
ROI_0 <- read.csv(paste("Results_0.csv", sep=""))
ROI_1 <- read.csv(paste("Results_1.csv", sep=""))
ROI_2 <- read.csv(paste("Results_2.csv", sep=""))
ROI_3 <- read.csv(paste("Results_3.csv", sep=""))
ROI_4 <- read.csv(paste("Results_4.csv", sep=""))

ROI_0 <- ROI_0 %>% mutate(ROI = 0) %>% mutate(unique = paste0(X.1, "_", ROI, sep="")) 
ROI_1 <- ROI_1 %>% mutate(ROI = 1) %>% mutate(unique = paste0(X.1, "_", ROI, sep="")) 
ROI_2 <- ROI_2 %>% mutate(ROI = 2) %>% mutate(unique = paste0(X.1, "_", ROI, sep="")) 
ROI_3 <- ROI_3 %>% mutate(ROI = 3) %>% mutate(unique = paste0(X.1, "_", ROI, sep="")) 
ROI_4 <- ROI_4 %>% mutate(ROI = 4) %>% mutate(unique = paste0(X.1, "_", ROI, sep="")) 

ROI_all <- rbind(ROI_0, ROI_1, ROI_2, ROI_3, ROI_4)
ROI_all$ROI <- factor(ROI_all$ROI, level = c("0","1","2","3","4"))
}

#do the above as a loop, so the below is an automated way :)
Extract_particles <- function(Image, roi) {
  data_list_cell_manual <- vector(mode = "list", length = roi)
  for(i in 0:(roi-1)) {
    skip_to_next <- FALSE
    tryCatch(
      {temp = read.csv(paste(Image, "_", i, ".csv", sep=""), header=TRUE)
      temp <- temp %>% mutate(ROI = i)
      temp <- temp %>% mutate(unique = paste0(X.1, "_", ROI, sep=""))
      data_list_cell_manual[[i+1]] <- temp
      assign(paste("ROI_", i, "_manual_results", sep=""), temp)
      }, error = function(e) { skip_to_next <<- TRUE})
    if(skip_to_next) { next }
    
  }
  temp <- rbindlist(data_list_cell_manual)
  newname <- paste("output_", Image, sep="")
  assign(  paste("output_", Image, sep=""), temp, pos=1)
}

#run below to combine all
Extract_particles("Results", 5)


#rough plotting for fun of mitochondria positions
ggplot(ROI_all, aes(x=X, y=-Y, size=Area)) +
  geom_point() +
  facet_wrap(~ROI)


#finding particle thresholds by area
ggplot(ROI_all, aes(x=unique, y=Area, size=Area, color=ROI)) +
  geom_point() +
  geom_hline(yintercept = 1000, color="red") +
  scale_y_log10()

ROI_all_org <- ROI_all %>% arrange(Area)
ROI_all_org_levels <- ROI_all_org$unique 
ROI_all_org$unique <- factor(ROI_all_org$unique, level = ROI_all_org_levels)

ggplot(ROI_all_org, aes(x=unique, y=Area, size=Area, color=ROI)) +
  geom_point() +
  geom_hline(yintercept = 1000, color="red") +
  scale_y_log10()




ROI_all %>% 
  filter(Area > 1000) %>%
ggplot(., aes(x=Area, y=Mean, size=Area, color=ROI)) +
  geom_point()






