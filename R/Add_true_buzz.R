# set working directory and load file
library(pacman)
pacman::p_load(dplyr, tidyr, lubridate, ggplot2)
options(digits.secs = 3)

# get buzz clip times
buzz_clips <- list.files("C:/Users/Edward/Dropbox/VivesiAudio/BuzzCheck/Mviv17/Mviv17_33/Buzz")
split_time <- strsplit(buzz_clips, split = "_")
buzz_times <- data.frame(file = buzz_clips,
  start_clip = ymd_hms("2000-01-01 12:00:00.001"), 
                                          segment = NA)

for(i in 1:length(split_time)){
  buzz_times$start_clip[i] <- ymd_hms(paste0("2017-", split_time[[i]][4],"-", split_time[[i]][5], " ",
                               split_time[[i]][6],":",split_time[[i]][7], ":", 
                               split_time[[i]][8]))
  buzz_times$segment[i] <- as.numeric(substr(split_time[[i]][16], 1, nchar(split_time[[i]][16])-4))  
}

# write.csv(buzz_times, file = "../../../Dropbox/VivesiACC/data/truebuzz/Mviv17_33_truebuzz.csv")
#################################################################################################
# manually add true buzz times
# reload buzz times with true buzzes added
buzz_times <- read.csv("../../../Dropbox/VivesiACC/data/truebuzz/Mviv17_33_truebuzz_offset.csv")
buzz_times$start_clip <- ymd_hms(buzz_times$start_clip)

buzz_times$true_buzz <- buzz_times$start_clip + buzz_times$offset + buzz_times$segment*0.5

# load DDMT data
Metrics <- read.csv("../../../Dropbox/VivesiACC/data/metrics/Mviv17_33_metrics.csv")
str(Metrics)
Metrics$datetime <- ymd_hms(Metrics$datetime)+7*3600

# add true_buzzes
Metrics$true_buzz <- 0

i = 6
# for(i in 1:length(na.omit(buzz_times$true_buzz))){
  buzz <- 
    which.min(abs(Metrics$datetime - (buzz_times$true_buzz[i])))
  true_start <- 
    which.min(abs(Metrics$datetime - (buzz_times$true_buzz[i] - 0.25)))
  true_end <- 
    which.min(abs(Metrics$datetime - (buzz_times$true_buzz[i] + 0.25)))
  Metrics$true_buzz[true_start:true_end] <- 1
  
  with(Metrics[(buzz-100):(buzz+100),], plot(datetime, VeDBA, type = "l",
                                             ylim = c(-5, max(VeDBA))))
  with(Metrics[true_start:true_end,], points(datetime, col = "red", 
                                             y = rep(0, length(true_start:true_end))))
  with(Metrics[(buzz-100):(buzz+100),], points(datetime, y = rep(-3, 201), col = buzz))
# }
