#interpulse interval matching wing beat freq
#

# how reliably can we get ipi peaks for an individual?

library(pacman)
p_load(tidyverse, dplyr, ggplot2, data.table, janitor, mclust, mixtools, bayesmix)
audio <- fread("../../../Dropbox/Vivesi2017/Analysis/Audio/Avisoft/Mviv16_07_original_Thresh_60_hold_3_freq10to40.csv") %>% 
  clean_names %>% 
  mutate(interval = interval %>% as.numeric)
layout(1)
audio$start_freq %>% hist
audio$end_freq %>% hist
audio$peak_amp %>% hist
audio$duration %>% hist(breaks = 10000, xlim = c(0,0.04))
# table(audio$duration)
# find commute
files <- unique(audio$filename)
for(i in 21:30){#length(files)){
  idx <- which(audio$filename == files[i])
  if(length(idx) > 5){
    plot(audio$start_time[idx], audio$start_freq[idx], 
         xlab = "time", ylab = "freq (kHz)",
         ylim = c(0,50000), col = 0,
         main = substr(files[i], 75, 103))
    points(audio$start_time[idx], audio$peak_freq[idx])
    for(j in 1:length(idx)){
      segments(x0 = audio$start_time[idx[j]], x1 = audio$end_time[idx[j]], 
               y0 = audio$start_freq[idx[j]], y1 = audio$end_freq[idx[j]],
               lwd = (audio$start_amp[idx[j]]+40)/max(audio$peak_amp+40)*20)  
    }
  }
}




normal_calls <- which(audio$interval < 1 & 
                        audio$duration < 0.04 &
                        audio$end_freq < 11000 &
                        audio$start_freq > 14000 &
                        audio$peak_amp > -30)
clean_audio <- audio[normal_calls,]
# if we filter out bad calls, we need to recalculate IPI
clean_audio$IPI <- NA
files <- unique(clean_audio$filename)
i = 2
for(i in 1:length(files)){
  idx <- which(clean_audio$filename == files[i])
  # clean_audio[idx,]
  if(length(idx) > 1){
    j = 2
    for(j in 2:idx){
      clean_audio$IPI[idx[j]] <- clean_audio$start_time[idx[j]] - clean_audio$end_time[idx[j-1]]
    }
  }
}

first_half <- 1:(nrow(clean_audio)/2)
second_half <- ((nrow(clean_audio)/2)+1):nrow(clean_audio)
hist(clean_audio$IPI, breaks = 10000, xlim = c(0,0.5))
d1 <- density(clean_audio$IPI[first_half] %>% na.omit, bw = 0.01)
d2 <- density(clean_audio$IPI[second_half] %>% na.omit, bw = 0.01)
layout(cbind(1:2))
plot(d1, main = "")
plot(d2, main = "")

layout(1)
# resample data?
b1 <- sample(clean_audio$IPI[first_half] %>% na.omit, size = 1e6, replace = TRUE)
density(b1) %>% plot

mx1 <- mixtools::normalmixEM(clean_audio$IPI[first_half] %>% 
                               na.omit, k = 5)
plot(mx1, which = 2, breaks = 100)
abline(v = mx1$mu, col = 2, lty = 2)



d1 %>% plot(xlim = c(0, 0.5))
# What should we expect the IPIs to be?
# wingbeat freq (Hz)
wb <- 14

# call eight times a flap
abline(v = 1/(8*wb))
# call four times a flap
abline(v = 1/(4*wb))
# call twice a flap
abline(v = 1/(2*wb), lty = 2)
# call once
abline(v = 1/wb)
#call every other wingbeat
abline(v = 1/(wb/2))
# call every two wingbeats
abline(v = 1/(wb/4))
# call every three wingbeats
abline(v = 1/(wb/))

# find periods of consistent call duration and IPI?
plot(clean_audio$IPI, clean_audio$duration, xlim = c(0,0.5), col = rgb(0,0,0,.1))


files <- unique(clean_audio$filename)
for(i in 21:30){#length(files)){
  idx <- which(clean_audio$filename == files[i])
  if(length(idx) > 5){
    plot(clean_audio$start_time[idx], clean_audio$start_freq[idx], 
         xlab = "time", ylab = "freq (kHz)",
         ylim = c(0,50000), col = 0,
         main = substr(files[i], 75, 103))
    points(clean_audio$start_time[idx], clean_audio$peak_freq[idx])
    for(j in 1:length(idx)){
      segments(x0 = clean_audio$start_time[idx[j]], x1 = clean_audio$end_time[idx[j]], 
               y0 = clean_audio$start_freq[idx[j]], y1 = clean_audio$end_freq[idx[j]],
               lwd = (clean_audio$start_amp[idx[j]]+40)/max(clean_audio$peak_amp+40)*20)  
    }
  }
}
