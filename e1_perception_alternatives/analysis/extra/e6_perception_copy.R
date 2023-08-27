#Julian De Freitas, 2019
#Analysis script for De Freitas, Rips, & Alvarez - The capacity of personal identity
#Experiment 4

## clear workspace
rm(list = ls()) 

options(download.file.method="libcurl")

## necessary libraries
if (!require(rjson)) {install.packages("rjson"); require(rjson)}
if (!require(ggplot2)) {install.packages("ggplot2"); require(ggplot2)}
if (!require(ltm)) {install.packages("ltm"); require(ltm)} 
if (!require(heplots)) {install.packages("heplots"); require(heplots)} 
if (!require(lmtest)) {install.packages("lmtest"); require(lmtest)} 
if (!require(compute.es)) {install.packages("compute.es"); require(compute.es)}
if (!require(jsonlite)) {install.packages("jsonlite"); require(jsonlite)}
if (!require(plyr)) {install.packages("plyr"); require(plyr)}
if (!require(lme4)) {install.packages("lme4"); require(lme4)}
if (!require(reshape2)) {install.packages("reshape2"); require(reshape2)}
if (!require(RColorBrewer)) {install.packages("RColorBrewer"); require(RColorBrewer)}
if (!require(Hmisc)) {install.packages("Hmisc"); require(Hmisc)}
if (!require(gridExtra)) {install.packages("gridExtra"); require(gridExtra)}
if (!require(devtools)) {install.packages("devtools"); require(devtools)}
if (!require(ggpubr)) {install.packages("ggpubr"); require(ggpubr)}

##================================================================================================================
                                              ##IMPORT DATA##
##================================================================================================================

## set directory to data folder
dir <- setwd("/Users/julian/Dropbox (Personal)/Research/Intuition/single_identity/e6_perception/data")

datalist = list()
#import data using jsonlite [automate this, by defining list of data frames]
files <- list.files(pattern=('*txt'))
for (i in 1:length(files)) {
    curData <- fromJSON(files[i], simplifyDataFrame = TRUE)
    datalist[[i]] <- curData$trialStruct
}

data = do.call(rbind, datalist)
head(data)
dim(data)
length(unique(data$workerId)) #201 subjects

#check that we have equal numbers for each condition
table(data$image)
table(data$label)
table(data$agentCond)
table(data$matchCond)

##================================================================================================================
                                                ##DATA PREP##
##================================================================================================================

### perform exclusions: attention, comprehension, and rts < 200 (this ends up performing both subject and trial)
### note, we exclude based on mean accuracy further below
### note, data$comp_mental_content actually refers to comp_number_copies
data <- subset(data,(data$attentionMCQ=="0" & data$comp_original_you==3 &
                       data$comp_mental_content==2 & data$comp=='D' & data$rt>=200)) 
length(unique(data$workerId)) #1 subjects
dim(data)

### exclude subjects with lower than 55% average accuracy
worker <- as.factor(data$workerId)
workers <- as.factor(unique(worker))
acc <- as.numeric(data$acc)
acc_use <- acc
acc_use[is.na(acc_use)] <- 0
perf_thresh <- 0.55

acc_mat <- array(0,dim=c(length(workers),1))
colnames(acc_mat) <- c('mean_acc')
for(i in 1:length(workers)) {
  acc_mat[i,] <- c(mean(acc_use[worker==workers[i]]))
  if(acc_mat[i,1] < perf_thresh) {
    data <- subset(data,data$workerId != workers[i])
  }
}
acc_mat

### get identity ratings for each subject
worker <- as.factor(data$workerId)
workers <- as.factor(unique(worker))
identity_bef <- data$identity_fetus
identity_aft <- data$identity_pvs
iden_mat <- array(0,dim=c(length(workers),6))
colnames(iden_mat) <- c('before', 'after','zeros','att_diff', 'total_perf','self_copy_perf')
for(i in 1:length(workers)) {
  iden_mat[i,] <- c( mean(identity_bef[worker==workers[i]]), mean(identity_aft[worker==workers[i]]),0, 0, 0, 0)
}
iden.mat <- as.data.frame(iden_mat, stringsAsFactors=FALSE); iden.mat

#assign variable names
acc <- as.numeric(data$acc)
acc_use <- acc
acc_use[is.na(acc_use)] <- 0
numTrials <- dim(data)[1]
matchCond <- as.factor(data$matchCond)
agentCond <- as.factor(data$agentCond)
#create numeric version of condition
for(i in 1:length(agentCond)) {
  if(agentCond[i] == 'original') {
    data$agentCond_n[i] = 1
  }
  else if(agentCond[i] == 'copy') {
    data$agentCond_n[i] = 2
  }
  else if(agentCond[i] == 'stranger') {
    data$agentCond_n[i] = 3
  }
}
agentCond_n <- as.factor(data$agentCond_n)
ans <- as.factor(data$ans)
corrAns <- as.factor(data$corrAns)
rts <- log(as.numeric(data$rt))
agentConds_n <- as.factor(unique(agentCond_n))
matchConds <- unique(matchCond)
age <- as.numeric(data$age); mean(age,na.rm = TRUE) #32.10
gender <- as.factor(data$sex); table(gender)[1]/sum(table(gender)) #0.51

#create matrix for collecting d prime-relevant values, and rts
d_mat <- array(0,dim=c(3*length(workers),7))
colnames(d_mat) <- c('worker', 'agentCond', 'hits', 'false_alarms', 'd', 'rt', 'after_resp')
d.mat <- as.data.frame(d_mat, stringsAsFactors=FALSE); d.mat

#for each worker and agent condition, get average hits, false alarms, d-prime, and rt
#hits: proportion of true hits you said were hits (out of all true hits)
#false alarms: proportin of non hits you said were hits (out of all non hits)
#get rts for trials that were correct
counter <- 1
for(i in 1:length(workers)) {
  for(j in 1:length(agentConds_n)) {
        length_h <- length(acc_use[worker==workers[i] & agentCond_n==agentConds_n[j] & corrAns=='y'])
        length_fa <- length(acc_use[worker==workers[i] & agentCond_n==agentConds_n[j] & corrAns=='n'])
        
        h <- length(acc_use[worker==workers[i] & agentCond_n==agentConds_n[j] & ans=='y' & corrAns=='y'])/length_h
        fa <- length(acc_use[worker==workers[i] & agentCond_n==agentConds_n[j] & ans=='y' & corrAns=='n'])/length_fa
        rt <- mean(rts[worker==workers[i] & agentCond_n==agentConds_n[j] & acc==1], na.rm=TRUE)
       
        #correction for if h=1 or fa=0, by (Macmillan & Kaplan, 1985)
        #if 0, 0.5/𝑛; if 1, (𝑛−0.5)/𝑛 , where n is the number of signal or noise trials
        #see https://stats.stackexchange.com/questions/134779/d-prime-with-100-hit-rate-probability-and-0-false-alarm-probability
        if(h==1) {
          h <- (length_h-0.5)/length_h
        }
        
        if(fa==0) {
          fa <- 0.05/length_fa
        }
        
        d.mat[counter,] <- c (workers[i], agentConds_n[j], h, fa, qnorm(h) - qnorm(fa), rt, mean(identity_aft[worker==workers[i]]))
        counter = counter + 1
  }
} 

for(i in 1:length(workers)) {
  iden.mat[i,4] <- d.mat$d[d.mat$worker==i & d.mat$agentCond == 1] - d.mat$d[d.mat$worker==i & d.mat$agentCond == 2]
  iden.mat[i,5] <- d.mat$d[d.mat$worker==i & d.mat$agentCond == 1] + d.mat$d[d.mat$worker==i & d.mat$agentCond == 2] + d.mat$d[d.mat$worker==i & d.mat$agentCond == 3]
  iden.mat[i,6] <- d.mat$d[d.mat$worker==i & d.mat$agentCond == 1] + d.mat$d[d.mat$worker==i & d.mat$agentCond == 2] 
}

#subset d.mat by response
d.mat_o <- subset(d.mat,(d.mat$after==1))
d.mat_c <- subset(d.mat,(d.mat$after==2))
d.mat_b <- subset(d.mat,(d.mat$after==4))

#sort identity mat by original - copy, for plotting
iden_ordered <- iden.mat[order(-iden.mat$att_diff),]
iden_ordered$counter <- c(1:length(workers))
iden_ordered

#sort identity mat by original - copy, for plotting
iden_ordered_perf <- iden.mat[order(-iden.mat$total_perf),]
iden_ordered_perf$counter <- c(1:length(workers))
iden_ordered_perf

#subset data based on whether participants say 1 (original) or 4 (both)
iden_ordered_original <- subset(iden_ordered,(iden_ordered$after == 1))
iden_ordered_copy <- subset(iden_ordered,(iden_ordered$after == 2))
iden_ordered_both <- subset(iden_ordered,(iden_ordered$after == 4))

#subset data based on whether participants say 1 (original) or 4 (both)
iden_ordered_perf_original <- subset(iden_ordered_perf,(iden_ordered_perf$after == 1))
iden_ordered_perf_copy <- subset(iden_ordered_perf,(iden_ordered_perf$after == 2))
iden_ordered_perf_both <- subset(iden_ordered_perf,(iden_ordered_perf$after == 4))

#prepare data to replicate revival effect
cond1 <- rep(1, times = dim(iden.mat)[1])
cond2 <- rep(2, times = dim(iden.mat)[1])
cond <- c(cond1,cond2)

subject1 <- seq(1, dim(iden.mat)[1], by=1)
subject2 <- seq(1, dim(iden.mat)[1], by=1)
subject <- c(subject1, subject2)

identity_before <- iden.mat$before
identity_after <- iden.mat$after
identity <- c(identity_before, identity_after)

identity_name <- rep(0, times = length(identity))

#code choice names
for(i in 1:length(identity)) {
  if(identity[i] == 1) {
    identity_name[i] = '1_original'
  } 
  else if(identity[i] == 2) {
    identity_name[i] = '2_copy'
  }
  else if(identity[i] == 3) {
    identity_name[i] = '3_neither'
  }
  else if(identity[i] == 4) {
    identity_name[i] = '4_both'
  }
}

#code choices for logistic regressions
identity_b_original <- rep(9, times = length(identity))
identity_b_copy <- rep(9, times = length(identity))
identity_b_neither <- rep(9, times = length(identity))
identity_b_both <- rep(9, times = length(identity))

for(i in 1:length(identity)) {
  #original = 1, everything else zero
  if(identity[i] == 1) {
    identity_b_original[i] = 1
    identity_b_copy[i] = 0
    identity_b_neither[i] = 0
    identity_b_both[i] = 0
  }
  #copy = 1, everything else zero
  else if(identity[i] == 2) {
    identity_b_original[i] = 0
    identity_b_copy[i] = 1
    identity_b_neither[i] = 0
    identity_b_both[i] = 0
  }
  #neither = 1, everything else zero
  else if(identity[i] == 3) {
    identity_b_original[i] = 0
    identity_b_copy[i] = 0
    identity_b_neither[i] = 1
    identity_b_both[i] = 0
  }
  #both = 1, everything else zero
  else if(identity[i] == 4) {
    identity_b_original[i] = 0
    identity_b_copy[i] = 0
    identity_b_neither[i] = 0
    identity_b_both[i] = 1
  }
}

# create matrix to plot self - original difference group effect for those who said original, copy, and both
diff_mat <- array(0,dim=c(3,5))
colnames(diff_mat) <- c('cond','mean','sd','n','sem')
diff.mat <- as.data.frame(diff_mat, stringsAsFactors=FALSE); diff.mat

diff.mat[1,] <- c(1,mean(iden_ordered$att_diff[iden_ordered$after==1]),sd(iden_ordered$att_diff[iden_ordered$after==1]),length(iden_ordered$att_diff[iden_ordered$after==1]),0)
diff.mat[1,5] <- diff.mat[1,3]/sqrt(diff.mat[1,4])

diff.mat[2,] <- c(2,mean(iden_ordered$att_diff[iden_ordered$after==2]),sd(iden_ordered$att_diff[iden_ordered$after==2]),length(iden_ordered$att_diff[iden_ordered$after==2]),0)
diff.mat[2,5] <- diff.mat[2,3]/sqrt(diff.mat[2,4])

diff.mat[3,] <- c(3,mean(iden_ordered$att_diff[iden_ordered$after==4]),sd(iden_ordered$att_diff[iden_ordered$after==4]),length(iden_ordered$att_diff[iden_ordered$after==4]),0)
diff.mat[3,5] <- diff.mat[3,3]/sqrt(diff.mat[3,4])

# create matrix to plot total performance for those who said original, copy, and both
total_perf_mat <- array(0,dim=c(3,5))
colnames(total_perf_mat) <- c('cond','mean','sd','n','sem')
total_perf.mat <- as.data.frame(total_perf_mat, stringsAsFactors=FALSE); total_perf.mat

total_perf.mat[1,] <- c(1,mean(iden_ordered$total_perf[iden_ordered$after==1]),sd(iden_ordered$total_perf[iden_ordered$after==1]),length(iden_ordered$total_perf[iden_ordered$after==1]),0)
total_perf.mat[1,5] <- total_perf.mat[1,3]/sqrt(total_perf.mat[1,4])

total_perf.mat[2,] <- c(2,mean(iden_ordered$total_perf[iden_ordered$after==2]),sd(iden_ordered$total_perf[iden_ordered$after==2]),length(iden_ordered$total_perf[iden_ordered$after==2]),0)
total_perf.mat[2,5] <- total_perf.mat[2,3]/sqrt(total_perf.mat[2,4])

total_perf.mat[3,] <- c(3,mean(iden_ordered$total_perf[iden_ordered$after==4]),sd(iden_ordered$total_perf[iden_ordered$after==4]),length(iden_ordered$total_perf[iden_ordered$after==4]),0)
total_perf.mat[3,5] <- total_perf.mat[3,3]/sqrt(total_perf.mat[3,4])

# create matrix to plot total performance for self + copy for those who said original, copy, and both
self_copy_perf_mat <- array(0,dim=c(3,5))
colnames(self_copy_perf_mat) <- c('cond','mean','sd','n','sem')
self_copy_perf.mat <- as.data.frame(self_copy_perf_mat, stringsAsFactors=FALSE); self_copy_perf.mat

self_copy_perf.mat[1,] <- c(1,mean(iden_ordered$self_copy_perf[iden_ordered$after==1]),sd(iden_ordered$self_copy_perf[iden_ordered$after==1]),length(iden_ordered$self_copy_perf[iden_ordered$after==1]),0)
self_copy_perf.mat[1,5] <- self_copy_perf.mat[1,3]/sqrt(self_copy_perf.mat[1,4])

self_copy_perf.mat[2,] <- c(2,mean(iden_ordered$self_copy_perf[iden_ordered$after==2]),sd(iden_ordered$self_copy_perf[iden_ordered$after==2]),length(iden_ordered$self_copy_perf[iden_ordered$after==2]),0)
self_copy_perf.mat[2,5] <- self_copy_perf.mat[2,3]/sqrt(self_copy_perf.mat[2,4])

self_copy_perf.mat[3,] <- c(3,mean(iden_ordered$self_copy_perf[iden_ordered$after==4]),sd(iden_ordered$self_copy_perf[iden_ordered$after==4]),length(iden_ordered$self_copy_perf[iden_ordered$after==4]),0)
self_copy_perf.mat[3,5] <- self_copy_perf.mat[3,3]/sqrt(self_copy_perf.mat[3,4])

#get identity before and after ratings for replication of E2 analysis
#counter <- 1
#for(i in 1:length(workers)) {
###================================================================================================================
                                                      ##ANALYSIS##
##================================================================================================================

#Replication of explicit judgment effect:

#glm for each of the four possible answers
# https://stats.stackexchange.com/questions/13166/rs-lmer-cheat-sheet

#original
length(identity_b_original[identity_b_original==1 & cond==2])/length(identity_b_original[cond==2]) #after
length(identity_b_original[identity_b_original==1 & cond==1])/length(identity_b_original[cond==1]) #before

glmer_original <- glmer(identity_b_original ~ cond + (1 | subject), family=binomial)
summary(glmer_original)###copy
length(identity_b_copy[identity_b_copy==1 & cond==2])/length(identity_b_copy[cond==2]) #after
length(identity_b_copy[identity_b_copy==1 & cond==1])/length(identity_b_copy[cond==1]) #before

glmer_copy <- glmer(identity_b_copy ~ cond + (1 | subject), family=binomial)
summary(glmer_copy)

#neither
length(identity_b_neither [identity_b_neither==1 & cond==2])/length(identity_b_neither[cond==2]) #after
length(identity_b_neither[identity_b_neither==1 & cond==1])/length(identity_b_neither[cond==1]) #before

glmer_neither <- glmer(identity_b_neither ~ cond + (1 | subject), family=binomial)
summary(glmer_neither)

#both
length(identity_b_both[identity_b_both==1 & cond==2])/length(identity_b_both[cond==2]) #after 
length(identity_b_both[identity_b_both==1 & cond==1])/length(identity_b_both[cond==1]) #before

glmer_both <- glmer(identity_b_both ~ cond + (1 | subject), family=binomial)
summary(glmer_both)


#### GROUP DIFFERENCES

#: PERCEPTION# all subjects

## means
mean(d.mat$d[d.mat$agentCond==1])
sd(d.mat$d[d.mat$agentCond==1])

mean(d.mat$d[d.mat$agentCond==2])
sd(d.mat$d[d.mat$agentCond==2])

mean(d.mat$d[d.mat$agentCond==3])
sd(d.mat$d[d.mat$agentCond==3])

att_1 <- t.test(d.mat$d[d.mat$agentCond==1 | d.mat$agentCond==2] ~ d.mat$agentCond[d.mat$agentCond==1 | d.mat$agentCond==2], var.equal=TRUE, paired=TRUE); att_1
att_2 <- t.test(d.mat$d[d.mat$agentCond==1 | d.mat$agentCond==3] ~ d.mat$agentCond[d.mat$agentCond==1 | d.mat$agentCond==3], var.equal=TRUE, paired=TRUE); att_2
att_3 <- t.test(d.mat$d[d.mat$agentCond==2 | d.mat$agentCond==3] ~ d.mat$agentCond[d.mat$agentCond==2 | d.mat$agentCond==3], var.equal=TRUE, paired=TRUE); att_3

tes(as.numeric(att_1[1]), length(workers), length(workers)) #cohen's d
tes(as.numeric(att_2[1]), length(workers), length(workers)) #cohen's d
tes(as.numeric(att_3[1]), length(workers), length(workers)) #cohen's d

####### chose original #####
mean(d.mat_o$d[d.mat_o$agentCond==1])
sd(d.mat_o$d[d.mat_o$agentCond==1])
n_o_1 = length(d.mat_o$d[d.mat_o$agentCond==1])

mean(d.mat_o$d[d.mat_o$agentCond==2])
sd(d.mat_o$d[d.mat_o$agentCond==2])
n_o_2 = length(d.mat_o$d[d.mat_o$agentCond==2])

mean(d.mat_o$d[d.mat_o$agentCond==3])
sd(d.mat_o$d[d.mat_o$agentCond==3])
n_o_3 = length(d.mat_o$d[d.mat_o$agentCond==3])

att_1 <- t.test(d.mat_o$d[d.mat_o$agentCond==1 | d.mat_o$agentCond==2] ~ d.mat_o$agentCond[d.mat_o$agentCond==1 | d.mat_o$agentCond==2], var.equal=TRUE, paired=TRUE); att_1
att_2 <- t.test(d.mat_o$d[d.mat_o$agentCond==1 | d.mat_o$agentCond==3] ~ d.mat_o$agentCond[d.mat_o$agentCond==1 | d.mat_o$agentCond==3], var.equal=TRUE, paired=TRUE); att_2
att_3 <- t.test(d.mat_o$d[d.mat_o$agentCond==2 | d.mat_o$agentCond==3] ~ d.mat_o$agentCond[d.mat_o$agentCond==2 | d.mat_o$agentCond==3], var.equal=TRUE, paired=TRUE); att_3

tes(as.numeric(att_1[1]), n_o_1, n_o_2) #cohen's d
tes(as.numeric(att_1[2]), n_o_1, n_o_3)
tes(as.numeric(att_1[3]), n_o_2, n_o_3)

####### chose copy #####
mean(d.mat_c$d[d.mat_c$agentCond==1])
sd(d.mat_c$d[d.mat_c$agentCond==1])
n_c_1 = length(d.mat_c$d[d.mat_c$agentCond==1])

mean(d.mat_c$d[d.mat_c$agentCond==2])
sd(d.mat_c$d[d.mat_c$agentCond==2])
n_c_2 = length(d.mat_c$d[d.mat_c$agentCond==2])

mean(d.mat_c$d[d.mat_c$agentCond==3])
sd(d.mat_c$d[d.mat_c$agentCond==3])
n_c_3 = length(d.mat_c$d[d.mat_c$agentCond==3])

att_1 <- t.test(d.mat_c$d[d.mat_c$agentCond==1 | d.mat_c$agentCond==2] ~ d.mat_c$agentCond[d.mat_c$agentCond==1 | d.mat_c$agentCond==2], var.equal=TRUE, paired=TRUE); att_1
att_2 <- t.test(d.mat_c$d[d.mat_c$agentCond==1 | d.mat_c$agentCond==3] ~ d.mat_c$agentCond[d.mat_c$agentCond==1 | d.mat_c$agentCond==3], var.equal=TRUE, paired=TRUE); att_2
att_3 <- t.test(d.mat_c$d[d.mat_c$agentCond==2 | d.mat_c$agentCond==3] ~ d.mat_c$agentCond[d.mat_c$agentCond==2 | d.mat_c$agentCond==3], var.equal=TRUE, paired=TRUE); att_3

tes(as.numeric(att_1[1]), n_c_1, n_c_2) #cohen's d
tes(as.numeric(att_1[2]), n_c_1, n_c_3)
tes(as.numeric(att_1[3]), n_c_2, n_c_3)

# chose both
mean(d.mat_b$d[d.mat_b$agentCond==1])
sd(d.mat_b$d[d.mat_b$agentCond==1])
n_b_1 = length(d.mat_b$d[d.mat_b$agentCond==1])

mean(d.mat_b$d[d.mat_b$agentCond==2])
sd(d.mat_b$d[d.mat_b$agentCond==2])
n_b_2 = length(d.mat_b$d[d.mat_b$agentCond==2])

mean(d.mat_b$d[d.mat_b$agentCond==3])
sd(d.mat_b$d[d.mat_b$agentCond==3])
n_b_3 = length(d.mat_b$d[d.mat_b$agentCond==3])

att_1 <- t.test(d.mat_b$d[d.mat_b$agentCond==1 | d.mat_b$agentCond==2] ~ d.mat_b$agentCond[d.mat_b$agentCond==1 | d.mat_b$agentCond==2], var.equal=TRUE, paired=TRUE); att_1
att_2 <- t.test(d.mat_b$d[d.mat_b$agentCond==1 | d.mat_b$agentCond==3] ~ d.mat_b$agentCond[d.mat_b$agentCond==1 | d.mat_b$agentCond==3], var.equal=TRUE, paired=TRUE); att_2
att_3 <- t.test(d.mat_b$d[d.mat_b$agentCond==2 | d.mat_b$agentCond==3] ~ d.mat_b$agentCond[d.mat_b$agentCond==2 | d.mat_b$agentCond==3], var.equal=TRUE, paired=TRUE); att_3

tes(as.numeric(att_1[1]), n_b_1, n_b_2) #cohen's d
tes(as.numeric(att_1[2]), n_b_1, n_b_3)
tes(as.numeric(att_1[3]), n_b_2, n_b_3)

####### GROUP DIFFERENCES, BY RESPONSE: ORIGINAL - COPY

mean(iden_ordered$att_diff[iden_ordered$after==1])
sd(iden_ordered$att_diff[iden_ordered$after==1])

mean(iden_ordered$att_diff[iden_ordered$after==2])
sd(iden_ordered$att_diff[iden_ordered$after==2])

mean(iden_ordered$att_diff[iden_ordered$after==4])
sd(iden_ordered$att_diff[iden_ordered$after==4])

att_1 <- t.test(iden_ordered$att_diff[iden_ordered$aft==1 | iden_ordered$aft==2] ~ iden_ordered$aft[iden_ordered$aft==1 | iden_ordered$aft==2], var.equal=TRUE, paired=FALSE); att_1
att_2 <- t.test(iden_ordered$att_diff[iden_ordered$aft==1 | iden_ordered$aft==4] ~ iden_ordered$aft[iden_ordered$aft==1 | iden_ordered$aft==4], var.equal=TRUE, paired=FALSE); att_2
att_3 <- t.test(iden_ordered$att_diff[iden_ordered$aft==2 | iden_ordered$aft==4] ~ iden_ordered$aft[iden_ordered$aft==2 | iden_ordered$aft==4], var.equal=TRUE, paired=FALSE); att_3

####### GROUP DIFFERENCES, BY RESPONSE: OVERALL PERFORMANCE

mean(iden_ordered$total_perf[iden_ordered$aft==1])
sd(iden_ordered$total_perf[iden_ordered$aft==1])

mean(iden_ordered$total_perf[iden_ordered$aft==2])
sd(iden_ordered$total_perf[iden_ordered$aft==2])

mean(iden_ordered$total_perf[iden_ordered$aft==4])
sd(iden_ordered$total_perf[iden_ordered$aft==4])

att_1 <- t.test(iden_ordered$total_perf[iden_ordered$aft==1 | iden_ordered$aft==2] ~ iden_ordered$aft[iden_ordered$aft==1 | iden_ordered$aft==2], var.equal=TRUE, paired=FALSE); att_1
att_2 <- t.test(iden_ordered$total_perf[iden_ordered$aft==1 | iden_ordered$aft==4] ~ iden_ordered$aft[iden_ordered$aft==1 | iden_ordered$aft==4], var.equal=TRUE, paired=FALSE); att_2
att_3 <- t.test(iden_ordered$total_perf[iden_ordered$aft==2 | iden_ordered$aft==4] ~ iden_ordered$aft[iden_ordered$aft==2 | iden_ordered$aft==4], var.equal=TRUE, paired=FALSE); att_3

### attention: rts (no effect)
mod <- aov(d.mat$rt ~ d.mat$agentCond)
summary(mod)

mean(d.mat$rt[d.mat$agentCond==1])
sd(d.mat$rt[d.mat$agentCond==1])

mean(d.mat$rt[d.mat$agentCond==2])
sd(d.mat$rt[d.mat$agentCond==2])

mean(d.mat$rt[d.mat$agentCond==3])
sd(d.mat$rt[d.mat$agentCond==3])

rt_1 <- t.test(d.mat$rt[d.mat$agentCond==1 | d.mat$agentCond==2] ~ d.mat$agentCond[d.mat$agentCond==1 | d.mat$agentCond==2], var.equal=TRUE, paired=TRUE); rt_1
rt_2 <- t.test(d.mat$rt[d.mat$agentCond==1 | d.mat$agentCond==3] ~ d.mat$agentCond[d.mat$agentCond==1 | d.mat$agentCond==3], var.equal=TRUE, paired=TRUE); rt_2
rt_3 <- t.test(d.mat$rt[d.mat$agentCond==2 | d.mat$agentCond==3] ~ d.mat$agentCond[d.mat$agentCond==2 | d.mat$agentCond==3], var.equal=TRUE, paired=TRUE); rt_3

tes(as.numeric(rt_1[1]), length(workers), length(workers)) #cohen's d
tes(as.numeric(rt_2[1]), length(workers), length(workers)) #cohen's d
tes(as.numeric(rt_3[1]), length(workers), length(workers)) #cohen's d

#t-test on subsets
t.test(iden_ordered$att_diff[iden_ordered$after == 1 | iden_ordered$after == 4], iden_ordered$after[iden_ordered$after == 1 | iden_ordered$after == 4], var.equal=TRUE, paired=TRUE)

##================================================================================================================
                                                        ##PLOT##
##================================================================================================================
condNames <- c('Original', 'Copy', 'Stranger')

###### REVIVAL EFFECT
#prepare data for plotting
tab <- matrix(NA,2,4)
colnames(tab) <- c('original', 'copy', 'neither', 'both')
rownames(tab) <- c('before', 'after')
tab[1,] <- c(length(identity[identity == 1 & cond == 1])/length(identity[cond == 1]), length(identity[identity == 2 & cond == 1])/length(identity[cond == 1]), length(identity[identity == 3 & cond == 1])/length(identity[cond == 1]), length(identity[identity == 4 & cond == 1])/length(identity[cond == 1]) )
tab[2,] <- c(length(identity[identity == 1 & cond == 2])/length(identity[cond == 2]), length(identity[identity == 2 & cond == 2])/length(identity[cond == 2]), length(identity[identity == 3 & cond == 2])/length(identity[cond == 2]), length(identity[identity == 4 & cond == 2])/length(identity[cond == 2]) )
tab

#plot barplot
tab_transpose <- t(tab)
barplot(tab_transpose, main="Who are you?", xlab = "Condition", ylab= "Proportion", beside=TRUE, legend = rownames(tab_transpose))

### GROUPED PLOTS

### dprime, group
p1.1<-ggplot(d.mat,aes(x=factor(agentCond),y=d,fill=factor(agentCond))) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",fill="darkgray",face="bold")+
  theme_bw()+coord_cartesian(ylim=c(0, 3))+
  theme_classic()
p1.1+scale_x_discrete(breaks = 1:length(condNames), labels=condNames)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text = element_text(size=32), 
        axis.text.x = element_text(size=32, face="bold", angle = 45, hjust=1,vjust=0.8, color="black"),
        axis.text.y = element_text(color="black"),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"))+
  xlab("Agent Condition")+ylab("d' Performance")+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))+
  scale_fill_manual(values = c("darkgray"))

### dprime, group, chose 'original'
title <- c('Identified with Original') 
p1.2<-ggplot(d.mat_o,aes(x=factor(agentCond),y=d,fill=factor(agentCond))) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",fill="darkgray",face="bold")+
  theme_bw()+coord_cartesian(ylim=c(0, 3))+
  theme_classic()
p1.2+ggtitle(title)+scale_x_discrete(breaks = 1:length(condNames), labels=condNames)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text = element_text(size=32), 
        axis.text.x = element_text(size=32, face="bold", angle = 45, hjust=1,vjust=0.8, color="black"),
        axis.text.y = element_text(color="black"),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"))+
  xlab("Agent Condition")+ylab("d' Performance")+
  theme(plot.title = element_text(hjust = 0.5), axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))+
  scale_fill_manual(values = c("darkgray"))

### dprime, group, chose 'copy'
title <- c('Identified with Copy') 
p1.3<-ggplot(d.mat_c,aes(x=factor(agentCond),y=d,fill=factor(agentCond))) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",fill="darkgray",face="bold")+
  theme_bw()+coord_cartesian(ylim=c(0, 3))+
  theme_classic()
p1.3+ggtitle(title)+scale_x_discrete(breaks = 1:length(condNames), labels=condNames)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text = element_text(size=32), 
        axis.text.x = element_text(size=32, face="bold", angle = 45, hjust=1,vjust=0.8, color="black"),
        axis.text.y = element_text(color="black"),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"))+
  xlab("Agent Condition")+ylab("d' Performance")+
  theme(plot.title = element_text(hjust = 0.5), axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))+
  scale_fill_manual(values = c("darkgray"))

### dprime, group, chose 'both'
title <- c('Identified with Both') 
p1.4<-ggplot(d.mat_b,aes(x=factor(agentCond),y=d,fill=factor(agentCond))) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",fill="darkgray",face="bold")+
  theme_bw()+coord_cartesian(ylim=c(0, 3))+
  theme_classic()
p1.4+ggtitle(title)+scale_x_discrete(breaks = 1:length(condNames), labels=condNames)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text = element_text(size=32), 
        axis.text.x = element_text(size=32, face="bold", angle = 45, hjust=1,vjust=0.8, color="black"),
        axis.text.y = element_text(color="black"),
        axis.title.x =x = element_text(face="bold"),
        axis.title. element_text(face="bold"))+
  xlab("Agent Condition")+ylab("d' Performance")+
  theme(plot.title = element_text(hjust = 0.5), axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))+
  scale_fill_manual(values = c("darkgray"))

grid.arrange(p1.1, p1.2, p1.3, p1.4, nrow=2, ncol=2)

##pretty version

#1st plot
p1.11<-ggplot(d.mat,aes(x=factor(agentCond),y=d,fill=factor(agentCond))) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",face="bold")+
  coord_cartesian(ylim=c(0, 3))+
  theme(axis.title.y = element_blank()) + 
  theme(axis.title.x = element_blank()) + 
  theme(legend.title = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(axis.ticks.y = element_blank())+
  theme_classic()

p1.111<-p1.11+scale_fill_discrete(name = "", labels = c ("Original", "Copy", "Stranger")) +
  xlab ("") + ylab ("") +
  theme(legend.text = element_text(size = "18", face = "plain")) +
  ggtitle ("All Participants") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.title = element_text(size = 18))

p1.1111<-p1.111+ theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(axis.text.y = element_text(size = 14))

######2nd plot
p1.12<-ggplot(d.mat_o,aes(x=factor(agentCond),y=d,fill=factor(agentCond))) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",face="bold")+
  coord_cartesian(ylim=c(0, 3))+
  theme(axis.title.y = element_blank()) + 
  theme(axis.title.x = element_blank()) + 
  theme(legend.title = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(axis.ticks.y = element_blank())+
  theme_classic()
p1.112<-p1.12+scale_fill_discrete(name = "", labels = c ("Original", "Copy", "Stranger")) +
  xlab ("") + ylab ("") +
  theme(legend.text = element_text(size = "18", face = "plain")) +
  ggtitle ("Identified with Original") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.title = element_text(size = 18))
p1.1112<-p1.112+theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(axis.text.y = element_blank())+
  theme(axis.ticks.y = element_blank())

########3rd plot 
p1.13<-ggplot(d.mat_c,aes(x=factor(agentCond),y=d,fill=factor(agentCond))) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",face="bold")+
  coord_cartesian(ylim=c(0, 3))+
  theme(axis.title.y = element_blank()) + 
  theme(axis.title.x = element_blank()) + 
  theme(legend.title = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(axis.ticks.y = element_blank())+
  theme_classic()
p1.113<-p1.13+scale_fill_discrete(name = "", labels = c ("Original", "Copy", "Stranger")) +
  xlab ("") + ylab ("") +
  theme(legend.text = element_text(size = "18", face = "plain")) +
  ggtitle ("Identified with Copy") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.title = element_text(size = 18))
p1.1113<-p1.113+theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(axis.text.y = element_text(size = 14))

#######4th plot
p1.14<-ggplot(d.mat_b,aes(x=factor(agentCond),y=d,fill=factor(agentCond))) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",face="bold")+
  coord_cartesian(ylim=c(0, 3))+
  theme(axis.title.y = element_blank()) + 
  theme(axis.title.x = element_blank()) + 
  theme(legend.title = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(axis.ticks.y = element_blank())+
  theme_classic()
p1.114<-p1.14+scale_fill_discrete(name = "", labels = c ("Original", "Copy", "Stranger")) +
  xlab ("") + ylab ("") +
  theme(legend.text = element_text(size = "18", face = "plain")) +
  ggtitle ("Identified with Both") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.title = element_text(size = 18))
p1.1114<-p1.114+theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(axis.text.y = element_blank())+
  theme(axis.ticks.y = element_blank())

quartz()
figure<-ggarrange(p1.1111,p1.1112,p1.1113,p1.1114, nrow=2,ncol=2,common.legend = TRUE, legend="top") 
annotate_figure(figure,top = NULL,left = text_grob("d' Performance", color="black", face ="bold",size=20)) 




###### GROUP ORIGINAL - COPY DIFFERENCE
condNames <- c('Original', 'Copy', 'Both')

quartz()
p1<-ggplot(diff.mat, aes(x=factor(cond),y=mean))+
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",width = 0.5)+
  theme_bw()+coord_cartesian(ylim=c(0,1))+
  theme_classic() 
p1+geom_errorbar(aes(ymax=mean+sem, ymin=mean-sem), position="dodge",width = 0.5)+
  scale_x_discrete(breaks = 1:length(condNames), labels=condNames)+
  theme(legend.key=element_blank(), legend.box="horizontal", legend.position = c(0.5, 0.95), legend.direction="horizontal")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text = element_text(size=30), axis.text.x = element_text(angle = 45, hjust=1,vjust=0.8))+
  xlab("Version Identified With")+ylab("d' Difference (Original - Copy)")+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))

###### TOTAL PERFORMANCE
quartz()
condNames <- c('Original', 'Copy', 'Both')

p1<-ggplot(total_perf.mat, aes(x=factor(cond),y=mean))+
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",width = 0.5)+
  theme_bw()+coord_cartesian(ylim=c(0,9))+
  theme_classic() 
p1+geom_errorbar(aes(ymax=mean+sem, ymin=mean-sem), position="dodge",width = 0.5)+
  scale_x_discrete(breaks = 1:length(condNames), labels=condNames)+
  theme(legend.key=element_blank(), legend.box="horizontal", legend.position = c(0.5, 0.95), legend.direction="horizontal")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text = element_text(size=30), axis.text.x = element_text(angle = 45, hjust=1,vjust=0.8))+
  xlab("Version Identified With")+ylab("d' Total (Original+Copy+Stranger)")+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))

######  PERFORMANCE SELF+COPY
quartz()

condNames <- c('Original', 'Copy', 'Both')
p1<-ggplot(self_copy_perf.mat, aes(x=factor(cond),y=mean))+
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",width = 0.5)+
  theme_bw()+coord_cartesian(ylim=c(0,9))+
  theme_classic() 
p1+geom_errorbar(aes(ymax=mean+sem, ymin=mean-sem), position="dodge",width = 0.5)+
  scale_x_discrete(breaks = 1:length(condNames), labels=condNames)+
  theme(legend.key=element_blank(), legend.box="horizontal", legend.position = c(0.5, 0.95), legend.direction="horizontal")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text = element_text(size=30), axis.text.x = element_text(angle = 45, hjust=1,vjust=0.8))+
  xlab("Version Identified With")+ylab("d' Total (Original + Copy)")+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))


###### GROUP TOTAL PERFORMANCE
condNames <- c('Original', 'Copy', 'Both')

p1<-ggplot(diff.mat, aes(x=factor(cond),y=mean))+
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",width = 0.5)+
  theme_bw()+coord_cartesian(ylim=c(0,1))+
  theme_classic() 
p1+geom_errorbar(aes(ymax=mean+sem, ymin=mean-sem), position="dodge",width = 0.5)+
  scale_x_discrete(breaks = 1:length(condNames), labels=condNames)+
  theme(legend.key=element_blank(), legend.box="horizontal", legend.position = c(0.5, 0.95), legend.direction="horizontal")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text = element_text(size=30), axis.text.x = element_text(angle = 45, hjust=1,vjust=0.8))+
  xlab("Version Identified With")+ylab("d' Performance Difference (Original - Copy)")+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))


###### INDIVIDUAL SUBJECTS

#individual subjects
title <- c('d prime, individual subjects')  
p2.1<-ggplot(d.mat,aes(x=factor(worker),y=d,fill=factor(agentCond))) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar")+
  theme_bw()+coord_cartesian(ylim=c(-1, 6))
p2.1+ggtitle(title)+
  scale_x_discrete(breaks = 1:length(workers), labels=workers)+
  theme(panel.grid.major = element_blank(),legend.position="none", panel.grid.minor = element_blank(), axis.title.y=element_blank())+
  theme(text = element_text(size=20), axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("Condition")+ylab("Mean")+scale_colour_manual(values=condNames)

#individual subjects, chose original
title <- c('Identified with original')  
p2.2<-ggplot(d.mat_o,aes(x=factor(worker),y=d,fill=factor(agentCond))) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar")+
  theme_bw()+coord_cartesian(ylim=c(-1, 6))
p2.2+ggtitle(title)+
  scale_x_discrete(breaks = 1:length(workers), labels=workers)+
  theme(panel.grid.major = element_blank(),legend.position="none", panel.grid.minor = element_blank(), axis.title.y=element_blank())+
  theme(text = element_text(size=20), axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("Condition")+ylab("Mean")+scale_colour_manual(values=condNames)

title <- c('Identified with copy')  
p2.3<-ggplot(d.mat_c,aes(x=factor(worker),y=d,fill=factor(agentCond))) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar")+
  theme_bw()+coord_cartesian(ylim=c(-1, 6))
p2.3+ggtitle(title)+
  scale_x_discrete(breaks = 1:length(workers), labels=workers)+
  theme(panel.grid.major = element_blank(),legend.position="none", panel.grid.minor = element_blank(), axis.title.y=element_blank())+
  theme(text = element_text(size=20), axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("Condition")+ylab("Mean")+scale_colour_manual(values=condNames)

title <- c('Identified with both')  
p2.4<-ggplot(d.mat_b,aes(x=factor(worker),y=d,fill=factor(agentCond))) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar")+
  theme_bw()+coord_cartesian(ylim=c(-1, 6))
p2.4+ggtitle(title)+
  scale_x_discrete(breaks = 1:length(workers), labels=workers)+
  theme(panel.grid.major = element_blank(),legend.position="none", panel.grid.minor = element_blank(), axis.title.y=element_blank())+
  theme(text = element_text(size=20), axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("Condition")+ylab("Mean")+scale_colour_manual(values=condNames)

quartz()
grid.arrange(p2.1, p2.2, p2.3, p2.4, nrow=2, ncol=2)




###### INDIVIDUAL SUBJECTS, ORDERED BY ORIGINAL - COPY DIFFERENCE

#individual subjects
p3.1<-ggplot(iden_ordered,aes(x=factor(counter),y=att_diff)) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",fill="darkgray")+
  theme_bw()+coord_cartesian(ylim=c(-2, 3))+
  theme_classic()
p3.1+scale_x_discrete(breaks = 1:length(workers), labels=iden_ordered$counter)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text = element_text(size=32), 
        axis.text.x = element_text(angle = 45, hjust=1,vjust=0.8, color="black"),
        axis.text.y = element_text(color="black"),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"))+
  xlab("Participant")+ylab("d' Difference (Original-Copy)")+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))+
  scale_fill_manual(values = c("gray"))

#individual subjects, chose original
title <- c('Identified with original')  
p3.2<-ggplot(iden_ordered_original,aes(x=factor(counter),y=att_diff)) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",fill="darkgray")+
  theme_bw()+coord_cartesian(ylim=c(-2, 3))+
  theme_classic()
p3.2+title(title)+scale_x_discrete(breaks = 1:length(workers), labels=iden_ordered$counter)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text = element_text(size=32), 
        axis.text.x = element_text(angle = 45, hjust=1,vjust=0.8, color="black"),
        axis.text.y = element_text(color="black"),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"))+
  xlab("Participant")+ylab("d' Difference (Original-Copy)")+
  theme(plot.title = element_text(hjust = 0.5), axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))+
  scale_fill_manual(values = c("gray"))

#individual subjects, chose copy
title <- c('Identified with copy')  
p3.3<-ggplot(iden_ordered_copy,aes(x=factor(counter),y=att_diff)) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",fill="darkgray")+
  theme_bw()+coord_cartesian(ylim=c(-2, 3))+
  theme_classic()
p3.3+title(title)+scale_x_discrete(breaks = 1:length(workers), labels=iden_ordered$counter)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text = element_text(size=32), 
        axis.text.x = element_text(angle = 45, hjust=1,vjust=0.8, color="black"),
        axis.text.y = element_text(color="black"),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"))+
  xlab("Participant")+ylab("d' Difference (Original-Copy)")+
  theme(plot.title = element_text(hjust = 0.5), axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))+
  scale_fill_manual(values = c("gray"))

#individual subjects, chose both
title <- c('Identified with both')  
p3.4<-ggplot(iden_ordered_both,aes(x=factor(counter),y=att_diff)) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",fill="darkgray")+
  theme_bw()+coord_cartesian(ylim=c(-2, 3))+
  theme_classic()
p3.4+title(title)+scale_x_discrete(breaks = 1:length(workers), labels=iden_ordered$counter)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text = element_text(size=32), 
        axis.text.x = element_text(angle = 45, hjust=1,vjust=0.8, color="black"),
        axis.text.y = element_text(color="black"),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"))+
  xlab("Participant")+ylab("d' Difference (Original-Copy)")+
  theme(plot.title = element_text(hjust = 0.5), axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))+
  scale_fill_manual(values = c("gray"))

quartz()
grid.arrange(p3.1, p3.2, p3.3, p3.4, nrow=2, ncol=2)

##pretty version

#1st plot
p1.11<-ggplot(iden_ordered,aes(x=factor(counter),y=att_diff)) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",face="bold")+
  coord_cartesian(ylim=c(-2, 3))+
  theme(axis.title.y = element_blank()) + 
  theme(axis.title.x = element_blank()) + 
  theme(legend.title = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(axis.ticks.y = element_blank())+
  theme_classic()

p1.111<-p1.11+scale_fill_discrete(name = "", labels = c ("Original", "Copy", "Stranger")) +
  xlab ("") + ylab ("") +
  theme(legend.text = element_text(size = "18", face = "plain")) +
  ggtitle ("All Participants") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.title = element_text(size = 18))

p1.1111<-p1.111+ theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(axis.text.y = element_text(size = 14))

######2nd plot
p1.12<-ggplot(iden_ordered_original,aes(x=factor(counter),y=att_diff)) + 
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",face="bold")+
  coord_cartesian(ylim=c(-2, 3))+
  theme(axis.title.y = element_blank()) + 
  theme(axis.title.x = element_blank()) + 
  theme(legend.title = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(axis.ticks.y = element_blank())+
  theme_classic()
p1.112<-p1.12+scale_fill_discrete(name = "", labels = c ("Original", "Copy", "Stranger")) +
  xlab ("") + ylab ("") +
  theme(legend.text = element_text(size = "18", face = "plain")) +
  ggtitle ("Identified with Original") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.title = element_text(size = 18))
p1.1112<-p1.112+theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(axis.text.y = element_blank())+
  theme(axis.ticks.y = element_blank())

########3rd plot 
p1.13<-ggplot(iden_ordered_copy,aes(x=factor(counter),y=att_diff)) + 
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",face="bold")+
  coord_cartesian(ylim=c(-2, 3))+
  theme(axis.title.y = element_blank()) + 
  theme(axis.title.x = element_blank()) + 
  theme(legend.title = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(axis.ticks.y = element_blank())+
  theme_classic()
p1.113<-p1.13+scale_fill_discrete(name = "", labels = c ("Original", "Copy", "Stranger")) +
  xlab ("") + ylab ("") +
  theme(legend.text = element_text(size = "18", face = "plain")) +
  ggtitle ("Identified with Copy") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.title = element_text(size = 18))
p1.1113<-p1.113+theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(axis.text.y = element_text(size = 14))

#######4th plot
p1.14<-ggplot(iden_ordered_both,aes(x=factor(counter),y=att_diff)) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",face="bold")+
  coord_cartesian(ylim=c(-2, 3))+
  theme(axis.title.y = element_blank()) + 
  theme(axis.title.x = element_blank()) + 
  theme(legend.title = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(axis.ticks.y = element_blank())+
  theme_classic()
p1.114<-p1.14+scale_fill_discrete(name = "", labels = c ("Original", "Copy", "Stranger")) +
  xlab ("") + ylab ("") +
  theme(legend.text = element_text(size = "18", face = "plain")) +
  ggtitle ("Identified with Both") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.title = element_text(size = 18))
p1.1114<-p1.114+theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(axis.text.y = element_blank())+
  theme(axis.ticks.y = element_blank())

quartz()
figure<-ggarrange(p1.1111,p1.1112,p1.1113,p1.1114, nrow=2,ncol=2,common.legend = TRUE, legend="top") 
annotate_figure(figure,top = NULL,left = text_grob("d' Original-Copy ", color="black", face ="bold",size=20)) 


###### INDIVIDUAL SUBJECTS, ORDERED BY TOTAL PERFORMANCE 

#individual subjects
p4.1<-ggplot(iden_ordered_perf,aes(x=factor(counter),y=total_perf)) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",fill="darkgray")+
  theme_bw()+coord_cartesian(ylim=c(-2, 14))+
  theme_classic()
p4.1+scale_x_discrete(breaks = 1:length(workers), labels=iden_ordered$counter)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text = element_text(size=32), 
        axis.text.x = element_text(angle = 45, hjust=1,vjust=0.8, color="black"),
        axis.text.y = element_text(color="black"),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"))+
  xlab("Participant")+ylab("Overall d' (Original+Copy+Stranger)")+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))+
  scale_fill_manual(values = c("gray"))

title <- c('Identified with original')  
p4.2<-ggplot(iden_ordered_perf_original,aes(x=factor(counter),y=total_perf)) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",fill="darkgray")+
  theme_bw()+coord_cartesian(ylim=c(-2, 14))+
  theme_classic()
p4.2+title(title)+scale_x_discrete(breaks = 1:length(workers), labels=iden_ordered$counter)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text = element_text(size=32), 
        axis.text.x = element_text(angle = 45, hjust=1,vjust=0.8, color="black"),
        axis.text.y = element_text(color="black"),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"))+
  xlab("Participant")+ylab("Overall d' (Original+Copy+Stranger)")+
  theme(plot.title = element_text(hjust = 0.5), axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))+
  scale_fill_manual(values = c("gray"))

title <- c('Identified with copy')  
p4.3<-ggplot(iden_ordered_perf_copy,aes(x=factor(counter),y=total_perf)) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",fill="darkgray")+
  theme_bw()+coord_cartesian(ylim=c(-2, 14))+
  theme_classic()
p4.3+title(title)+scale_x_discrete(breaks = 1:length(workers), labels=iden_ordered$counter)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text = element_text(size=32), 
        axis.text.x = element_text(angle = 45, hjust=1,vjust=0.8, color="black"),
        axis.text.y = element_text(color="black"),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"))+
  xlab("Participant")+ylab("Overall d' (Original+Copy+Stranger)")+
  theme(plot.title = element_text(hjust = 0.5), axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))+
  scale_fill_manual(values = c("gray"))

title <- c('Identified with both')  
p4.4<-ggplot(iden_ordered_perf_both,aes(x=factor(counter),y=total_perf)) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",fill="darkgray")+
  theme_bw()+coord_cartesian(ylim=c(-2, 14))+
  theme_classic()
p4.4+title(title)+scale_x_discrete(breaks = 1:length(workers), labels=iden_ordered$counter)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text = element_text(size=32), 
        axis.text.x = element_text(angle = 45, hjust=1,vjust=0.8, color="black"),
        axis.text.y = element_text(color="black"),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"))+
  xlab("Participant")+ylab("Overall d' (Original+Copy+Stranger)")+
  theme(plot.title = element_text(hjust = 0.5), axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))+
  scale_fill_manual(values = c("gray"))


quartz()
grid.arrange(p4.1, p4.2, p4.3, p4.4, nrow=2, ncol=2)

##pretty version

#1st plot
p1.11<-ggplot(iden_ordered_perf,aes(x=factor(counter),y=total_perf)) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",face="bold")+
  coord_cartesian(ylim=c(-2, 3))14
  theme(axis.title.y = element_blank()) + 
  theme(axis.title.x = element_blank()) + 
  theme(legend.title = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(axis.ticks.y = element_blank())+
  theme_classic()

p1.111<-p1.11+scale_fill_discrete(name = "", labels = c ("Original", "Copy", "Stranger")) +
  xlab ("") + ylab ("") +
  theme(legend.text = element_text(size = "18", face = "plain")) +
  ggtitle ("All Participants") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.title = element_text(size = 18))

p1.1111<-p1.111+ theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(axis.text.y = element_text(size = 14))

######2nd plot
p1.12<-ggplot(iden_ordered_perf_original,aes(x=factor(counter),y=total_perf)) + 
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",face="bold")+
  coord_cartesian(ylim=c(-2, 3))14
  theme(axis.title.y = element_blank()) + 
  theme(axis.title.x = element_blank()) + 
  theme(legend.title = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(axis.ticks.y = element_blank())+
  theme_classic()
p1.112<-p1.12+scale_fill_discrete(name = "", labels = c ("Original", "Copy", "Stranger")) +
  xlab ("") + ylab ("") +
  theme(legend.text = element_text(size = "18", face = "plain")) +
  ggtitle ("Identified with Original") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.title = element_text(size = 18))
p1.1112<-p1.112+theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(axis.text.y = element_blank())+
  theme(axis.ticks.y = element_blank())

########3rd plot 
p1.13<-ggplot(iden_ordered_perf_copy,aes(x=factor(counter),y=total_perf)) + 
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",face="bold")+
  coord_cartesian(ylim=c(-2, 3))14
  theme(axis.title.y = element_blank()) + 
  theme(axis.title.x = element_blank()) + 
  theme(legend.title = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(axis.ticks.y = element_blank())+
  theme_classic()
p1.113<-p1.13+scale_fill_discrete(name = "", labels = c ("Original", "Copy", "Stranger")) +
  xlab ("") + ylab ("") +
  theme(legend.text = element_text(size = "18", face = "plain")) +
  ggtitle ("Identified with Copy") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.title = element_text(size = 18))
p1.1113<-p1.113+theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(axis.text.y = element_text(size = 14))

#######4th plot
p1.14<-ggplot(iden_ordered_perf_both,aes(x=factor(counter),y=total_perf)) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",face="bold")+
  coord_cartesian(ylim=c(-2, 3))14
  theme(axis.title.y = element_blank()) + 
  theme(axis.title.x = element_blank()) + 
  theme(legend.title = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(axis.ticks.y = element_blank())+
  theme_classic()
p1.114<-p1.14+scale_fill_discrete(name = "", labels = c ("Original", "Copy", "Stranger")) +
  xlab ("") + ylab ("") +
  theme(legend.text = element_text(size = "18", face = "plain")) +
  ggtitle ("Identified with Both") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.title = element_text(size = 18))
p1.1114<-p1.114+theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(axis.text.y = element_blank())+
  theme(axis.ticks.y = element_blank())

quartz()
figure<-ggarrange(p1.1111,p1.1112,p1.1113,p1.1114, nrow=2,ncol=2,common.legend = TRUE, legend="top") 
annotate_figure(figure,top = NULL,left = text_grob("d' OriTotalcolor="black", face ="bold",size=20)) 



#attention: rt, group
title <- c('rt, group')  
p1<-ggplot(d.mat,aes(x=factor(agentCond),y=rt,fill=factor(agentCond))) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar")+theme_bw()+coord_cartesian(ylim=c(0, 1000)) 
p1+ggtitle(title)+
  scale_x_discrete(breaks = 1:length(workers), labels=workers)+
  theme(panel.grid.major = element_blank(),legend.position="none", panel.grid.minor = element_blank(), axis.title.y=element_blank())+
  theme(text = element_text(size=20), axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("Condition")+ylab("Mean")+scale_colour_manual(values=condNames)

#identity vs. attention
plot(iden.mat$diff, iden.mat$att_diff)
abline(lm(iden.mat$diff~iden.mat$att_diff), col="black", cex=5) 

##================================================================================================================
                                                        ##END##
##================================================================================================================




















