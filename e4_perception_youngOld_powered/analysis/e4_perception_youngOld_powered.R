#Julian De Freitas, 2019
#Analysis script for De Freitas, Rips, & Alvarez - The limit of personal identity
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
if (!require(ggpubr)) {install.packages("ggpubr"); require(ggpubr)}
if (!require(ggsignif)) {install.packages("ggsignif"); require(ggsignif)}

##================================================================================================================
                                              ##IMPORT DATA##
##================================================================================================================

## set directory to data folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory
setwd("../data/")

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

#check that we have equal numbers for each condition
table(data$image)
table(data$label)
table(data$agentCond)
table(data$matchCond)
table(data$condNum)

table(data$condNum)[1]/300
table(data$condNum)[2]/300
table(data$condNum)[3]/300

#number of workers before exclusions
n_bef_excl <- length(unique(data$workerId)); n_bef_excl

##================================================================================================================
                                                ##EXCLUSIONS##
##================================================================================================================

#perform exclusions: attention, comprehension, and rts < 200 (this ends up performing both subject and trial)
#note, we exclude based on mean accuracy further below
data <- subset(data,(data$attentionMCQ=="0" & data$comp =="D")) 
length(unique(data$workerId)) #1 subjects
data$rt[is.na(data$rt)] <- 0

#mark which trials had reasonable rts
for(i in 1:dim(data)[1]) {
  if(data$rt[i] >= 100) {
    data$badRt[i] = 0
  }
  else {
    data$badRt[i] = 1
  }
}

#exclude subjects with lower than 55% average accuracy
worker <- as.factor(data$workerId)
workers <- as.factor(unique(worker))
acc <- as.numeric(data$acc)
acc_use <- acc
acc_use[is.na(acc_use)] <- 0 #if na, just say it's accuracy of 0
perf_thresh <- 0.60
trial_thresh <- 0.4

ss_excl_mat <- array(0,dim=c(length(workers),2))
colnames(ss_excl_mat) <- c('mean_acc', 'rt_err_prop')

#if their accuracy < 55% or total bad RTs > 50% of trials, exclude them from dataset
for(i in 1:length(workers)) {
  ss_excl_mat[i,1] <- c(mean(acc_use[worker==workers[i]])) #get accuracy for each worker
  ss_excl_mat[i,2] <- sum(data$badRt[data$workerId == workers[i]])/length(data$rt[data$workerId == workers[i]])
  if( (ss_excl_mat[i,1] < perf_thresh) & (ss_excl_mat[i,2] > trial_thresh) ) {
    data <- subset(data,data$workerId != workers[i]) 
  }
}


#check it worked
worker <- as.factor(data$workerId)
workers <- as.factor(unique(worker))
acc <- as.numeric(data$acc)
acc_use <- acc
acc_use[is.na(acc_use)] <- 0

ss_excl_mat <- array(0,dim=c(length(workers),2))
colnames(ss_excl_mat) <- c('mean_acc', 'rt_err_prop')

for(i in 1:length(workers)) {
  ss_excl_mat[i,1] <- c(mean(acc_use[worker==workers[i]])) #get accuracy for each worker
  ss_excl_mat[i,2] <- sum(data$badRt[data$workerId == workers[i]])/length(data$rt[data$workerId == workers[i]])
}
ss_excl_mat

#final subjects after exclusions
length(unique(data$workerId)) #24 subjects

#check that numbers for each condition look reasonable
table(data$image)
table(data$label)
table(data$agentCond)
table(data$matchCond)
table(data$condNum)

#assign variable names
worker <- as.factor(data$workerId)
workers <- as.factor(unique(worker))
n_aft_excl <- length(workers); n_aft_excl
n_excl <- n_bef_excl - n_aft_excl; n_excl

age <- as.numeric(data$age); mean(age,na.rm = TRUE) 
gender <- as.factor(data$sex); table(gender)[1]/sum(table(gender)) 

##================================================================================================================
                                              ##DATA PREP##
##================================================================================================================

acc <- as.numeric(data$acc)
acc_use <- acc
acc_use[is.na(acc_use)] <- 0
numTrials <- dim(data)[1]
matchCond <- as.factor(data$matchCond)
agentCond <- as.factor(data$agentCond)
condNum <- as.factor(data$condNum)

#create numeric version of condition
for(i in 1:length(agentCond)) {
  if(condNum[i] == 1) {
    if(agentCond[i] == 'young-you') {
      data$agentCond_n[i] = 1
    }
    else if(agentCond[i] == 'stranger-john') {
      data$agentCond_n[i] = 2
    }
    else if(agentCond[i] == 'stranger-bill') {
      data$agentCond_n[i] = 3
    }
  }
  else if(condNum[i] == 2) {
    if(agentCond[i] == 'young-you') {
      data$agentCond_n[i] = 1
    }
    else if(agentCond[i] == 'old-you') {
      data$agentCond_n[i] = 2
    }
    else if(agentCond[i] == 'stranger-john') {
      data$agentCond_n[i] = 3
    }
    data$condNum[i] <- 3
  }
  else if(condNum[i] == 3) {
    if(agentCond[i] == 'old-you') {
      data$agentCond_n[i] = 1
    }
    else if(agentCond[i] == 'stranger-john') {
      data$agentCond_n[i] = 2
    }
    else if(agentCond[i] == 'stranger-bill') {
      data$agentCond_n[i] = 3
    }
    data$condNum[i] <- 2
  }
}

condNum <- as.factor(data$condNum)
agentCond_n <- as.factor(data$agentCond_n)
ans <- as.factor(data$ans)
corrAns <- as.factor(data$corrAns)
rts <- log(as.numeric(data$rt))
agentConds_n <- as.factor(unique(agentCond_n))
matchConds <- unique(matchCond)
label <- as.factor(data$label)

#create matrix for collecting d prime-relevant values, and rts
d_mat <- array(0,dim=c(3*length(workers),7))
colnames(d_mat) <- c('worker', 'mainCond', 'agentCond', 'hits', 'false_alarms', 'd', 'rt')
d.mat <- as.data.frame(d_mat, stringsAsFactors=FALSE); d.mat

#for each worker and agent condition, get average hits, false alarms, d-prime, and rt
#hits: proportion of true hits you said were hits (out of all true hits)
#false alarms: proportin of non hits you said were hits (out of all non hits)
#get rts for trials that were correct
counter <- 1
for(i in 1:length(workers)) {
  for(j in 1:length(agentConds_n)) {
        length_h <- length(acc_use[worker==workers[i] & agentCond_n==j & corrAns=='y'])
        length_fa <- length(acc_use[worker==workers[i] & agentCond_n==j & corrAns=='n'])
        
        h <- length(acc_use[worker==workers[i] & agentCond_n==j & ans=='y' & corrAns=='y'])/length_h
        fa <- length(acc_use[worker==workers[i] & agentCond_n==j & ans=='y' & corrAns=='n'])/length_fa
        rt <- mean(rts[worker==workers[i] & agentCond_n==j & acc_use==1], na.rm=TRUE)
       
        #see https://stats.stackexchange.com/questions/134779/d-prime-with-100-hit-rate-probability-and-0-false-alarm-probability
        if(h==1) { h <- (length_h - 0.5) / length_h }
        if(h==0) { h <- 0.5 / length_h }
        if(fa==1) { fa <- (length_fa - 0.5) / length_fa }
        if(fa==0) { fa <- 0.5 / length_fa }
        
        d.mat[counter,] <- c(workers[i],unique(condNum[worker==workers[i]]), j, h, fa, qnorm(h) - qnorm(fa), rt)
        counter = counter + 1
  }
} 

# collect total performance and performance difference (cond 1 - 2) for each subject
perf_mat <- array(0, dim=c(length(workers), 3))
colnames(perf_mat) <- c('mainCond', 'perf_diff', 'total_perf')
perf.mat <- as.data.frame(perf_mat, stringsAsFactors=FALSE); perf.mat

for(i in 1:length(workers)) {
  perf.mat[i,1] <- unique(d.mat$mainCond[d.mat$worker==i])
  if(unique(d.mat$mainCond[d.mat$worker==i]) %in% c(1, 2)) {
    #future-you1 - john
    perf.mat[i,2] <- d.mat$d[d.mat$worker==i & d.mat$agentCond == 1] - d.mat$d[d.mat$worker==i & d.mat$agentCond == 2] 
  }
  else if(unique(d.mat$mainCond[d.mat$worker==i]) == 3) {
    #best of future-you1 and future-you2 - john
    perf.mat[i,2] <- max(d.mat$d[d.mat$worker==i & d.mat$agentCond == 1], d.mat$d[d.mat$worker==i & d.mat$agentCond == 2]) - d.mat$d[d.mat$worker==i & d.mat$agentCond == 3] 
  }
  perf.mat[i,3] <- d.mat$d[d.mat$worker==i & d.mat$agentCond == 1] + d.mat$d[d.mat$worker==i & d.mat$agentCond == 2] + d.mat$d[d.mat$worker==i & d.mat$agentCond == 3]
}


# make matrix for measuring confusions
conf_mat <- array(0,dim=c(3*length(workers),7))
colnames(conf_mat) <- c('worker', 'self_cond', 'agentCond', 'n_fa', 'n_self', 'n_stranger', 'diff')
conf.mat <- as.data.frame(conf_mat, stringsAsFactors=FALSE); conf.mat

counter <- 1
for(i in 1:length(workers)) {
  for(j in 1:length(agentConds_n)) {
    total_fa <- length(acc_use[worker==workers[i] & agentCond_n==j & corrAns=='n'])
    length_fa <- length(acc_use[worker==workers[i] & agentCond_n==j & ans=='y' & corrAns=='n'])/total_fa
    length_self <- length(acc_use[worker==workers[i] & agentCond_n==j & ans=='y' & corrAns=='n' & (label == 'young-you' | label == 'old-you')])/total_fa
    length_stranger <- length(acc_use[worker==workers[i] & agentCond_n==j & ans=='y' & corrAns=='n' & label == 'stranger-john'])/total_fa
    
    conf.mat[counter,] <- c (workers[i], unique(condNum[worker==workers[i]]),
                             j, length_fa, length_self, length_stranger, length_self - length_stranger )
    counter = counter + 1
  }
}

conf.mat <- subset(conf.mat, conf.mat$self_cond == 3); conf.mat

#===============================================================================================================
                                  ##ANALYSIS##
##================================================================================================================

#------- GROUP MEANS--------#
p_mat <- rep(9, times = 5)
star_mat <- rep(9, times = 5)

#one self 
d_one <- subset(d.mat, d.mat$mainCond==1)

mean(d_one$d[d_one$agentCond==1]) #future-you1
sd(d_one$d[d_one$agentCond==1])
n_o_1 = length(d_one$d[d_one$agentCond==1]); n_o_1

mean(d_one$d[d_one$agentCond==2]) #stranger-john
sd(d_one$d[d_one$agentCond==2])
n_o_2 = length(d_one$d[d_one$agentCond==2])

mean(d_one$d[d_one$agentCond==3]) #stranger-bill
sd(d_one$d[d_one$agentCond==3])
n_o_3 = length(d_one$d[d_one$agentCond==3])

att_1_o <- t.test(d_one$d[d_one$agentCond==1 | d_one$agentCond==2] ~ d_one$agentCond[d_one$agentCond==1 | d_one$agentCond==2], var.equal=TRUE, paired=TRUE); att_1_o
att_2_o <- t.test(d_one$d[d_one$agentCond==1 | d_one$agentCond==3] ~ d_one$agentCond[d_one$agentCond==1 | d_one$agentCond==3], var.equal=TRUE, paired=TRUE); att_2_o
att_3_o <- t.test(d_one$d[d_one$agentCond==2 | d_one$agentCond==3] ~ d_one$agentCond[d_one$agentCond==2 | d_one$agentCond==3], var.equal=TRUE, paired=TRUE); att_3_o

tes(as.numeric(att_1_o[1]), n_o_1, n_o_2) #cohen's d
tes(as.numeric(att_2_o[1]), n_o_1, n_o_3) #cohen's d
tes(as.numeric(att_3_o[1]), n_o_2, n_o_3) #cohen's d

#one self #2
d_oneAlt <- subset(d.mat, d.mat$mainCond==2)

mean(d_oneAlt$d[d_oneAlt$agentCond==1]) #future-you2
sd(d_oneAlt$d[d_oneAlt$agentCond==1])
n_o_1 = length(d_oneAlt$d[d_oneAlt$agentCond==1]); n_o_1

mean(d_oneAlt$d[d_oneAlt$agentCond==2]) #stranger-john
sd(d_oneAlt$d[d_oneAlt$agentCond==2])
n_o_2 = length(d_oneAlt$d[d_oneAlt$agentCond==2]); n_o_2

mean(d_oneAlt$d[d_oneAlt$agentCond==3]) #stranger-bill
sd(d_oneAlt$d[d_oneAlt$agentCond==3])
n_o_3 = length(d_oneAlt$d[d_oneAlt$agentCond==3]); n_o_3

att_1_alt <- t.test(d_oneAlt$d[d_oneAlt$agentCond==1 | d_oneAlt$agentCond==2] ~ d_oneAlt$agentCond[d_oneAlt$agentCond==1 | d_oneAlt$agentCond==2], var.equal=TRUE, paired=TRUE); att_1_alt
att_2_alt <- t.test(d_oneAlt$d[d_oneAlt$agentCond==1 | d_oneAlt$agentCond==3] ~ d_oneAlt$agentCond[d_oneAlt$agentCond==1 | d_oneAlt$agentCond==3], var.equal=TRUE, paired=TRUE); att_2_alt
att_3_alt <- t.test(d_oneAlt$d[d_oneAlt$agentCond==2 | d_oneAlt$agentCond==3] ~ d_oneAlt$agentCond[d_oneAlt$agentCond==2 | d_oneAlt$agentCond==3], var.equal=TRUE, paired=TRUE); att_3_alt

tes(as.numeric(att_1_alt[1]), n_o_1, n_o_2) #cohen's d
tes(as.numeric(att_2_alt[1]), n_o_1, n_o_3) #cohen's d
tes(as.numeric(att_3_alt[1]), n_o_2, n_o_3) #cohen's d

#two selves
d_two <- subset(d.mat, d.mat$mainCond==3)

mean(d_two$d[d_two$agentCond==1]) #future-you1
sd(d_two$d[d_two$agentCond==1])
n_o_1 = length(d_two$d[d_two$agentCond==1]); n_o_1

mean(d_two$d[d_two$agentCond==2]) #future-you2
sd(d_two$d[d_two$agentCond==2])
n_o_2 = length(d_two$d[d_two$agentCond==2])

mean(d_two$d[d_two$agentCond==3]) #stranger-bill
sd(d_two$d[d_two$agentCond==3])
n_o_3 = length(d_two$d[d_two$agentCond==3])

att_1_b <- t.test(d_two$d[d_two$agentCond==1 | d_two$agentCond==2] ~ d_two$agentCond[d_two$agentCond==1 | d_two$agentCond==2], var.equal=TRUE, paired=TRUE); att_1_b
att_2_b <- t.test(d_two$d[d_two$agentCond==1 | d_two$agentCond==3] ~ d_two$agentCond[d_two$agentCond==1 | d_two$agentCond==3], var.equal=TRUE, paired=TRUE); att_2_b
att_3_b <- t.test(d_two$d[d_two$agentCond==2 | d_two$agentCond==3] ~ d_two$agentCond[d_two$agentCond==2 | d_two$agentCond==3], var.equal=TRUE, paired=TRUE); att_3_b

tes(as.numeric(att_1_b[1]), n_o_1, n_o_2) #cohen's d
tes(as.numeric(att_2_b[1]), n_o_1, n_o_3) #cohen's d
tes(as.numeric(att_3_b[1]), n_o_2, n_o_3) #cohen's d

#------- TOTAL PERFORMANCE--------#

mean(perf.mat$total_perf[perf.mat$mainCond == 1])
sd(perf.mat$total_perf[perf.mat$mainCond == 1])

mean(perf.mat$total_perf[perf.mat$mainCond == 2])
sd(perf.mat$total_perf[perf.mat$mainCond == 2])

mean(perf.mat$total_perf[perf.mat$mainCond == 3])
sd(perf.mat$total_perf[perf.mat$mainCond == 3])

perf_1 <- t.test(perf.mat$total_perf[perf.mat$mainCond == 1 | perf.mat$mainCond == 3] ~ perf.mat$mainCond[perf.mat$mainCond == 1 | perf.mat$mainCond == 3], var.equal=TRUE, paired=FALSE); perf_1
perf_2 <- t.test(perf.mat$total_perf[perf.mat$mainCond == 2 | perf.mat$mainCond == 3] ~ perf.mat$mainCond[perf.mat$mainCond == 2 | perf.mat$mainCond == 3], var.equal=TRUE, paired=FALSE); perf_2

tes(as.numeric(perf_1[1]), length(workers), length(workers)) #cohen's d
tes(as.numeric(perf_2[1]), length(workers), length(workers)) #cohen's d


#------- PERFORMANCE DIFF --------#
mean(perf.mat$perf_diff[perf.mat$mainCond == 1])
sd(perf.mat$perf_diff[perf.mat$mainCond == 1])

mean(perf.mat$perf_diff[perf.mat$mainCond == 2])
sd(perf.mat$perf_diff[perf.mat$mainCond == 2])

mean(perf.mat$perf_diff[perf.mat$mainCond == 3])
sd(perf.mat$perf_diff[perf.mat$mainCond == 3])

perfd_1 <- t.test(perf.mat$perf_diff[perf.mat$mainCond == 1 | perf.mat$mainCond == 3] ~ perf.mat$mainCond[perf.mat$mainCond == 1 | perf.mat$mainCond == 3], var.equal=TRUE, paired=FALSE); perfd_1
perfd_2 <- t.test(perf.mat$perf_diff[perf.mat$mainCond == 2 | perf.mat$mainCond == 3] ~ perf.mat$mainCond[perf.mat$mainCond == 2 | perf.mat$mainCond == 3], var.equal=TRUE, paired=FALSE); perfd_2

tes(as.numeric(perfd_1[1]), length(workers), length(workers)) #cohen's d
tes(as.numeric(perfd_2[1]), length(workers), length(workers)) #cohen's d


#***mean performance, for norming plot
((mean(perf.mat$total_perf[perf.mat$mainCond == 3]) - mean(perf.mat$total_perf[perf.mat$mainCond == 1])) + (mean(perf.mat$total_perf[perf.mat$mainCond == 3]) - mean(perf.mat$total_perf[perf.mat$mainCond == 2])))/2 

p_mat <- c(att_1_o[3], att_1_alt[3], att_1_b[3], perf_1[3], perf_2[3])
for(i in 1:length(p_mat)) {
  if(p_mat[i] > 0.10) {
    star_mat[i] = 'ns'
  }
  else if( (p_mat[i] < 0.10) & (p_mat[i] > 0.05) ) { 
    star_mat[i] = '\u2020'
  }
  else if( (p_mat[i] < 0.05) & (p_mat[i] > 0.01) ) { 
    star_mat[i] = '*'
  }
  else if( (p_mat[i] < 0.01) & (p_mat[i] > 0.001) ) { 
    star_mat[i] = '**'
  }
  else if(p_mat[i] < 0.001) { 
    star_mat[i] = '***'
  }
}

#===################## CONFUSIONS
#For cases in which (i) the shape represented the self, (ii) the label was a mismatch,
#and (iii) people were instructed to imagine 2 selves, did people do worse when the
#label represented another self vs. a stranger?

mean(conf.mat$diff[conf.mat$agentCond == 1])
sd(conf.mat$diff[conf.mat$agentCond == 1])
mean(conf.mat$diff[conf.mat$agentCond == 2])
sd(conf.mat$diff[conf.mat$agentCond == 2])

conf_1 <- t.test(conf.mat$n_self[conf.mat$agentCond==1], conf.mat$n_stranger[conf.mat$agentCond==1], var.equal=TRUE, paired=TRUE); conf_1
tes(as.numeric(conf_1[1]), dim(conf.mat)[1],  dim(conf.mat)[1]) #cohen's d

conf_2 <- t.test(conf.mat$n_self[conf.mat$agentCond==2], conf.mat$n_stranger[conf.mat$agentCond==2], var.equal=TRUE, paired=TRUE); conf_2
tes(as.numeric(conf_2[1]), dim(conf.mat)[1],  dim(conf.mat)[1]) #cohen's d

conf_diff <- t.test(conf.mat$diff[conf.mat$agentCond==1], conf.mat$diff[conf.mat$agentCond==2], var.equal=TRUE, paired=TRUE); conf_diff

#=============================================================================================================
##PREPARE FOR PLOTTING##
##================================================================================================================

#make d mat for plotting: condition 1
d_mat_plot_one <- array(0,dim=c(3, 5))
colnames(d_mat_plot_one) <- c('cond','mean','sd','n','sem')
perf_mat_plot_one <- as.data.frame(d_mat_plot_one, stringsAsFactors=FALSE); d_mat_plot_one

for(i in 1:length(unique(agentCond_n))) {
  d_mat_plot_one[i, ] <- c(i, mean(d_one$d[d_one$agentCond == i]), sd(d_one$d[d_one$agentCond == i]), length(d_one$d[d_one$agentCond == i]), 0)
  d_mat_plot_one[i, 5] <- d_mat_plot_one[i,3]/sqrt(d_mat_plot_one[i,4])
}
d_mat_plot_one
d.one <- as.data.frame(d_mat_plot_one, stringsAsFactors=FALSE); d.one

#make d mat for plotting: condition 1 alternative
d_mat_plot_alt <- array(0,dim=c(3, 5))
colnames(d_mat_plot_alt) <- c('cond','mean','sd','n','sem')
perf_mat_plot_alt <- as.data.frame(d_mat_plot_alt, stringsAsFactors=FALSE); d_mat_plot_alt

for(i in 1:length(unique(agentCond_n))) {
  d_mat_plot_alt[i, ] <- c(i, mean(d_oneAlt$d[d_oneAlt$agentCond == i]), sd(d_oneAlt$d[d_oneAlt$agentCond == i]), length(d_oneAlt$d[d_oneAlt$agentCond == i]), 0)
  d_mat_plot_alt[i, 5] <- d_mat_plot_alt[i,3]/sqrt(d_mat_plot_alt[i,4])
}
d_mat_plot_alt
d.oneAlt <- as.data.frame(d_mat_plot_alt, stringsAsFactors=FALSE); d.oneAlt


#make d mat for plotting: condition 2
d_mat_plot_two <- array(0,dim=c(3, 5))
colnames(d_mat_plot_two) <- c('cond','mean','sd','n','sem')
perf_mat_plot_two <- as.data.frame(d_mat_plot_two, stringsAsFactors=FALSE); d_mat_plot_two

for(i in 1:length(unique(agentCond_n))) {
  d_mat_plot_two[i, ] <- c(i, mean(d_two$d[d_two$agentCond == i]), sd(d_two$d[d_two$agentCond == i]), length(d_two$d[d_two$agentCond == i]), 0)
  d_mat_plot_two[i, 5] <- d_mat_plot_two[i,3]/sqrt(d_mat_plot_two[i,4])
}
d_mat_plot_two
d.two <- as.data.frame(d_mat_plot_two, stringsAsFactors=FALSE); d.two

# make performance mat for plotting
perf_mat_plot <- array(0,dim=c(3, 5))
colnames(perf_mat_plot) <- c('cond','mean','sd','n','sem')
perf_mat_plot <- as.data.frame(perf_mat_plot, stringsAsFactors=FALSE); perf_mat_plot

for(i in 1:length(unique(condNum))) {
  perf_mat_plot[i, ] <- c(i, mean(perf.mat$total_perf[perf.mat$mainCond == i]), sd(perf.mat$total_perf[perf.mat$mainCond == i]), length(perf.mat$total_perf[perf.mat$mainCond == i]), 0)
  perf_mat_plot[i,5] <- perf_mat_plot[i,3]/sqrt(perf_mat_plot[i,4])
}
perf_mat_plot

#==========================================================================================================
##PLOT##
##================================================================================================================


#identified with future1
p1.11<-ggplot(d.one,aes(x=factor(cond),y=mean,fill=factor(cond))) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar", face="bold")+
  coord_cartesian(ylim=c(0, 2.5))+
  theme(axis.title.y = element_blank()) + 
  theme(axis.title.x = element_blank()) + 
  theme(legend.title = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme_classic()

p1.111<-p1.11+scale_fill_discrete(name = "", labels = c ("True\nYou", "Stranger\nJohn", "Stranger\nBill")) +
  xlab ("") + ylab ("") +
  theme(legend.text = element_text(size = "16", face = "plain")) +
  geom_errorbar(aes(ymax=mean+sem, ymin=mean-sem), position="dodge", size=1.5)+
  geom_signif(data=d.one,
              aes(xmin=1, xmax=2, annotations=star_mat[1], y_position=2.1),
              textsize = 12, vjust = 0.3,
              manual=TRUE, size=1.5) +
  #ggtitle ("Self #1") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title = element_text(size = 26))

p1.1111<-p1.111+ theme(axis.text.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.line = element_line(colour = '#585858', size = 1.5), axis.text.y = element_blank(), axis.ticks.length=unit(.25, "cm"), axis.ticks = element_line(colour = "black", size = 1.5)) + scale_fill_manual(values=c("#802520", "#78a973", "#3e70bd"))

#identified with future2
p1.12<-ggplot(d.oneAlt,aes(x=factor(cond),y=mean,fill=factor(cond))) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar", face="bold")+
  coord_cartesian(ylim=c(0, 2.5))+
  theme(axis.title.y = element_blank()) + 
  theme(axis.title.x = element_blank()) + 
  theme(legend.title = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme_classic()

p1.112<-p1.12+scale_fill_discrete(name = "", labels = c ("Surface\nYou", "Stranger\nJohn", "Stranger\nBill")) +
  xlab ("") + ylab ("") +
  theme(legend.text = element_text(size = "16", face = "plain")) +
  geom_errorbar(aes(ymax=mean+sem, ymin=mean-sem), position="dodge", size=1.5)+
  geom_signif(data=d.one,
              aes(xmin=1, xmax=2, annotations=star_mat[2], y_position=2.1),
              textsize = 12, vjust = 0.3,
              manual=TRUE, size=1.5) +
  #ggtitle ("Self #2") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title = element_text(size = 26))

p1.1112<-p1.112+ theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank()) +
  theme(axis.line = element_line(colour = '#585858', size = 1.5), axis.text.y = element_blank(), axis.ticks.length=unit(.25, "cm"), axis.ticks = element_line(colour = "black", size = 1.5)) + scale_fill_manual(values=c("#d95b5f", "#78a973", "#3e70bd"))

p1.14<-ggplot(d.two,aes(x=factor(cond),y=mean,fill=factor(cond))) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",face="bold")+
  coord_cartesian(ylim=c(0, 2.5))+
  theme(axis.title.y = element_blank()) + 
  theme(axis.title.x = element_blank()) + 
  theme(legend.title = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme_classic()

p1.114<-p1.14+scale_fill_discrete(name = "", labels = c ("True\nYou", "Surface\nYou", "Stranger\nJohn")) +
  xlab ("") + ylab ("") +
  theme(legend.text = element_text(size = "16", face = "plain")) +
  geom_errorbar(aes(ymax=mean+sem, ymin=mean-sem), position="dodge", size=1.5)+
  geom_signif(data=d.two,
              aes(xmin=1, xmax=2, annotations=star_mat[3], y_position=2.1),
              textsize = 12, vjust = -0.1,
              manual=TRUE, size=1.5) +
  #ggtitle ("Two Selves") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title = element_text(size = 26))

p1.1114<-p1.114+theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(axis.line = element_line(colour = '#585858', size = 1.5), axis.text.y = element_blank(), axis.ticks.length=unit(.25, "cm"), axis.ticks = element_line(colour = "black", size = 1.5)) + scale_fill_manual(values=c("#802520", "#d95b5f", "#78a973"))

#total performance
p1.15<-ggplot(perf_mat_plot,aes(x=factor(cond),y=mean)) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",face="bold")+
  coord_cartesian(ylim=c(0, 6))+
  theme(axis.title.y = element_blank()) + 
  theme(axis.title.x = element_blank()) + 
  theme(legend.title = element_blank())+
  theme(axis.ticks.x = element_blank(), margin = margin(t = 3))+
  theme_classic()

p1.115<-p1.15+scale_fill_discrete(name = "", labels = c ("Original", "Copy", "Stranger")) +
  xlab ("") + ylab ("") +
  geom_errorbar(aes(ymax=mean+sem, ymin=mean-sem), position="dodge", size=1.5)+
  geom_signif(data=perf_mat_plot,
              aes(xmin=1, xmax=3, annotations=star_mat[4], y_position=5.6),
              textsize = 12, vjust = -0.1,
              manual=TRUE, size=1.5) +
  geom_signif(data=perf_mat_plot,
              aes(xmin=2, xmax=3, annotations=star_mat[5], y_position=4.5),
              textsize = 12, vjust = -0.3,
              manual=TRUE, size=1.5) +
  #ggtitle ("Total Performance:\n One Self v. Two Selves") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title = element_text(size = 22))+
  theme(axis.text.y = element_text(size = 26))

p1.1115<-p1.115+ theme(axis.text.x = element_blank())+
  theme(axis.line = element_line(colour = '#585858', size = 1.5), axis.ticks.x = element_blank(), axis.ticks.length=unit(.25, "cm"), axis.ticks = element_line(colour = "black", size = 1.5))

figure <- ggarrange(p1.1111, p1.1112, p1.1114, p1.1115, nrow=1,ncol=4,common.legend = FALSE, legend="none", vjust = -1.0)

png("../fig1.png", width = 8640 * 2.5/6, height = 2560 * 2.5/6, res=300)
figure
dev.off()


##=====##
## END ##
##=====##
