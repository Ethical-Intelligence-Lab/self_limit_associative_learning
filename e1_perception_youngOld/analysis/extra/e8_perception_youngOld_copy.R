#Julian De Freitas, 2019
#Analysis script for De Freitas & Alvarez - Personal Identity, Self-Projection, and Psychological Essentialism
#Experiment 7

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

##================================================================================================================
                                              ##IMPORT DATA##
##================================================================================================================

## set directory to data folder
dir <- setwd("/Users/julian/Dropbox (Personal)/Research/Intuition/single_identity/e9_perception_youngOld/data")

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

#number of workers before exclusions
length(unique(data$workerId)) #12 subjects

##================================================================================================================
                                                ##DATA PREP##
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
perf_thresh <- 0.55
trial_thresh <- 0.5

ss_excl_mat <- array(0,dim=c(length(workers),2))
colnames(ss_excl_mat) <- c('mean_acc', 'rt_err_prop')

#if their accuracy < 55% or total bad RTs > 50% of trials, exclude them from dataset
for(i in 1:length(workers)) {
  ss_excl_mat[i,1] <- c(mean(acc_use[worker==workers[i]])) #get accuracy for each worker
  ss_excl_mat[i,2] <- sum(data$badRt[data$workerId == workers[i]])/length(data$rt[data$workerId == workers[i]])
  if( (ss_excl_mat[i,1] < perf_thresh) | (ss_excl_mat[i,2] > trial_thresh) ) {
    data <- subset(data,data$workerId != workers[i]) 
  }
}
ss_excl_mat

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
  }
}

condNum <- as.factor(data$condNum)
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
       
        #correction for if h=1 or fa=0, by (Macmillan & Kaplan, 1985)
        #if fa = 0, 0.5/𝑛; ifh= 1, (𝑛−0.5)/𝑛 , where n is the number of signal or noise trials
        #see https://stats.stackexchange.com/questions/134779/d-prime-with-100-hit-rate-probability-and-0-false-alarm-probability
        if(h==1) {
          h <- (length_h-0.5)/length_h
        }
        
        if(fa==0) {
          fa <- 0.05/length_fa
        }
        
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
  if(unique(d.mat$mainCond[d.mat$worker==i]) == 1) {
    #future-you1 - john
    perf.mat[i,2] <- d.mat$d[d.mat$worker==i & d.mat$agentCond == 1] - d.mat$d[d.mat$worker==i & d.mat$agentCond == 2] 
  }
  else if(unique(d.mat$mainCond[d.mat$worker==i]) == 2) {
    #best of future-you1 and future-you2 - john
    perf.mat[i,2] <- max(d.mat$d[d.mat$worker==i & d.mat$agentCond == 1], d.mat$d[d.mat$worker==i & d.mat$agentCond == 2]) - d.mat$d[d.mat$worker==i & d.mat$agentCond == 3] 
  }
  perf.mat[i,3] <- d.mat$d[d.mat$worker==i & d.mat$agentCond == 1] + d.mat$d[d.mat$worker==i & d.mat$agentCond == 2] + d.mat$d[d.mat$worker==i & d.mat$agentCond == 3]
}

##================================================================================================================
                                                      ##ANALYSIS##
##================================================================================================================

#------- GROUP MEANS--------#

#one self 
d_one <- subset(d.mat, d.mat$mainCond==1)

mean(d_one$d[d_one$agentCond==1]) #future-you1
sd(d_one$d[d_one$agentCond==1])

mean(d_one$d[d_one$agentCond==2]) #stranger-john
sd(d_one$d[d_one$agentCond==2])

mean(d_one$d[d_one$agentCond==3]) #stranger-bill
sd(d_one$d[d_one$agentCond==3])

att_1 <- t.test(d_one$d[d_one$agentCond==1 | d_one$agentCond==2] ~ d_one$agentCond[d_one$agentCond==1 | d_one$agentCond==2], var.equal=TRUE, paired=TRUE); att_1
att_2 <- t.test(d_one$d[d_one$agentCond==1 | d_one$agentCond==3] ~ d_one$agentCond[d_one$agentCond==1 | d_one$agentCond==3], var.equal=TRUE, paired=TRUE); att_2
att_3 <- t.test(d_one$d[d_one$agentCond==2 | d_one$agentCond==3] ~ d_one$agentCond[d_one$agentCond==2 | d_one$agentCond==3], var.equal=TRUE, paired=TRUE); att_3

tes(as.numeric(att_1[1]), length(workers), length(workers)) #cohen's d
tes(as.numeric(att_2[1]), length(workers), length(workers)) #cohen's d
tes(as.numeric(att_3[1]), length(workers), length(workers)) #cohen's d

#two selves
d_two <- subset(d.mat, d.mat$mainCond==2)

mean(d_two$d[d_two$agentCond==1]) #future-you1
sd(d_two$d[d_two$agentCond==1])

mean(d_two$d[d_two$agentCond==2]) #future-you2
sd(d_two$d[d_two$agentCond==2])

mean(d_two$d[d_two$agentCond==3]) #stranger-bill
sd(d_two$d[d_two$agentCond==3])

att_1 <- t.test(d_two$d[d_two$agentCond==1 | d_two$agentCond==2] ~ d_two$agentCond[d_two$agentCond==1 | d_two$agentCond==2], var.equal=TRUE, paired=TRUE); att_1
att_2 <- t.test(d_two$d[d_two$agentCond==1 | d_two$agentCond==3] ~ d_two$agentCond[d_two$agentCond==1 | d_two$agentCond==3], var.equal=TRUE, paired=TRUE); att_2
att_3 <- t.test(d_two$d[d_two$agentCond==2 | d_two$agentCond==3] ~ d_two$agentCond[d_two$agentCond==2 | d_two$agentCond==3], var.equal=TRUE, paired=TRUE); att_3

tes(as.numeric(att_1[1]), length(workers), length(workers)) #cohen's d
tes(as.numeric(att_2[1]), length(workers), length(workers)) #cohen's d
tes(as.numeric(att_3[1]), length(workers), length(workers)) #cohen's d

#------- TOTAL PERFORMANCE--------#

mean(perf.mat$total_perf[perf.mat$mainCond == 1])
sd(perf.mat$total_perf[perf.mat$mainCond == 1])

mean(perf.mat$total_perf[perf.mat$mainCond == 2])
sd(perf.mat$total_perf[perf.mat$mainCond == 2])

perf_1 <- t.test(perf.mat$total_perf ~ perf.mat$mainCond, var.equal=TRUE, paired=FALSE); perf_1
tes(as.numeric(perf_1[1]), length(workers), length(workers)) #cohen's d

#-------PERFORMANCE DIFF --------#

mean(perf.mat$perf_diff[perf.mat$mainCond == 1])
sd(perf.mat$perf_diff[perf.mat$mainCond == 1])

mean(perf.mat$perf_diff[perf.mat$mainCond == 2])
sd(perf.mat$perf_diff[perf.mat$mainCond == 2])

perf_2 <- t.test(perf.mat$perf_diff ~ perf.mat$mainCond, var.equal=TRUE, paired=FALSE); perf_2
tes(as.numeric(perf_2[1]), length(workers), length(workers)) #cohen's d

#divide performance data into two subsets, for plotting purposes

#================================================================================================================
                                                        ##PLOT##
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
perf_mat_plot <- array(0,dim=c(2, 5))
colnames(perf_mat_plot) <- c('cond','mean','sd','n','sem')
perf_mat_plot <- as.data.frame(perf_mat_plot, stringsAsFactors=FALSE); perf_mat_plot

for(i in 1:length(unique(condNum))) {
  perf_mat_plot[i, ] <- c(i, mean(perf.mat$total_perf[perf.mat$mainCond == i]), sd(perf.mat$total_perf[perf.mat$mainCond == i]), length(perf.mat$total_perf[perf.mat$mainCond == i]), 0)
  perf_mat_plot[i,5] <- perf_mat_plot[i,3]/sqrt(perf_mat_plot[i,4])
}
perf_mat_plot

# make performance diff mat for plotting
perf_diff_mat_plot <- array(0,dim=c(2, 5))
colnames(perf_diff_mat_plot) <- c('cond','mean','sd','n','sem')
perf_diff_mat_plot <- as.data.frame(perf_diff_mat_plot, stringsAsFactors=FALSE); perf_diff_mat_plot

for(i in 1:length(unique(condNum))) {
  perf_diff_mat_plot[i, ] <- c(i, mean(perf.mat$perf_diff[perf.mat$mainCond == i]), sd(perf.mat$perf_diff[perf.mat$mainCond == i]), length(perf.mat$perf_diff[perf.mat$mainCond == i]), 0)
  perf_diff_mat_plot[i,5] <- perf_diff_mat_plot[i,3]/sqrt(perf_diff_mat_plot[i,4])
}
perf_diff_mat_plot

# make ordered performance mat for plotting
perf_ordered_diff <- perf.mat[order(-perf.mat$perf_diff),]
perf_ordered_diff$counter <- c(1:length(workers))

perf_ordered_diff_one <- subset(perf_ordered_diff, perf_ordered_diff$mainCond == 1)
perf_ordered_diff_two <- subset(perf_ordered_diff, perf_ordered_diff$mainCond == 2)

condNames_one <- c('Future-you1', 'Stranger-John', 'Stranger-Bill')
condNames_two <- c('Future-you1', 'Future-you2', 'Stranger-John')

#attention: dprime, group
#quartz()
title <- c('Performance for One Self') 
p1<-ggplot(d.one,aes(x=factor(cond),y=mean,fill=factor(cond))) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar")+theme_bw()+coord_cartesian(ylim=c(0, 3)) 
p1+geom_errorbar(aes(ymax=mean+sem, ymin=mean-sem), position="dodge")+ggtitle(title)+
  scale_x_discrete(breaks = 1:length(condNames_one), labels=condNames_one)+
  theme(panel.grid.major = element_blank(),legend.position="none", panel.grid.minor = element_blank(), axis.title.y=element_blank())+
  theme(text = element_text(size=20), axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("Number of Selves")+ylab("Mean")

#attention: dprime, group
#quartz()
title <- c('Performance for two selves') 
p2<-ggplot(d.two,aes(x=factor(cond),y=mean,fill=factor(cond))) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar")+theme_bw()+coord_cartesian(ylim=c(0, 3)) 
p2+geom_errorbar(aes(ymax=mean+sem, ymin=mean-sem), position="dodge")+ggtitle(title)+
  scale_x_discrete(breaks = 1:length(condNames_two), labels=condNames_two)+
  theme(panel.grid.major = element_blank(),legend.position="none", panel.grid.minor = element_blank(), axis.title.y=element_blank())+
  theme(text = element_text(size=20), axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("Agent Condition")+ylab("Mean")

grid.arrange(p1, p2)

perf_conds <- c('One', 'Two')

#performance
#quartz()
title <- c('Overall Performance Across Shapes') 
p1<-ggplot(perf_mat_plot,aes(x=factor(cond),y=mean,fill=factor(cond))) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar")+theme_bw()+coord_cartesian(ylim=c(1, 9)) 
p1+geom_errorbar(aes(ymax=mean+sem, ymin=mean-sem), position="dodge")+ggtitle(title)+
  scale_x_discrete(breaks = 1:length(perf_conds), labels=perf_conds)+
  theme(panel.grid.major = element_blank(),legend.position="none", panel.grid.minor = element_blank(), axis.title.y=element_blank())+
  theme(text = element_text(size=20), axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("Number of Selves")+ylab("Mean")

#performance diff
quartz()
title <- c('Performance Diff: Best self vs. stranger') 
p1<-ggplot(perf_diff_mat_plot,aes(x=factor(cond),y=mean,fill=factor(cond))) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar")+theme_bw()+coord_cartesian(ylim=c(0, 2)) 
p1+geom_errorbar(aes(ymax=mean+sem, ymin=mean-sem), position="dodge")+ggtitle(title)+
  scale_x_discrete(breaks = 1:length(perf_conds), labels=perf_conds)+
  theme(panel.grid.major = element_blank(),legend.position="none", panel.grid.minor = element_blank(), axis.title.y=element_blank())+
  theme(text = element_text(size=20), axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("Number of Selves")+ylab("Mean")

#attividual subjects
quartz()
title <- c('Individual Performance Diff: One Self') 
p1<-ggplot(perf_ordered_diff_one,aes(x=factor(counter),y=perf_diff)) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",fill="darkgray")+
  theme_bw()+coord_cartesian(ylim=c(-1, 4))+ggtitle(title)+
  theme_classic()
p1+scale_x_discrete(breaks = 1:length(workers), labels=perf_ordered_diff$counter)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text = element_text(size=32), 
        axis.text.x = element_text(angle = 45, hjust=1,vjust=0.8, color="black"),
        axis.text.y = element_text(color="black"),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"))+
  xlab("Participant")+ylab("Performance Difference")+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))+
  scale_fill_manual(values = c("gray"))

##=#individual subjects
quartz()
title <- c('Individual Performance Diff: Two Selves') 
p1<-ggplot(perf_ordered_diff_two,aes(x=factor(counter),y=perf_diff)) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",fill="darkgray")+
  theme_bw()+coord_cartesian(ylim=c(-1, 4))+ggtitle(title)+
  theme_classic()
p1+scale_x_discrete(breaks = 1:length(workers), labels=perf_ordered_diff$counter)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text = element_text(size=32), 
        axis.text.x = element_text(angle = 45, hjust=1,vjust=0.8, color="black"),
        axis.text.y = element_text(color="black"),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"))+
  xlab("Participant")+ylab("Performance Difference")+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))+
  scale_fill_manual(values = c("gray"))

=================================================================================================================
                                                        ##END##
##================================================================================================================




















