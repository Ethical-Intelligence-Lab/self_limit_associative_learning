#Julian De Freitas, 2019
#Analysis script for De Freitas, Rips, & Alvarez - The capacity of personal identity
#Experiment 5

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
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory
setwd("../data/")

datalist = list()
#import data using jsonlite [automate this, by defining list of data frames]
files <- list.files(pattern=('*txt'))
for (i in 1:length(files)) {
    curData <- fromJSON(files[i], simplifyDataFrame = TRUE)
    datalist[[i]] <- curData$trialStruct
}


##================ anonymize worker ids ========================================

if(FALSE) {
  filenames_old <- list.files(pattern=('*txt'))
  
  # Rename all files in the files list, and rename them by renaming the part before the first "_" with the integer in order
  # Also, save the part that we renamed as well as the integers we assigned them to in a csv file, so that it's easy to see which file corresponds to which participant
  for(i in 1:length(files)) {
    # Print "Renaming [worker_id] to [num]"
    print(paste0("Renaming ", files[i], " to ", i))
    
    #file.rename(files[i], paste0(i, substr(files[i], regexpr("_", files[i]), nchar(files[i]))))
    #files[i] <- paste0(i, substr(files[i], regexpr("_", files[i]), nchar(files[i])))
    # If there are files with the same worker_id, rename them to the same integer
    
    if(i > 1) {
      print(paste(substr(filenames_old[i], 1, regexpr("_", filenames_old[i])-1), substr(filenames_old[i-1], 1, regexpr("_", filenames_old[i-1])-1)))
      if(substr(filenames_old[i], 1, regexpr("_", filenames_old[i])-1) == substr(filenames_old[i-1], 1, regexpr("_", filenames_old[i-1])-1)) {
        file.rename(files[i], paste0(i-1, substr(files[i], regexpr("_", files[i]), nchar(files[i]))))
        files[i] <- paste0(i-1, substr(files[i], regexpr("_", files[i]), nchar(files[i])))
      }
      else {
        file.rename(filenames_old[i], paste0(i, substr(filenames_old[i], regexpr("_", filenames_old[i]), nchar(filenames_old[i]))))
        files[i] <- paste0(i, substr(files[i], regexpr("_", files[i]), nchar(files[i])))
      }
    }
    else {
      file.rename(filenames_old[i], paste0(i, substr(filenames_old[i], regexpr("_", filenames_old[i]), nchar(filenames_old[i]))))
      files[i] <- paste0(i, substr(files[i], regexpr("_", files[i]), nchar(files[i])))
    }
  }
  
  # Also anonymize the 'workerID' field in each file to the integer we assigned it to, and save the file
  for(i in 1:length(files)) {
    myJSON <- fromJSON(files[i])
    
    # We should save the myJSON$workerID as the integer before the first "_"
    w_num <- substr(files[i], 1, regexpr("_", files[i])-1)
    print(paste0(files[i], " -- ", w_num))
    myJSON$workerID <- w_num
    
    # Change workerId's in the trialStructs as well
    myJSON$trialStruct['workerId'] <- rep(w_num, dim(myJSON$trialStruct['workerId'])[1])
    
    # Write the text into files[i]
    write(toJSON(myJSON, na = "string"), files[i])
  }
  
  worker_ids <- c()
  nums <- c()
  for(i in 1:length(filenames_old)) {
    worker_ids <- c(worker_ids, substr(filenames_old[i], 1, regexpr("_", filenames_old[i]) - 1))
    nums <- c(nums, substr(files[i], 1, regexpr("_", files[i]) - 1))
  }
  
  filenames <- as.data.frame(cbind(worker_ids, nums))
  colnames(filenames) <- c('worker_id', 'integer')
  # Remove NA column
  filenames <- filenames[filenames$worker_id != 'NA',]
  filenames$`NA`<- NULL
  write.csv(filenames, 'filenames_e6.csv')  
}


data = do.call(rbind, datalist)
head(data)
dim(data)
length(unique(data$workerId)) #1 subjects

#check that we have equal numbers for each condition
table(data$label)
table(data$cond)

##================================================================================================================
                                                ##DATA PREP##
##================================================================================================================

### perform exclusions: attention, comprehension, and rts < 200 (this ends up performing both subject and trial)
### note, we exclude based on mean accuracy further below
### note, data$comp_mental_content actually refers to comp_number_copies

data <- subset(data,(data$attentionMCQ=="0" & data$comp=="B" &
                       data$comp2=="C" & data$rt>=100)) 
length(unique(data$workerId)) #1 subjects
dim(data)

### exclude subjects with lower than 55% average accuracy
worker <- as.factor(data$workerId)
workers <- as.factor(unique(worker))
acc <- as.numeric(data$acc)
acc_use <- acc
acc_use[is.na(acc_use)] <- 0
perf_thresh <- 0.40
cond <- as.factor(data$cond)

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
iden_mat <- array(0,dim=c(length(workers),5))
colnames(iden_mat) <- c('before', 'after','zeros','ltm_diff', 'total_perf')
for(i in 1:length(workers)) {
  iden_mat[i,] <- c( mean(identity_bef[worker==workers[i]]), mean(identity_aft[worker==workers[i]]),0, 0, 0)
}
iden.mat <- as.data.frame(iden_mat, stringsAsFactors=FALSE); iden.mat

#assign variable names
acc <- as.numeric(data$acc)
acc_use <- acc
acc_use[is.na(acc_use)] <- 0
numTrials <- dim(data)[1]
matchCond <- as.factor(data$matchCond)
agentCond <- as.factor(data$cond)
#create numeric version of condition
for(i in 1:length(agentCond)) {
  if(agentCond[i] == 'you') {
    data$agentCond_n[i] = 1
  }
  else if(agentCond[i] == 'friend') {
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
age <- as.numeric(data$age); mean(age,na.rm = TRUE) #32.10
gender <- as.factor(data$sex); table(gender)[1]/sum(table(gender)) #0.51

#create matrix for collecting d prime-relevant values, and rts
d_mat <- array(0,dim=c(3*length(workers), 4))
colnames(d_mat) <- c('worker', 'agentCond', 'acc', 'rt')
d.mat <- as.data.frame(d_mat, stringsAsFactors=FALSE); d.mat

#for each worker and agent condition, get average hits, false alarms, d-prime, and rt
#hits: proportion of true hits you said were hits (out of all true hits)
#false alarms: proportin of non hits you said were hits (out of all non hits)
#get rts for trials that were correct

corr_answers <- c('original', 'copy', 'stranger')

counter <- 1
for(i in 1:length(workers)) {
  for(j in 1:length(agentConds_n)) {
    mean_acc <- mean(acc_use[worker==workers[i] & agentCond_n==j])
    rt <- mean(rts[worker==workers[i] & agentCond_n==j], na.rm=TRUE)
    
    d.mat[counter,] <- c(workers[i], j, mean_acc, rt)
    counter = counter + 1
  }
} 

for(i in 1:length(workers)) {
  iden.mat[i,4] <- d.mat$acc[d.mat$worker==i & d.mat$agentCond == 1] - d.mat$acc[d.mat$worker==i & d.mat$agentCond == 2]
  iden.mat[i,5] <- d.mat$acc[d.mat$worker==i & d.mat$agentCond == 1] + d.mat$acc[d.mat$worker==i & d.mat$agentCond == 2] + d.mat$acc[d.mat$worker==i & d.mat$agentCond == 3]
}

#subset d.mat by response
d.mat_o <- subset(d.mat,(d.mat$after==1))
d.mat_c <- subset(d.mat,(d.mat$after==2))
d.mat_b <- subset(d.mat,(d.mat$after==4))

#sort identity mat by original - copy, for plotting
iden_ordered <- iden.mat[order(-iden.mat$ltm_diff),]
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

##================================================================================================================
                                                      ##ANALYSIS##
##================================================================================================================

####### GROUP DIFFERENCES

## all subjects

## means
mean(d.mat$acc[d.mat$agentCond==1])
sd(d.mat$acc[d.mat$agentCond==1])

mean(d.mat$acc[d.mat$agentCond==2])
sd(d.mat$acc[d.mat$agentCond==2])

mean(d.mat$acc[d.mat$agentCond==3])
sd(d.mat$acc[d.mat$agentCond==3])

att_1 <- t.test(d.mat$acc[d.mat$agentCond==1 | d.mat$agentCond==2] ~ d.mat$agentCond[d.mat$agentCond==1 | d.mat$agentCond==2], var.equal=TRUE, paired=TRUE); att_1
att_2 <- t.test(d.mat$acc[d.mat$agentCond==1 | d.mat$agentCond==3] ~ d.mat$agentCond[d.mat$agentCond==1 | d.mat$agentCond==3], var.equal=TRUE, paired=TRUE); att_2
att_3 <- t.test(d.mat$acc[d.mat$agentCond==2 | d.mat$agentCond==3] ~ d.mat$agentCond[d.mat$agentCond==2 | d.mat$agentCond==3], var.equal=TRUE, paired=TRUE); att_3

tes(as.numeric(att_1[1]), length(workers), length(workers)) #cohen's d
tes(as.numeric(att_2[1]), length(workers), length(workers)) #cohen's d
tes(as.numeric(att_3[1]), length(workers), length(workers)) #cohen's d

## chose original
att_1 <- t.test(d.mat_o$d[d.mat_o$agentCond==1 | d.mat_o$agentCond==2] ~ d.mat_o$agentCond[d.mat_o$agentCond==1 | d.mat_o$agentCond==2], var.equal=TRUE, paired=TRUE); att_1
att_2 <- t.test(d.mat_o$d[d.mat_o$agentCond==1 | d.mat_o$agentCond==3] ~ d.mat_o$agentCond[d.mat_o$agentCond==1 | d.mat_o$agentCond==3], var.equal=TRUE, paired=TRUE); att_2
att_3 <- t.test(d.mat_o$d[d.mat_o$agentCond==2 | d.mat_o$agentCond==3] ~ d.mat_o$agentCond[d.mat_o$agentCond==2 | d.mat_o$agentCond==3], var.equal=TRUE, paired=TRUE); att_3

# chose copy
att_1 <- t.test(d.mat_c$d[d.mat_c$agentCond==1 | d.mat_c$agentCond==2] ~ d.mat_c$agentCond[d.mat_c$agentCond==1 | d.mat_c$agentCond==2], var.equal=TRUE, paired=TRUE); att_1
att_2 <- t.test(d.mat_c$d[d.mat_c$agentCond==1 | d.mat_c$agentCond==3] ~ d.mat_c$agentCond[d.mat_c$agentCond==1 | d.mat_c$agentCond==3], var.equal=TRUE, paired=TRUE); att_2
att_3 <- t.test(d.mat_c$d[d.mat_c$agentCond==2 | d.mat_c$agentCond==3] ~ d.mat_c$agentCond[d.mat_c$agentCond==2 | d.mat_c$agentCond==3], var.equal=TRUE, paired=TRUE); att_3

# chose both
att_1 <- t.test(d.mat_b$d[d.mat_b$agentCond==1 | d.mat_b$agentCond==2] ~ d.mat_b$agentCond[d.mat_b$agentCond==1 | d.mat_b$agentCond==2], var.equal=TRUE, paired=TRUE); att_1
att_2 <- t.test(d.mat_b$d[d.mat_b$agentCond==1 | d.mat_b$agentCond==3] ~ d.mat_b$agentCond[d.mat_b$agentCond==1 | d.mat_b$agentCond==3], var.equal=TRUE, paired=TRUE); att_2
att_3 <- t.test(d.mat_b$d[d.mat_b$agentCond==2 | d.mat_b$agentCond==3] ~ d.mat_b$agentCond[d.mat_b$agentCond==2 | d.mat_b$agentCond==3], var.equal=TRUE, paired=TRUE); att_3

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

attmean(iden_ordered$total_perf[iden_ordered$aft==1])
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
condNames <- c('You', 'Friend', 'Stranger')

#make d mat for plotting: condition 1
d_mat_plot <- array(0,dim=c(3, 5))
colnames(d_mat_plot) <- c('cond','mean','sd','n','sem')

for(i in 1:length(unique(agentCond_n))) {
  d_mat_plot[i, ] <- c(i, mean(d.mat$acc[d.mat$agentCond == i]), sd(d.mat$acc[d.mat$agentCond == i]), length(d.mat$acc[d.mat$agentCond == i]), 0)
  d_mat_plot[i, 5] <- d_mat_plot[i,3]/sqrt(d_mat_plot[i,4])
}
d_mat_plot
d.plot <- as.data.frame(d_mat_plot, stringsAsFactors=FALSE); d.plot

###### GROUPED PLOTS

### dprime, group
quartz()
title <- c('Performance levels') 
p1<-ggplot(d.plot,aes(x=factor(cond),y=mean,fill=factor(cond))) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar")+theme_bw()+coord_cartesian(ylim=c(0, 1)) 
p1+geom_errorbar(aes(ymax=mean+sem, ymin=mean-sem), position="dodge")+ggtitle(title)+
  scale_x_discrete(breaks = 1:length(condNames), labels=condNames)+
  theme(panel.grid.major = element_blank(),legend.position="none", panel.grid.minor = element_blank(), axis.title.y=element_blank())+
  theme(text = element_text(size=20), axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("Number of Selves")+ylab("Mean")

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
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"))+
  xlab("Agent Condition")+ylab("d' Performance")+
  theme(plot.title = element_text(hjust = 0.5), axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))+
  scale_fill_manual(values = c("darkgray"))

quartz()
grid.arrange(p1.1, p1.2, p1.3, p1.4, nrow=2, ncol=2)

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
quartz()
p3.1<-ggplot(iden_ordered,aes(x=factor(counter),y=ltm_diff)) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",fill="darkgray")+
  theme_bw()+coord_cartesian(ylim=c(-1, 1))+
  theme_classic()
p3.1+scale_x_discrete(breaks = 1:length(workers), labels=iden_ordered$counter)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text = element_text(size=32), 
        axis.text.x = element_text(angle = 45, hjust=1,vjust=0.8, color="black"),
        axis.text.y = element_text(color="black"),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"))+
  xlab("Participant")+ylab("d' Difference (Self-Friend)")+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))+
  scale_fill_manual(values = c("gray"))

#individual subjects, chose original
title <- c('Identified with original')  
p1<-3.2gplot(iden_ordered_original,aes(x=factor(counter),y=att_diff)) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",fill="darkgray")+
  theme_bw()+coord_cartesian(ylim=c(-2, 3))+
  theme_classic()
p1+g3.2title(title)+scale_x_discrete(breaks = 1:length(workers), labels=iden_ordered$counter)+
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
p2<-3.3gplot(iden_ordered_copy,aes(x=factor(counter),y=att_diff)) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",fill="darkgray")+
  theme_bw()+coord_cartesian(ylim=c(-2, 3))+
  theme_classic()
p2+g3.3title(title)+scale_x_discrete(breaks = 1:length(workers), labels=iden_ordered$counter)+
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
p3<-g.4gplot(iden_ordered_both,aes(x=factor(counter),y=att_diff)) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",fill="darkgray")+
  theme_bw()+coord_cartesian(ylim=c(-2, 3))+
  theme_classic()
p3+gg.4title(title)+scale_x_discrete(breaks = 1:length(workers), labels=iden_ordered$counter)+
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
grid.arrange(p2.13 p2.23 p2.33 p2.43 nrow=2, ncol=2)

###### INDIVIDUAL SUBJECTS, ORDERED BY TOTAL PERFORMANCE 

#individual subjects
p1<-4.ggplot(iden_ordered_perf,aes(x=factor(counter),y=total_perf)) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",fill="darkgray")+
  theme_bw()+coord_cartesian(ylim=c(-2, 12))+
  theme_classic()
p1+s4.cale_x_discrete(breaks = 1:length(workers), labels=iden_ordered$counter)+
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
p1<-4.2gplot(iden_ordered_perf_original,aes(x=factor(counter),y=total_perf)) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",fill="darkgray")+
  theme_bw()+coord_cartesian(ylim=c(-2, 12))+
  theme_classic()
p1+g4.2title(title)+scale_x_discrete(breaks = 1:length(workers), labels=iden_ordered$counter)+
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
p1<-4.3gplot(iden_ordered_perf_copy,aes(x=factor(counter),y=total_perf)) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",fill="darkgray")+
  theme_bw()+coord_cartesian(ylim=c(-2, 12))+
  theme_classic()
p1+g4.3title(title)+scale_x_discrete(breaks = 1:length(workers), labels=iden_ordered$counter)+
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
p1<-4.4gplot(iden_ordered_perf_both,aes(x=factor(counter),y=total_perf)) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",fill="darkgray")+
  theme_bw()+coord_cartesian(ylim=c(-2, 12))+
  theme_classic()
p1+p4.4title(title)+scale_x_discrete(breaks = 1:length(workers), labels=iden_ordered$counter)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text = element_text(size=32), 
        axis.text.x = element_text(angle = 45, hjust=1,vjust=0.8, color="black"),
        axis.text.y = element_text(color="black"),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"))+
  xlab("Participant")+ylab("Overall d' (Original+Copy+Stranger)")+
  theme(plot.title = element_text(hjust = 0.5), axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))+
  scale_fill_manual(values = c("gray"))


##quartz()
grid.arrange(p4.1, p4.2, p4.3, p4.4, nrow=2, ncol=2)


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




















