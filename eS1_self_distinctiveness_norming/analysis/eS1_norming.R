#Julian De Freitas, 2019
#Analysis script for De Freitas, Rips, & Alvarez - The limit of personal identity
#Experiment S1

## clear workspace
rm(list = ls())  

## necessary libraries
if (!require(rjson)) {install.packages("rjson"); require(rjson)}
if (!require(ggplot2)) {install.packages("ggplot2"); require(ggplot2)}
if (!require(ltm)) {install.packages("ltm"); require(ltm)} 
if (!require(heplots)) {install.packages("heplots"); require(heplots)} 
if (!require(lmtest)) {install.packages("lmtest"); require(lmtest)} 
if (!require(compute.es)) {install.packages("compute.es"); require(compute.es)}
if (!require(lsr)) {install.packages("lsr"); require(lsr)} 
if (!require(lsmeans)) {install.packages("lsmean"); require(lsmeans)}
if (!require(nnet)) {install.packages("nnet"); require(nnet)}
if (!require(mlogit)) {install.packages("mlogit"); require(mlogit)}
library(ggrepel)

##================ import data ================================================================================================

dir <- setwd("/Users/julian/Documents/github/juliandefreitas/self/limited_self_reference/eS1_self_distinctiveness_norming/data")

files <- list.files(pattern=('*txt'))
myJSON <- lapply(files, function(x) fromJSON(file=x)) #join into one single JSON file
(data <- lapply(myJSON, function(myJSON) { as.data.frame(myJSON) }))
data <- (data <- do.call(rbind, data))

dim(data) 

##======================== counts and exclusions =============================================================================

#exclude those who failed attention check
data <- subset(data, (data$trialStruct.attention==0) &
                 (data$trialStruct.comp_imagine==2) &
                 (data$trialStruct.comp_num_versions==3))

dim(data)

age <- data$trialStruct.age[data$trialStruct.age %in% 1:100] #ignore folks who said they're 0 yrs old
mean(age,na.rm = TRUE) 
gender <- as.factor(data$trialStruct.sex); table(gender)[2]/sum(table(gender)) 

##======================== prep data for analysis ================================================================================

q1 <- as.numeric(data$trialStruct.q2_futures)
q2 <- as.numeric(data$trialStruct.q3_20young_20old)
q3 <- as.numeric(data$trialStruct.q4_true_surface)
q4 <- as.numeric(data$trialStruct.q3_20young_20old)
q5 <- as.numeric(data$trialStruct.q6_18young_60old)
q6 <- as.numeric(data$trialStruct.q8_healthy_unhealthy)

similarities <- c(mean(q1, na.rm = TRUE),
                  mean(q2), 
                  mean(q3),
                  mean(q4),
                  mean(q5),
                  mean(q6))

experiments <- c('future-you1 vs future-you2',                       #alternatives 
                 '20-years-younger vs 20-years-older',               #young and old 
                 'true vs surface',                                  #true and surface 
                 '20-years-younger vs 20-years-older, powered',      #young and old, powered
                 '18-year-old vs 60-year-old',                       #young and old, distinct
                 'healthy vs unhealthy')                             #alternatives, distinct

#performance differences two selves - one                 
e1_alternatives <- -0.1012608           #alternatives
e2_20yr_youngOld <- 0.1771778            #young and old
e3_true_surface <- -0.438542            #true and surface
e4_20yr_youngOld_powered <- -0.3087165    #young and old, powered
e5_1860_youngOld <- -0.3848361            #young and old, distinct
e6_healthy_unhealthy <- -0.8335105        #alternatives, distinct

effects <- c(e1_alternatives,
             e2_20yr_youngOld,
             e3_true_surface,
             e4_20yr_youngOld_powered,
             e5_1860_youngOld, 
             e6_healthy_unhealthy)

##========================================== analysis ======================================================================

cor.test(similarities, effects)

plot(similarities, effects)
abline(effects ~ similarities)

df <- data.frame(matrix(ncol = 2, nrow = 6))
colnames(df) <-  c("similarities", "effects")
df$similarities <- similarities
df$effects <- effects

##========================================== plot ======================================================================

quartz()
pc <- predict(prcomp(~effects+similarities),df)[,1]

p1 <- ggplot(df,aes(similarities, effects, color=pc)) +
  geom_point(shape = 16, size = 5, show.legend = FALSE) +
  #geom_text(aes(label=experiments),hjust=1, vjust=0)+
  geom_smooth(method=lm , color="black", se=TRUE) + # Add linear regression line 
  theme_minimal() +
  scale_color_gradient(low = "#f0650e", high = "#0091ff")+
  scale_alpha(range = c(.25, .6))+
  xlab("Self Distinctiveness Rating")+ylab("Performance Accuracy for 2 Selves - 1 Self")+
      ggtitle("Categorization Performance by Self Similarity") +
  theme(axis.title.x = element_text(color='black', margin=margin(20,0,0,0)), 
        axis.title.y = element_text(margin=margin(0,0,0,0)),
        plot.title = element_text(size=30, face='bold'),
        text = element_text(size=30))+
        theme(legend.position = "none")
  coord_cartesian(ylim=c(-0.5, 0.5)) 

### geom_label_repel
p1 + 
  geom_label_repel(aes(label = experiments),
                   size= 5.0,
                   box.padding   = 1.8, 
                   point.padding = 1.5,
                   segment.color = 'grey50') 

####======================================= end =========================================================

rm(list = ls()) 

