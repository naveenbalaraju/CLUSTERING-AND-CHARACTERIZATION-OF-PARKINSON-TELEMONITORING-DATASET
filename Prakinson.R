########################################################################################################################
############# FORECASTING PARKINSON'S DISEASE SYMPTON SCORES (UPDRS) USING PARKINSONS TELEMONITORING DATASET ############
########################################################################################################################
# Dataset description
#subject# - Integer that uniquely identifies each subject 
#age - Subject age 
#sex - Subject gender '0' - male, '1' - female 
#test_time - Time since recruitment into the trial. The integer part is the number of days since recruitment. 
#motor_UPDRS - Clinician's motor UPDRS score, linearly interpolated 
#total_UPDRS - Clinician's total UPDRS score, linearly interpolated 
#Jitter(%),Jitter(Abs),Jitter:RAP,Jitter:PPQ5,Jitter:DDP - Several measures of variation in fundamental frequency 
#Shimmer,Shimmer(dB),Shimmer:APQ3,Shimmer:APQ5,Shimmer:APQ11,Shimmer:DDA - Several measures of variation in amplitude
#NHR,HNR - Two measures of ratio of noise to tonal components in the voice 
#RPDE - A nonlinear dynamical complexity measure 
#DFA - Signal fractal scaling exponent 
#PPE - A nonlinear measure of fundamental frequency variation 

# Loading the dataset
data=  read.csv("/Users/naveen/Downloads/parkinsons_updrs.data")
dummy_data = data
k= data

########################################## EXPOLATORY DATA ANALYSIS ################################################
# Analysis of Variable Age
quartz()
hist(data$age,col = rainbow(14),main = "HISTOGRAM OF AGE",
     xlab = "Age")
# The Histogram shows that approximately 90% of the population is of age 60 and above, while only
# 10% is below age of 60

# Analysis of Variable Sex
quartz()
per6=round((table(data$sex)/5875)*100)
lab6=paste(names(table(data$sex)),"[",per6,"%]")
pie(table(data$sex),labels = lab6,main = "PIE CHART FOR VARIABLE SEX,[0:male, 1:female]",
    col = rainbow(14),cex=0.6)
# The dataset contains 68% percent of male population and 32% female

# Analysis of Variable Test_time
quartz()
hist(data$test_time,col = rainbow(14),main = "HISTOGRAM OF TEST-TIME",
     xlab = "Time")
# There are only 732(12.45% of the polpulation) patients who are under the clinical trial
# for less than 25 days and majority of the patients are undergoing this trial for more than 25 days

# Analysis of the variable Jitter(%)
quartz()
hist(data$Jitter...,col = rainbow(14),main = "HISTOGRAM OF JITTER(%) FREQUENCY",
     xlab = "Fundamental frequency",xlim = c(0,0.06),ylim =c(0,6000))
# The histogram shows that 93.6% of the patients have the jitter% frequency between 0 - 0.01.
# Whilst,only a small percentage of patients have the jitter% greater than 0.01

# Analysis of the variable Jitter Abs
quartz()
hist(data$Jitter.Abs.,col = rainbow(14),main = "HISTOGRAM OF JITTER ABS FREQUENCY",
     xlab = "Fundamental frequency",xlim = c(0,0.0003),ylim =c(0,5000))
# The histogram shows that 70% of the patients have Jitter Abs frequency between 0 and 0.00005, while
# rest 30% have above 0.00005

# Analysis of the variable Jitter Rap
quartz()
hist(data$Jitter.RAP,col = rainbow(14),main = "HISTOGRAM OF JITTER RAP FREQUENCY",
     xlab = "Fundamental frequency",xlim = c(0,0.03),ylim =c(0,6000))
# The histogram shows that 88.5% of the patients have Jitter Rap frequency between 0 and 0.005,while
# rest have above 0.005

# Analysis of the variable Jitter PPQ5
quartz()
hist(data$Jitter.PPQ5,col = rainbow(14),main = "HISTOGRAM OF JITTER PPQ5 FREQUENCY",
     xlab = "Fundamental frequency",xlim = c(0,0.03),ylim =c(0,6000))
# The histogram shows that 88.5% of the patients have Jitter PPQ5 frequency between 0 and 0.005,while
# rest have above 0.005

# Analysis of the variable Jitter DDP
quartz()
hist(data$Jitter.DDP,col = rainbow(14),main = "HISTOGRAM OF JITTER DDP FREQUENCY",
     xlab = "Fundamental frequency",xlim = c(0,0.07),ylim =c(0,5000))
# The histogram shows that 76% of the patienst have Jitter DDP frequency between 0-0.01 and 17% have frequency
# between 0.01-0.02. while rest 7% have frequency above 0.02

# Analysis of the variable Shimmer
quartz()
hist(data$Shimmer,col = rainbow(14),main = "HISTOGRAM OF SHIMMER AMPLITUDE",
     xlab = "Fundamental Amplitude",xlim = c(0,0.2),ylim =c(0,3000))
# The histogram shows that about 85% of the patients(5032) have the shimmer Amplitude less than 0.05.
# While, the rest 155 have Amplitude above 0.05


# Analysis of the variable Shimmer DB
quartz()
hist(data$Shimmer.dB.,col = rainbow(14),main = "HISTOGRAM OF SHIMMER DB  AMPLITUDE",
     xlab = "Fundamental Amplitude",xlim = c(0,2),ylim =c(0,3000))
# About 89% of the patients have shimmer DB Amplitude less than 0.5 and the rest 11% have above 0.5


# Analysis of the variable Shimmer APQ3
quartz()
hist(data$Shimmer.APQ3,col = rainbow(14),main = "HISTOGRAM OF SHIMMER APQ3 AMPLITUDE",
     xlab = "Fundamental Amplitude ",xlim = c(0,0.1),ylim =c(0,3000))
# The histogram shows that about 73% of the patients have Shimmer APQ3 Amplitude less than 0.02
# and the rest 27% have shimmer APQ3 Amplitude above 0.02


# Analysis of the variable Shimmer APQ5
quartz()
hist(data$Shimmer.APQ5,col = rainbow(14),main = "HISTOGRAM OF SHIMMER APQ5  AMPLITUDE",
     xlab = "Fundamental Amplitude",xlim = c(0,0.1),ylim =c(0,3000))
# The histogram shows that 65% of the patients have Shimmer APQ5 Amplitude between 0-0.02 and
# rest of the patients have above 0.02

# Analysis of the variable Shimmer APQ11
quartz()
hist(data$Shimmer.APQ11,col = rainbow(14),main = "HISTOGRAM OF SHIMMER APQ11  AMPLITUDE",
     xlab = "Fundamental Amplitude",xlim = c(0,0.15),ylim =c(0,3000))
# The data shows 92% of the patients have the Shimmer APQ11 Amplitude of 0.05 or less

# Analysis of the variable Shimmer DDA
quartz()
hist(data$Shimmer.DDA,col = rainbow(14),main = "HISTOGRAM OF SHIMMER DDA AMPLITUDE",
     xlab = "Fundamental Amplitude",xlim = c(0,0.4),ylim =c(0,4000))
# The histogram shows that 92.5% of the patients have the Shimmer DDA Amplitude of 0.1 or less


# Analysis of the variable NHR
quartz()
hist(data$NHR,col = rainbow(14),main = "HISTOGRAM OF NHR RATIO",
     xlab = "NHR ratio",xlim = c(0,0.4),ylim =c(0,6000))
# The histogram shows that about 89% of the patients have NHR ratio of 0.05 or less

# Analysis of the variable HNR
quartz()
hist(data$HNR,col = rainbow(14),main = "HISTOGRAM OF HNR RATIO",
     xlab = "HNR ratio",xlim = c(0,40))#,ylim =c(0,6000))
# The histogram for the HNR ratio shows a normal distribution with majority(58%) of the 
# patients have HNR ratio between 19 and 25

# Analysis of the variable RPDE
quartz()
hist(data$RPDE,col = rainbow(14),main = "HISTOGRAM OF RPDE",
     xlab = "RPDE")
# The histogram of RPDE show a normal distribution centred at RDPE value of 0.54 and 52% of the 
# patients have the RPDE value between 0.46 and 0.61

# Analysis of the variable DFA
quartz()
hist(data$DFA,col = rainbow(14),main = "HISTOGRAM OF DFA",
     xlab = "DFA")
# The Signal fractal scaling exponent shows a normal distribution centered at 0.65


# Analysis of the variable PPE
quartz()
hist(data$PPE,col = rainbow(14),main = "HISTOGRAM OF PPE",
     xlab = "Fundamental Frequency variation")
# About 95% of the patients have PPE fundamental variation of 0.1 and above


############################################### FEATURE SELECTION #############################################
data[["subject."]]= NULL
data[["motor_UPDRS"]]=NULL
data[["total_UPDRS"]]=NULL

####################################### PRINCIPAL COMPONENT ANALYSIS ###########################################
pc=prcomp(data,center = TRUE,scale. = TRUE)
quartz()
plot(pc,type="l",main = "SCREE PLOT")
comp1=pc$x[,1]
comp2=pc$x[,2]
pc_data =cbind(comp1,comp2)
quartz()
plot(comp1,comp2,main = "SCATTER PLOT OF THE DATA POINTS AFTER PCA",col = "blue")
# For the further analysis I've considered the first two principal components 

########################################### K MEANS CLUSTERING ##################################################
PK_kmeans = kmeans(pc_data,2)
quartz()
plot(pc_data, col = PK_kmeans$cluster, main = " K-MEANS CLUSTERING WITH PCA DATA")
points(PK_kmeans$centers, col = c("yellow"), pch = 17, cex= 2)

########################################### HIERARCHICAL CLUSTERING #############################################
set.seed(666)
library (cluster)
p = sample(length(pc_data),400) #I've randomly sampled 400 observations from the PC_data
pk_den = hclust(dist(p),method = "complete")
cut_clust=cutree(pk_den,k=2)
quartz()
plot(pk_den,main = "DENDROGRAM WITH COMPLETE LINKAGE")
rect.hclust(pk_den,k=2,border = "blue")
s= silhouette(cut_clust,dist = dist(p))
quartz()
plot(s,main="SILHOUETTE PLOT FOR K=2 FOR COMPLETE LINKAGE",col = rainbow(14)) 

# Performing Hierarchical clustering after Principal component analysis, clusters the  data into
# two healthy looking clusters with a good average silhouette width of 0.64.



##################################### BAYESIAN NETWORK #############################################
# To find the optimal bayesian network,I've have used Max-min Hill climbing alogorithm  with "k2" as the score
library(bnlearn)
library(Rgraphviz)
library(gRain)
library(bitops)
library(Matrix)
library(methods)
library(igraph)
library(bnstruct)


######################################## DATA PREPROCESSING ####################################################
dummy_data[["subject."]]=NULL
dummy_data[["motor_UPDRS"]]=NULL

#Binning variable "Age"
dummy_data$age = ifelse(dummy_data$age>=60,">=60","<60")
dummy_data$age=factor(dummy_data$age)
# I've Binned age varible according to the mean value of the age. All the values >=60 are considered as age 
# 60 or more and all the values below 60 are considered as <60

#Binning variable "sex"
dummy_data$sex = ifelse(dummy_data$sex==0,"male","female")
dummy_data$sex =  factor(dummy_data$sex)

#Binning variable test_time
dummy_data$test_time = floor(dummy_data$test_time)
dummy_data$test_time = ifelse(dummy_data$test_time<=25,"<=25 days",">25 days")
dummy_data$test_time = factor(dummy_data$test_time)
# All the patients who are under observation for 25 days or less is considered as <=25 days and patients
# who under observation for more than 25 days sis considerd as >25days


# Binning variable "total_UPDRS"
dummy_data$total_UPDRS = ifelse(dummy_data$total_UPDRS>=50.42,"High UPDRS","Low UPDRS")
dummy_data$total_UPDRS=factor(dummy_data$total_UPDRS)
# Since our collaborator has asked to characterize jitter based for a high UPDRS, whose value
# two std above the mean.I've binned the total_UPDRS values into  2 categories. All the values
# >= 50.42 i.e two std above mean are considered a High UPDRS

# Binning varible jitter%
dummy_data$Jitter... = ifelse(dummy_data$Jitter...<=0.01,"low jitter % Fq","High jitter % Fq")
dummy_data$Jitter... = factor(dummy_data$Jitter...)
# All the patients with  jitter% frequency between 0 - 0.01 is considered as "low jitter % Fq"
# Whilts,all patients  who have the jitter% greater than 0.01 is cosidered as "High jitter % Fq"


#Binning varibale Jitter Abs
dummy_data$Jitter.Abs. =  ifelse(dummy_data$Jitter.Abs.<=0.00005,"Low Jitter Abs Fq","High jitter Abs Fq")
dummy_data$Jitter.Abs. = factor(dummy_data$Jitter.Abs.)
# All patients with Jitter Abs frequency between 0 and 0.00005 as considered as  "Low Jitter Abs"
# and the patients with the jitter Abs above 0.00005 is considered as "High jitter Abs"


# Binning variable Jitter RAP
dummy_data$Jitter.RAP = ifelse(dummy_data$Jitter.RAP<=0.005,"Low jitter RAP Fq","High Jitter RAP Fq")
dummy_data$Jitter.RAP = factor(dummy_data$Jitter.RAP)
# Patients with Jitter Rap frequency between 0 and 0.005 are considered as "Low jitter RAP"
# and above 0.005 is considered as "High Jitter RAP"

# Binning variable Jitter.PPQ5
dummy_data$Jitter.PPQ5 = ifelse(dummy_data$Jitter.PPQ5<=0.005,"Low jitter PPQ5 Fq","High jitter PPQ5 Fq")
dummy_data$Jitter.PPQ5 = factor(dummy_data$Jitter.PPQ5)
# Patients with Jitter PPQ5 frequency between 0 and 0.005 are considered as "Low jitter PPQ5 Fq" and 
#  above 0.005 is "High jitter PPQ5 Fq"

# Binning variable jitter DDP
dummy_data$Jitter.DDP = ifelse(dummy_data$Jitter.DDP<=0.01,"Low jitter DDP Fq","High jitter DDP Fq")
dummy_data$Jitter.DDP = factor(dummy_data$Jitter.DDP)
# All the patients with Jitter DDP frquency between 0-0.01 are considered as "Low jitter DDP Fq" and 
# above 0.01 is considered as "High jitter DDP Fq"


# Binning variable Shimmer 
dummy_data$Shimmer = ifelse(dummy_data$Shimmer<=0.05,"Low shimmer Amp","High shimmer Amp")
dummy_data$Shimmer = factor(dummy_data$Shimmer)
# All the patients with shimmer amplitude <= 0.05 are considered as "Low shimmer Amp" and 
# above 0.05 is considered as "High shimmer Amp"

# Binning variable Shimmer DB
dummy_data$Shimmer.dB. = ifelse(dummy_data$Shimmer.dB.<=0.5,"Low shimmer DB Amp","High shimmer DB Amp")
dummy_data$Shimmer.dB. = factor(dummy_data$Shimmer.dB.)
# The patients with shimmer DB Amplitude <= 0.5  are considered as "Low shimmer DB Amp"and 
# above 0.5 is "High shimmer DB Amp"

# Binning variable Shimmer.APQ3
dummy_data$Shimmer.APQ3 = ifelse(dummy_data$Shimmer.APQ3<=0.02,"Low shimmer APQ3 Amp","High shimmer APQ3 Amp")
dummy_data$Shimmer.APQ3 = factor(dummy_data$Shimmer.APQ3)
# Patients who have Shimmer APQ3 Amplitude <= 0.02 are considered as "Low shimmer APQ3 Amp" and 
# above 0.02 are considerd as "High shimmer APQ3 Amp

# Binning variables Shimmer.APQ5
dummy_data$Shimmer.APQ5 = ifelse(dummy_data$Shimmer.APQ5 <=0.02,"Low shimmer APQ5 Amp","High shimmer APQ5 Amp")
dummy_data$Shimmer.APQ5 = factor(dummy_data$Shimmer.APQ5)
# Patients who have Shimmer APQ5 Amplitude <= 0.02 are considered as "Low shimmer APQ5 Amp" and 
# above 0.02 are considerd as "High shimmer APQ5 Amp

# Binning variable Shimmer.APQ11
dummy_data$Shimmer.APQ11 = ifelse(dummy_data$Shimmer.APQ11 <=0.05,"Low shimmer APQ11 Amp","High shimmer APQ11 Amp")
dummy_data$Shimmer.APQ11 = factor(dummy_data$Shimmer.APQ11)
# Patients who have Shimmer APQ11 Amplitude <= 0.05 are considered as "Low shimmer APQ11 Amp" and 
# above 0.05 are considerd as "High shimmer APQ11 Amp

# Binning variable Shimmer.DDA
dummy_data$Shimmer.DDA = ifelse(dummy_data$Shimmer.DDA <=0.1,"Low shimmer DDA Amp","High shimmer DDA Amp")
dummy_data$Shimmer.DDA = factor(dummy_data$Shimmer.DDA)
# The patients with  Shimmer DDA Amplitude of 0.1 or less is considered as "Low shimmer DDA Amp" and
# above 0.1 is considered as "High shimmer DDA Amp"


# Binning variable NHR
dummy_data$NHR = ifelse(dummy_data$NHR <=0.05,"Low NHR","High NHR")
dummy_data$NHR = factor(dummy_data$NHR)
#The patients  who have NHR ratio of 0.05 or less are considered as "Low NHR" and above
# 0.05 is considered as "High NHR"


# Binning variable HNR
dummy_data$HNR = ifelse(dummy_data$HNR <=25,"Low HNR","High HNR")
dummy_data$HNR = factor(dummy_data$HNR)
#The patients  who have HNR ratio of 25 or less are considered as "Low HNR" and above
# 25 is considered as "High HNR"

# Binning variable RPDE
dummy_data$RPDE = ifelse(dummy_data$RPDE<=0.61,"Low RPDE","High RPDE")
dummy_data$RPDE = factor(dummy_data$RPDE)
# The patients with the RDPE measure of 0.61 or less is considered as "Low RPDE" and 
# above 0.61 is considered as "High RPDE"


# Binning variable DFA
dummy_data$DFA = ifelse(dummy_data$DFA<=0.65,"Low DFA","High DFA")
dummy_data$DFA = factor(dummy_data$DFA)
# The patients with the DFA measure of 0.65 or less is considered as "Low DFA" and 
# above 0.65 is considered as "High DFA"

# Binning variable PPE
dummy_data$PPE = ifelse(dummy_data$PPE>=0.1,"High PPE","Low PPE")
dummy_data$PPE = factor(dummy_data$PPE)
# The patients who have PPE fundamental variation of 0.1 and above is considered as "High PPE"
# and below 0.1 is "Low PPE"


######################################### BAYESIAN NETWORK ###################################################

# I've have used Hill climbing algorithm with "BIC" as the score measure to find the optimalDAG.Furthermore,
# Since our collaborator has asked to characterize "jitter" realted variables based for a high UPDRS, 
# whose value is  two std above the mean.I've binned the total_UPDRS values into  2 categories. All the values
# >= 50.42 i.e two std above mean are considered a High UPDRS

dag = hc(dummy_data,score = "bic")
bn_cpt= bn.fit(dag,data = dummy_data,method = "mle")
bn_cpt$total_UPDRS
dag = Rgraphviz::layoutGraph(bnlearn::as.graphNEL(dag))
graph::nodeRenderInfo(dag) = list(fontsize=60)
quartz()
Rgraphviz::renderGraph(dag)

# The Model generates few interesting results.
# 1.None of the female patients have a High UPDRS scores.
# 2.For a Male of age >=60,having high jitter abs frequency and high DFA,there is only 3% chance of having
#   high UPDRS score.
# 3.For a Male of age >=60,having low jitter abs frequency and high DFA, there is 23.4% chances of having 
#   high UPDRS score.
# 4.For a Male of age >=60,having high jitter abs frequency and low DFA,there is only 5% chance of having
#   high UPDRS score.
# 5.For a Male of age >=60,having low jitter abs frequency and low DFA, there is only 5% chances of having 
#   high UPDRS score.


