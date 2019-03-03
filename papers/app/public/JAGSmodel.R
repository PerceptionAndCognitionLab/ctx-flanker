### Tester File for JAGS model
##############################
# setwd("~/git/ctx-flanker/papers/app/revision")
# install.packages("R2jags")
library("R2jags")

## Getting Data
###############

# Way 1 (Original)
#######
# mysub=c(1:13,15,17:44,46:51,53:60,62:65,68:72,74:94,96:97,99:101)
# mySubLab=sprintf("%03d",mysub)
# 
# fileroot="https://raw.githubusercontent.com/PerceptionCognitionLab/data1/master/ctxIndDif/flankerMorph4/flankerMorph4.dat."
# filename=paste(fileroot,mySubLab,sep='')
# 
# inputDat=read.table(url(filename[1]))
# for(i in 2:length(mysub)){
#   inputDatPiece=read.table(url(filename[i]))
#   inputDat=rbind(inputDat,inputDatPiece)
# }
# 
# dat=inputDat

# Way 2 (Function)
#######
#Data Retrieval Function
read_github_dat <- function(url, extension, read_fun, which_extension) {
  #Load required packages
  if(!require("rvest")) stop("Please install the 'rvest' package.")
  if(!require("RCurl")) stop("Please install the 'RCurl' package.")
  if(!require("stringr")) stop("Please install the 'stringr' package.")
  # Fetch file names
  github_page <- read_html(url)
  file_nodes <- html_nodes(github_page
                           , ".content .css-truncate-target .js-navigation-open")
  file_names <- html_text(file_nodes)
  file_url <- html_attr(file_nodes, "href")[grep(extension, file_names)]
  which_files <- html_attr(file_nodes, "href")[grep(which_extension, file_names)]
  which_files <- as.numeric(str_sub(which_files, -3, -1))
  file_url <- paste0("https://raw.githubusercontent.com", file_url)
  file_url <- gsub("blob/", "", file_url)
  data <- lapply(file_url[which_files], read_fun, fill = T)
  data <- do.call("rbind", data)
  data
}

dat <- read_github_dat(url = "https://github.com/PerceptionCognitionLab/data1/tree/master/ctxIndDif/flankerMorph4"
, extension = ".dat."
, read_fun = read.table
, which_extension = ".ses.")


## JAGS code
############
##############
#DATA SETUP
##############
header <- c("sub", "trial", "block", "trialpblock", "file", "frame", "pixels", "response", "choice", "rt", "toofast", "toofastN")
colnames(dat) <- header

# dat.clean <- subset(dat, rt<2 & toofast == 0 & pixels != 1 & pixels != 5 & sub!=75)


# Participants 45 and 73 had some data collection hickups and have to be excluded
# Participant 75 was an outlier contrasting more than assimilating; opposite of all others in experiment
sub.ex <- c(43, 73, 75)
dat.clean <- subset(dat, rt < 2 & toofast == 0 & pixels != 1 & pixels != 5 & !(sub %in% sub.ex) & block != 1 & block != 2)
######
# Other Setup
######
Y=dat.clean$response
N=length(Y)

I = length(unique(dat.clean$sub)) # number of participants 
J = length(unique(dat.clean$frame))/2 # number of frame types
K = length(unique(dat.clean$frame))/2 #number of frame congruency
L = length(unique(dat.clean$pixels)) #number of morphs
n = 60 #number of replications

sub=as.factor(dat.clean$sub)
levels(sub)=1:I
sub = as.numeric(sub)
stim=ifelse(dat.clean$frame=="CAT"|dat.clean$frame=="THE",1,2)
frame=ifelse(dat.clean$frame=="CAT"|dat.clean$frame=="H",1,2)
morph=dat.clean$pixels-1

# Things JAGS needs to worry about
####
M=1000

stimval=c(0,1)
frameval=c(-(1/2),(1/2))

theta=c("alpha","beta")
nVar=length(theta)
sigma=.05*diag(nVar)

data=list("Y","I","J","K","L","N","sub","stim","frame","morph","stimval","frameval","nVar","sigma")

# Initial Values jags figures out

# Parameters
param = c("mu","theta","rho","alpha","beta")

# JAGS(R2jags)
#####
samples = jags(data, parameters.to.save = param, model.file="CAjags.txt", n.chains=1, n.iter=M, n.burnin=1)

# From R2jags
####
# Population Level
rho=samples$BUGSoutput$sims.list$rho
rho.hist=hist(rho,prob=T,col='lightblue',xlim=c(-1,1),breaks = seq(-1.025,1.025,.05),main = " ", cex=1.3,xlab = "Population Correlation")
abline(h=.5)
abline(v=0,col="gray",lty=3)

meanrho=mean(rho)
nullrhofactor=.5/rho.hist$density[21]
rhofactor=rho.hist$density[28]/.5

meanrho
nullrhofactor

# Attempts (Original)
##########
# 1) mean = .33; 3.567857
# 2) mean = .33; 12.4875
# 3) mean = .33; 6.24375
# 4) mean = .34; 3.121875
# 5) mean = .32; 3.121875

# Attempts (Function)
##########
# 1) mean = .34; 6.24375 
# 2) mean = .33; 2.775
# 3) mean = .34; 6.24375
# 4) mean = .34; 8.325 
# 5) mean = .32; 3.567857
# 6) mean = .34; 24.975 !!!!!!
# 7) mean = .23; .8919643 (with 75 included!)

# Attempts (No Block 1/2)
##########
# 1) .31; 3.56
# 2) .31; Inf
# 3) .31; 4.995
# 4) .32; 4.1625
# 5) .32; 4.1325
# 6) .30; 12.4875




