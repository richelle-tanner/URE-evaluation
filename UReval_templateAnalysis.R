library(stringr)

####functions####
ind_comp_function <- function(postData, postQ, preData, preQ) { #Qs need to be in quotations
  ID <- c(preData[,"ID"], postData[,"ID"])
  pre_post <- c(rep("pre", length(preData[,1])), rep("post", length(postData[,1])))
  response <- c(preData[,preQ], postData[,postQ])
  comp <- data.frame(ID = ID,
                     Pre.Post = pre_post,
                     Response = response)
  return(comp)
}

####data import####
#datasets are reduced from the Qualtrics download to only include quantitative indicators
#qualitative data can be summarized through thematic coding outside of R
preQuantALL <- read.delim(paste(ddr, "Pre-Exp_QuantCleanALL.csv", sep=''), sep=',')
postQuantALL <- read.delim(paste(ddr, "Post-Exp_QuantCleanALL.csv", sep=''), sep=',')
allQuantALL <- merge(preQuantALL, postQuantALL, by = "ID")
write.csv(allQuantALL, "~/Documents/DSF_pedagogy/allQuant_matched.csv") #this is all of the individuals that answered both surveys

####data analysis####
#science career
sciCarALL <- ind_comp_function(postQuantALL, "ScienceCareer", preQuantALL, "ScienceCareer")
summary(aov(Response ~ Pre.Post + Error(ID), data=sciCarALL)) #NS

#research career
resCarALL <- ind_comp_function(postQuantALL, "ResearchCareer", preQuantALL, "ResearchCareer")
summary(aov(Response ~ Pre.Post + Error(ID), data=resCarALL)) #NS

#MS
msALL <- ind_comp_function(postQuantALL, "MS", preQuantALL, "MS")
summary(aov(Response ~ Pre.Post + Error(ID), data=msALL)) #NS

#PhD
phdALL <- ind_comp_function(postQuantALL, "PhD", preQuantALL, "PhD")
summary(aov(Response ~ Pre.Post + Error(ID), data=msALL)) #NS

#1. sense of belonging
SOB1 <- ind_comp_function(postQuantALL, "SOB_1", preQuantALL, "SOB_1")
summary(aov(Response ~ Pre.Post + Error(ID), data=SOB1)) #SIG p = 0.02

#2. satisfaction from teamwork
SOB2 <- ind_comp_function(postQuantALL, "SOB_2", preQuantALL, "SOB_2")
summary(aov(Response ~ Pre.Post + Error(ID), data=SOB2)) #NS

#3. I am a scientist
SOB3 <- ind_comp_function(postQuantALL, "SOB_3", preQuantALL, "SOB_3")
summary(aov(Response ~ Pre.Post + Error(ID), data=SOB3)) #SIG p = 0.02

#4. Daily work of a scientist is appealing
SOB4 <- ind_comp_function(postQuantALL, "SOB_4", preQuantALL, "SOB_4")
summary(aov(Response ~ Pre.Post + Error(ID), data=SOB4)) #NS

#these comparisons are for post-experience outcomes
#they can be replicated for pre-experience outcomes by changing .y to .x
#impact on students with out of school time burdens
summary(aov(ScienceCareer.y ~ Non.school, data=allQuantALL)) #NS
summary(aov(ResearchCareer.y ~ Non.school, data=allQuantALL)) #NS
summary(aov(MS.y ~ Non.school, data=allQuantALL)) #NS
summary(aov(PhD.y ~ Non.school, data=allQuantALL)) #NS
summary(aov(SOB_1.y ~ Non.school, data=allQuantALL)) #SIG p = 0.04
summary(aov(SOB_2.y ~ Non.school, data=allQuantALL)) #NS
summary(aov(SOB_3.y ~ Non.school, data=allQuantALL)) #NS
summary(aov(SOB_4.y ~ Non.school, data=allQuantALL)) #NS

#impact on transfer students
summary(aov(ScienceCareer.y ~ Transfer, data=allQuantALL)) #NS
summary(aov(ResearchCareer.y ~ Transfer, data=allQuantALL)) #NS
summary(aov(MS.y ~ Transfer, data=allQuantALL)) #NS
summary(aov(PhD.y ~ Transfer, data=allQuantALL)) #NS
summary(aov(SOB_1.y ~ Transfer, data=allQuantALL)) #NS
summary(aov(SOB_2.y ~ Transfer, data=allQuantALL)) #NS
summary(aov(SOB_3.y ~ Transfer, data=allQuantALL)) #NS
summary(aov(SOB_4.y ~ Transfer, data=allQuantALL)) #NS

#impact on students who have never had an inverts or microscopy course
summary(aov(ScienceCareer.y ~ Microscopy * Inverts, data=allQuantALL)) #microscopy SIG p = 0.004
summary(aov(ResearchCareer.y ~ Microscopy * Inverts, data=allQuantALL)) #NS
summary(aov(MS.y ~ Microscopy * Inverts, data=allQuantALL)) #NS
summary(aov(PhD.y ~ Microscopy * Inverts, data=allQuantALL)) #NS
summary(aov(SOB_1.y ~ Microscopy * Inverts, data=allQuantALL)) #NS
summary(aov(SOB_2.y ~ Microscopy * Inverts, data=allQuantALL)) #NS
summary(aov(SOB_3.y ~ Microscopy * Inverts, data=allQuantALL)) #NS
summary(aov(SOB_4.y ~ Microscopy * Inverts, data=allQuantALL)) #NS

#impact on students with a high course load
summary(aov(ScienceCareer.y ~ Units, data=allQuantALL)) #NS
summary(aov(ResearchCareer.y ~ Units, data=allQuantALL)) #NS
summary(aov(MS.y ~ Units, data=allQuantALL)) #NS
summary(aov(PhD.y ~ Units, data=allQuantALL)) #NS
summary(aov(SOB_1.y ~ Units, data=allQuantALL)) #NS
summary(aov(SOB_2.y ~ Units, data=allQuantALL)) #NS
summary(aov(SOB_3.y ~ Units, data=allQuantALL)) #NS
summary(aov(SOB_4.y ~ Units, data=allQuantALL)) #NS

#career value
summary(aov(ScienceCareer.y ~ CareerValue.y, data=allQuantALL)) #NS
summary(aov(ResearchCareer.y ~ CareerValue.y, data=allQuantALL)) #NS
summary(aov(MS.y ~ CareerValue.y, data=allQuantALL)) #NS
summary(aov(PhD.y ~ CareerValue.y, data=allQuantALL)) #NS
summary(aov(SOB_1.y ~ CareerValue.y, data=allQuantALL)) #NS
summary(aov(SOB_2.y ~ CareerValue.y, data=allQuantALL)) #NS
summary(aov(SOB_3.y ~ CareerValue.y, data=allQuantALL)) #NS
summary(aov(SOB_4.y ~ CareerValue.y, data=allQuantALL)) #NS

#rec to a peer
summary(aov(ScienceCareer.y ~ Recommend.y, data=allQuantALL)) #NS
summary(aov(ResearchCareer.y ~ Recommend.y, data=allQuantALL)) #NS
summary(aov(MS.y ~ Recommend.y, data=allQuantALL)) #NS
summary(aov(PhD.y ~ Recommend.y, data=allQuantALL)) #NS
summary(aov(SOB_1.y ~ Recommend.y, data=allQuantALL)) #NS
summary(aov(SOB_2.y ~ Recommend.y, data=allQuantALL)) #NS
summary(aov(SOB_3.y ~ Recommend.y, data=allQuantALL)) #NS
summary(aov(SOB_4.y ~ Recommend.y, data=allQuantALL)) #NS




