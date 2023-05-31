library("lavaan")
library("dplyr")
library("readxl")
library("bruceR")

setwd("C:/Users/redclass/OneDrive/学生/苟泽鹏/Computational musicology/Validation for AWE-S/overall analysis/data")


load("C:/Users/redclass/OneDrive/学生/苟泽鹏/Computational musicology/Validation for AWE-S/overall analysis/data/results2b3b4b520230319.RData)
##############################################################
#################read data and data clean#####################
##############################################################


#reading data
data_study2b <- read_excel("Study 2b_writing.xlsx",sheet = "Sheet1") 
data_study3b <- read_excel("Study 3b_movie.xlsx"  ,sheet = "Sheet1") 
data_study4b <- read_excel("Study 4b_music.xlsx"  ,sheet = "Sheet1") 



#check data class 
class(data_study2b$awes1)
class(data_study3b$awes1)
class(data_study4b$awes1)

#transfer the data class from "character" to "numeric"
is_all_numeric <- function(x) 
{
  !any(is.na(suppressWarnings(as.numeric(na.omit(x))))) & is.character(x)
}

data_study2b <- data_study2b %>%
  mutate_if(is_all_numeric,as.numeric)  
data_study3b <- data_study3b %>%
  mutate_if(is_all_numeric,as.numeric) 
data_study4b <- data_study4b %>%
  mutate_if(is_all_numeric,as.numeric) 




#coding items in Chinese version of AWE-S

#check the internal reliability of total scale and sub-scales in Study 2b_writing
Alpha(data_study2b, "awes", 1:30 )  #Total Scale            Cronbach’s α = 0.909
Alpha(data_study2b, "awes", 1:5  )  #Time Dilation          Cronbach’s α = 0.792
Alpha(data_study2b, "awes", 6:10 )  #Self-Diminishment      Cronbach’s α = 0.857
Alpha(data_study2b, "awes", 11:15)  #Connectedness          Cronbach’s α = 0.796
Alpha(data_study2b, "awes", 16:20)  #Vastness               Cronbach’s α = 0.839
Alpha(data_study2b, "awes", 21:25)  #Physical Sensations    Cronbach’s α = 0.813 
Alpha(data_study2b, "awes", 26:30)  #Need for Accommodation Cronbach’s α = 0.611

#check the internal reliability of total scale and sub-scales in Study 3b_movie
Alpha(data_study3b, "awes", 1:30 )  #Total Scale            Cronbach’s α = 0.905
Alpha(data_study3b, "awes", 1:5  )  #Time Dilation          Cronbach’s α = 0.748
Alpha(data_study3b, "awes", 6:10 )  #Self-Diminishment      Cronbach’s α = 0.837
Alpha(data_study3b, "awes", 11:15)  #Connectedness          Cronbach’s α = 0.766
Alpha(data_study3b, "awes", 16:20)  #Vastness               Cronbach’s α = 0.814
Alpha(data_study3b, "awes", 21:25)  #Physical Sensations    Cronbach’s α = 0.824 
Alpha(data_study3b, "awes", 26:30)  #Need for Accommodation Cronbach’s α = 0.591

#check the internal reliability of total scale and sub-scales in Study 4b_music
Alpha(data_study4b, "awes", 1:30 )  #Total Scale            Cronbach’s α = 0.905
Alpha(data_study4b, "awes", 1:5  )  #Time Dilation          Cronbach’s α = 0.753
Alpha(data_study4b, "awes", 6:10 )  #Self-Diminishment      Cronbach’s α = 0.818
Alpha(data_study4b, "awes", 11:15)  #Connectedness          Cronbach’s α = 0.865
Alpha(data_study4b, "awes", 16:20)  #Vastness               Cronbach’s α = 0.852
Alpha(data_study4b, "awes", 21:25)  #Physical Sensations    Cronbach’s α = 0.813 
Alpha(data_study4b, "awes", 26:30)  #Need for Accommodation Cronbach’s α = 0.628

#calculate the mean scores for the total scale and sub-scales in Study 2b_writing
data_study2b$AWES_score<-(data_study2b$awes1 +data_study2b$awes2 +data_study2b$awes3 +data_study2b$awes4 +data_study2b$awes5 +
                          data_study2b$awes6 +data_study2b$awes7 +data_study2b$awes8 +data_study2b$awes9 +data_study2b$awes10+
                          data_study2b$awes11+data_study2b$awes12+data_study2b$awes13+data_study2b$awes14+data_study2b$awes15+
                          data_study2b$awes16+data_study2b$awes17+data_study2b$awes18+data_study2b$awes19+data_study2b$awes20+
                          data_study2b$awes21+data_study2b$awes22+data_study2b$awes23+data_study2b$awes24+data_study2b$awes25+
                          data_study2b$awes26+data_study2b$awes27+data_study2b$awes28+data_study2b$awes29+data_study2b$awes30)/30

data_study2b$TimeD_score<-(data_study2b$awes1 +data_study2b$awes2 +data_study2b$awes3 +data_study2b$awes4 +data_study2b$awes5)/5
data_study2b$SelfD_score<-(data_study2b$awes6 +data_study2b$awes7 +data_study2b$awes8 +data_study2b$awes9 +data_study2b$awes10)/5
data_study2b$Conne_score<-(data_study2b$awes11+data_study2b$awes12+data_study2b$awes13+data_study2b$awes14+data_study2b$awes15)/5
data_study2b$Vastn_score<-(data_study2b$awes16+data_study2b$awes17+data_study2b$awes18+data_study2b$awes19+data_study2b$awes20)/5
data_study2b$PhysS_score<-(data_study2b$awes21+data_study2b$awes22+data_study2b$awes23+data_study2b$awes24+data_study2b$awes25)/5
data_study2b$NeedA_score<-(data_study2b$awes26+data_study2b$awes27+data_study2b$awes28+data_study2b$awes29+data_study2b$awes30)/5


#calculate the mean scores for the total scale and sub-scales in Study 3b_movie
data_study3b$AWES_score<-(data_study3b$awes1 +data_study3b$awes2 +data_study3b$awes3 +data_study3b$awes4 +data_study3b$awes5 +
                          data_study3b$awes6 +data_study3b$awes7 +data_study3b$awes8 +data_study3b$awes9 +data_study3b$awes10+
                          data_study3b$awes11+data_study3b$awes12+data_study3b$awes13+data_study3b$awes14+data_study3b$awes15+
                          data_study3b$awes16+data_study3b$awes17+data_study3b$awes18+data_study3b$awes19+data_study3b$awes20+
                          data_study3b$awes21+data_study3b$awes22+data_study3b$awes23+data_study3b$awes24+data_study3b$awes25+
                          data_study3b$awes26+data_study3b$awes27+data_study3b$awes28+data_study3b$awes29+data_study3b$awes30)/30

data_study3b$TimeD_score<-(data_study3b$awes1 +data_study3b$awes2 +data_study3b$awes3 +data_study3b$awes4 +data_study3b$awes5)/5
data_study3b$SelfD_score<-(data_study3b$awes6 +data_study3b$awes7 +data_study3b$awes8 +data_study3b$awes9 +data_study3b$awes10)/5
data_study3b$Conne_score<-(data_study3b$awes11+data_study3b$awes12+data_study3b$awes13+data_study3b$awes14+data_study3b$awes15)/5
data_study3b$Vastn_score<-(data_study3b$awes16+data_study3b$awes17+data_study3b$awes18+data_study3b$awes19+data_study3b$awes20)/5
data_study3b$PhysS_score<-(data_study3b$awes21+data_study3b$awes22+data_study3b$awes23+data_study3b$awes24+data_study3b$awes25)/5
data_study3b$NeedA_score<-(data_study3b$awes26+data_study3b$awes27+data_study3b$awes28+data_study3b$awes29+data_study3b$awes30)/5

#calculate the mean scores for the total scale and sub-scales in Study 4b_music
data_study4b$AWES_score<-(data_study4b$awes1 +data_study4b$awes2 +data_study4b$awes3 +data_study4b$awes4 +data_study4b$awes5 +
                          data_study4b$awes6 +data_study4b$awes7 +data_study4b$awes8 +data_study4b$awes9 +data_study4b$awes10+
                          data_study4b$awes11+data_study4b$awes12+data_study4b$awes13+data_study4b$awes14+data_study4b$awes15+
                          data_study4b$awes16+data_study4b$awes17+data_study4b$awes18+data_study4b$awes19+data_study4b$awes20+
                          data_study4b$awes21+data_study4b$awes22+data_study4b$awes23+data_study4b$awes24+data_study4b$awes25+
                          data_study4b$awes26+data_study4b$awes27+data_study4b$awes28+data_study4b$awes29+data_study4b$awes30)/30

data_study4b$TimeD_score<-(data_study4b$awes1 +data_study4b$awes2 +data_study4b$awes3 +data_study4b$awes4 +data_study4b$awes5)/5
data_study4b$SelfD_score<-(data_study4b$awes6 +data_study4b$awes7 +data_study4b$awes8 +data_study4b$awes9 +data_study4b$awes10)/5
data_study4b$Conne_score<-(data_study4b$awes11+data_study4b$awes12+data_study4b$awes13+data_study4b$awes14+data_study4b$awes15)/5
data_study4b$Vastn_score<-(data_study4b$awes16+data_study4b$awes17+data_study4b$awes18+data_study4b$awes19+data_study4b$awes20)/5
data_study4b$PhysS_score<-(data_study4b$awes21+data_study4b$awes22+data_study4b$awes23+data_study4b$awes24+data_study4b$awes25)/5
data_study4b$NeedA_score<-(data_study4b$awes26+data_study4b$awes27+data_study4b$awes28+data_study4b$awes29+data_study4b$awes30)/5



#coding items in big five personality

#calculate the mean scores for five personality traits in Study 2b_writing
data_study2b$Extra_score<-(6-data_study2b$bf1+data_study2b$bf6 )/2
data_study2b$Agree_score<-(data_study2b$bf2+6-data_study2b$bf7 )/2
data_study2b$Consc_score<-(6-data_study2b$bf3+data_study2b$bf8 )/2
data_study2b$Neuro_socre<-(6-data_study2b$bf4+data_study2b$bf9 )/2
data_study2b$Openn_score<-(6-data_study2b$bf5+data_study2b$bf10)/2

#calculate the mean scores for five personality traits in Study 3b_movie
data_study3b$Extra_score<-(6-data_study3b$bf1+data_study3b$bf6 )/2
data_study3b$Agree_score<-(data_study3b$bf2+6-data_study3b$bf7 )/2
data_study3b$Consc_score<-(6-data_study3b$bf3+data_study3b$bf8 )/2
data_study3b$Neuro_socre<-(6-data_study3b$bf4+data_study3b$bf9 )/2
data_study3b$Openn_score<-(6-data_study3b$bf5+data_study3b$bf10)/2

#calculate the mean scores for five personality traits in Study 4b_music

data_study4b$Extra_score<-(6-data_study4b$bf1+data_study4b$bf6 )/2
data_study4b$Agree_score<-(data_study4b$bf2+6-data_study4b$bf7 )/2
data_study4b$Consc_score<-(6-data_study4b$bf3+data_study4b$bf8 )/2
data_study4b$Neuro_socre<-(6-data_study4b$bf4+data_study4b$bf9 )/2
data_study4b$Openn_score<-(6-data_study4b$bf5+data_study4b$bf10)/2

#coding items in mDES

#check the internal reliability of mdes in Study 2b_writing
Alpha(data_study2b, "mdes", 1:8 )  #Negative emotion Cronbach’s α = 0.778
Alpha(data_study2b, "mdes", 9:18)  #Positive emotion Cronbach’s α = 0.870
#check the internal reliability of mdes in Study 3b_movie
Alpha(data_study3b, "mdes", 1:8 )  #Negative emotion Cronbach’s α = 0.768
Alpha(data_study3b, "mdes", 9:18)  #Positive emotion Cronbach’s α = 0.816
#check the internal reliability of mdes in Study 4b_music
Alpha(data_study4b, "mdes", 1:8 )  #Negative emotion Cronbach’s α = 0.778
Alpha(data_study4b, "mdes", 9:18)  #Positive emotion Cronbach’s α = 0.826

#calculate the mean scores for negative/positive emotion in Study 2b_writing
data_study2b$NegaE_score<-(data_study2b$mdes1 +data_study2b$mdes2 +data_study2b$mdes3 +
                           data_study2b$mdes4 +data_study2b$mdes5 +data_study2b$mdes6 +
                           data_study2b$mdes7 +data_study2b$mdes8)/8
data_study2b$PosiE_score<-(data_study2b$mdes9 +data_study2b$mdes10+data_study2b$mdes11+
                           data_study2b$mdes12+data_study2b$mdes13+data_study2b$mdes14+
                           data_study2b$mdes15+data_study2b$mdes16+data_study2b$mdes17+
                           data_study2b$mdes18)/10
#calculate the mean scores for negative/positive emotion in Study 3b_movie
data_study3b$NegaE_score<-(data_study3b$mdes1 +data_study3b$mdes2 +data_study3b$mdes3 +
                           data_study3b$mdes4 +data_study3b$mdes5 +data_study3b$mdes6 +
                           data_study3b$mdes7 +data_study3b$mdes8)/8
data_study3b$PosiE_score<-(data_study3b$mdes9 +data_study3b$mdes10+data_study3b$mdes11+
                           data_study3b$mdes12+data_study3b$mdes13+data_study3b$mdes14+
                           data_study3b$mdes15+data_study3b$mdes16+data_study3b$mdes17+
                           data_study3b$mdes18)/10
#calculate the mean scores for negative/positive emotion in Study 4b_music
data_study4b$NegaE_score<-(data_study4b$mdes1 +data_study4b$mdes2 +data_study4b$mdes3 +
                           data_study4b$mdes4 +data_study4b$mdes5 +data_study4b$mdes6 +
                           data_study4b$mdes7 +data_study4b$mdes8)/8
data_study4b$PosiE_score<-(data_study4b$mdes9 +data_study4b$mdes10+data_study4b$mdes11+
                           data_study4b$mdes12+data_study4b$mdes13+data_study4b$mdes14+
                           data_study4b$mdes15+data_study4b$mdes16+data_study4b$mdes17+
                           data_study4b$mdes18)/10


##############################################################
#######################data analysis##########################
##############################################################

###1.1CFA-construct validity in study 2b

#set model

mod_6F_study2b <- '
#factor

TimeD =~ awes1 +awes2 +awes3 +awes4 +awes5
SelfD =~ awes6 +awes7 +awes8 +awes9 +awes10
Conne =~ awes11+awes12+awes13+awes14+awes15
Vastn =~ awes16+awes17+awes18+awes19+awes20
PhysS =~ awes21+awes22+awes23+awes24+awes25
NeedA =~ awes26+awes27+awes28+awes29+awes30'

#estimate sem

fit6F_study2b    <- cfa(mod_6F_study2b  , data=data_study2b, std.lv=T, estimator='MLR')

#summary results and model fits
summary(fit6F_study2b   , fit.measures = TRUE, standardized = TRUE)





###1.2CFA-construct validity in study 3b

#set model

mod_6F_study3b <- '
#factor

TimeD =~ awes1 +awes2 +awes3 +awes4 +awes5
SelfD =~ awes6 +awes7 +awes8 +awes9 +awes10
Conne =~ awes11+awes12+awes13+awes14+awes15
Vastn =~ awes16+awes17+awes18+awes19+awes20
PhysS =~ awes21+awes22+awes23+awes24+awes25
NeedA =~ awes26+awes27+awes28+awes29+awes30'

#estimate sem

fit6F_study3b    <- cfa(mod_6F_study3b  , data=data_study3b, std.lv=T, estimator='MLR')

#summary results and model fits
summary(fit6F_study3b   , fit.measures = TRUE, standardized = TRUE)


#we first delete awes 27 since its factor loading is low

mod_6F_study3b_r1 <- '
#factor

TimeD =~ awes1 +awes2 +awes3 +awes4 +awes5
SelfD =~ awes6 +awes7 +awes8 +awes9 +awes10
Conne =~ awes11+awes12+awes13+awes14+awes15
Vastn =~ awes16+awes17+awes18+awes19+awes20
PhysS =~ awes21+awes22+awes23+awes24+awes25
NeedA =~ awes26+awes28+awes29+awes30'

#estimate sem

fit6F_study3b_r1    <- cfa(mod_6F_study3b_r1  , data=data_study3b, std.lv=T, estimator='MLR')

#summary results and model fits
summary(fit6F_study3b_r1   , fit.measures = TRUE, standardized = TRUE)

#modify the model
modindices(fit6F_study3b_r1, minimum.value = 10, sort = TRUE)  #we further delete awes25


mod_6F_study3b_r2 <- '
#factor

TimeD =~ awes1 +awes2 +awes3 +awes5
SelfD =~ awes6 +awes7 +awes8 +awes9 +awes10
Conne =~ awes11+awes12+awes13+awes14+awes15
Vastn =~ awes16+awes17+awes18+awes19+awes20
PhysS =~ awes21+awes22+awes23+awes24+awes25
NeedA =~ awes26+awes28+awes29+awes30'

#estimate sem

fit6F_study3b_r2    <- cfa(mod_6F_study3b_r2  , data=data_study3b, std.lv=T, estimator='MLR')

#summary results and model fits
summary(fit6F_study3b_r2   , fit.measures = TRUE, standardized = TRUE)

Alpha(data_study3b, vars=cc("awes1 ,awes2 ,awes3 ,awes5 ,awes6 ,awes7 ,awes8 ,awes9 ,awes10,
                             awes11,awes12,awes13,awes14,awes15,awes16,awes17,awes18,awes19,awes20,
                             awes21,awes22,awes23,awes24,awes25,
                             awes26,awes28,awes29,awes30") )  #Cronbach’s α = 0.904
Alpha(data_study3b, vars=cc("awes1 ,awes2 ,awes3 ,awes5")  )  #Cronbach’s α = 0.725
Alpha(data_study3b, vars=cc("awes26,awes28,awes29,awes30") )  #Cronbach’s α = 0.636


data_study3b$AWES_score_m<-(data_study3b$awes1 +data_study3b$awes2 +data_study3b$awes3 +data_study3b$awes5 +
                            data_study3b$awes6 +data_study3b$awes7 +data_study3b$awes8 +data_study3b$awes9 +data_study3b$awes10+
                            data_study3b$awes11+data_study3b$awes12+data_study3b$awes13+data_study3b$awes14+data_study3b$awes15+
                            data_study3b$awes16+data_study3b$awes17+data_study3b$awes18+data_study3b$awes19+data_study3b$awes20+
                            data_study3b$awes21+data_study3b$awes22+data_study3b$awes23+data_study3b$awes24+
                            data_study3b$awes26+data_study3b$awes28+data_study3b$awes29+data_study3b$awes30)/28
data_study3b$TimeD_score_m<-(data_study3b$awes1 +data_study3b$awes2 +data_study3b$awes3 +data_study3b$awes5)/4
data_study3b$NeedA_score_m<-(data_study3b$awes26+data_study3b$awes28+data_study3b$awes29+data_study3b$awes30)/4


cor(data_study3b$AWES_score,data_study3b$AWES_score_m)   #0.9941501
cor(data_study3b$TimeD_score,data_study3b$TimeD_score_m) #0.9748106
cor(data_study3b$NeedA_score,data_study3b$NeedA_score_m) #0.9291592

###1.3CFA-construct validity in study 4b

#set model

mod_6F_study4b <- '
#factor

TimeD =~ awes1 +awes2 +awes3 +awes4 +awes5
SelfD =~ awes6 +awes7 +awes8 +awes9 +awes10
Conne =~ awes11+awes12+awes13+awes14+awes15
Vastn =~ awes16+awes17+awes18+awes19+awes20
PhysS =~ awes21+awes22+awes23+awes24+awes25
NeedA =~ awes26+awes27+awes28+awes29+awes30'

#estimate sem

fit6F_study4b    <- cfa(mod_6F_study4b  , data=data_study4b, std.lv=T, estimator='MLR')

#summary results and model fits
summary(fit6F_study4b   , fit.measures = TRUE, standardized = TRUE)

#we first delete awes 27 since its factor loading is low

mod_6F_study4b_r1 <- '
#factor

TimeD =~ awes1 +awes2 +awes3 +awes4 +awes5
SelfD =~ awes6 +awes7 +awes8 +awes9 +awes10
Conne =~ awes11+awes12+awes13+awes14+awes15
Vastn =~ awes16+awes17+awes18+awes19+awes20
PhysS =~ awes21+awes22+awes23+awes24+awes25
NeedA =~ awes26+awes28+awes29+awes30'

#estimate sem

fit6F_study4b_r1    <- cfa(mod_6F_study4b_r1  , data=data_study4b, std.lv=T, estimator='MLR')

#summary results and model fits
summary(fit6F_study4b_r1   , fit.measures = TRUE, standardized = TRUE)

#modify the model
modindices(fit6F_study4b_r1, minimum.value = 10, sort = TRUE)  #we further delete awes10

mod_6F_study4b_r2 <- '
#factor

TimeD =~ awes1 +awes2 +awes3 +awes4 +awes5
SelfD =~ awes6 +awes7 +awes8 +awes9 
Conne =~ awes11+awes12+awes13+awes14+awes15
Vastn =~ awes16+awes17+awes18+awes19+awes20
PhysS =~ awes21+awes22+awes23+awes24+awes25
NeedA =~ awes26+awes28+awes29+awes30'
#delete awes10

fit6F_study4b_r2 <- cfa(mod_6F_study4b_r2, data=data_study4b, std.lv=T, estimator='MLR')
summary(fit6F_study4b_r2 , fit.measures = TRUE, standardized = TRUE)
modindices(fit6F_study4b_r2, minimum.value = 10, sort = TRUE) #we further delete awes1

mod_6F_study4b_r3 <- '
#factor

TimeD =~ awes2 +awes3 +awes4 +awes5
SelfD =~ awes6 +awes7 +awes8 +awes9 
Conne =~ awes11+awes12+awes13+awes14+awes15
Vastn =~ awes16+awes17+awes18+awes19+awes20
PhysS =~ awes21+awes22+awes23+awes24+awes25
NeedA =~ awes26+awes28+awes29+awes30'

#delete awes1

fit6F_study4b_r3 <- cfa(mod_6F_study4b_r3, data=data_study4b, std.lv=T, estimator='MLR')
summary(fit6F_study4b_r3 , fit.measures = TRUE, standardized = TRUE)


Alpha(data_study4b, vars=cc("awes2 ,awes3 ,awes4 ,awes5 ,awes6 ,awes7 ,awes8 ,awes9,
                             awes11,awes12,awes13,awes14,awes15,awes16,awes17,awes18,awes19,awes20,
                             awes21,awes22,awes23,awes24,awes25,
                             awes26,awes28,awes29,awes30") )  #Cronbach’s α = 0.904
Alpha(data_study4b, vars=cc("awes2 ,awes3 ,awes4 ,awes5 ") )  #Cronbach’s α = 0.723
Alpha(data_study4b, vars=cc("awes6 ,awes7 ,awes8 ,awes9 ") )  #Cronbach’s α = 0.837
Alpha(data_study4b, vars=cc("awes26,awes28,awes29,awes30") )  #Cronbach’s α = 0.693


data_study4b$AWES_score_m<-(data_study4b$awes2 +data_study4b$awes3 +data_study4b$awes4 +data_study4b$awes5 +
                            data_study4b$awes6 +data_study4b$awes7 +data_study4b$awes8 +data_study4b$awes9 +
                            data_study4b$awes11+data_study4b$awes12+data_study4b$awes13+data_study4b$awes14+data_study4b$awes15+
                            data_study4b$awes16+data_study4b$awes17+data_study4b$awes18+data_study4b$awes19+data_study4b$awes20+
                            data_study4b$awes21+data_study4b$awes22+data_study4b$awes23+data_study4b$awes24+data_study4b$awes25+
                            data_study4b$awes26+data_study4b$awes28+data_study4b$awes29+data_study4b$awes30)/27

data_study4b$TimeD_score_m<-(data_study4b$awes2 +data_study4b$awes3 +data_study4b$awes4 +data_study4b$awes5)/4
data_study4b$SelfD_score_m<-(data_study4b$awes6 +data_study4b$awes7 +data_study4b$awes8 +data_study4b$awes9)/4
data_study4b$NeedA_score_m<-(data_study4b$awes26+data_study4b$awes28+data_study4b$awes29+data_study4b$awes30)/4

cor(data_study4b$AWES_score,data_study4b$AWES_score_m)   #0.9932474
cor(data_study4b$TimeD_score,data_study4b$TimeD_score_m) #0.9599163
cor(data_study4b$SelfD_score,data_study4b$SelfD_score_m) #0.9778036
cor(data_study4b$NeedA_score,data_study4b$NeedA_score_m) #0.9351482


#criterion validation

#correlations between big five personality traits, emotions, and awe

#1.1 study 2b_original variable
CV1_traits_study2b_ori<-furniture::tableC(data_study2b,
                  "Total score of AWES"   =AWES_score,
                  "Time Dilation"         =TimeD_score,
                  "Self-Diminishment"     =SelfD_score,
                  "Connectedness"         =Conne_score,
                  "Vastness"              =Vastn_score,
                  "Physical Sensations"   =PhysS_score,
                  "Need for Accommodation"=NeedA_score,
                  "Extraversion"          =Extra_score,
                  "Agreeableness"         =Agree_score,
                  "Conscientiousness"     =Consc_score,
                  "Neuroticism"           =Neuro_socre,
                  "Openness"              =Openn_score,
                  "Negative Emotion"      =NegaE_score,
                  "Positive Emption"      =PosiE_score)
write.csv(CV1_traits_study2b_ori,file = "CV1_study2b_ori.csv")

#2.1 study 3b_original variable
CV2_traits_study3b_ori<-furniture::tableC(data_study3b,
                                          "Total score of AWES"   =AWES_score,
                                          "Time Dilation"         =TimeD_score,
                                          "Self-Diminishment"     =SelfD_score,
                                          "Connectedness"         =Conne_score,
                                          "Vastness"              =Vastn_score,
                                          "Physical Sensations"   =PhysS_score,
                                          "Need for Accommodation"=NeedA_score,
                                          "Extraversion"          =Extra_score,
                                          "Agreeableness"         =Agree_score,
                                          "Conscientiousness"     =Consc_score,
                                          "Neuroticism"           =Neuro_socre,
                                          "Openness"              =Openn_score,
                                          "Negative Emotion"      =NegaE_score,
                                          "Positive Emption"      =PosiE_score)
write.csv(CV2_traits_study3b_ori,file = "CV2_study3b_ori.csv")
  
#2.2 study 3b_modified variable

CV3_traits_study3b_mod<-furniture::tableC(data_study3b,
                                          "Total score of AWES"   =AWES_score_m,
                                          "Time Dilation"         =TimeD_score_m,
                                          "Self-Diminishment"     =SelfD_score,
                                          "Connectedness"         =Conne_score,
                                          "Vastness"              =Vastn_score,
                                          "Physical Sensations"   =PhysS_score,
                                          "Need for Accommodation"=NeedA_score_m,
                                          "Extraversion"          =Extra_score,
                                          "Agreeableness"         =Agree_score,
                                          "Conscientiousness"     =Consc_score,
                                          "Neuroticism"           =Neuro_socre,
                                          "Openness"              =Openn_score,
                                          "Negative Emotion"      =NegaE_score,
                                          "Positive Emption"      =PosiE_score)
write.csv(CV3_traits_study3b_mod,file = "CV3_study3b_mod.csv")


#3.1 study 4b_original variable
CV4_traits_study4b_ori<-furniture::tableC(data_study4b,
                                          "Total score of AWES"   =AWES_score,
                                          "Time Dilation"         =TimeD_score,
                                          "Self-Diminishment"     =SelfD_score,
                                          "Connectedness"         =Conne_score,
                                          "Vastness"              =Vastn_score,
                                          "Physical Sensations"   =PhysS_score,
                                          "Need for Accommodation"=NeedA_score,
                                          "Extraversion"          =Extra_score,
                                          "Agreeableness"         =Agree_score,
                                          "Conscientiousness"     =Consc_score,
                                          "Neuroticism"           =Neuro_socre,
                                          "Openness"              =Openn_score,
                                          "Negative Emotion"      =NegaE_score,
                                          "Positive Emption"      =PosiE_score)
write.csv(CV4_traits_study4b_ori,file = "CV4_study4b_ori.csv")

#3.3 study 4b_modified variable
CV5_traits_study4b_mod<-furniture::tableC(data_study4b,
                                          "Total score of AWES"   =AWES_score_m,
                                          "Time Dilation"         =TimeD_score_m,
                                          "Self-Diminishment"     =SelfD_score_m,
                                          "Connectedness"         =Conne_score,
                                          "Vastness"              =Vastn_score,
                                          "Physical Sensations"   =PhysS_score,
                                          "Need for Accommodation"=NeedA_score_m,
                                          "Extraversion"          =Extra_score,
                                          "Agreeableness"         =Agree_score,
                                          "Conscientiousness"     =Consc_score,
                                          "Neuroticism"           =Neuro_socre,
                                          "Openness"              =Openn_score,
                                          "Negative Emotion"      =NegaE_score,
                                          "Positive Emption"      =PosiE_score)
write.csv(CV5_traits_study4b_mod,file = "CV5_study4b_mod.csv")


##############################################################
#################Study 5: network structure###################
##############################################################

#load packages of network analysis
library("BGGM")
library("qgraph")

#read data
study2b_awe_domain<-data_study2b%>%
  dplyr::select(TimeD_score,SelfD_score,
         Conne_score,Vastn_score,
         PhysS_score,NeedA_score,
         mdes1 ,mdes2 ,mdes3 ,mdes4 ,mdes5 ,
         mdes6 ,mdes7 ,mdes8 ,mdes9 ,mdes10,
         mdes11,mdes12,mdes13,mdes14,mdes15,
         mdes16,mdes17,mdes18,mdes19,mdes20)
study3b_awe_domain<-data_study3b%>%
  dplyr::select(TimeD_score,SelfD_score,
                Conne_score,Vastn_score,
                PhysS_score,NeedA_score,
                mdes1 ,mdes2 ,mdes3 ,mdes4 ,mdes5 ,
                mdes6 ,mdes7 ,mdes8 ,mdes9 ,mdes10,
                mdes11,mdes12,mdes13,mdes14,mdes15,
                mdes16,mdes17,mdes18,mdes19,mdes20)
study4b_awe_domain<-data_study4b%>%
  dplyr::select(TimeD_score,SelfD_score,
                Conne_score,Vastn_score,
                PhysS_score,NeedA_score,
                mdes1 ,mdes2 ,mdes3 ,mdes4 ,mdes5 ,
                mdes6 ,mdes7 ,mdes8 ,mdes9 ,mdes10,
                mdes11,mdes12,mdes13,mdes14,mdes15,
                mdes16,mdes17,mdes18,mdes19,mdes20)

# Create legend
groups<- list("Awe Domains"          = 1:6,
              "Negative Emotions"    = 7:14,
              "Positive Emotions"    = 15:24,
              "Other Emotions"       = 25:26)
nodenames<-c("Time Dilation",	"Self-Diminishment","Connectedness",
             "Vastness","Physical Sensations","Need for Accommodation",
             "Angry/irritated/annoyed","Sad/downhearted/unhappy",
             "Scared/fearful/afraid","Disgust/distaste/revulsion",
             "Contemptuous/scornful/disdainful","Embarrassed/self-conscious/blushing",
             "Repentant/guilty/blameworthy","Ashamed/humiliated/disgraced",
             "Grateful/appreciative/thankful","Interested/alert/curious",
             "Love/closeness/trust","Amused/fun-loving/silly",
             "Glad/happy/joyful","Hopeful/optimistic/encouraged",
             "Sexual/desiring/flirtatious","Proud/confident/self-assured",
             "Content/serene/peaceful","Awe/wonder/amazement",
             "Sympathy/concern/compassion","Surprised/amazed/astonished")

# Estimate network:

model_study2b<-BGGM::estimate(study2b_awe_domain, type = "continuous",
                              prior_sd = 0.5,
                              iter = 5000,
                              progress = TRUE,
                              seed = 4321)
model_study3b<-BGGM::estimate(study3b_awe_domain, type = "continuous",
                              prior_sd = 0.5,
                              iter = 5000,
                              progress = TRUE,
                              seed = 4321)
model_study4b<-BGGM::estimate(study4b_awe_domain, type = "continuous",
                              prior_sd = 0.5,
                              iter = 5000,
                              progress = TRUE,
                              seed = 4321)


summary_domain2b<-summary(model_study2b,cred = 0.95)
summary_domain3b<-summary(model_study3b,cred = 0.95)
summary_domain4b<-summary(model_study4b,cred = 0.95)

write.csv(summary_domain2b$dat_results, file = "network1_coef_study2b.csv")
write.csv(summary_domain3b$dat_results, file = "network2_coef_study3b.csv")
write.csv(summary_domain4b$dat_results, file = "network3_coef_study4b.csv")

selected_study2b<-BGGM::select(model_study2b, 
                               cred = 0.95, alternative = "two.sided")
selected_study3b<-BGGM::select(model_study3b, 
                               cred = 0.95, alternative = "two.sided")
selected_study4b<-BGGM::select(model_study4b, 
                               cred = 0.95, alternative = "two.sided")


#plot network
netgraph_study2b<-qgraph(selected_study2b$pcor_adj, groups=groups, 
                         layout= "spring",palette = "pastel",  
                         vsize = 8, esize = 10,vTrans = 200,
                         legend.cex=0.35, theme="colorblind", 
                         legend = F, nodeNames = F,
                         labels = T,cut =0.1, 
                         title = "(a) Study 2b_writing",
                         edge.labels=FALSE)

netgraph_study3b<-qgraph(selected_study3b$pcor_adj, groups=groups, 
                         layout= "spring",palette = "pastel",  
                         vsize = 8, esize = 10,vTrans = 200,
                         legend.cex=0.35, theme="colorblind",
                         legend = F, nodeNames = F,
                         labels = T,cut =0.1, 
                         title = "(b) Study 3b_movie",
                         edge.labels=FALSE)

netgraph_study4b<-qgraph(selected_study4b$pcor_adj, groups=groups, 
                         layout= "spring",palette = "pastel",  
                         vsize = 8, esize = 10,vTrans = 200,
                         legend.cex=0.35, theme="colorblind",
                         legend = F, nodeNames = F,
                         labels = T,cut =0.1, 
                         title = "(c) Study 4b_music",
                         edge.labels=FALSE)

pdf("figure_network_results.pdf", height=8, width=8)
par(mfrow = c(2, 2))
plot(netgraph_study2b)
plot(netgraph_study3b)
plot(netgraph_study4b)
dev.off()

netgraph_legend<-qgraph(selected_study2b$pcor_adj, groups=groups, 
                         layout= "spring",palette = "pastel",  
                         vsize = 6, esize = 10,vTrans = 240,
                         legend.cex=0.35, theme="colorblind", 
                         legend = T, nodeNames = nodenames,
                         labels = T,cut =0.1, 
                         title = "(a) Study 2b_writing",
                         edge.labels=FALSE)

pdf("figure_network_legend.pdf", height=8, width=8)
plot(netgraph_legend)
dev.off()


#network node


####strength

#define function

communities_a <-list("1" = 1,
                     "2" = 2,
                     "3" = 3,
                     "4" = 4,
                     "5" = 5,
                     "6" = 6,
                     "7" = 7,
                     "8" = 8,
                     "9" = 9,
                     "10" = 10,
                     "11" = 11,
                     "12" = 12,
                     "13" = 13,
                     "14" = 14,
                     "15" = 15,
                     "16" = 16,
                     "17" = 17,
                     "18" = 18,
                     "19" = 19,
                     "20" = 20,
                     "21" = 21,
                     "22" = 22,
                     "23" = 23,
                     "24" = 24,
                     "25" = 25,
                     "26" = 26)


strengthfunction<-function(x, ...){
  networktools::bridge(x, ...)$`Bridge Strength`
}


strength_study2b<-roll_your_own(object = model_study2b,
                                FUN = strengthfunction,
                                communities = communities_a,
                                cred = 0.95,
                                select = TRUE,
                                iter = 5000)
strength_study3b<-roll_your_own(object = model_study3b,
                                FUN = strengthfunction,
                                communities = communities_a,
                                cred = 0.95,
                                select = TRUE,
                                iter = 5000)
strength_study4b<-roll_your_own(object = model_study4b,
                                FUN = strengthfunction,
                                communities = communities_a,
                                cred = 0.95,
                                select = TRUE,
                                iter = 5000)

#write.csv(strength_workday$results, file = "strength_weekday.csv")
#write.csv(strength_weekend, file = "strength_weekend.csv")

#bridge
#we make two communities--one for awe and another for emotions

communities_b <-list("1" = 1:6,
                     "2" = 7:26)

bridge_study2b<-roll_your_own(object = model_study2b,
                              FUN = strengthfunction,
                              communities = communities_b,
                              cred = 0.95,
                              select = TRUE,
                              iter = 5000)
bridge_study3b<-roll_your_own(object = model_study3b,
                              FUN = strengthfunction,
                              communities = communities_b,
                              cred = 0.95,
                              select = TRUE,
                              iter = 5000)
bridge_study4b<-roll_your_own(object = model_study4b,
                              FUN = strengthfunction,
                              communities = communities_b,
                              cred = 0.95,
                              select = TRUE,
                              iter = 5000)

library("gridExtra")
pdf("Figure_Strength.pdf",width=12,height=12)
a<-plot(strength_study2b)+
  ggtitle("(a) Study 2b_writing ") +
  xlab("Score")
b<-plot(strength_study3b)+
  ggtitle("(b) Study 3b_movie") +
  xlab("Score")
c<-plot(strength_study4b)+
  ggtitle("(c) Study 4b_music") +
  xlab("Score")
d<-plot(bridge_study2b,
        fill = "lightblue") +
  ggtitle("(d) Study 2b_writing") +
  xlab("Score")
e<-plot(bridge_study3b,
        fill = "lightblue") +
  ggtitle("(e) Study 3b_movie") +
  xlab("Score")
f<-plot(bridge_study4b,
        fill = "lightblue") +
  ggtitle("(f) Study 4b_music") +
  xlab("Score")
grid.arrange(a,b,c,
             d,e,f,
             nrow = 2, ncol = 3)
dev.off()


##network differences


#the estimation model
compare_2bvs3b<-ggm_compare_estimate(study2b_awe_domain,study3b_awe_domain, 
                                    type = "continuous",
                                    prior_sd = 0.5,
                                    iter = 5000,
                                    impute = TRUE,
                                    progress = TRUE,
                                    seed = 1234)
compare_2bvs4b<-ggm_compare_estimate(study2b_awe_domain,study4b_awe_domain, 
                                     type = "continuous",
                                     prior_sd = 0.5,
                                     iter = 5000,
                                     impute = TRUE,
                                     progress = TRUE,
                                     seed = 1234)
compare_3bvs4b<-ggm_compare_estimate(study3b_awe_domain,study4b_awe_domain, 
                                     type = "continuous",
                                     prior_sd = 0.5,
                                     iter = 5000,
                                     impute = TRUE,
                                     progress = TRUE,
                                     seed = 1234)


summary_compare_2bvs3b<-summary(compare_2bvs3b, cred = 0.95,
                                col_names = F)
summary_compare_2bvs4b<-summary(compare_2bvs4b, cred = 0.95,
                                col_names = F)
summary_compare_3bvs4b<-summary(compare_3bvs4b, cred = 0.95,
                                col_names = F)

write.csv(summary_compare_2bvs3b$dat_results, file = "network_coef_diff_2bvs3b.csv")
write.csv(summary_compare_2bvs4b$dat_results, file = "network_coef_diff_2bvs4b.csv")
write.csv(summary_compare_3bvs4b$dat_results, file = "network_coef_diff_3bvs4b.csv")

plot(summary_compare_2bvs3b)
plot(summary_compare_2bvs4b)
plot(summary_compare_3bvs4b)

pdf("Figure_compare.pdf",width=35,height=4)
par(mfrow = c(1, 3))
plot(summary_compare_2bvs3b)
plot(summary_compare_2bvs4b)
plot(summary_compare_3bvs4b)
dev.off()

save.image("C:/Users/redclass/OneDrive/学生/苟泽鹏/Computational musicology/Validation for AWE-S/overall analysis/data/results2b3b4b520230319.RData")