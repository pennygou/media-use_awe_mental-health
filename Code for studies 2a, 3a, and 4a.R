library("lavaan")
library("dplyr")
library("readxl")
library("bruceR")
library("ggstatsplot")

setwd("C:/Users/redclass/OneDrive/学生/苟泽鹏/Computational musicology/Validation for AWE-S/overall analysis/data")

##############################################################
#################read data and data clean#####################
##############################################################

#reading data
data_study2a <- read_excel("Study 2a_writing.xlsx",sheet = "Sheet1") 
data_study3a <- read_excel("Study 3a_movie.xlsx"  ,sheet = "Sheet1") 
data_study4a <- read_excel("Study 4a_music.xlsx"  ,sheet = "Sheet1") 

#check data class 
class(data_study2a$awes1)
class(data_study3a$awes1)
class(data_study4a$awes1)

#transfer the data class from "character" to "numeric"
is_all_numeric <- function(x) 
{
  !any(is.na(suppressWarnings(as.numeric(na.omit(x))))) & is.character(x)
}

data_study2a <- data_study2a %>%
  mutate_if(is_all_numeric,as.numeric)  
data_study3a <- data_study3a %>%
  mutate_if(is_all_numeric,as.numeric) 
data_study4a <- data_study4a %>%
  mutate_if(is_all_numeric,as.numeric) 


#check the number of participants in each group 
table(data_study2a$random)  #amusement74;awe70;neutral73 
table(data_study3a$random)  #amusement73;awe70;neutral71 
table(data_study4a$random)  #amusement76;awe76;neutral44 


#coding items in Chinese version of AWE-S

#check the internal reliability of total scale and sub-scales in Study 2a_writing
Alpha(data_study2a, "awes", 1:30 )  #Total Scale            Cronbach’s α = 0.933
Alpha(data_study2a, "awes", 1:5  )  #Time Dilation          Cronbach’s α = 0.816
Alpha(data_study2a, "awes", 6:10 )  #Self-Diminishment      Cronbach’s α = 0.892
Alpha(data_study2a, "awes", 11:15)  #Connectedness          Cronbach’s α = 0.834
Alpha(data_study2a, "awes", 16:20)  #Vastness               Cronbach’s α = 0.914
Alpha(data_study2a, "awes", 21:25)  #Physical Sensations    Cronbach’s α = 0.798 
Alpha(data_study2a, "awes", 26:30)  #Need for Accommodation Cronbach’s α = 0.721

#check the internal reliability of total scale and sub-scales in Study 3a_movie
Alpha(data_study3a, "awes", 1:30 )  #Total Scale            Cronbach’s α = 0.962
Alpha(data_study3a, "awes", 1:5  )  #Time Dilation          Cronbach’s α = 0.872
Alpha(data_study3a, "awes", 6:10 )  #Self-Diminishment      Cronbach’s α = 0.885
Alpha(data_study3a, "awes", 11:15)  #Connectedness          Cronbach’s α = 0.898
Alpha(data_study3a, "awes", 16:20)  #Vastness               Cronbach’s α = 0.939
Alpha(data_study3a, "awes", 21:25)  #Physical Sensations    Cronbach’s α = 0.855 
Alpha(data_study3a, "awes", 26:30)  #Need for Accommodation Cronbach’s α = 0.774

#check the internal reliability of total scale and sub-scales in Study 4a_music
Alpha(data_study4a, "awes", 1:30 )  #Total Scale            Cronbach’s α = 0.962
Alpha(data_study4a, "awes", 1:5  )  #Time Dilation          Cronbach’s α = 0.890
Alpha(data_study4a, "awes", 6:10 )  #Self-Diminishment      Cronbach’s α = 0.890
Alpha(data_study4a, "awes", 11:15)  #Connectedness          Cronbach’s α = 0.932
Alpha(data_study4a, "awes", 16:20)  #Vastness               Cronbach’s α = 0.949
Alpha(data_study4a, "awes", 21:25)  #Physical Sensations    Cronbach’s α = 0.837 
Alpha(data_study4a, "awes", 26:30)  #Need for Accommodation Cronbach’s α = 0.797


#calculate the mean scores for the total scale and sub-scales in Study 2a_writing
data_study2a$AWES_score<-(data_study2a$awes1 +data_study2a$awes2 +data_study2a$awes3 +data_study2a$awes4 +data_study2a$awes5 +
                          data_study2a$awes6 +data_study2a$awes7 +data_study2a$awes8 +data_study2a$awes9 +data_study2a$awes10+
                          data_study2a$awes11+data_study2a$awes12+data_study2a$awes13+data_study2a$awes14+data_study2a$awes15+
                          data_study2a$awes16+data_study2a$awes17+data_study2a$awes18+data_study2a$awes19+data_study2a$awes20+
                          data_study2a$awes21+data_study2a$awes22+data_study2a$awes23+data_study2a$awes24+data_study2a$awes25+
                          data_study2a$awes26+data_study2a$awes27+data_study2a$awes28+data_study2a$awes29+data_study2a$awes30)/30

data_study2a$TimeD_score<-(data_study2a$awes1 +data_study2a$awes2 +data_study2a$awes3 +data_study2a$awes4 +data_study2a$awes5)/5
data_study2a$SelfD_score<-(data_study2a$awes6 +data_study2a$awes7 +data_study2a$awes8 +data_study2a$awes9 +data_study2a$awes10)/5
data_study2a$Conne_score<-(data_study2a$awes11+data_study2a$awes12+data_study2a$awes13+data_study2a$awes14+data_study2a$awes15)/5
data_study2a$Vastn_score<-(data_study2a$awes16+data_study2a$awes17+data_study2a$awes18+data_study2a$awes19+data_study2a$awes20)/5
data_study2a$PhysS_score<-(data_study2a$awes21+data_study2a$awes22+data_study2a$awes23+data_study2a$awes24+data_study2a$awes25)/5
data_study2a$NeedA_score<-(data_study2a$awes26+data_study2a$awes27+data_study2a$awes28+data_study2a$awes29+data_study2a$awes30)/5

#calculate the mean scores for the total scale and sub-scales in Study 3a_movie
data_study3a$AWES_score<-(data_study3a$awes1 +data_study3a$awes2 +data_study3a$awes3 +data_study3a$awes4 +data_study3a$awes5 +
                          data_study3a$awes6 +data_study3a$awes7 +data_study3a$awes8 +data_study3a$awes9 +data_study3a$awes10+
                          data_study3a$awes11+data_study3a$awes12+data_study3a$awes13+data_study3a$awes14+data_study3a$awes15+
                          data_study3a$awes16+data_study3a$awes17+data_study3a$awes18+data_study3a$awes19+data_study3a$awes20+
                          data_study3a$awes21+data_study3a$awes22+data_study3a$awes23+data_study3a$awes24+data_study3a$awes25+
                          data_study3a$awes26+data_study3a$awes27+data_study3a$awes28+data_study3a$awes29+data_study3a$awes30)/30

data_study3a$TimeD_score<-(data_study3a$awes1 +data_study3a$awes2 +data_study3a$awes3 +data_study3a$awes4 +data_study3a$awes5)/5
data_study3a$SelfD_score<-(data_study3a$awes6 +data_study3a$awes7 +data_study3a$awes8 +data_study3a$awes9 +data_study3a$awes10)/5
data_study3a$Conne_score<-(data_study3a$awes11+data_study3a$awes12+data_study3a$awes13+data_study3a$awes14+data_study3a$awes15)/5
data_study3a$Vastn_score<-(data_study3a$awes16+data_study3a$awes17+data_study3a$awes18+data_study3a$awes19+data_study3a$awes20)/5
data_study3a$PhysS_score<-(data_study3a$awes21+data_study3a$awes22+data_study3a$awes23+data_study3a$awes24+data_study3a$awes25)/5
data_study3a$NeedA_score<-(data_study3a$awes26+data_study3a$awes27+data_study3a$awes28+data_study3a$awes29+data_study3a$awes30)/5

#calculate the mean scores for the total scale and sub-scales in Study 4a_music
data_study4a$AWES_score<-(data_study4a$awes1 +data_study4a$awes2 +data_study4a$awes3 +data_study4a$awes4 +data_study4a$awes5 +
                          data_study4a$awes6 +data_study4a$awes7 +data_study4a$awes8 +data_study4a$awes9 +data_study4a$awes10+
                          data_study4a$awes11+data_study4a$awes12+data_study4a$awes13+data_study4a$awes14+data_study4a$awes15+
                          data_study4a$awes16+data_study4a$awes17+data_study4a$awes18+data_study4a$awes19+data_study4a$awes20+
                          data_study4a$awes21+data_study4a$awes22+data_study4a$awes23+data_study4a$awes24+data_study4a$awes25+
                          data_study4a$awes26+data_study4a$awes27+data_study4a$awes28+data_study4a$awes29+data_study4a$awes30)/30

data_study4a$TimeD_score<-(data_study4a$awes1 +data_study4a$awes2 +data_study4a$awes3 +data_study4a$awes4 +data_study4a$awes5)/5
data_study4a$SelfD_score<-(data_study4a$awes6 +data_study4a$awes7 +data_study4a$awes8 +data_study4a$awes9 +data_study4a$awes10)/5
data_study4a$Conne_score<-(data_study4a$awes11+data_study4a$awes12+data_study4a$awes13+data_study4a$awes14+data_study4a$awes15)/5
data_study4a$Vastn_score<-(data_study4a$awes16+data_study4a$awes17+data_study4a$awes18+data_study4a$awes19+data_study4a$awes20)/5
data_study4a$PhysS_score<-(data_study4a$awes21+data_study4a$awes22+data_study4a$awes23+data_study4a$awes24+data_study4a$awes25)/5
data_study4a$NeedA_score<-(data_study4a$awes26+data_study4a$awes27+data_study4a$awes28+data_study4a$awes29+data_study4a$awes30)/5


#coding items in Dispositional Awe Subscale of the Dispositional Positive Emotions Scale(Dawe)

#check the internal reliability of Dispositional Awe Subscale in Study 2a_writing
Alpha(data_study2a, "dawe", 1:6)  #Cronbach’s α = 0.722
#calculate the mean scores for Dispositional Awe Subscale in Study 2a_writing
data_study2a$Dawe_score<-(data_study2a$dawe1+data_study2a$dawe2+data_study2a$dawe3+
                          data_study2a$dawe4+data_study2a$dawe5+data_study2a$dawe6)/6

#check the internal reliability of Dispositional Awe Subscale in Study 3a_movie
Alpha(data_study3a, "dawe", 1:6)  #Cronbach’s α = 0.800
#calculate the mean scores for Dispositional Awe Subscale in Study 3a_movie
data_study3a$Dawe_score<-(data_study3a$dawe1+data_study3a$dawe2+data_study3a$dawe3+
                          data_study3a$dawe4+data_study3a$dawe5+data_study3a$dawe6)/6

#check the internal reliability of Dispositional Awe Subscale in Study 4a_music
Alpha(data_study4a, "dawe", 1:6)  #Cronbach’s α = 0.734
#calculate the mean scores for Dispositional Awe Subscale in Study 4a_music
data_study4a$Dawe_score<-(data_study4a$dawe1+data_study4a$dawe2+data_study4a$dawe3+
                          data_study4a$dawe4+data_study4a$dawe5+data_study4a$dawe6)/6



#coding items in big five personality

#calculate the mean scores for five personality traits in Study 2a_writing
data_study2a$Extra_score<-(6-data_study2a$bf1+data_study2a$bf6 )/2
data_study2a$Agree_score<-(data_study2a$bf2+6-data_study2a$bf7 )/2
data_study2a$Consc_score<-(6-data_study2a$bf3+data_study2a$bf8 )/2
data_study2a$Neuro_socre<-(6-data_study2a$bf4+data_study2a$bf9 )/2
data_study2a$Openn_score<-(6-data_study2a$bf5+data_study2a$bf10)/2

#calculate the mean scores for five personality traits in Study 3a_movie
data_study3a$Extra_score<-(6-data_study3a$bf1+data_study3a$bf6 )/2
data_study3a$Agree_score<-(data_study3a$bf2+6-data_study3a$bf7 )/2
data_study3a$Consc_score<-(6-data_study3a$bf3+data_study3a$bf8 )/2
data_study3a$Neuro_socre<-(6-data_study3a$bf4+data_study3a$bf9 )/2
data_study3a$Openn_score<-(6-data_study3a$bf5+data_study3a$bf10)/2

#calculate the mean scores for five personality traits in Study 4a_music

data_study4a$Extra_score<-(6-data_study4a$bf1+data_study4a$bf6 )/2
data_study4a$Agree_score<-(data_study4a$bf2+6-data_study4a$bf7 )/2
data_study4a$Consc_score<-(6-data_study4a$bf3+data_study4a$bf8 )/2
data_study4a$Neuro_socre<-(6-data_study4a$bf4+data_study4a$bf9 )/2
data_study4a$Openn_score<-(6-data_study4a$bf5+data_study4a$bf10)/2

##############################################################
#######################data analysis##########################
##############################################################

#1.1 Self‑reported manipulation check in study 2a

set.seed(123)

anova_AWES_score_study2a<-ggbetweenstats(
  data  = data_study2a,
  x     = random,
  y     = AWES_score,
  k     = 3,
  type  = "parametric",
  plot.type = "boxviolin",
  p.adjust.method = "bonferroni",
  pairwise.display= "all",
  effsize.type= "eta",
  bf.message = FALSE,
  nboot = 1000,
  title = "Distribution of Chinese version of AWE-S across groups in Study 2a"
)

anova_TimeD_score_study2a<-ggbetweenstats(
  data  = data_study2a,
  x     = random,
  y     = TimeD_score,
  k     = 3,
  type  = "parametric", 
  plot.type = "boxviolin",
  p.adjust.method = "bonferroni",
  pairwise.display= "all",
  effsize.type= "eta",
  bf.message = FALSE,
  nboot = 1000,
  title = "Distribution of Chinese version of TimeD across groups"
)

anova_SelfD_score_study2a<-ggbetweenstats(
  data  = data_study2a,
  x     = random,
  y     = SelfD_score,
  k     = 3,
  plot.type = "boxviolin",
  type  = "parametric",
  p.adjust.method = "bonferroni",
  pairwise.display= "all",
  effsize.type= "eta",
  bf.message = FALSE,
  nboot = 1000,
  title = "Distribution of Chinese version of SelfD across groups"
)

anova_Conne_score_study2a<-ggbetweenstats(
  data  = data_study2a,
  x     = random,
  y     = Conne_score,
  k     = 3,
  plot.type = "boxviolin",
  type  = "parametric",
  p.adjust.method = "bonferroni",
  pairwise.display= "all",
  effsize.type= "eta",
  bf.message = FALSE,
  nboot = 1000,
  title = "Distribution of Chinese version of Conne across groups"
)

anova_Vastn_score_study2a<-ggbetweenstats(
  data  = data_study2a,
  x     = random,
  y     = Vastn_score,
  k     = 3,
  plot.type = "boxviolin",
  type  = "parametric",
  p.adjust.method = "bonferroni",
  pairwise.display= "all",
  effsize.type= "eta",
  bf.message = FALSE,
  nboot = 1000,
  title = "Distribution of Chinese version of Vastn across groups"
)

anova_PhysS_score_study2a<-ggbetweenstats(
  data  = data_study2a,
  x     = random,
  y     = PhysS_score,
  k     = 3,
  type  = "parametric",
  plot.type = "boxviolin",
  p.adjust.method = "bonferroni",
  pairwise.display= "all",
  effsize.type= "eta",
  bf.message = FALSE,
  nboot = 1000,
  title = "Distribution of Chinese version of PhysS across groups"
)

anova_NeedA_score_study2a<-ggbetweenstats(
  data  = data_study2a,
  x     = random,
  y     = NeedA_score,
  k     = 3,
  type  = "parametric",
  plot.type = "boxviolin",
  p.adjust.method = "bonferroni",
  pairwise.display= "all",
  effsize.type= "eta",
  bf.message = FALSE,
  nboot = 1000,
  title = "Distribution of Chinese version of NeedA across groups"
)

# combining the individual plots into a single plot
pdf("figure_study2a_check_subscale.pdf", height=10, width=16) 
combine_plots(
  list(anova_TimeD_score_study2a,anova_SelfD_score_study2a,
       anova_Conne_score_study2a,anova_Vastn_score_study2a,
       anova_PhysS_score_study2a,anova_NeedA_score_study2a),
  plotgrid.args = list(nrow = 2),
  annotation.args = list(
    title = "",
    caption = ""
  )
)
dev.off() 

#1.2 Self‑reported manipulation check in study 3

set.seed(123)

anova_AWES_score_study3a<-ggbetweenstats(
  data  = data_study3a,
  x     = random,
  y     = AWES_score,
  k     = 3,
  type  = "parametric",
  plot.type = "boxviolin",
  p.adjust.method = "bonferroni",
  pairwise.display= "all",
  effsize.type= "eta",
  bf.message = FALSE,
  nboot = 1000,
  title = "Distribution of Chinese version of AWE-S across groups in Study 3a"
)

anova_TimeD_score_study3a<-ggbetweenstats(
  data  = data_study3a,
  x     = random,
  y     = TimeD_score,
  k     = 3,
  type  = "parametric", 
  plot.type = "boxviolin",
  p.adjust.method = "bonferroni",
  pairwise.display= "all",
  effsize.type= "eta",
  bf.message = FALSE,
  nboot = 1000,
  title = "Distribution of Chinese version of TimeD across groups"
)

anova_SelfD_score_study3a<-ggbetweenstats(
  data  = data_study3a,
  x     = random,
  y     = SelfD_score,
  k     = 3,
  plot.type = "boxviolin",
  type  = "parametric",
  p.adjust.method = "bonferroni",
  pairwise.display= "all",
  effsize.type= "eta",
  bf.message = FALSE,
  nboot = 1000,
  title = "Distribution of Chinese version of SelfD across groups"
)

anova_Conne_score_study3a<-ggbetweenstats(
  data  = data_study3a,
  x     = random,
  y     = Conne_score,
  k     = 3,
  plot.type = "boxviolin",
  type  = "parametric",
  p.adjust.method = "bonferroni",
  pairwise.display= "all",
  effsize.type= "eta",
  bf.message = FALSE,
  nboot = 1000,
  title = "Distribution of Chinese version of Conne across groups"
)

anova_Vastn_score_study3a<-ggbetweenstats(
  data  = data_study3a,
  x     = random,
  y     = Vastn_score,
  k     = 3,
  plot.type = "boxviolin",
  type  = "parametric",
  p.adjust.method = "bonferroni",
  pairwise.display= "all",
  effsize.type= "eta",
  bf.message = FALSE,
  nboot = 1000,
  title = "Distribution of Chinese version of Vastn across groups"
)

anova_PhysS_score_study3a<-ggbetweenstats(
  data  = data_study3a,
  x     = random,
  y     = PhysS_score,
  k     = 3,
  type  = "parametric",
  plot.type = "boxviolin",
  p.adjust.method = "bonferroni",
  pairwise.display= "all",
  effsize.type= "eta",
  bf.message = FALSE,
  nboot = 1000,
  title = "Distribution of Chinese version of PhysS across groups"
)

anova_NeedA_score_study3a<-ggbetweenstats(
  data  = data_study3a,
  x     = random,
  y     = NeedA_score,
  k     = 3,
  type  = "parametric",
  plot.type = "boxviolin",
  p.adjust.method = "bonferroni",
  pairwise.display= "all",
  effsize.type= "eta",
  bf.message = FALSE,
  nboot = 1000,
  title = "Distribution of Chinese version of NeedA across groups"
)

# combining the individual plots into a single plot
pdf("figure_study3a_check_subscale.pdf", height=10, width=16) 
combine_plots(
  list(anova_TimeD_score_study3a,anova_SelfD_score_study3a,
       anova_Conne_score_study3a,anova_Vastn_score_study3a,
       anova_PhysS_score_study3a,anova_NeedA_score_study3a),
  plotgrid.args = list(nrow = 2),
  annotation.args = list(
    title = "",
    caption = ""
  )
)
dev.off() 

#1.3 Self‑reported manipulation check in study 4


set.seed(123)

anova_AWES_score_study4a<-ggbetweenstats(
  data  = data_study4a,
  x     = random,
  y     = AWES_score,
  k     = 3,
  type  = "parametric",
  plot.type = "boxviolin",
  p.adjust.method = "bonferroni",
  pairwise.display= "all",
  effsize.type= "eta",
  bf.message = FALSE,
  nboot = 1000,
  title = "Distribution of Chinese version of AWE-S across groups in Study 4a"
)

anova_TimeD_score_study4a<-ggbetweenstats(
  data  = data_study4a,
  x     = random,
  y     = TimeD_score,
  k     = 3,
  type  = "parametric", 
  plot.type = "boxviolin",
  p.adjust.method = "bonferroni",
  pairwise.display= "all",
  effsize.type= "eta",
  bf.message = FALSE,
  nboot = 1000,
  title = "Distribution of Chinese version of TimeD across groups"
)

anova_SelfD_score_study4a<-ggbetweenstats(
  data  = data_study4a,
  x     = random,
  y     = SelfD_score,
  k     = 3,
  plot.type = "boxviolin",
  type  = "parametric",
  p.adjust.method = "bonferroni",
  pairwise.display= "all",
  effsize.type= "eta",
  bf.message = FALSE,
  nboot = 1000,
  title = "Distribution of Chinese version of SelfD across groups"
)

anova_Conne_score_study4a<-ggbetweenstats(
  data  = data_study4a,
  x     = random,
  y     = Conne_score,
  k     = 3,
  plot.type = "boxviolin",
  type  = "parametric",
  p.adjust.method = "bonferroni",
  pairwise.display= "all",
  effsize.type= "eta",
  bf.message = FALSE,
  nboot = 1000,
  title = "Distribution of Chinese version of Conne across groups"
)

anova_Vastn_score_study4a<-ggbetweenstats(
  data  = data_study4a,
  x     = random,
  y     = Vastn_score,
  k     = 3,
  plot.type = "boxviolin",
  type  = "parametric",
  p.adjust.method = "bonferroni",
  pairwise.display= "all",
  effsize.type= "eta",
  bf.message = FALSE,
  nboot = 1000,
  title = "Distribution of Chinese version of Vastn across groups"
)

anova_PhysS_score_study4a<-ggbetweenstats(
  data  = data_study4a,
  x     = random,
  y     = PhysS_score,
  k     = 3,
  type  = "parametric",
  plot.type = "boxviolin",
  p.adjust.method = "bonferroni",
  pairwise.display= "all",
  effsize.type= "eta",
  bf.message = FALSE,
  nboot = 1000,
  title = "Distribution of Chinese version of PhysS across groups"
)

anova_NeedA_score_study4a<-ggbetweenstats(
  data  = data_study4a,
  x     = random,
  y     = NeedA_score,
  k     = 3,
  type  = "parametric",
  plot.type = "boxviolin",
  p.adjust.method = "bonferroni",
  pairwise.display= "all",
  effsize.type= "eta",
  bf.message = FALSE,
  nboot = 1000,
  title = "Distribution of Chinese version of NeedA across groups"
)

# combining the individual plots into a single plot
pdf("figure_study4a_check_subscale.pdf", height=10, width=16) 
combine_plots(
  list(anova_TimeD_score_study4a,anova_SelfD_score_study4a,
       anova_Conne_score_study4a,anova_Vastn_score_study4a,
       anova_PhysS_score_study4a,anova_NeedA_score_study4a),
  plotgrid.args = list(nrow = 2),
  annotation.args = list(
    title = "",
    caption = ""
  )
)
dev.off() 


pdf("figure_check_totalscale.pdf", height=5, width=16) 
combine_plots(
  list(anova_AWES_score_study2a,anova_AWES_score_study3a,anova_AWES_score_study4a),
  plotgrid.args = list(nrow = 1),
  annotation.args = list(
    title = "",
    caption = ""
  )
)
dev.off() 

#2.1 differences of dawe and big five personality among different groups in study 2a

set.seed(123)

anova_Dawe_score_study2a<-ggbetweenstats(
  data  = data_study2a,
  x     = random,
  y     = Dawe_score,
  k     = 3,
  xlab  = "",
  ylab  = "Dispositional Awe",
  type  = "parametric",
  plot.type = "boxviolin",
  p.adjust.method = "bonferroni",
  pairwise.display= "all",
  effsize.type= "eta",
  bf.message = FALSE,
  nboot = 1000,
  title = "(a)"
)


anova_Extra_score_study2a<-ggbetweenstats(
  data  = data_study2a,
  x     = random,
  y     = Extra_score,
  k     = 3,
  xlab  = "",
  ylab  = "Extraversion",
  type  = "parametric",
  plot.type = "boxviolin",
  p.adjust.method = "bonferroni",
  pairwise.display= "all",
  effsize.type= "eta",
  bf.message = FALSE,
  nboot = 1000,
  title = "(b)"
)

anova_Agree_score_study2a<-ggbetweenstats(
  data  = data_study2a,
  x     = random,
  y     = Agree_score,
  k     = 3,
  xlab  = "",
  ylab  = "Agreeableness",
  type  = "parametric",
  plot.type = "boxviolin",
  p.adjust.method = "bonferroni",
  pairwise.display= "all",
  effsize.type= "eta",
  bf.message = FALSE,
  nboot = 1000,
  title = "(c)"
)

anova_Consc_score_study2a<-ggbetweenstats(
  data  = data_study2a,
  x     = random,
  y     = Consc_score,
  k     = 3,
  xlab  = "",
  ylab  = "Conscientiousness",
  type  = "parametric",
  plot.type = "boxviolin",
  p.adjust.method = "bonferroni",
  pairwise.display= "all",
  effsize.type= "eta",
  bf.message = FALSE,
  nboot = 1000,
  title = "(d)"
)

anova_Neuro_socre_study2a<-ggbetweenstats(
  data  = data_study2a,
  x     = random,
  y     = Neuro_socre,
  k     = 3,
  xlab  = "",
  ylab  = "Neuroticism",
  type  = "parametric",
  plot.type = "boxviolin",
  p.adjust.method = "bonferroni",
  pairwise.display= "all",
  effsize.type= "eta",
  bf.message = FALSE,
  nboot = 1000,
  title = "(e)"
)

anova_Openn_score_study2a<-ggbetweenstats(
  data  = data_study2a,
  x     = random,
  y     = Openn_score,
  k     = 3,
  xlab  = "",
  ylab  = "Openness",
  type  = "parametric",
  plot.type = "boxviolin",
  p.adjust.method = "bonferroni",
  pairwise.display= "all",
  effsize.type= "eta",
  bf.message = FALSE,
  nboot = 1000,
  title = "(f)"
)



# combining the individual plots into a single plot
pdf("figure_study2a_traits.pdf", height=10, width=16) 
combine_plots(
  list(anova_Dawe_score_study2a , anova_Extra_score_study2a, anova_Agree_score_study2a,
       anova_Consc_score_study2a, anova_Neuro_socre_study2a, anova_Openn_score_study2a),
  plotgrid.args = list(nrow = 2),
  annotation.args = list(
    title = "",
    caption = ""
  )
)
dev.off()


#2.2 differences of dawe and big five personality among different groups in study 3a


set.seed(123)

anova_Dawe_score_study3a<-ggbetweenstats(
  data  = data_study3a,
  x     = random,
  y     = Dawe_score,
  k     = 3,
  xlab  = "",
  ylab  = "Dispositional Awe",
  type  = "parametric",
  plot.type = "boxviolin",
  p.adjust.method = "bonferroni",
  pairwise.display= "all",
  effsize.type= "eta",
  bf.message = FALSE,
  nboot = 1000,
  title = "(a)"
)


anova_Extra_score_study3a<-ggbetweenstats(
  data  = data_study3a,
  x     = random,
  y     = Extra_score,
  k     = 3,
  xlab  = "",
  ylab  = "Extraversion",
  type  = "parametric",
  plot.type = "boxviolin",
  p.adjust.method = "bonferroni",
  pairwise.display= "all",
  effsize.type= "eta",
  bf.message = FALSE,
  nboot = 1000,
  title = "(b)"
)

anova_Agree_score_study3a<-ggbetweenstats(
  data  = data_study3a,
  x     = random,
  y     = Agree_score,
  k     = 3,
  xlab  = "",
  ylab  = "Agreeableness",
  type  = "parametric",
  plot.type = "boxviolin",
  p.adjust.method = "bonferroni",
  pairwise.display= "all",
  effsize.type= "eta",
  bf.message = FALSE,
  nboot = 1000,
  title = "(c)"
)

anova_Consc_score_study3a<-ggbetweenstats(
  data  = data_study3a,
  x     = random,
  y     = Consc_score,
  k     = 3,
  xlab  = "",
  ylab  = "Conscientiousness",
  type  = "parametric",
  plot.type = "boxviolin",
  p.adjust.method = "bonferroni",
  pairwise.display= "all",
  effsize.type= "eta",
  bf.message = FALSE,
  nboot = 1000,
  title = "(d)"
)

anova_Neuro_socre_study3a<-ggbetweenstats(
  data  = data_study3a,
  x     = random,
  y     = Neuro_socre,
  k     = 3,
  xlab  = "",
  ylab  = "Neuroticism",
  type  = "parametric",
  plot.type = "boxviolin",
  p.adjust.method = "bonferroni",
  pairwise.display= "all",
  effsize.type= "eta",
  bf.message = FALSE,
  nboot = 1000,
  title = "(e)"
)

anova_Openn_score_study3a<-ggbetweenstats(
  data  = data_study3a,
  x     = random,
  y     = Openn_score,
  k     = 3,
  xlab  = "",
  ylab  = "Openness",
  type  = "parametric",
  plot.type = "boxviolin",
  p.adjust.method = "bonferroni",
  pairwise.display= "all",
  effsize.type= "eta",
  bf.message = FALSE,
  nboot = 1000,
  title = "(f)"
)

# combining the individual plots into a single plot
pdf("figure_study3a_traits.pdf", height=10, width=16) 
combine_plots(
  list(anova_Dawe_score_study3a , anova_Extra_score_study3a, anova_Agree_score_study3a,
       anova_Consc_score_study3a, anova_Neuro_socre_study3a, anova_Openn_score_study3a),
  plotgrid.args = list(nrow = 2),
  annotation.args = list(
    title = "",
    caption = ""
  )
)
dev.off()
#2.3 differences of dawe and big five personality among different groups in study 4a


set.seed(123)

anova_Dawe_score_study4a<-ggbetweenstats(
  data  = data_study4a,
  x     = random,
  y     = Dawe_score,
  k     = 3,
  xlab  = "",
  ylab  = "Dispositional Awe",
  type  = "parametric",
  plot.type = "boxviolin",
  p.adjust.method = "bonferroni",
  pairwise.display= "all",
  effsize.type= "eta",
  bf.message = FALSE,
  nboot = 1000,
  title = "(a)"
)


anova_Extra_score_study4a<-ggbetweenstats(
  data  = data_study4a,
  x     = random,
  y     = Extra_score,
  k     = 3,
  xlab  = "",
  ylab  = "Extraversion",
  type  = "parametric",
  plot.type = "boxviolin",
  p.adjust.method = "bonferroni",
  pairwise.display= "all",
  effsize.type= "eta",
  bf.message = FALSE,
  nboot = 1000,
  title = "(b)"
)

anova_Agree_score_study4a<-ggbetweenstats(
  data  = data_study4a,
  x     = random,
  y     = Agree_score,
  k     = 3,
  xlab  = "",
  ylab  = "Agreeableness",
  type  = "parametric",
  plot.type = "boxviolin",
  p.adjust.method = "bonferroni",
  pairwise.display= "all",
  effsize.type= "eta",
  bf.message = FALSE,
  nboot = 1000,
  title = "(c)"
)

anova_Consc_score_study4a<-ggbetweenstats(
  data  = data_study4a,
  x     = random,
  y     = Consc_score,
  k     = 3,
  xlab  = "",
  ylab  = "Conscientiousness",
  type  = "parametric",
  plot.type = "boxviolin",
  p.adjust.method = "bonferroni",
  pairwise.display= "all",
  effsize.type= "eta",
  bf.message = FALSE,
  nboot = 1000,
  title = "(d)"
)

anova_Neuro_socre_study4a<-ggbetweenstats(
  data  = data_study4a,
  x     = random,
  y     = Neuro_socre,
  k     = 3,
  xlab  = "",
  ylab  = "Neuroticism",
  type  = "parametric",
  plot.type = "boxviolin",
  p.adjust.method = "bonferroni",
  pairwise.display= "all",
  effsize.type= "eta",
  bf.message = FALSE,
  nboot = 1000,
  title = "(e)"
)

anova_Openn_score_study4a<-ggbetweenstats(
  data  = data_study4a,
  x     = random,
  y     = Openn_score,
  k     = 3,
  xlab  = "",
  ylab  = "Openness",
  type  = "parametric",
  plot.type = "boxviolin",
  p.adjust.method = "bonferroni",
  pairwise.display= "all",
  effsize.type= "eta",
  bf.message = FALSE,
  nboot = 1000,
  title = "(f)"
)

# combining the individual plots into a single plot
pdf("figure_study4a_traits.pdf", height=10, width=16) 
combine_plots(
  list(anova_Dawe_score_study4a , anova_Extra_score_study4a, anova_Agree_score_study4a,
       anova_Consc_score_study4a, anova_Neuro_socre_study4a, anova_Openn_score_study4a),
  plotgrid.args = list(nrow = 2),
  annotation.args = list(
    title = "",
    caption = ""
  )
)
dev.off()

