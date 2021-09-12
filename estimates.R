library(readr)
library(fitdistrplus)
library(actuar)
library(evir)
library(recipes)
library(tidyverse)
library(janitor)
library(readxl)
library(gridExtra)
library(skimr)
library(DataExplorer)
library(dplyr)

setwd("D:/THESIS!/###Baseline Noise Survey###/Official Data/##R ANALYSIS##")

data <- read_excel("final_data.xlsx")%>% clean_names()
data <- data[,colSums(is.na(data))<nrow(data)]%>% na.exclude()

#############################
# start a loop to add new estimate columns for every column in main data
names <- read_csv("distribution_names.csv")%>% data.frame()

for (x in names(data)) {
br <- try( fitdist(as.numeric(na.exclude(data[[x]])), "burr", start = list(shape1 = 0.3, shape2 = 1, rate = 3),lower=c(0,0)) )
#if("try-error" %in% class(br)) {br<- fitdist(as.numeric(na.exclude(data[[x]])), "burr", start = list(shape1 = 0.4, shape2 = 1, rate = 3), lower=c(0,0))}
gm<- fitdist(as.numeric(na.exclude(data[[x]])), "gamma")
ll<- fitdist(as.numeric(na.exclude(data[[x]])), "llogis", start = list(shape = 1, scale = 500))
ln<- fitdist(as.numeric(na.exclude(data[[x]])), "lnorm")

names<- data.frame(names,rbind(
  cbind(br$estimate),
  cbind(gm$estimate),
  cbind(ll$estimate),
  cbind(ln$estimate)
))%>% 
  mutate_if(is.numeric, round, digits = 3)
}
# Rename the column names
names(names)[3:ncol(names)]<- colnames(data)

#write.csv(names, "dist_estimates.csv")
######################################################################################
round_df <- function(df, digits=4) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  (df)
}

stats<- data.frame(rbind("Burr", "Gamma", "Loglogistic", "Lognormal"))
for (x in names(data)) {
  br <- try( fitdist(as.numeric(na.exclude(data[[x]])), "burr", start = list(shape1 = 0.3, shape2 = 1, rate = 3),lower=c(0,0)) )
  gm<- fitdist(as.numeric(na.exclude(data[[x]])), "gamma")
  ll<- fitdist(as.numeric(na.exclude(data[[x]])), "llogis", start = list(shape = 1, scale = 500))
  ln<- fitdist(as.numeric(na.exclude(data[[x]])), "lnorm")
  
  gof<-gofstat(list(br, gm, ll, ln), 
               fitnames = c("Burr", "Gamma", "Loglogistic", "Lognormal"))
  
  gof_aic_bic<-data.frame(cbind(gof$aic, rank(gof$aic), gof$bic, rank(gof$bic)))
  
  colnames(stats)<- NULL
  stats<- data.frame(stats, rbind(gof_aic_bic))
}

names(stats)[2:ncol(stats)]<- rep(colnames(data), each=4)
#view(stats)
#write.csv(stats, "aic_bic.csv")
##################################

colnames(sb_13_ks_cvm_ad)<-c("Kolmogorov-Smirnov Statistic","ranking", "Cramer-von Mises Statistic","ranking","Anderson-Darling Statistic","ranking")

stats_ks<- data.frame(rbind("Burr", "Gamma", "Loglogistic", "Lognormal"))
for (x in names(data)) {
  br <- try( fitdist(as.numeric(na.exclude(data[[x]])), "burr", start = list(shape1 = 0.3, shape2 = 1, rate = 3),lower=c(0,0)) )
  gm<- fitdist(as.numeric(na.exclude(data[[x]])), "gamma")
  ll<- fitdist(as.numeric(na.exclude(data[[x]])), "llogis", start = list(shape = 1, scale = 500))
  ln<- fitdist(as.numeric(na.exclude(data[[x]])), "lnorm")
  
  gof<-gofstat(list(br, gm, ll, ln), 
               fitnames = c("Burr", "Gamma", "Loglogistic", "Lognormal"))
  
  gof_ks<- data.frame(cbind(gof$ks, rank(gof$ks), gof$cvm, rank(gof$cvm), gof$ad, rank(gof$ad)))
  
  colnames(stats_ks)<- NULL
  stats_ks<- data.frame(stats_ks, rbind(gof_ks))
}

names(stats_ks)[2:ncol(stats_ks)]<- rep(colnames(data), each=6)
#view(stats_ks)
#write.csv(stats_ks, "ks_cvm_ad.csv")
##########################################

#GRAPHS
df = data.frame(data$fg16)

ggplot(data.frame(data$fg16), aes(sample = data$fg16))+
  stat_qq(distribution = qlnorm, dparams = as.list(lognormal_fg_16$estimate),size=3, alpha=0.75, aes(color = "darkblue"))+
  stat_qq(distribution = qllogis,dparams = as.list(loglogis_fg_16$estimate), size=1.5, alpha=0.3, aes(color = "orange"))+
  geom_abline(slope = 1, color = "black",lwd=0.75)+
  labs(title = "Q-Q Plot", x = "Theoritical quantiles", y = "Empirical quantiles")+
  xlim(0,1200)+
  scale_color_identity(name= "Distributions",
                       labels= c("Lognormal", "Loglogistic"),
                       guide= "legend")+
  #scale_shape_manual(values = c(5, 16)) +
  theme_tq()+
  guides(alpha= FALSE)+
  theme(legend.position="top")
####

#######################
dfs_ks = np.split(stats_ks[-1], np.arange(6, len(df.columns), 6), axis=1) ## python code for splitting vertically


##################
cols_aicbic<- rep(c("aic","rank1", "bic", "rank2"), times=200)
cols_aicbic<- rep(c("Burr","Gamma", "Loglogistic", "Lognormal"), times=200)
cols_aicbic<- rep(c("ks","rank1", "cvm", "rank2", "ad", "rank3"), times=200)
write.csv(cols_aicbic, "colnames.csv")











