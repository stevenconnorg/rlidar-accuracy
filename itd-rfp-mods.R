###--------------------------------------------------------------------------------------------------------###
#----------------------------------------------------------------------------------------------------------###
# GONZALEZ, Steven C.  (2017)                                                                              ###
# M.S. Student at Texas State University                                                                   ###
# Department of Geography                                                                                  ###
# San Marcos, Texas                                                                                        ###
# stevenconnorg@gmail.com                                                                                  ###
#----------------------------------------------------------------------------------------------------------###
# assessing effects of lidar pulse density on the accuracy and precision 
# on individual tree detection using the R package rLiDAR

### Accuracy statistics and linear-mixed models
rm(list = ls())
setwd("E:/01_Lidar_Steven_Manuscript/04_Assessment/") # set working directory as root folder
dir=getwd()                                           # assign directory object
dir.data<-paste0(dir,"/data/")
dir.figs<-paste0(dir,"/figs/")
dir.out<-paste0(dir,"/output/")
load(paste0(dir,"/output/Full_Accuracy_Assessment-df-prep.RData"))
source(paste0(dir,("/R/functions.R")))

# 
requiredPackages = c('MASS'
                     ,'stats'
                     ,'sjPlot'
                     ,'sjmisc'
                     ,'sjlabelled'
                     ,'dplyr'
                     ,'ggplot2'
                     ,'multcompView'
                     ,'car'
                     ,'lattice'
                     ,'lsmeans'
                     ,'effects'
                     ,'ICC'
                     ,'stats'
                     ,'plyr'
                     ,'tidyr'
                     ,'tibble'
                     ,'phia'
                     ,'psych'
                     ,'nlme'
)

Install_And_Load(requiredPackages)
View(df)
# Descriptive tables # 

#  mean f, p, r by pulse density, regardless of sws, fws
means_by_pulse<-ddply(df,~pulse,summarise,mean_f=mean(f),mean_p=mean(p),mean_r=mean(r))
head(means_by_pulse)

# tp, fn, fp counts and r, f, p means by pulse
pulse_counts<- df %>% group_by(pulse) %>% 
  dplyr::summarize(Total = mean(total),FP = mean(fp),FN = mean(fn),TP= mean(tp),Recall = mean(r), Precision = mean(p),Fscore = mean(f)) 
df
# tp, fn, fp counts and r, f, p means by fws
fws_counts<- df %>% group_by(fws) %>% 
  dplyr::summarize(Total = mean(total),FP = mean(fp),FN = mean(fn),TP= mean(tp),Recall = mean(r), Precision = mean(p),Fscore = mean(f)) 

# tp, fn, fp counts and r, f, p means by sws
ws_counts<- df %>% group_by(ws) %>% 
  dplyr::summarize(Total = mean(total),FP = mean(fp),FN = mean(fn),TP= mean(tp),Recall = mean(r), Precision = mean(p),Fscore = mean(f)) 
?step


plot_counts<- df %>% group_by(plots) %>% 
dplyr::summarize(Total = mean(total),FP = mean(fp),FN = mean(fn),TP= mean(tp),Recall = mean(r),Precision = mean(p),Fscore = mean(f)) 
View(plot_counts)

# mean f-score by pulse, fws, ws combo
spread_f<-spread(df,key=pulse_id,value=f)      # spread f-score values by pulse into columns
f.df1<- spread_f %>% group_by(fws,ws) %>%      # fws/ws/pulse grouping
  summarise_all(funs(mean(., na.rm = TRUE)))   # get mean f
f.df<-f.df1[,c(1,2,15:28)]                     # trim down df for table
f.df$FWS_x_SWS<-paste0(f.df$fws," | ",f.df$ws) # create fws x sws factor column for compact table
f.df<-as.data.frame(f.df)                      # make data frame
row.names(f.df)<-f.df$FWS_x_SWS                # set fws x sws factor as rowname to get row means
f.df<-f.df[,3:16]                              # trim down for table
f.df$mean<-rowMeans(f.df)                      # get mean by pulse 
f.df<-rownames_to_column(f.df,var="FWS_SWS")   # extract rowname to column for exporting table
f.df


# mean recall by pulse, fws, ws combo
spread_r<-spread(df,key=pulse_id,value=r) 
r.df1<- spread_r %>% group_by(fws,ws) %>%
  summarise_all(funs(mean(., na.rm = TRUE)))

r.df<-r.df1[,c(1,2,15:28)]
r.df<-r.df1[,c(1,2,15:28)]
r.df$FWS_x_SWS<-paste0(r.df$fws,"|",r.df$ws)
r.df$FWS_x_SWS
r.df<-as.data.frame(r.df)
row.names(r.df)<-r.df$FWS_x_SWS
r.df<-r.df[,3:16]
r.df$mean<-rowMeans(r.df)
r.df<-rownames_to_column(r.df,var="FWS_SWS")
r.df

# mean precision by pulse, fws, ws combo
spread_p<-spread(df,key=pulse_id,value=p)
p.df1<- spread_p %>% group_by(fws,ws) %>% summarise_all(funs(mean(., na.rm = TRUE)))
p.df<-p.df1[,c(1,2,15:28)]
p.df<-p.df1[,c(1,2,15:28)]
p.df$FWS_x_SWS<-paste0(p.df$fws," | ",p.df$ws)
p.df$FWS_x_SWS
p.df<-as.data.frame(p.df)
row.names(p.df)<-p.df$FWS_x_SWS
p.df<-p.df[,3:16]
p.df$mean<-rowMeans(p.df)
p.df<-rownames_to_column(p.df,var="FWS_SWS")
p.df

## create data frame for ggplot-ing
## used describeBy to get mean, sd, mean, etc. of f by fws, ws, and pulse
dff = describeBy(df$f,list(df$fws,df$ws,df$pulse), mat=TRUE,digits=2)
dfr = describeBy(df$r,list(df$fws,df$ws,df$pulse), mat=TRUE,digits=2)
dfp = describeBy(df$p,list(df$fws,df$ws,df$pulse), mat=TRUE,digits=2)

dffplots = describeBy(df$f,list(df$fws,df$ws,df$pulse,df$plots), mat=TRUE,digits=2)
dfrplots = describeBy(df$r,list(df$fws,df$ws,df$pulse,df$plots), mat=TRUE,digits=2)
dfpplots = describeBy(df$p,list(df$fws,df$ws,df$pulse,df$plots), mat=TRUE,digits=2)

# rename columns to more meaningful name
names(dff)[names(dff) == 'group1'] = 'FWS'
names(dff)[names(dff) == 'group2'] = 'SWS'
names(dff)[names(dff) == 'group3'] = 'Pulse'

names(dfr)[names(dfr) == 'group1'] = 'FWS'
names(dfr)[names(dfr) == 'group2'] = 'SWS'
names(dfr)[names(dfr) == 'group3'] = 'Pulse'

names(dfp)[names(dfp) == 'group1'] = 'FWS'
names(dfp)[names(dfp) == 'group2'] = 'SWS'
names(dfp)[names(dfp) == 'group3'] = 'Pulse'

names(dffplots)[names(dffplots) == 'group1'] = 'FWS'
names(dffplots)[names(dffplots) == 'group2'] = 'SWS'
names(dffplots)[names(dffplots) == 'group3'] = 'Pulse'
names(dffplots)[names(dffplots) == 'group4'] = 'Plots'

dff_sd_fws_sws<- dff %>% group_by(SWS,FWS) %>% dplyr::summarize(mean(sd))
dfr_sd_fws_sws<- dfr %>% group_by(SWS,FWS) %>% dplyr::summarize(mean(sd))
dfp_sd_fws_sws<- dfp %>% group_by(SWS,FWS) %>% dplyr::summarize(mean(sd))

dff_sd_pulse<- dff %>% group_by(Pulse) %>% dplyr::summarize(mean(sd))
dfr_sd_pulse<- dfr %>% group_by(Pulse) %>% dplyr::summarize(mean(sd))
dfp_sd_pulse<- dfp %>% group_by(Pulse) %>% dplyr::summarize(mean(sd))


write.csv(means_by_pulse,file=paste0(dir.out,"means_by_pulse.csv"))
write.csv(pulse_counts,file=paste0(dir.out,"pulse_counts.csv"))
write.csv(fws_counts,file=paste0(dir.out,"fws_counts.csv"))
write.csv(ws_counts,file=paste0(dir.out,"ws_counts.csv"))
write.csv(f.df,file=paste0(dir.out,"f_means.csv"))
write.csv(r.df,file=paste0(dir.out,"r_means.csv"))
write.csv(p.df,file=paste0(dir.out,"p_means.csv"))


write.csv(dff_sd_fws_sws,file=paste0(dir.out,"f_fws-sws-sd.csv"))
write.csv(dfr_sd_fws_sws,file=paste0(dir.out,"r_fws-sws-sd.csv"))
write.csv(dfp_sd_fws_sws,file=paste0(dir.out,"p_fws-sws-sd.csv"))
write.csv(dff_sd_pulse,file=paste0(dir.out,"f_pulse-sd.csv"))
write.csv(dfr_sd_pulse,file=paste0(dir.out,"r_pulse-sd.csv"))
write.csv(dfp_sd_pulse,file=paste0(dir.out,"p_pulse-sd.csv"))

# Descriptive plots

View(dfp)

# get standard error for ggplot error bars
dff$se = dff$sd/sqrt(dff$n)
dfr$se = dfr$sd/sqrt(dfr$n)
dfp$se = dfp$sd/sqrt(dfp$n)

# get standard error for ggplot error bars
dffplots$se = dff$sd/sqrt(dff$n)
dfrplots$se = dfr$sd/sqrt(dfr$n)
dfpplots$se = dfp$sd/sqrt(dfp$n)

# get mean limits of error bars at 0.95 confidence
limits = aes(ymax = mean + (1.96*se), ymin= mean - (1.96*se))

# dodge plot position
dodge = position_dodge(width=0.9)

# save bar plot to file
# mean f for FWS x SWS combo
# each pulse gets it's own graph with facet_wrap

# F plots
tiff(paste0(dir.figs,"f_means_bar_facetwrap.tif"))
ggplot(dff, aes(x = FWS, y = mean, fill = SWS))+
  geom_bar(stat='identity', position=dodge)+
  geom_errorbar(limits, position=dodge, width=0.25) +
  facet_wrap(~ Pulse) +
  xlab('Fixed Tree Window Size')+
  ylab('Mean F-score')
dev.off() # turn off tiff()


# R Plots
tiff(paste0(dir.figs,"r_means_bar_facetwrap.tif"))
ggplot(dfr, aes(x = FWS, y = mean, fill = SWS))+
  geom_bar(stat='identity', position=dodge)+
  geom_errorbar(limits, position=dodge, width=0.25) +
  facet_wrap(~ Pulse) +
  xlab('Fixed Tree Window Size')+
  ylab('Mean Recall')
dev.off() # turn off tiff()
# P Plots

tiff(paste0(dir.figs,"p_means_bar_facetwrap.tif"))
ggplot(dfp, aes(x = FWS, y = mean, fill = SWS))+
  geom_bar(stat='identity', position=dodge)+
  geom_errorbar(limits, position=dodge, width=0.25) +
  facet_wrap(~ Pulse) +
  xlab('Fixed Tree Window Size')+
  ylab('Mean Precision')
dev.off() # turn off tiff()

# modeling


# testing ICC for mixed models - f-score
f.lmenull<-lme(f ~ 1,random = ~1|plots,data=df)
summary(f.lmenull)
0.04274029/ (0.04274029+0.1506788) # # ICC = Random Effects Intercept / (Intercept + Residual)
# 0.2209724

# p ICC
p.lmenull<-lme(p ~ 1,random = ~1|plots,data=df)
summary(p.lmenull)
0.0904094 / (0.0904094 + 0.226268) # # ICC =Random Effects Intercept / (Intercept + Residual)
# 0.2854937

# r ICC
r.lmenull<-lme(r ~ 1,random = ~1|plots,data=df)
summary(r.lmenull)
0.06199028 / (0.06199028+ 0.1259807) # # ICC Random Effects Intercept / (Intercept + Residual)
# 0.3297864
?Anova

# create linear models from forward/backward selection
f.lm.full<-lm(f~pulse*fws*ws+crr,data=df)                     # create full model with all interactions of factor predictors
f.lm.full.step<-stats::step(f.lm.full,direction= "both")  # stepwise elimination of predictors
f.lm<-stats::lm(formula(f.lm.full.step),data=df)          # get formula from stepwise elimation with lowest AIC, assign to model
formula(f.lm.full.step)                                   # inspect formula
summary(f.lm)


r.lm.full<-lm(r~pulse*fws*ws+crr,data=df)                     # Does this need to be GLM with log-link function?
r.lm.full.step<-stats::step(r.lm.full,direction= "both")
r.lm<-stats::lm(formula(r.lm.full.step),data=df)
formula(r.lm.full.step)
summary(r.lm)

p.lm.full<-lm(p~pulse*fws*ws+crr,data=df)
p.lm.full.step<-stats::step(p.lm.full,direction= "both")
p.lm<-stats::lm(formula(p.lm.full.step),data=df)
formula(p.lm.full.step)
summary(p.lm)

# get tables of model summaries in html viewer
tab.f<-sjt.lm(f.lm)
tab.p<-sjt.lm(p.lm)
tab.r<-sjt.lm(r.lm)
tab.f
tab.p
tab.r
tabs<-sjt.lm(r.lm,p.lm,f.lm)
tabs
# extract model summaries
f.lm_summary<-summary(f.lm)
f.lm_summary

r.lm_summary<-summary(r.lm)
r.lm_summary
plot(r.lm)
p.lm_summary<-summary(p.lm)
p.lm_summary

write.csv(tab.f$data,file=paste0(dir.out,"f_model-summary.csv"))
write.csv(tab.p$data,file=paste0(dir.out,"p_model-summary.csv"))
write.csv(tab.r$data,file=paste0(dir.out,"r_model-summary.csv"))


# model diagnostic plots and write to file

tiff(paste0(dir.figs,"f_model_diags.tif"))
par(mfrow=c(2,2))
plot(f.lm)
dev.off()

tiff(paste0(dir.figs,"r_model_diags.tif"))
par(mfrow=c(2,2))
plot(r.lm)
dev.off()

tiff(paste0(dir.figs,"p_model_diags.tif"))

plot(p.lm)
dev.off()

# anova tables
f.aov<-Anova(f.lm,type=3) # test significance of predictors/interactions
f.aov

r.aov<-Anova(r.lm,type=3)
r.aov

p.aov<-Anova(p.lm,type=3)
p.aov

write.csv(f.aov,file=paste0(dir.out,"f_model-aov.csv"))
write.csv(r.aov,file=paste0(dir.out,"r_model-aov.csv"))
write.csv(p.aov,file=paste0(dir.out,"p_model-aov.csv"))



# testing first-order interaction effects
# ~ f
print(as.formula(f.lm$call))
f.lm_fws_pulse_across_fws_effs<-testInteractions(f.lm,fixed="pulse",across="fws")
f.lm_sws_across_fws_effs<-testInteractions(f.lm,fixed="ws",across="fws")

# significant

?testInteractions
# ~ r
print(as.formula(r.lm$call))
r.lm_sws_across_fws_effs<-testInteractions(r.lm,fixed="ws",across="fws")
r.lm_pulse_across_sws_effs<-testInteractions(r.lm,fixed="pulse",across="ws")

# ~ p
print(as.formula(p.lm$call))
p.lm_sws_across_fws_effs<-testInteractions(p.lm,fixed="ws",across="fws")

## rbind test interactions sws across fws for r, p, f-score
rpf_sws_across_fws<-rbind(r.lm_sws_across_fws_effs,p.lm_sws_across_fws_effs,f.lm_sws_across_fws_effs)

# write tables
write.csv(rpf_sws_across_fws,file=paste0(dir.out,"rpf_sws_across_fws.csv"))
write.csv(f.lm_fws_pulse_across_fws_effs,file=paste0(dir.out,"f.lm_fws_pulse_across_fws_effs.csv"))
write.csv(r.lm_pulse_across_sws_effs,file=paste0(dir.out,"r.lm_pulse_across_sws_effs.csv"))



### Interaction Plots

# visualize Least-squares (predicted marginal) means interaction plot interaction effects per https://stats.stackexchange.com/questions/187996/interaction-term-in-a-linear-mixed-effect-model-in-r
tiff(paste0(dir.figs,"f_lsmip_fws-ws.tif"))
lsmip(f.lm, fws ~ ws) 
dev.off()

tiff(paste0(dir.figs,"f_lsmip_pulse-fws.tif"))
lsmip(f.lm, pulse ~ fws) 
dev.off()

tiff(paste0(dir.figs,"r_lsmip_fws-ws.tif"))
lsmip(r.lm, fws ~ ws)
dev.off()

tiff(paste0(dir.figs,"r_lsmip_pulse-ws.tif"))
lsmip(r.lm, pulse ~ ws)
dev.off()

tiff(paste0(dir.figs,"p_lsmip_fws-ws.tif"))
lsmip(p.lm, fws ~ ws)
dev.off()

## zero-order interacton means across pulse

flm.se<-phia::interactionMeans(f.lm,factors="pulse")
rlm.se<-phia::interactionMeans(r.lm,factors="pulse")
plm.se<-phia::interactionMeans(p.lm,factors="pulse")


write.csv(flm.se,file=paste0(dir.out,"f-interaction-means-pulse.csv"))
write.csv(rlm.se,file=paste0(dir.out,"r-interaction-means-pulse.csv"))
write.csv(plm.se,file=paste0(dir.out,"p-interaction-means-pulse.csv"))

tiff(paste0(dir.figs,"f_pulse_means-zeroord.tif"))
plot(flm.se)
dev.off()

tiff(paste0(dir.figs,"r_pulse_means-zeroord.tif"))
plot(rlm.se)
dev.off()

tiff(paste0(dir.figs,"p_pulse_means-zeroord.tif"))
plot(plm.se)
dev.off()

## zero-order interacton means across fws
flm.sefws<-phia::interactionMeans(f.lm,factors="fws")
rlm.sefws<-phia::interactionMeans(r.lm,factors="fws")
plm.sefws<-phia::interactionMeans(p.lm,factors="fws")

tiff(paste0(dir.figs,"f_fws_means-zeroord.tif"))
plot(flm.sefws)
dev.off()

tiff(paste0(dir.figs,"r_fws_means-zeroord.tif"))
plot(rlm.sefws)
dev.off()

tiff(paste0(dir.figs,"p_fws_means-zeroord.tif"))
plot(plm.sefws)
dev.off()

## zero-order interacton means across sws
summary(r.lm)
flm.sesws<-phia::interactionMeans(f.lm,factors="ws")
rlm.sesws<-phia::interactionMeans(r.lm,factors="ws")
plm.sesws<-phia::interactionMeans(p.lm,factors="ws")


rbind(rlm.sesws,plm.sesws,flm.sesws)
tiff(paste0(dir.figs,"f_sws_means-zeroord.tif"))
plot(flm.sesws)
dev.off()

tiff(paste0(dir.figs,"r_sws_means-zeroord.tif"))
plot(rlm.sesws)
dev.off()

tiff(paste0(dir.figs,"p_sws_means-zeroord.tif"))
plot(plm.sesws)
dev.off()


qplot(f, pulse, data=df, geom="point") +  
  facet_wrap(~ws)+  
  geom_smooth(method = "lm", se = TRUE)  


# first order interactions
flm.means<-interactionMeans(f.lm)
rlm.means<-interactionMeans(r.lm)
plm.means<-interactionMeans(p.lm)
?interactionMeans
tiff(paste0(dir.figs,"f-firstord.tif"))
plot(flm.means)
dev.off()

tiff(paste0(dir.figs,"r-firstord.tif"))
plot(rlm.means)
dev.off()

tiff(paste0(dir.figs,"p-firstord.tif"))
plot(plm.means)
dev.off()
?plot

flm.sws_fws<-phia::interactionMeans(f.lm,factors=c("ws","fws"))
flm.pulse_fws<-phia::interactionMeans(f.lm,factors=c("pulse","fws"))

rlm.sesws<-phia::interactionMeans(r.lm,factors="ws")
plm.sesws<-phia::interactionMeans(p.lm,factors="ws")


toLatex(sessionInfo())

write.bib(requiredPackages)
save.image(paste0(dir,"/output/Full_Accuracy_Assessment-models.RData"))
savehistory(file=paste0(dir,"/output/Full_Accuracy_Assessment-models.RHistory"))







## Playing with mixed models
####################
# create mixed models 
####################

# precision mixed models
# using lmer
p.lmer1<-lmer(fixed = pulse*fws*ws, random = ~1|plots,data=df)
p.lmer.step<-MASS::step(p.lmer1)  # step through model to select final model
p.lmer<-p.lmer.step$model
summary(p.lmer)

anova(p.lmer,p.lm)

p.lme1<-lme(p ~ pulse*fws*ws,random = ~1|plots,data=df)
p.lme1<-update(p.lme1, method = "ML")                   # fit with ML to step AIC
p.lme.step<-stepAIC(p.lme1)
p.lme.step$call
p.lme<-lme(p ~ pulse+fws+ws+pulse*ws+fws*ws,random = ~1|plots,data=df)
summary(p.lme)

p.lm<-lm(p ~ pulse+fws+ws+pulse*ws+fws*ws,data=df)
p.lme<-update(p.lme, method = "REML")                   # fit with ML to step AIC

anova(p.lme,p.lm)

lme(f ~ pulse + (fws*ws|plots),data=df)

## mixed effects testing
# fit mixed models with random plot intercept 
f.fit1<-lmer(f~ fws + ws + pulse + (1|plots) ,df)
f.fit2<-lmer(f~ fws * ws * pulse + (1|plots) ,df)
f.fit3<-lmer(f~ fws * ws + pulse + (1|plots) ,df)

# fit mixed models with random plot intercept 
r.fit1<-lmer(r~ fws + ws + pulse + (1|plots) ,df)
r.fit2<-lmer(r~ fws * ws * pulse + (1|plots) ,df)
r.fit3<-lmer(r~ fws * ws + pulse + (1|plots) ,df)


# fit mixed models with random plot intercept 
p.fit1<-lmer(p~ fws + ws + pulse + (1|plots) ,df)
p.fit2<-lmer(p~ fws * ws * pulse + (1|plots) ,df)
p.fit3<-lmer(p~ fws * ws + pulse + (1|plots) ,df)

# test models to select
anova(f.fit1,f.fit2,f.fit3) # interaction effects significant
f.fit.step<-lmerTest::step(f.fit2) # step through model to select final model
f.fit<-f.fit.step$model  # assign final model as final fit
summary(f.fit)
# model
f.fit.reml <- update(f.fit,REML=TRUE)     # Kenward-Roger approximation to compute FF-statistics. 
f.fit.Anova<-Anova(f.fit.reml,type="II",test.statistic="F") # k-r approximation only valid for REML and thus we must first refit our model.
f.fit.coef<-coef(f.fit) # model coefficients


#  TK Continue here with lmer/lme testing vs lm
## recall mixed models
r.lmer1<-lmer(r~pulse*fws*ws+(1|plots),data=df)
r.lmer.step<-lmerTest::step(r.lmer1) # step through model to select final model
r.lmer<-r.lmer.step$model
r.lmer

anova(r.lmer,r.lm)


r.lme1<-lme(r ~ pulse*fws*ws,random = ~1|plots,data=df)
r.lme1<-update(r.lme1, method = "ML")                   # fit with ML to step AIC
r.lme.step<-stepAIC(r.lme1)
r.lme.step$call
r.lme<-lme(r ~ pulse+fws+ws+pulse*ws+fws*ws,random = ~1|plots,data=df)
summary(r.lme)



## f-score  mixed models
f.lmer1<-lmer(f~pulse*fws*ws+(1|plots),data=df)
f.lmer.step<-lmerTest::step(f.lmer1) # step through model to select final model
f.lmer<-f.lmer.step$model
f.lmer

anova(f.lmer,f.lm)


f.lme1<-lme(f ~ pulse*fws*ws,random = ~1|plots,data=df)
f.lme1<-update(f.lme1, method = "ML")                   # fit with ML to step AIC
f.lme.step<-stepAIC(f.lme1)
f.lme.step$call
f.lme<-lme(f ~ pulse+fws+ws+pulse*ws+fws*ws,random = ~1|plots,data=df)
summary(f.lme)

sjt.lmer(f.lmer)


save.image(paste0(dir,"/output/Full_Accuracy_Assessment-models.RData"))
# for (i in c('f','p','r')) {} # loop all of this for r, f, p?



##  diagnostics
par(mfrow = c(1,1))
lme.diag(f.fit)
lme.diag(p.fit)
lme.diag(r.fit)

f.fit.test <- as(f.fit,"merModLmerTest") # for further testing
f.fit.summary<-print(summary(f.fit.test,ddf="Kenward"),correlation=FALSE) # for p values
    # print(summary(f.fit.test,ddf="lme4"),correlation=FALSE) # for t values
    # print(summary(f.fit.test,ddf="Satterthwaite"),correlation=FALSE) # Kenward more accurate : https://rpubs.com/palday/mixed-interactions

f.fit.lsmeansLT<-lsmeansLT(f.fit) # fws~ws interaction contracts -- tukey hsd of model interactions

cld(f.fit.lsmeansLT)
plot(f.fit.lsmeansLT,comparisons=TRUE)

write.csv(lsmeansLT,file=paste0(dir.out,"lsm-fixed-eff.csv"))

f.fit.lsmeans<-lmerTest::lsmeans(f.fit,ddf="Kenward-Roger") # fws~ws interaction contracts -- tukey hsd of model interactions

f.fit.contrast<-lsmeans(f.fit,pairwise ~ fws : pulse) # fws~ws interaction contracts -- tukey hsd of model interactions
f.fit.contrast


write.csv(f.fit.coef,file=paste0(dir.out,""))

####

p.e <- allEffects(model)
jpeg(paste0(dir.figs,"f_score-fws-pulse.tif"))
plot(p.e[[2]],multiline=TRUE,confint=TRUE,ci.style="bars"
     ,main="F-score with Fixed-window size and Lidar Pulse Density Interaction"
     ,xlab="Lidar Pulses per 25-square meters"
     ,ylab="F-Score")
dev.off()
View(df)
jpeg(paste0(dir.figs,"f_score-fws-sws.tif"))
plot(p.e[[1]],multiline=TRUE,confint=TRUE,ci.style="bars"
     ,main="F-score with Fixed-window size and 
     CHM Smoothing-window size Interaction"
     ,xlab="Fixed Window Size"
     ,ylab="F-Score"
     ,cex.lab=1.5,cex.axis=1.5)
?plot
dev.off()

####
jpeg(paste0(dir.figs,"f_score-fws-pulse.tif"))
plot(p.e[[2]],multiline=TRUE,confint=TRUE,ci.style="bars"
     ,main="F-score with Fixed-window size and Lidar Pulse Density Interaction"
     ,xlab="Lidar Pulses per 25-square meters"
     ,ylab="F-Score")
dev.off()
View(df)
jpeg(paste0(dir.figs,"f_score-fws-sws.tif"))
plot(p.e[[1]],multiline=TRUE,confint=TRUE,ci.style="bars"
     ,main="F-score with Fixed-window size and 
     CHM Smoothing-window size Interaction"
     ,xlab="Fixed Window Size"
     ,ylab="F-Score"
     ,cex.lab=1.5,cex.axis=1.5)
?plot
dev.off()


##########################################################


# using sjp.lmer plot "types"

lapply(names(plot.re), 
       function(x)ggsave(filename=paste(dir,"/output/",x,".jpeg",sep=""), plot=list.figs[[x]]))
?sjp.lmer
f.plot.re<-sjp.lmer(f.fit, type="re",sort.est = TRUE) # "re" (default) for conditional modes of random effects as forest plot
f.plot.re$plot.list[[1]] + ggtitle("Random Effect of Sample Plots")+ labs(x="Sample Plot",y="Intercept Effect")

# f.plot.re<-sjp.lmer(f.fit, type="fe",sort.est = TRUE) # "re" (default) for conditional modes of random effects as forest plot
jpeg(paste0(dir.figs,"qq-random-eff.tif"))
f.plot.re_qq<-sjp.lmer(f.fit,type = "re.qq", sort.est = "sort.all",show.legend = TRUE) # qq plot of random effects
dev.off()

f.plot.pred<-sjp.lmer(f.fit,vars="f",type = "pred", sort.est = "sort.all") # "pred" to plot predicted values for the response, related to specific model predictors and conditioned on random effects.
f.plot.pred$plot + ggtitle("Predicted Values Conditioned on Random Effects")

f.plot.pred_fe<-sjp.lmer(f.fit,vars="f",type = "pred.fe", sort.est = "sort.all") # "pred.fe" to plot predicted values for the response, related to specific model predictors and conditioned on fixed effects only. 
f.plot.pred_fe$plot + ggtitle("Predicted Values Conditioned on Fixed-effect Only Model")

# f.plot.eff_ri<-sjp.lmer(f.fit,type = "eff.ri", prnt.plot = TRUE) # "eff.ri"  to plot marginal effects of all fixed terms in fit, varying by the random intercepts.
# f.plot.ri_slope<-sjp.lmer(f.fit, type = "ri.slope") # plots fixed effects slopes dependent on random intercepts

lapply(names(f.plot.re_qq), 
       function(x)ggsave(filename=paste(dir,"/figs/",x,".jpeg",sep=""), plot=f.plot.re_qq[[x]]))

save.image(paste0(dir,"/output/Full_Accuracy_Assessment-models.RData"))
savehistory(file=paste0(dir,"/output/Full_Accuracy_Assessment-models.RHistory"))a
