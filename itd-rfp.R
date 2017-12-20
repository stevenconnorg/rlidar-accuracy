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

### Data reading, cleaning, and aggregating

# rm(list=ls())

setwd("E:/01_Lidar_Steven_Manuscript/04_Assessment/") # set working directory as root folder
dir=getwd() # assign dir variable as wd address # assign directory object
dir.data<-paste0(dir,"/data/")
dir.out<-paste0(dir,"/output/")
load(paste0(dir,"/output/Full_Accuracy_Assessment-df-prep.RData"))

# need functions for Install_And_Load function and automated geometry repair
source(paste0(dir,("/R/functions.R")))


# 
requiredPackages = c('rLiDAR'
                     ,'spatstat'
                     ,'sp'
                     ,'maptools'
                     ,'rgdal'
                     ,'rgeos'
                     ,'plyr'
                     ,'data.table'
                     ,'cleangeo'
                     ,'tidyr'
                     ,'sjPlot'
                     ,'sjmisc'
                     ,'sjlabelled'
                     ,'tibble'
                     ,'dplyr'
                     ,'bibtex'
                     )

Install_And_Load(requiredPackages)

### Data cleaning 



## read in data

trees<-read.csv(paste0(dir.data,"TreeList.csv")) # lidar-detected trees coded by lidar-pulse density, 
head(trees)
trees<-trees[trees$height>=5,]
dim(trees)
ltrees<-SpatialPointsDataFrame(trees[,1:2],trees) # make spatial points data frame from x, y
ltrees<-ltrees[ltrees@data$ws!=0,]
dim(ltrees@data)
# ltrees<-ltrees[ltrees@data$pulse!="Conifer0.2_chm.asc",]

ttrees<-readOGR(dsn=paste0(dir.data,"obs_trees.shp")) #  true tree points
obs_crown<-readOGR(dsn =paste0(dir.data,"obs_crown.shp")) # true tree crown polygons
out_crown<-readOGR(dsn=paste0(dir.data,"out_obs_crown.shp")) # plot polygon minus crown polygons
plots<-readOGR(dsn=paste0(dir.data,"plots.shp")) # study area grid
plot_cover<-read.csv(paste0(dir.data,"plot_cover.csv")) # lidar metrics for sample plots from an unthinned dataset

ltrees@proj4string<-CRS("+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
utm10(ttrees)
utm10(obs_crown)
utm10(out_crown)
utm10(plots)

dim(ttrees@data)

# save.image(paste0(dir,"/output/input-data.RData"))

head(ltree.n@data)
## format data
# lidar-derived trees
colnames(ltrees@data)                        # format lidar-derived trees
ltrees<-ltrees[,c(4:6)]                      # subset ltrees data for only fws, pulse ("id"), ws
ltrees@data<-ltree_id(ltrees@data)           # add ltree id
colnames(ltrees@data)[3]<-"pulse"            # rename column to pulse 
ltrees@data$fws<-as.integer(ltrees@data$fws) # set fws as integer
ltrees@data$ws<-as.integer(ltrees@data$ws)   # set sws as integer
head(ltrees@data)

# observed crown and associated plot
colnames(obs_crown)                          # format obs_crown data
colnames(obs_crown@data)[1]<-"obs_tree_id"   # name observed tree id column
obs_crown@data<-obs_crown@data[,1:2]         # subset only tree id and associated plot    
head(obs_crown@data)   

# plot lidar metrics
head(plot_cover)                             # format plot_cover data
plotvars<-c(4,25,22,52,93:96,89,90,93:96)    # subset variables of interest for each plot
plot_cover<-plot_cover[,plotvars]  
colnames(plot_cover) # inspect
plot_cover<-rownames_to_column(plot_cover,var="plots") # set rownames as column for joining later
plot_cover
plots_coveronly<-plot_cover[,c(1,5)]
write.csv(plots_coveronly,file=paste0(dir.out,"/plot-cover.csv"))
## clean polygons for topological errors
obs_crown<-clean.crown(obs_crown) # clean observed crown
par(mfrow = c(1,1))
plot(obs_crown[obs_crown$plot==37,]) # visualize
plot(out_crown,add=TRUE) #inspect for errors
unique(obs_crown$plot)

plot(plots)
plot(obs_crown[obs_crown$plot==37,],add=TRUE)
## subset ltrees to study area
# since tree canopies do not always fall completely within the sample plots
# the 'true' sample area encompasses the area of the plots plus the 
# canopies of observed trees, whose tree top falls within the sample plots

studyarea<-gUnion(plots,obs_crown) #total, 'true' sample plot
plot(studyarea)
studyarea@proj4string
ltrees@proj4string

ltree.n<-ltrees[studyarea,] # subset ltrees within study area
dim(ltree.n@data)           # 70035 x  8

save.image(paste0(dir,"/output/Full_Accuracy_Assessment-df-prep.RData"))             # save workspace 
save(list = c('ltree.n'),file=paste0(dir,"/output/ltree-plot-sample.RData")) # plot sample trees


## spatial joining true tree locations to their observed crowns
ttrees.crown<-over(ttrees,obs_crown) # observed trees %over% observed crown, inherently 1:1 

## spatial joining lidar derived tree locations to observed tree crowns
ltreecrown<-over(ltree.n,obs_crown) # lidar trees %over% observed crown
 
 # alternatively, use rgeos to constrain dimensionality; however, no errors required this step for this data:
    # ltreecrown<-overGeomGeomDF(ltree.n,obs_crown,returnList=FALSE,min_Dimensions=2)

## spatial joining lidar derived tree locations to out_crown
ltreeoutcrown<-over(ltree.n,out_crown) # lidar tree %over% area outside of crown but within plot
ltreecrown<-ltree_id(ltreecrown)       # add ltree id 
ltreeoutcrown<-ltree_id(ltreeoutcrown) #add ltree id 
colnames(ltreeoutcrown)[2]<-"out.plot" #rename associated plot for outside 


## join dataframe back to ltrees spatial objects by ltree_id column
ltree.n@data<-merge(x=ltree.n@data,y=ltreecrown,by.x="ltree_id",by.y="ltree_id",all=TRUE)    # trees within observed crown
ltree.n@data<-merge(x=ltree.n@data,y=ltreeoutcrown,by.x="ltree_id",by.y="ltree_id",all=TRUE) # trees outside observed crown

##  create new plot variable, synthesis of 'plot' (crown) and 'out.plot' (out_crown)
ltree.n@data<-transform(ltree.n@data,plots=ifelse(!is.na(plot),plot,out.plot)) 


View(ltree.x) # inspect for errors. 'plots' variable should not be NA
ltree.n<-ltree.n[!is.na(ltree.n@data$plots),] # remove one outlier
save(list = c('ltree.n'),file=paste0(dir,"/output/ltree-plot-sample-joined.RData")) # plot sample trees



##########
## get number of observed trees per plot
obsplotcount<- ttrees.crown%>% group_by(plot) %>% dplyr::summarize(obs_count=n()) %>% arrange(plot)
obsplotcount # true number of trees per plot 
obsplotcount <- rownames_to_column(obsplotcount,var="plots")
obsplotcount
write.csv(obsplotcount,file=paste0(dir.out,"obsplotcount.csv"))
##  get total number of lidar trees:

# per tree
total.ltree.tree <-ltree.n@data %>%
  group_by(plots,pulse,fws,ws,obs_tree_id) %>%
  dplyr::summarize(total=n())  %>%
  arrange(pulse,fws,ws,plots) 
  total.ltree.tree  

# per plot
total.ltree.plot <-total.ltree.tree %>%
  group_by(plots,pulse,fws,ws) %>%
  dplyr::summarize(total=sum(total))  %>%
  ungroup() %>%
  complete(plots,pulse,fws,ws,fill = list(total=0)) %>%
  arrange(pulse,fws,ws,plots) 
  total.ltree.plot 

# by pulse
total.ltree.pulse <-total.ltree.plot %>%
  group_by(pulse,fws,ws) %>%
  dplyr::summarize(total=sum(total)) %>%
  arrange(pulse,fws,ws) 
  total.ltree.pulse 

## false positives in crown
# 1 number of trees within tree crown
in.tree.tree<-ltree.n@data %>% 
  group_by(plots,pulse,fws,ws,obs_tree_id) %>%
  filter(!is.na(obs_tree_id)) %>%
  dplyr::summarize(in.count=n()) %>%
  arrange(pulse) 

# number of false positives per tree crown (trees identified per tree minus 1)
fp.in.tree<-in.tree.tree %>% 
  group_by(obs_tree_id,plots,pulse,fws,ws) %>%
  mutate(fp.in=in.count-1) %>%
  arrange(pulse)
  fp.in.tree

# number of false positives per plot
fp.in.plot<-fp.in.tree %>% 
  group_by(plots,pulse,fws,ws) %>%
  dplyr::summarize(fp.in=sum(fp.in)) %>%
  arrange(pulse)
  fp.in.plot

#by pulse density
fp.in.pulse<- fp.in.plot %>% 
  group_by(pulse,fws,ws) %>%
  dplyr::summarize(fp.in=sum(fp.in)) %>%
  arrange(pulse)
  fp.in.pulse


## false positives out crown
# number of false positives outside tree crowns by plot
out.tree.plot<-ltree.n@data %>% 
  group_by(plots,pulse,fws,ws) %>%
  filter(is.na(obs_tree_id) )%>% # ltrees that fall outside observed tree crowns (is.na)
  dplyr::summarize(fp.out=n())  %>%
  arrange(pulse)
  out.tree.plot 

# number of false positives outside tree crowns by pulse
out.tree.pulse <- out.tree.plot  %>%
  group_by(pulse,fws,ws) %>%
  dplyr::summarize(fp.out=sum(fp.out))%>%
  arrange(pulse)
  out.tree.pulse 


## create dataframe and clean
df<-merge(total.ltree.plot,fp.in.plot,all=TRUE)      # total number of trees and false positives in crown per plot by pulse, fws, ws
View(df)
df[is.na(df)]<-0 
df<-merge(df,out.tree.plot,all=TRUE)                 # join false positives outside crown per plot by pulse, fws, ws
df[is.na(df)]<-0
df$fp<-df$fp.in+df$fp.out                            # total false positives equals false positives in plus false positives out
df[is.na(df)]<-0
df$tp<-df$total-(df$fp)                              # true positives equals total number of trees detected minus false positives
df[is.na(df)]<-0
df<-merge(df,obsplotcount,by.x="plots",by.y="plots") # join number of observed trees per plot
df$fn<-df$obs_count-df$tp                            # false negatives equals number of observed trees per plot minus true positives per plot
df[is.na(df)]<-0
df$r<-df$tp/(df$tp+df$fn)                            # recall
df$p<-df$tp/(df$tp+df$fp)                            # precision
df$f<-2*((df$r*df$p)/(df$r+df$p))                    # f = balance of r and p
df$pulse_id<-df$pulse
str(df)                                              # structure of  data
df$pulse<-gsub("[^0-9\\.]", "", df$pulse)
df$pulse<-substr(df$pulse, 1, nchar(df$pulse)-1)
head(df)
df$pulse<-as.factor(df$pulse)
df$plots<-as.factor(df$plots)                        # set plot as factor
df$fws<-as.factor(df$fws)                            # set fws as factor
df$ws<-as.factor(df$ws)                              # set ws as factor
# df$pulse<-as.ordered(df$pulse)
colnames(df)
colnames(plot_cover)
df<-merge(df,plot_cover,by="plots")
# df$canopycatbreaks<-cut(df$Canopy.relief.ratio, breaks=c(.3,.4,.5,.6,.7), right = FALSE)
# df$canopycat<-cut(df$Canopy.relief.ratio, breaks=c(.3,.4,.5,.6,.7), right = FALSE,labels=FALSE)
# df$canopycat<-as.factor(df$canopycat)

str(df)
View(df)

save(list=c('df'),file=paste0(dir,"/output/df.RData"))
save.image(paste0(dir,"/output/Full_Accuracy_Assessment-df-prep.RData"))
write.bib(requiredPackages,file="Accuracy_Assessment-packages")
?write.bib
