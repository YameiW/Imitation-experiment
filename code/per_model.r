data <- read.csv("dataset_1.csv")

# remove some observations 
data$word_id <- paste0(data$word,data$id)
nrow(data) #3895

data <- subset(data,data$word_id!="pallet49")
data <- subset(data,data$word_id!="panama25")
data <- subset(data,data$word_id!="parcel25")
data <- subset(data,data$word_id!="payment15")
data <- subset(data,data$word_id!="payment6")
data <- subset(data,data$word_id!="peal6")
data <- subset(data,data$word_id!="pebble14")
data <- subset(data,data$word_id!="pendant25")
data <- subset(data,data$word_id!="permit42")
data <- subset(data,data$word_id!="picture37")
data <- subset(data,data$word_id!="pitiful25")
data <- subset(data,data$word_id!="pitiful37")
data <- subset(data,data$word_id!="polarize10")
data <- subset(data,data$word_id!="policy30")
data <- subset(data,data$word_id!="politic9")
data <- subset(data,data$word_id!="popular29")
data <- subset(data,data$word_id!="positive12")
data <- subset(data,data$word_id!="possibly12")
data <- subset(data,data$word_id!="possibly30")
data <- subset(data,data$word_id!="possibly49")
data <- subset(data,data$word_id!="possibly9")
data <- subset(data,data$word_id!="power12")
data <- subset(data,data$word_id!="public12")
data <- subset(data,data$word_id!="public27")
data <- subset(data,data$word_id!="park6")

nrow(data) #3869

# add rest variable and add the frequency information
data$rest <- data$word_duration-data$vot
data <- data[order(data$word),]
summary(data$word)
data$freq <- c(rep(1,98),rep(1,98),rep(0,98),rep(0,96),rep(0,98),rep(0,96),rep(0,98),rep(0,96),rep(1,98),rep(1,96),rep(1,98),rep(0,98),
               rep(1,98),rep(0,98),rep(0,98),rep(1,94),rep(1,98),rep(0,98),rep(0,96),rep(0,96),rep(0,96),rep(0,98),rep(1,96),rep(1,98),
               rep(1,96),rep(0,98),rep(0,94),rep(1,98),rep(1,98),rep(0,98),rep(0,96),rep(1,96),rep(1,96),rep(1,96),rep(0,98),rep(1,96),
               rep(1,88),rep(1,96),rep(1,94),rep(1,98))

data$KF <- c(rep(91,98),rep(58,98),rep(42,98),rep(1,96),rep(1,98),rep(4,96),rep(1,98),rep(1,96),rep(NA,98),rep(94,96),rep(216,98),rep(4,98),
             rep(113,98),rep(2,98),rep(3,98),rep(53,94),rep(327,98),rep(2,98),rep(4,96),rep(1,96),rep(1,96),rep(1,98),rep(77,96),rep(196,98),
             rep(162,96),rep(5,98),rep(4,94),rep(46,98),rep(395,98),rep(3,98),rep(1,96),rep(222,96),rep(69,96),rep(98,96),rep(2,98),rep(74,96),
             rep(61,88),rep(342,96),rep(438,94),rep(47,98))

head(data)
data <- data[order(data$word_id),]
nrow(data) #3869

# add the percentage info
data1 <- aggregate(data$vot_duration, by=list(data$word,data$id,data$cond,data$KF,data$freq,data$word_id), FUN=diff,na.rm=TRUE)
colnames(data1) <- c("word","id","cond","KF","freq","word_id","vot_diff")
data1 <- data1[order(data1$word_id),]
nrow(data1) #1885
summary(data1$word)

data2 <- aggregate(data$rest, by=list(data$word,data$id,data$cond,data$KF,data$freq,data$word_id), FUN=diff,na.rm=TRUE)
colnames(data2) <- c("word","id","cond","KF","freq","word_id","rest_diff")
data2 <- data2[order(data2$word_id),]
nrow(data2) #1885

data3 <- subset(data,data$block=="a")
data3 <- subset(data3,data3$word !="Paris")
data3 <- data3[order(data3$word_id),]
nrow(data3) #1885

head(data3)
head(data2)
head(data1)
data1$base_vot <- data3$vot_duration
data1$rest_diff <- data2$rest_diff
data1$base_rest <- data3$rest
head(data1)

data1$vot_diff <- unlist(data1$vot_diff)
data1$rest_diff <- unlist(data1$rest_diff)

data1$vot_per <- (data1$vot_diff/data1$base_vot)*100
data1$rest_per <- (data1$rest_diff/data1$base_rest)*100

data4 <- data1[,-c(7:10)]
head(data4)

# from wide to long
library(reshape2)
data5 <- melt(data4,id.vars=c("word","id","cond","KF","freq","word_id"))
head(data5)
colnames(data5) <- c("word","id","cond","KF","freq","word_id","word_part","per_increase")

# check the varaible types
str(data5)
data5$id <- as.factor(data5$id)
data5$word_id <- as.factor(data5$word_id)

# try the modeling 
library(lme4)
library(lmerTest)
library(optimx)
library(emmeans)
library(plyr)

data5$logfreq <- log(data5$KF)
levels(data5$word_part)
levels(data5$word_part)[levels(data5$word_part)=="vot_per"] <- "vot"
levels(data5$word_part)[levels(data5$word_part)=="rest_per"] <- "rest"

############################################### model fitting with logfreq ##############################

contrasts(data5$cond) <- c(-.5,.5)
contrasts(data5$word_part) <- c(-.5, .5)


model.1=lmer(per_increase~cond*word_part*logfreq+
               (1+cond|word)+(1+word_part+logfreq|id),
             control=lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=1e5)),
             data=data5,REML=FALSE)

summary(model.1)

# three way interactions

model.2 <- update(model.1,.~.-cond:word_part:logfreq)
anova(model.1,model.2) # 0.8955
model.1 <- model.2

# two way interactions
model.3 <- update(model.1,.~.-cond:word_part)
anova(model.1,model.3) # 0.9862

model.4 <- update(model.1,.~.-cond:logfreq)
anova(model.1,model.4) # 0.8766

model.5 <- update(model.1,.~.-word_part:logfreq)
anova(model.1,model.5) # 0.8154

# final model
model.6=lmer(per_increase~cond+word_part+logfreq+word_part:logfreq+
               (1+cond|word)+(1+word_part+logfreq|id),
             control = lmerControl(
               optimizer ='optimx', optCtrl=list(method='nlminb')),
             data=data5,REML=FALSE)

summary(model.6)

# Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: per_increase ~ cond + word_part + logfreq + word_part:logfreq +      (1 + cond | word) + (1 + word_part + logfreq | id)
#    Data: data5
# Control: lmerControl(optimizer = "optimx", optCtrl = list(method = "nlminb"))
# 
#      AIC      BIC   logLik deviance df.resid 
#  36316.2  36409.7 -18143.1  36286.2     3755 
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -6.1728 -0.4636 -0.0719  0.3239 14.7488 
# 
# Random effects:
#  Groups   Name        Variance  Std.Dev. Corr       
#  id       (Intercept) 121.32245 11.0146             
#           word_part1  331.89289 18.2179  -0.92      
#           logfreq       0.08806  0.2967  -0.21  0.17
#  word     (Intercept)   2.59970  1.6124             
#           cond1        10.59247  3.2546  -1.00      
#  Residual             842.87940 29.0324             
# Number of obs: 3770, groups:  id, 49; word, 39
# 
# Fixed effects:
#                     Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)           6.0008     1.8019   51.7319   3.330 0.001606 ** 
# cond1                 0.3481     1.8933   51.6071   0.184 0.854832    
# word_part1          -12.1315     3.0353   69.9056  -3.997 0.000157 ***
# logfreq              -0.1190     0.2474   37.2391  -0.481 0.633226    
# word_part1:logfreq    0.1021     0.4374 3584.8919   0.234 0.815382    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr) cond1  wrd_p1 logfrq
# cond1       -0.029                     
# word_part1  -0.690  0.001              
# logfreq     -0.410  0.000  0.025       
# wrd_prt1:lg  0.000  0.000 -0.410  0.000
# convergence code: 0
# boundary (singular) fit: see ?isSingular

emmeans(model.6, pairwise~word_part|cond,adjust="tukey",options=get_emm_option("emmeans"))

# $emmeans
# cond = g1:
#   word_part  emmean   SE   df lower.CL upper.CL
# vot       11.4090 3.06 59.8     5.30    17.52
# rest      -0.4323 1.37 53.1    -3.19     2.32
# 
# cond = g2:
#   word_part  emmean   SE   df lower.CL upper.CL
# vot       11.7573 3.02 57.6     5.71    17.81
# rest      -0.0839 1.30 49.3    -2.70     2.53
# 
# Degrees-of-freedom method: satterthwaite 
# Confidence level used: 0.95 
# 
# $contrasts
# cond = g1:
#   contrast   estimate   SE   df t.ratio p.value
# vot - rest     11.8 2.77 48.5 4.275   0.0001 
# 
# cond = g2:
#   contrast   estimate   SE   df t.ratio p.value
# vot - rest     11.8 2.77 48.5 4.275   0.0001 
# 
# Degrees-of-freedom method: satterthwaite 


  
############################################### model fitting with freq category ##################################

data5$freq <- as.factor(data5$freq)
contrasts(data5$freq) <- c(.5, -.5)

model.7=lmer(per_increase~cond*word_part*freq+
               (1+cond|word)+(1+word_part+freq|id),
             control = lmerControl(
               optimizer ='optimx', optCtrl=list(method='nlminb')),
             data=data5,REML=FALSE)

summary(model.7)

#three way interaction
model.8 <- update(model.7,.~.-cond:word_part:freq)
anova(model.7,model.8) #0.7256
model.7 <- model.8

#two way interaction
model.9 <- update(model.7,.~.-cond:word_part)
anova(model.7,model.9) # 0.986

model.10 <- update(model.7,.~.-cond:freq)
anova(model.7,model.10) # 0.5751

model.11 <- update(model.7,.~.-word_part:freq)
anova(model.7,model.11) # 0.865

# final model

model.12=lmer(per_increase~cond+word_part+freq+word_part:freq+
               (1+cond|word)+(1+word_part+freq|id),
              control = lmerControl(
                optimizer ='optimx', optCtrl=list(method='nlminb')),
             data=data5,REML=FALSE)

summary(model.12)

# Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: per_increase ~ cond + word_part + freq + word_part:freq + (1 +      cond | word) + (1 + word_part + freq | id)
#    Data: data5
# Control: lmerControl(optimizer = "optimx", optCtrl = list(method = "nlminb"))
# 
#      AIC      BIC   logLik deviance df.resid 
#  36316.0  36409.5 -18143.0  36286.0     3755 
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -6.1379 -0.4623 -0.0698  0.3253 14.6927 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev. Corr       
#  id       (Intercept) 117.999  10.863              
#           word_part1  332.076  18.223   -0.92      
#           freq1         3.994   1.998    0.26 -0.33
#  word     (Intercept)   2.620   1.619              
#           cond1        10.540   3.247   -1.00      
#  Residual             842.347  29.023              
# Number of obs: 3770, groups:  id, 49; word, 39
# 
# Fixed effects:
#                    Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)         5.65871    1.64300   50.59947   3.444  0.00116 ** 
# cond1               0.35787    1.89238   51.57715   0.189  0.85075    
# word_part1        -11.84540    2.76977   48.45754  -4.277 8.87e-05 ***
# freq1              -0.06034    1.09406   37.47308  -0.055  0.95631    
# word_part1:freq1   -0.32164    1.89160 3584.33317  -0.170  0.86499    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr) cond1  wrd_p1 freq1 
# cond1       -0.032                     
# word_part1  -0.818  0.001              
# freq1        0.072 -0.001 -0.081       
# wrd_prt1:f1  0.000  0.001  0.007  0.000
# convergence code: 0
# boundary (singular) fit: see ?isSingular






###################################################### plot the word_part increase ####################################################
library(ggplot2)
data_plot <- aggregate(data5$per_increase,by=list(data5$word_part),FUN=function(x) c(mean=mean(x),sd=sd(x),n=length(x)))
data_plot <- do.call(data.frame,data_plot)
data_plot$se <- data_plot$x.sd/sqrt(data_plot$x.n)

limit <- aes(ymin=data_plot$mean-data_plot$se,
             ymax=data_plot$mean+data_plot$se)
colnames(data_plot) <- c("word_part","mean","sd","n","se")

ggplot(data_plot,aes(x=word_part,y=mean,fill=word_part))+geom_bar(stat="identity",width=.5)+geom_errorbar(limit,width=.2,color="black")




data_plot2 <- aggregate(data5$per_increase,by=list(data5$word_part,data5$cond),FUN=function(x) c(mean=mean(x),sd=sd(x),n=length(x)))
data_plot2 <- do.call(data.frame,data_plot2)
data_plot2$se <- data_plot2$x.sd/sqrt(data_plot2$x.n)

limit1 <- aes(ymin=data_plot2$mean-data_plot$se,
             ymax=data_plot2$mean+data_plot$se)
colnames(data_plot2) <- c("word_part","cond","mean","sd","n","se")
ggplot(data_plot2,aes(x=word_part,y=mean,fill=word_part))+geom_bar(stat="identity",width=.5)+geom_errorbar(limit1,width=.2,color="black")+facet_grid(.~cond)


ggplot(data5)+geom_point(aes(x=logfreq,y=per_increase,col=word_part))
ggplot(data5)+geom_density(aes(x=per_increase,fill=word_part,alpha=0.4))
ggplot(data5)+geom_boxplot(aes(x=word_part,y=per_increase))
ggplot(data5)+geom_boxplot(aes(x=word_part,y=per_increase))+facet_grid(.~cond)

####################################################### only cond1 ######################################################################
