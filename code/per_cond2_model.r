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
data6 <- subset(data5,data5$cond=="g2") # g2 without picturized meaning


str(data6)
data6$id <- as.factor(data6$id)
data6$word_id <- as.factor(data6$word_id)
head(data6)


# try the modeling 
library(lme4)
library(lmerTest)
library(optimx)
library(emmeans)
library(plyr)

data6$logfreq <- log(data6$KF)
levels(data6$word_part)
levels(data6$word_part)[levels(data6$word_part)=="vot_per"] <- "vot"
levels(data6$word_part)[levels(data6$word_part)=="rest_per"] <- "rest"

# model with logfreq
contrasts(data6$word_part) <- c(-.5, .5)


model.1=lmer(per_increase~word_part*logfreq+
               (1|word)+(1+word_part+logfreq|id),
             control=lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=1e5)),
             data=data6,REML=FALSE)

summary(model.1)

# Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: per_increase ~ word_part * logfreq + (1 | word) + (1 + word_part +      logfreq | id)
#    Data: data6
# Control: lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e+05))
# 
#      AIC      BIC   logLik deviance df.resid 
#  17420.1  17486.2  -8698.0  17396.1     1826 
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -3.3941 -0.4767 -0.0730  0.3328 11.5394 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev. Corr       
#  word     (Intercept)   0.0000  0.0000             
#  id       (Intercept)  42.1787  6.4945             
#           word_part1  227.4626 15.0819  -0.83      
#           logfreq       0.5016  0.7083   0.57 -0.68
#  Residual             723.6034 26.8999             
# Number of obs: 1838, groups:  word, 39; id, 24
# 
# Fixed effects:
#                     Estimate Std. Error        df t value Pr(>|t|)   
# (Intercept)           6.2710     1.6828   23.8093   3.727  0.00106 **
# word_part1          -12.2489     3.7114   37.2231  -3.300  0.00214 **
# logfreq              -0.1564     0.3240   24.1473  -0.483  0.63373   
# word_part1:logfreq    0.1589     0.5800 1767.0419   0.274  0.78417   
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr) wrd_p1 logfrq
# word_part1  -0.544              
# logfreq     -0.240 -0.252       
# wrd_prt1:lg  0.000 -0.444  0.000
# convergence code: 0
# boundary (singular) fit: see ?isSingular

emmeans(model.1, pairwise~word_part,adjust="tukey",options=get_emm_option("emmeans"))



# model with categorical freq
data6$freq <- as.factor(data6$freq)
contrasts(data6$word_part) <- c(-.5, .5)

model.2=lmer(per_increase~word_part*freq+
               (1|word)+(1+word_part+freq|id),
             control = lmerControl(
               optimizer ='optimx', optCtrl=list(method='nlminb')),
             data=data6,REML=FALSE)

summary(model.2)

# Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: per_increase ~ word_part * freq + (1 | word) + (1 + word_part +      freq | id)
#    Data: data6
# Control: lmerControl(optimizer = "optimx", optCtrl = list(method = "nlminb"))
# 
#      AIC      BIC   logLik deviance df.resid 
#  17418.8  17485.0  -8697.4  17394.8     1826 
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -3.3510 -0.4786 -0.0798  0.3391 11.5557 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev. Corr       
#  word     (Intercept)   0.000   0.000              
#  id       (Intercept)  40.860   6.392              
#           word_part1  227.373  15.079   -0.87      
#           freq1         8.827   2.971    0.92 -0.79
#  Residual             723.799  26.904              
# Number of obs: 1838, groups:  word, 39; id, 24
# 
# Fixed effects:
#                   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)         6.0712     1.5839   23.8177   3.833 0.000811 ***
# word_part1        -12.3021     3.5637   31.6789  -3.452 0.001599 ** 
# freq1              -0.4715     1.3945   23.5690  -0.338 0.738249    
# word_part1:freq1    0.9875     2.5115 1766.7610   0.393 0.694240    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr) wrd_p1 freq1 
# word_part1  -0.620              
# freq1       -0.036 -0.298       
# wrd_prt1:f1  0.000 -0.360  0.000
# convergence code: 0
# boundary (singular) fit: see ?isSingular

###################################################### plot the word_part increase ####################################################
library(ggplot2)
data8 <- data6
head(data8)

levels(data8$word_part)[levels(data8$word_part)=="vot_per"] <- "VOT"
levels(data8$word_part)[levels(data8$word_part)=="rest_per"] <- "REST"

data_plot <- aggregate(data8$per_increase,by=list(data8$word_part),FUN=function(x) c(mean=mean(x),sd=sd(x),n=length(x)))
data_plot <- do.call(data.frame,data_plot)

data_plot$se <- data_plot$x.sd/sqrt(data_plot$x.n)

limit <- aes(ymin=data_plot$mean-data_plot$se,
             ymax=data_plot$mean+data_plot$se)
colnames(data_plot) <- c("word_part","mean","sd","n","se")

ggplot(data_plot,aes(x=word_part,y=mean,fill=word_part))+geom_bar(stat="identity",width=.5)+geom_errorbar(limit,width=.2,color="black")+
  theme(axis.text=element_text(size=25),axis.title.x=element_text(size=25),axis.title.y=element_text(size=25))+xlab("")+ 
  ylab("Percentage increases")+theme(legend.position="top")+theme(legend.title = element_text(size=25))+
  theme(legend.text = element_text(size=25))+
  guides(fill=guide_legend(title="Word part"))
                                   
 
####################################################### plot with the log_frequency #####################################################
data7 <- subset(data6,word_part=="vot")
nrow(data7)
ggplot(data7,aes(logfreq,per_increase))+geom_point()+geom_smooth(method ="loess")+theme(axis.text=element_text(size=20),axis.title.x=element_text(size=20),axis.title.y=element_text(size=20))+xlab("Log lexical frequency")+ 
  ylab("Percentage increases")


####################################################### plot with lexical frequency ######################################################

data9 <- data6
head(data9)
data9 <- data9[,c(5,6,7,8)]
nrow(data9) # 1838

data10 <- subset(data9,word_part=='vot_per')
nrow(data10) # 919
colnames(data10)[4]='VOT_Increase'

data11 <- subset(data9,word_part=='rest_per')
nrow(data11) # 919
colnames(data11)[4]='Rest_Increase'

data12 <- merge(data10,data11,by=1:2)
head(data12)
nrow(data12)
data12 <- data12[,c(1,4,6)]
colnames(data12) <- c('Frequency','VOT_increase','Rest_increase')
data12$Frequency <- as.factor(data12$Frequency)


levels(data12$Frequency)[levels(data12$Frequency)==1] <- 'HIGH'
levels(data12$Frequency)[levels(data12$Frequency)==0] <- 'LOW'

library(ggpubr)

ggscatterhist(
  data12, x = "Rest_increase", y = "VOT_increase",
  color = "Frequency", size = 3, alpha = 0.6,
  palette = c("#00AFBB", "#FC4E07"),
  margin.plot = "boxplot",
  ggtheme = theme_bw(),
  xlab='REST percentage changes',ylab='VOT percentage changes',
  font.xtickslab=25,
  font.ytickslab=25,
  font.x = 25,
  font.y = 25,
  font.legend=25)





