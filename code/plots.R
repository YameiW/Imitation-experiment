library(lme4)
library(lmerTest)
library(plyr)
library(reshape)
library(ggplot2)
library(moments) # for skewness and kurtosis
library(ggrepel)
library(ggsignif)
dataset <- read.csv("dataset_1.csv")
names(dataset) <- c("word","block","id","cond","order","word_duration","vot","mem_1","mem_2")
dataset$id <- as.factor(dataset$id)
dataset$order <- as.numeric(dataset$order)
dataset$word_rest <- dataset$word_duration-dataset$vot

dataset$word_rest=dataset$word_rest/1000

contrasts(dataset$cond) <- c(.5,-.5)
contrasts(dataset$block) <- c(-.5,.5)

model.final <- lmer(vot~cond+block+word_rest+order
                    +(1+cond+block|word)+(1+block+word_rest+order|id),
                    control=lmerControl(optimizer = "bobyqa",
                                        optCtrl = list(maxfun=1e5)),
                    data=dataset,REML=FALSE)

summary(model.final)

# plot for conds
data_1 <- aggregate(dataset$vot,by=list(dataset$cond),FUN=function(x) c(mean=mean(x),sd=sd(x),n=length(x)))
data_1 <- do.call(data.frame, data_1)
data_1$se <- c(23.12673/sqrt(1990.00000),19.63133/sqrt(1905.00000))

colnames(data_1) <- c("cond","mean","sd","n","se")
data_1$cond <- revalue(data_1$cond,c("g1"="meaning","g2"="no meaning"))

data_1 <- data_1[order(data_1$cond),]

limits1 <- aes(ymin=data_1$mean-data_1$se,
              ymax=data_1$mean+data_1$se)

ggplot(data_1,aes(x=cond,y=mean,fill=cond))+geom_bar(stat="identity",width=.5)+geom_errorbar(limits1,width=.2,color="black")+
  scale_y_continuous(breaks = seq(0,85,10))+
  labs(x="Condtions",y="Mean VOT (ms)",fill="Conditions")+
  theme(legend.position="none")+
  ylim(0,85)

# plot for block
data_2 <- aggregate(dataset$vot,by=list(dataset$block),FUN=function(x) c(mean=mean(x),sd=sd(x),n=length(x)))
data_2 <- do.call(data.frame, data_2)
data_2$se <- data_2$x.sd/sqrt(data_2$x.n)
colnames(data_2) <- c("block","mean","sd","n","se")
data_2$block <- revalue(data_2$block,c("a"="baseline","b"="post-shadowing"))

data_2 <- data_2[order(data_2$block),]

limits2 <- aes(ymin=data_2$mean-data_2$se,
              ymax=data_2$mean+data_2$se)
anno="p=0.00313 **"

ggplot(data_2,aes(x=block,y=mean,fill=block))+geom_bar(stat="identity",width=.5)+geom_errorbar(limits2,width=.2,color="black")+
  scale_y_continuous(breaks = seq(0,85,10))+
  labs(x="Blocks",y="Mean VOT (ms)",fill="Blocks")+
  theme(legend.position="none")+
  ylim(0,85)+ geom_signif(annotation=formatC(anno, digits=1),
                          y_position=80, xmin=1, xmax=2, 
                          tip_length = c(0.2, 0.1))

  
# plot for interaction between cond and block
data_3 <- aggregate(dataset$vot,by=list(dataset$block,dataset$cond),FUN=function(x) c(mean=mean(x),sd=sd(x),n=length(x)))
data_3 <- do.call(data.frame, data_3)
data_3$se <- data_3$x.sd/sqrt(data_3$x.n)
colnames(data_3) <- c("block","cond","mean","sd","n","se")
data_3$block <- revalue(data_3$block,c("a"="baseline","b"="post-shadowing"))
data_3$cond <- revalue(data_3$cond,c("g1"="meaning","g2"="no meaning"))

ggplot(data_3, aes(x=cond, y=mean, fill=block)) +
  geom_bar(position=position_dodge(), stat="identity", colour='black') +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,position=position_dodge(.9))+
  scale_y_continuous(breaks = seq(0,85,10))+
  ylim(0,85)+
  labs(x="Conditions",y="Mean VOT (ms)",fill="Blocks")+
  theme(legend.position="top")


# interaction plot
head(data_3)
ggplot(data_3)+aes(x=block, y=mean,color=cond)+geom_line(aes(group=cond))+geom_point()+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.05)+
  labs(x="Blocks",y="Mean VOT (ms)",color="Conditions")+
  theme(legend.position="top")


# plot for individual variability for different conditions 
data_4 <- dataset[,c(2,3,4,7)]
data_4 <- cast(data_4,id+cond~block,mean)
colnames(data_4) <- c("participant","cond","baseline","post_shadowing")
data_4$cond <- revalue(data_4$cond,c("g1"="meaning","g2"="no meaning"))

ggplot(data_4,aes(x=baseline, y=post_shadowing,color=cond))+ geom_point(size=2)+
  geom_abline(slope=1,intercept=0,linetype="dashed")+
  geom_text(label=data_4$participant,color="black", size=3,hjust=0, vjust=0)+
  theme(aspect.ratio = 1)+
  xlim(40,130)+
  ylim(40,130)+
  labs(x="Baseline Mean VOTs (ms)",y="Post-shadowing Mean VOTs (ms)",color="Conditions")

colnames(data_4)[1] <- "id"
data_5 <- melt(data_4,id="id")
summary(data_5)

summary(data_4)

data_4$change <- (data_4$post_shadowing-data_4$baseline)/data_4$baseline
mean(data_4$change)
median(data_4$change)
skewness(data_4$change)
kurtosis(data_4$change)

data_6 <- aggregate(dataset$word_duration,by=list(dataset$id),FUN=function(x) c(mean=mean(x),sd=sd(x),n=length(x)))
summary(data_6)


# plot for individual words
data_7 <- aggregate(dataset$vot, by=list(dataset$word,dataset$block),FUN=mean)
colnames(data_7) <- c("word","block","mean VOT")
data_7 <- cast(data_7,word~block)
colnames(data_7) <- c("word","baseline","post_shadowing")

ggplot(data_7,aes(x=baseline, y=post_shadowing))+ geom_point(size=2,color="red")+
  geom_abline(slope=1,intercept=0,linetype="dashed")+
  geom_text_repel(label=data_7$word,color="black",size=3)+
  theme(aspect.ratio = 1)+
  xlim(40,130)+
  ylim(40,130)+
  labs(x="Baseline Mean VOTs (ms)",y="Post-shadowing Mean VOTs (ms)",color="Conditions")

summary(data_7)
data_7$change=(data_7$post_shadowing-data_7$baseline)/data_7$baseline
mean(data_7$change)
median(data_7$change)
skewness(data_7$change)
kurtosis(data_7$change)
hist(data_7$change)


data_8 <- aggregate(dataset$vot, by=list(dataset$word,dataset$block,dataset$cond),FUN=mean)
colnames(data_8) <- c("word","block","cond","vot")
data_8 <- cast(data_8,word+cond~block, mean)
data_8$cond <- revalue(data_8$cond,c("g1"="meaning","g2"="no meaning"))
colnames(data_8) <- c("word","cond","baseline","post_shadowing")

ggplot(data_8,aes(x=baseline, y=post_shadowing))+ geom_point(size=2,color="red")+
  geom_abline(slope=1,intercept=0,linetype="dashed")+
  theme(aspect.ratio = 1)+
  xlim(40,100)+
  ylim(40,100)+
  labs(x="Baseline Mean VOTs (ms)",y="Post-shadowing Mean VOTs (ms)",color="Conditions")+
  geom_text_repel(label=data_8$word,color="black",size=3)+
  facet_grid(.~cond)

data_8$change <-(data_8$post_shadowing-data_8$baseline)/data_8$baseline 
mean(data_8$change)
median(data_8$change)
skewness(data_8$change)
kurtosis(data_8$change)
hist(data_8$change)
summary(data_8)




