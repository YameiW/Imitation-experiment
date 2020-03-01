library(lme4)
library(lmerTest)
library(optimx)
library(emmeans)
library(reshape2)

dataset <- read.csv("dataset_1.csv")
names(dataset) <- c("word","block","id","cond","order","word_duration","vot","mem_1","mem_2")
dataset$id <- as.factor(dataset$id)
dataset$order <- as.numeric(dataset$order)
dataset$word_rest <- dataset$word_duration-dataset$vot

# try to convert word_rest into seconds
# otherwise 
# Warning messages:
#   1: Some predictor variables are on very different scales: consider rescaling 
#   2: Some predictor variables are on very different scales: consider rescaling 
   dataset$word_rest=dataset$word_rest/1000

# Model failed to converge with max|grad| = 11.4568 (tol = 0.002, component 1)
# Model is nearly unidentifiable: very large eigenvalue
# - Rescale variables?
#   Model is nearly unidentifiable: large eigenvalue ratio
# - Rescale variables?
# set the contrast
contrasts(dataset$cond) <- c(.5,-.5)
contrasts(dataset$block) <- c(-.5,.5)
# dataset <- within(dataset,block <- relevel(block,ref="b")) change the reference level into post-shadowing level

################################################ modeling ########################################
# model.1=lmer(vot ~cond*block*word_rest*order+
#                (1+cond+block+order|word)+(1+block+word_rest+order|id),
#                control=lmerControl(optimizer = "bobyqa",
#                                    optCtrl = list(maxfun=2e5)),
#                data=dataset,REML=FALSE)
# Model failed to converge with 1 negative eigenvalue: -1.4e+02 

model.1=lmer(vot~cond*block*word_rest*order+
                               (1+cond+block|word)+(1+block+word_rest+order|id),
                               control=lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=1e5)),
                               data=dataset,REML=FALSE)

# Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: vot ~ cond * block * word_rest * order + (1 + cond + block |      word) + (1 + block + word_rest + order | id)
#    Data: dataset
# Control: lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e+05))
# 
#      AIC      BIC   logLik deviance df.resid 
#  32920.8  33127.6 -16427.4  32854.8     3862 
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -4.2788 -0.6209 -0.0296  0.5700 14.1372 
# 
# Random effects:
#  Groups   Name        Variance  Std.Dev. Corr             
#  id       (Intercept) 185.26255 13.6111                   
#           block1       69.20480  8.3189  -0.30            
#           word_rest    90.94374  9.5364  -0.46  0.48      
#           order         0.01547  0.1244  -0.67  0.36  0.96
#  word     (Intercept)  62.21043  7.8874                   
#           cond1         0.96041  0.9800   0.09            
#           block1        3.44076  1.8549   0.76 -0.58      
#  Residual             238.57200 15.4458                   
# Number of obs: 3895, groups:  id, 49; word, 40
# 
# Fixed effects:
#                                Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                   6.434e+01  4.223e+00  3.342e+02  15.235   <2e-16 ***
# cond1                        -7.082e+00  6.001e+00  1.256e+02  -1.180   0.2402    
# block1                        3.088e+00  4.982e+00  1.783e+02   0.620   0.5362    
# word_rest                     1.907e+01  7.801e+00  7.784e+02   2.445   0.0147 *  
# order                         1.503e-02  7.251e-02  3.205e+02   0.207   0.8359    
# cond1:block1                  7.876e+00  8.984e+00  1.923e+03   0.877   0.3808    
# cond1:word_rest               1.382e+01  1.053e+01  2.949e+02   1.313   0.1903    
# block1:word_rest              1.612e+00  1.090e+01  1.860e+02   0.148   0.8826    
# cond1:order                   1.194e-01  1.076e-01  4.180e+02   1.110   0.2678    
# block1:order                 -4.318e-03  1.098e-01  2.321e+02  -0.039   0.9686    
# word_rest:order               2.737e-02  1.503e-01  3.828e+02   0.182   0.8555    
# cond1:block1:word_rest       -1.637e+01  1.933e+01  2.149e+03  -0.847   0.3973    
# cond1:block1:order           -3.689e-01  1.926e-01  1.663e+03  -1.915   0.0556 .  
# cond1:word_rest:order        -2.226e-01  2.268e-01  5.447e+02  -0.982   0.3266    
# block1:word_rest:order        2.389e-02  2.477e-01  2.756e+02   0.096   0.9232    
# cond1:block1:word_rest:order  7.609e-01  4.302e-01  1.480e+03   1.769   0.0771 .  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


## four way interaction (1)
model.2 <- update(model.1,.~.-cond:block:word_rest:order)
anova(model.1,model.2) # 0.07909
model.1 <- update(model.1,.~.-cond:block:word_rest:order)

## three way interaction (4)
model.3 <- update(model.1,.~.-cond:block:word_rest)
anova(model.1,model.3) # 0.203
model.4 <- update(model.1,.~.-cond:block:order)
anova(model.1,model.4) # 0.4095
model.5 <- update(model.1,.~.-block:word_rest:order)
anova(model.1,model.5) # 0.6736
model.6 <- update(model.1,.~.-cond:word_rest:order)
anova(model.1,model.6) # 0.2967
model.1 <- update(model.1,.~.-cond:block:word_rest-cond:block:order-
                    block:word_rest:order-cond:word_rest:order)

## two way interaction (6)
model.7 <- update(model.1,.~.-cond:word_rest)
anova(model.1,model.7) # 0.5489
model.8 <- update(model.1,.~.-word_rest:order)
anova(model.1,model.8) # 0.9203
model.9 <- update(model.1,.~.-cond:block)
anova(model.1,model.9) # 0.8032
model.10 <- update(model.1,.~.-cond:order)
anova(model.1,model.10) # 0.6511
model.11 <- update(model.1,.~.-block:word_rest)
anova(model.1,model.11) # 0.546
model.12 <- update(model.1,.~.-block:order)
anova(model.1,model.12) # 0.7656

## final modal
model.final <- lmer(vot~cond+block+word_rest+order
                    +(1+cond+block|word)+(1+block+word_rest+order|id),
                    control=lmerControl(optimizer = "bobyqa",
                                        optCtrl = list(maxfun=1e5)),
                    data=dataset,REML=FALSE)

summary(model.final)


# Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: vot ~ cond + block + word_rest + order + (1 + cond + block |      word) + (1 + block + word_rest + order | id)
#    Data: dataset
# Control: lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e+05))
# 
#      AIC      BIC   logLik deviance df.resid 
#  32906.5  33044.4 -16431.3  32862.5     3873 
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -4.2667 -0.6155 -0.0329  0.5684 14.1963 
# 
# Random effects:
#  Groups   Name        Variance  Std.Dev. Corr             
#  id       (Intercept) 184.36811 13.5782                   
#           block1       70.51813  8.3975  -0.29            
#           word_rest    98.51641  9.9255  -0.45  0.47      
#           order         0.01514  0.1231  -0.67  0.36  0.92
#  word     (Intercept)  62.45060  7.9026                   
#           cond1         1.35939  1.1659  -0.08            
#           block1        2.86777  1.6935   0.75 -0.72      
#  Residual             238.95417 15.4581                   
# Number of obs: 3895, groups:  id, 49; word, 40
# 
# Fixed effects:
#              Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)  63.86643    3.14157 144.35727  20.329  < 2e-16 ***
# cond1        -0.32664    3.21274  47.92661  -0.102  0.91944    
# block1        4.11187    1.32613  51.52200   3.101  0.00313 **
# word_rest    20.10838    4.67351 264.95117   4.303 2.38e-05 ***
# order         0.02751    0.02540  86.57791   1.083  0.28178    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#           (Intr) cond1  block1 wrd_rs
# cond1     -0.013                     
# block1    -0.121 -0.009              
# word_rest -0.685  0.001  0.153       
# order     -0.456  0.001  0.243  0.192
# convergence code: 0
# boundary (singular) fit: see ?isSingular


emmip(model.final, ~block|cond)

emmeans(model.final, list(pairwise~block|cond),adjust="tukey",options=get_emm_option("emmeans"))

# $`emmeans of block | cond`
# cond = g1:
#   block emmean   SE   df lower.CL upper.CL
# a       71.7 2.62 73.1     66.5     76.9
# b       75.8 2.70 78.4     70.5     81.2
# 
# cond = g2:
#   block emmean   SE   df lower.CL upper.CL
# a       72.1 2.65 72.4     66.8     77.3
# b       76.2 2.75 78.2     70.7     81.6
# 
# Degrees-of-freedom method: satterthwaite 
# Confidence level used: 0.95 
# 
# $`pairwise differences of block | cond`
# cond = g1:
#   contrast estimate   SE   df t.ratio p.value
# a - b       -4.11 1.33 51.5 -3.101  0.0031 
# 
# cond = g2:
#   contrast estimate   SE   df t.ratio p.value
# a - b       -4.11 1.33 51.5 -3.101  0.0031 
# 
# Degrees-of-freedom method: satterthwaite 



model.final.2 <- lmer(vot~cond+block+word_rest+order+cond:block
                    +(1+cond+block|word)+(1+block+word_rest+order|id),
                    control=lmerControl(optimizer = "bobyqa",
                                        optCtrl = list(maxfun=1e5)),
                    data=dataset,REML=FALSE)

summary(model.final.2)


# Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: vot ~ cond + block + word_rest + order + cond:block + (1 + cond +  
#     block | word) + (1 + block + word_rest + order | id)
#    Data: dataset
# Control: lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e+05))
# 
#      AIC      BIC   logLik deviance df.resid 
#  32908.3  33052.4 -16431.1  32862.3     3872 
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -4.2702 -0.6168 -0.0346  0.5694 14.1926 
# 
# Random effects:
#  Groups   Name        Variance  Std.Dev. Corr             
#  id       (Intercept) 184.50751 13.5834                   
#           block1       70.47140  8.3947  -0.29            
#           word_rest    98.24004  9.9116  -0.45  0.49      
#           order         0.01515  0.1231  -0.67  0.37  0.92
#  word     (Intercept)  62.42502  7.9010                   
#           cond1         1.32882  1.1527  -0.07            
#           block1        2.85782  1.6905   0.75 -0.71      
#  Residual             238.96533 15.4585                   
# Number of obs: 3895, groups:  id, 49; word, 40
# 
# Fixed effects:
#              Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)   63.8822     3.1419 144.2677  20.332  < 2e-16 ***
# cond1         -0.3055     3.2130  47.9223  -0.095  0.92465    
# block1         4.1224     1.3259  51.4489   3.109  0.00306 ** 
# word_rest     20.0729     4.6730 265.1082   4.296 2.45e-05 ***
# order          0.0275     0.0254  86.4500   1.083  0.28201    
# cond1:block1  -1.1283     2.4592  49.0169  -0.459  0.64840    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr) cond1  block1 wrd_rs order 
# cond1       -0.012                            
# block1      -0.124 -0.008                     
# word_rest   -0.685  0.000  0.156              
# order       -0.456  0.001  0.247  0.193       
# cond1:blck1 -0.013 -0.017 -0.019  0.019  0.001
# convergence code: 0
# boundary (singular) fit: see ?isSingular

emm <- emmeans(model.final.2, pairwise~block|cond,adjust="tukey",options=get_emm_option("emmeans"))


# $`emmeans of block | cond`
# cond = g1:
#   block emmean   SE   df lower.CL upper.CL
# a       72.0 2.69 67.9     66.6     77.4
# b       75.6 2.77 73.7     70.1     81.1
# 
# cond = g2:
#   block emmean   SE   df lower.CL upper.CL
# a       71.8 2.73 67.1     66.3     77.2
# b       76.4 2.82 73.4     70.8     82.1
# 
# Degrees-of-freedom method: satterthwaite 
# Confidence level used: 0.95 
# 
# $`pairwise differences of block | cond`
# cond = g1:
#   contrast estimate   SE   df t.ratio p.value
# a - b       -3.56 1.79 53.3 -1.987  0.0521 
# 
# cond = g2:
#   contrast estimate   SE   df t.ratio p.value
# a - b       -4.69 1.83 53.3 -2.567  0.0131 
# 
# Degrees-of-freedom method: satterthwaite

summary(emm,side="<")
# cond = g1:
#   block emmean   SE   df lower.CL upper.CL
# a       72.0 2.69 67.9     -Inf     76.5
# b       75.6 2.77 73.7     -Inf     80.2
# 
# cond = g2:
#   block emmean   SE   df lower.CL upper.CL
# a       71.8 2.73 67.1     -Inf     76.3
# b       76.4 2.82 73.4     -Inf     81.1
# 
# Degrees-of-freedom method: satterthwaite 
# Confidence level used: 0.95 
# 
# $contrasts
# cond = g1:
#   contrast estimate   SE   df t.ratio p.value
# a - b       -3.56 1.79 53.3 -1.987  0.0261 
# 
# cond = g2:
#   contrast estimate   SE   df t.ratio p.value
# a - b       -4.69 1.83 53.3 -2.567  0.0065 
# 
# Degrees-of-freedom method: satterthwaite 
# P values are left-tailed 


########################################### model about word_rest and block ###############################################

# model.block <- lmer(word_rest~block*cond*order +(1+block+cond|word)+(1+block+order|id),
#                     control=lmerControl(optimizer = "bobyqa",
#                                         optCtrl = list(maxfun=1e5)),
#                     data=dataset,REML=FALSE)  

model.block <- lmer(word_rest~block*cond*order +(1+block+cond|word)+(1+block+order|id),
                     control=lmerControl(optimizer = "nloptwrap",calc.derivs = FALSE),
                     data=dataset, REML=FALSE)  

summary(model.block)

## three way interaction
model.block.1 <- lmer(word_rest~block+cond+order+block:cond+cond:order+block:order +(1+block+cond|word)+(1+block+order|id),
                      control=lmerControl(optimizer = "nloptwrap",calc.derivs = FALSE),
                      data=dataset,REML=FALSE)  

anova(model.block,model.block.1) # 0.3076

model.block <- lmer(word_rest~block+cond+order+block:cond+cond:order+block:order +(1+block+cond|word)+(1+block+order|id),
                    control=lmerControl(optimizer = "nloptwrap",calc.derivs = FALSE),
                    data=dataset,REML=FALSE) 

## two way interaction

model.block.2 <- lmer(word_rest~block+cond+order+block:cond+cond:order +(1+block+cond|word)+(1+block+order|id),
                      control=lmerControl(optimizer = "nloptwrap",calc.derivs = FALSE),
                      data=dataset,REML=FALSE)  
                                       
anova(model.block,model.block.2) # 1


model.block.3 <- lmer(word_rest~block+cond+order+block:cond+block:order +(1+block+cond|word)+(1+block+order|id),
                      control=lmerControl(optimizer = "nloptwrap",calc.derivs = FALSE),
                      data=dataset,REML=FALSE) 

anova(model.block, model.block.3) # 0.2181

model.block.4 <- lmer(word_rest~block+cond+order+cond:order+block:order +(1+block+cond|word)+(1+block+order|id),
                      control=lmerControl(optimizer = "bobyqa",
                      optCtrl = list(maxfun=2e5)),
                      data=dataset,REML=FALSE) 

anova(model.block, model.block.4) #0.8751


model.block.final <- lmer(word_rest~cond+block+order+(1+block+cond|word)+(1+block+order|id),
                          control = lmerControl(
                            optimizer ='optimx', optCtrl=list(method='nlminb')),
                          data=dataset,REML=FALSE)


summary(model.block.final)

# Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: word_rest ~ cond + block + order + (1 + block + cond | word) +      (1 + block + order | id)
#    Data: dataset
# Control: lmerControl(optimizer = "optimx", optCtrl = list(method = "nlminb"))
# 
#      AIC      BIC   logLik deviance df.resid 
# -11188.7 -11082.1   5611.3 -11222.7     3878 
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -4.2121 -0.6066 -0.0161  0.5611  5.9187 
# 
# Random effects:
#  Groups   Name        Variance  Std.Dev.  Corr       
#  id       (Intercept) 1.939e-03 0.0440379            
#           block1      6.430e-04 0.0253570 -0.28      
#           order       1.694e-07 0.0004116 -0.34  0.15
#  word     (Intercept) 6.937e-03 0.0832879            
#           block1      2.301e-05 0.0047971 -0.96      
#           cond1       5.380e-05 0.0073346  0.53 -0.28
#  Residual             2.844e-03 0.0533283            
# Number of obs: 3895, groups:  id, 49; word, 40
# 
# Fixed effects:
#               Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)  4.433e-01  1.484e-02  6.077e+01  29.875   <2e-16 ***
# cond1        6.407e-03  1.174e-02  4.923e+01   0.546   0.5877    
# block1      -7.244e-03  4.078e-03  5.169e+01  -1.776   0.0816 .  
# order        5.326e-05  8.547e-05  1.078e+02   0.623   0.5345    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#        (Intr) cond1  block1
# cond1   0.038              
# block1 -0.270 -0.005       
# order  -0.222  0.000  0.109
# convergence code: 0
# boundary (singular) fit: see ?isSingular

########################## model without the order #############################################
model.trial <- update(model.final,.~.-order)
summary(model.trial)

# Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: vot ~ cond + block + word_rest + (1 + cond + block | word) +      (1 + block + word_rest + order | id)
#    Data: dataset
# Control: lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e+05))
# 
#      AIC      BIC   logLik deviance df.resid 
#  32905.6  33037.3 -16431.8  32863.6     3874 
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -4.2800 -0.6138 -0.0354  0.5704 14.2079 
# 
# Random effects:
#  Groups   Name        Variance  Std.Dev. Corr             
#  id       (Intercept) 185.88321 13.6339                   
#           block1       70.63963  8.4047  -0.29            
#           word_rest    98.67819  9.9337  -0.46  0.48      
#           order         0.01546  0.1243  -0.67  0.36  0.93
#  word     (Intercept)  61.55945  7.8460                   
#           cond1         1.45879  1.2078  -0.07            
#           block1        3.24183  1.8005   0.72 -0.74      
#  Residual             238.94697 15.4579                   
# Number of obs: 3895, groups:  id, 49; word, 40
# 
# Fixed effects:
#             Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)  65.4511     2.7904 159.5400  23.456  < 2e-16 ***
# cond1        -0.3258     3.2141  47.9650  -0.101   0.9197    
# block1        3.7696     1.2904  53.2792   2.921   0.0051 ** 
# word_rest    19.0632     4.5807 276.8223   4.162 4.22e-05 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#           (Intr) cond1  block1
# cond1     -0.013              
# block1    -0.011 -0.010       
# word_rest -0.684  0.000  0.112
# convergence code: 0
# boundary (singular) fit: see ?isSingular

emmeans(model.trial, pairwise~block|cond,adjust="tukey",options=get_emm_option("emmeans"))

# $emmeans
# cond = g1:
#   block emmean   SE   df lower.CL upper.CL
# a       71.9 2.61 72.7     66.7     77.1
# b       75.7 2.70 78.2     70.3     81.0
# 
# cond = g2:
#   block emmean   SE   df lower.CL upper.CL
# a       72.2 2.65 71.9     66.9     77.5
# b       76.0 2.75 78.0     70.5     81.5
# 
# Degrees-of-freedom method: satterthwaite 
# Confidence level used: 0.95 
# 
# $contrasts
# cond = g1:
#   contrast estimate   SE   df t.ratio p.value
# a - b       -3.77 1.29 53.3 -2.921  0.0051 
# 
# cond = g2:
#   contrast estimate   SE   df t.ratio p.value
# a - b       -3.77 1.29 53.3 -2.921  0.0051 
# 
# Degrees-of-freedom method: satterthwaite 


####################################### high-frequency v.s low-frequency#####################################

dataset <- read.csv("dataset_1.csv")
names(dataset) <- c("word","block","id","cond","order","word_duration","vot","mem_1","mem_2")
dataset$id <- as.factor(dataset$id)
dataset <- dataset[order(dataset$word),]


dataset$freq <- c(rep(1,98),rep(1,98),rep(0,98),rep(0,97),rep(0,98),rep(0,97),rep(0,98),rep(0,97),rep(1,98),rep(1,99),rep(1,98),rep(0,98),
               rep(1,98),rep(0,98),rep(0,98),rep(1,96),rep(1,98),rep(0,98),rep(0,97),rep(0,97),rep(0,97),rep(0,98),rep(1,97),rep(1,98),
               rep(1,97),rep(0,98),rep(0,96),rep(1,98),rep(1,98),rep(0,98),rep(0,97),rep(1,97),rep(1,97),rep(1,97),rep(0,98),rep(1,97),
               rep(1,92),rep(1,97),rep(1,96),rep(1,98))

dataset$KF <- c(rep(91,98),rep(58,98),rep(42,98),rep(1,97),rep(1,98),rep(4,97),rep(1,98),rep(1,97),rep(NA,98),rep(94,99),rep(216,98),rep(4,98),
             rep(113,98),rep(2,98),rep(3,98),rep(53,96),rep(327,98),rep(2,98),rep(4,97),rep(1,97),rep(1,97),rep(1,98),rep(77,97),rep(196,98),
             rep(162,97),rep(5,98),rep(4,96),rep(46,98),rep(395,98),rep(3,98),rep(1,97),rep(222,97),rep(69,97),rep(98,97),rep(2,98),rep(74,97),
             rep(61,92),rep(342,97),rep(438,96),rep(47,98))

dataset$vot_per <- (data$vot/data$word_duration)*100
data_1 <- dataset[,c("word","block","id","cond","order","word_duration","vot","KF","vot_per")]
data_1 <- na.omit(data_1) # 3797    9
data_1$word_id <- paste0(data_1$word,data_1$id)
data_1$word_id <- as.factor(data_1$word_id)
data_1 <- as.data.frame(data_1)
write.csv(data_1,"dataset_2.csv")

data_1 <- read.csv("freq_updated.csv")
nrow(data_1) #3895

data_2 <- aggregate(data_1$vot_per, by=list(data_1$word,data_1$id,data_1$cond,data_1$KF), FUN=diff,na.rm=TRUE)

colnames(data_2) <- c("word","id","cond","KF","vot_change")
data_2 <- data_2[order(data_2$word),]
data_2$logKF <- log(data_2$KF)
data_2 <- na.omit(data_2) # 1885
data_2$vot_change <- as.numeric(unlist(data_2$vot_change))
data_2$id <- as.factor(data_2$id)
data_2$KF <- as.numeric(data_2$KF)
contrasts(data_2$cond) <- c(.5,-.5)
data_2$freq <- ifelse(data_2$KF>=6,1,0)
data_2$freq <- as.factor(data_2$freq)

model.a <- lmer(vot_change~cond*logKF+
              (1+cond|word)+(1+logKF|id),
              control=lmerControl(optimizer = "nloptwrap",calc.derivs = FALSE),
            data=data_2,REML=FALSE)



# > summary(model.a)
# Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: vot_change ~ cond * logKF + (1 + cond | word) + (1 + logKF |      id)
#    Data: data_2
# Control: lmerControl(optimizer = "nloptwrap", calc.derivs = FALSE)
# 
#      AIC      BIC   logLik deviance df.resid 
#  10516.4  10577.3  -5247.2  10494.4     1874 
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -7.7793 -0.5663 -0.0114  0.5696  4.3349 
# 
# Random effects:
#  Groups   Name        Variance  Std.Dev. Corr 
#  id       (Intercept)  1.570461 1.25318       
#           logKF        0.002594 0.05093  1.00 
#  word     (Intercept)  0.132459 0.36395       
#           cond1        0.005565 0.07460  -1.00
#  Residual             14.505229 3.80857       
# Number of obs: 1885, groups:  id, 49; word, 39
# 
# Fixed effects:
#              Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)   0.95673    0.24973  50.92682   3.831 0.000352 ***
# cond1        -0.24416    0.46118  53.98963  -0.529 0.598675    
# logKF        -0.03918    0.04926  39.30251  -0.795 0.431254    
# cond1:logKF   0.04073    0.08265 420.62937   0.493 0.622412    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr) cond1  logKF 
# cond1       -0.037              
# logKF       -0.443  0.026       
# cond1:logKF  0.029 -0.357 -0.056


model.b <- lmer(vot_change~cond*KF+
                  (1+cond|word)+(1+KF|id),
                control=lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=1e5)),
                data=data_2,REML=FALSE)

summary(model.b)

model.c <- lmer(vot_change~cond*freq+
                  (1+cond|word)+(1+freq|id),
                control=lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=1e5)),
                data=data_2,REML=FALSE)
summary(model.c)
emmeans(model.c, pairwise~freq|cond,adjust="tukey",options=get_emm_option("emmeans")) # no significant p
