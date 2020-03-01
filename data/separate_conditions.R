library(lme4)
library(lmerTest)
library(optimx)

dataset <- read.csv("dataset_1.csv")
names(dataset) <- c("word","block","id","cond","order","word_duration","vot","mem_1","mem_2")
dataset$id <- as.factor(dataset$id)
dataset$order <- as.numeric(dataset$order)
dataset$word_rest <- dataset$word_duration-dataset$vot
dataset$word_rest=dataset$word_rest/1000
contrasts(dataset$cond) <- c(.5,-.5)
contrasts(dataset$block) <- c(-.5,.5)

data_meaning <- subset(dataset,cond=="g1")
data_no <- subset(dataset,cond=="g2")

############################################################## model for data_meaning#############################################################
model.1=lmer(vot~block*word_rest*order+
               (1+block|word)+(1+block+word_rest+order|id),
             data=data_meaning,REML=FALSE)

# three way interaction
model.2 <- update(model.1,.~.-block:word_rest:order)
anova(model.1,model.2) # 0.2704
model.1 <- model.2

# two way interaction
model.3 <- update(model.1,.~.-block:word_rest)
anova(model.1,model.3) #0.2594

model.4 <- lmer(vot~block+word_rest+order+block:word_rest+block:order+
                  (1+block|word)+(1+block+word_rest+order|id),
                control=lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun=1e5)),
                data=data_meaning,REML=FALSE)
  
anova(model.1, model.4) #0.6388

model.5 <- lmer(vot~block+word_rest+order+block:word_rest+word_rest:order+
                  (1+block|word)+(1+block+word_rest+order|id),
                control=lmerControl(optimizer = "nloptwrap",calc.derivs = FALSE),
                data=data_meaning,REML=FALSE)

anova(model.1,model.5) # 1

# final model for cond: meaning
model.6 <- lmer(vot~block+word_rest+order+
                  (1+block|word)+(1+block+word_rest+order|id),
                control=lmerControl(optimizer = "nloptwrap",calc.derivs = FALSE),
                data=data_meaning,REML=FALSE)

summary(model.6)
# Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: vot ~ block + word_rest + order + (1 + block | word) + (1 + block +      word_rest + order | id)
#    Data: data_meaning
# Control: lmerControl(optimizer = "nloptwrap", calc.derivs = FALSE)
# 
#      AIC      BIC   logLik deviance df.resid 
#  17040.9  17141.6  -8502.5  17004.9     1972 
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -3.2428 -0.6029 -0.0236  0.5023 13.5484 
# 
# Random effects:
#  Groups   Name        Variance  Std.Dev. Corr             
#  word     (Intercept) 4.682e+01  6.84287                  
#           blockb      3.471e+00  1.86316 0.97             
#  id       (Intercept) 2.620e+02 16.18663                  
#           blockb      8.105e+01  9.00283 -0.35            
#           word_rest   3.498e+02 18.70194 -0.17  0.16      
#           order       6.848e-03  0.08275 -0.56  0.38  0.89
#  Residual             2.609e+02 16.15242                  
# Number of obs: 1990, groups:  word, 40; id, 25
# 
# Fixed effects:
#             Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept) 64.84299    4.48640 42.57070  14.453   <2e-16 ***
# blockb       3.82878    1.96490 27.14477   1.949   0.0618 .  
# word_rest   13.62020    7.01355 46.98227   1.942   0.0581 .  
# order        0.02467    0.02976 51.60536   0.829   0.4109    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#           (Intr) blockb wrd_rs
# blockb    -0.249              
# word_rest -0.569  0.098       
# order     -0.413  0.212  0.260


######################################## model for no meaning #####################################################

model.7=lmer(vot~block*word_rest*order+
               (1+block|word)+(1+block+word_rest+order|id),
             control=lmerControl(optimizer = "nloptwrap",calc.derivs = FALSE),
             data=data_no,REML=FALSE)

# three way interaction
model.8 <- update(model.7,.~.-block:word_rest:order)
anova(model.7,model.8) # 1
model.7 <- model.8

# two way interaction
model.9 <- update(model.7,.~.-block:word_rest)
anova(model.7, model.9) # 0.3194

model.10 <- update(model.7,.~.-word_rest:order)
anova(model.7, model.10) # 0.6818

model.11 <- update(model.7,.~.-block:order)
anova(model.7,model.11) # 1

# final model for cond: no meaning
model.12 <- lmer(vot~block+word_rest+order+
                  (1+block|word)+(1+block+word_rest+order|id),
                control=lmerControl(optimizer = "nloptwrap",calc.derivs = FALSE),
                data=data_no,REML=FALSE)

summary(model.12)
# Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: vot ~ block + word_rest + order + (1 + block | word) + (1 + block +      word_rest + order | id)
#    Data: data_no
# Control: lmerControl(optimizer = "nloptwrap", calc.derivs = FALSE)
# 
#      AIC      BIC   logLik deviance df.resid 
#  15943.9  16043.8  -7953.9  15907.9     1887 
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -4.3736 -0.6293 -0.0226  0.6252  6.2621 
# 
# Random effects:
#  Groups   Name        Variance  Std.Dev. Corr             
#  word     (Intercept)  54.56068  7.3865                   
#           blockb        0.87160  0.9336  1.00             
#  id       (Intercept) 261.03167 16.1565                   
#           blockb       58.46807  7.6464  -0.71            
#           word_rest   103.27650 10.1625  -0.63  0.73      
#           order         0.02435  0.1560  -0.76  0.39  0.69
#  Residual             214.78125 14.6554                   
# Number of obs: 1905, groups:  word, 40; id, 24
# 
# Fixed effects:
#              Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)  63.46155    4.60557  53.03433  13.779   <2e-16 ***
# blockb        4.44663    1.70661  24.27462   2.606   0.0154 *  
# word_rest    16.07376    6.64049 134.61396   2.421   0.0168 *  
# order         0.02709    0.03932  37.65862   0.689   0.4951    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#           (Intr) blockb wrd_rs
# blockb    -0.487              
# word_rest -0.718  0.229       
# order     -0.564  0.302  0.173

######################################### new models ###################################################

model.a <- lmer(vot~cond*block*word_rest+
               (1+cond+block|word)+(1+block+word_rest|id),
             control=lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=1e5)),
             data=dataset,REML=FALSE)

# three-way interaction
model.b <- update(model.a,.~.-cond:block:word_rest,control=lmerControl(optimizer = "nloptwrap",calc.derivs = FALSE))
anova(model.a,model.b) # 0.2133
model.a <- model.b

# two-way interaction
model.c <- update(model.a,.~.-cond:block,control=lmerControl(optimizer = "nloptwrap",calc.derivs = FALSE))
anova(model.a,model.c) #0.7975

model.d <- update(model.a,.~.-cond:word_rest,control=lmerControl(optimizer = "nloptwrap",calc.derivs = FALSE))
anova(model.a,model.d) #0.6539

model.e <- update(model.a,.~.-block:word_rest,control=lmerControl(optimizer = "nloptwrap",calc.derivs = FALSE))
anova(model.a,model.e) #0.3273

# final 
model.a <- update(model.a,.~.-block:word_rest-cond:word_rest,control=lmerControl(optimizer = "nloptwrap",calc.derivs = FALSE))
summary(model.a)

# Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: vot ~ cond + block + word_rest + (1 + cond + block | word) +      (1 + block + word_rest | id) + cond:block
#    Data: dataset
# Control: lmerControl(optimizer = "nloptwrap", calc.derivs = FALSE)
# 
#      AIC      BIC   logLik deviance df.resid 
#  32975.2  33088.1 -16469.6  32939.2     3877 
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -4.0018 -0.6228 -0.0308  0.5647 14.3371 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev. Corr       
#  id       (Intercept) 125.123  11.186              
#           block1       68.409   8.271   -0.24      
#           word_rest   154.882  12.445   -0.27  0.51
#  word     (Intercept)  62.729   7.920              
#           cond1         1.448   1.203   -0.07      
#           block1        3.478   1.865    0.73 -0.73
#  Residual             247.116  15.720              
# Number of obs: 3895, groups:  id, 49; word, 40
# 
# Fixed effects:
#              Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)   63.8256     2.8601 156.1979  22.316  < 2e-16 ***
# cond1         -0.3584     3.1776  48.1111  -0.113  0.91066    
# block1         4.1304     1.3191  52.1392   3.131  0.00285 ** 
# word_rest     22.6845     4.8372 249.0198   4.690 4.51e-06 ***
# cond1:block1  -0.9679     2.4760  49.2373  -0.391  0.69755    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr) cond1  block1 wrd_rs
# cond1       -0.012                     
# block1      -0.068 -0.010              
# word_rest   -0.706  0.000  0.189       
# cond1:blck1 -0.017 -0.029 -0.019  0.024

model.aa <-  update(model.a,.~.-block:cond,control=lmerControl(optimizer = "nloptwrap",calc.derivs = FALSE))
summary(model.aa)


# post-hoc
emmeans(model.a, list(pairwise~cond+block),adjust="tukey",options=get_emm_option("emmeans"))

# $`emmeans of cond, block`
# cond block emmean   SE   df lower.CL upper.CL
# g1   a       71.7 2.59 73.5     66.5     76.8
# g2   a       72.1 2.63 72.7     66.8     77.3
# g1   b       75.8 2.69 79.2     70.4     81.1
# g2   b       76.2 2.74 79.0     70.7     81.6
# 
# Degrees-of-freedom method: satterthwaite 
# Confidence level used: 0.95 
# 
# $`pairwise differences of cond, block`
# contrast    estimate   SE   df t.ratio p.value
# g1,a - g2,a   -0.391 3.18 48.1 -0.123  0.9993 
# g1,a - g1,b   -4.122 1.32 52.1 -3.122  0.0151 
# g1,a - g2,b   -4.513 3.45 65.0 -1.307  0.5618 
# g2,a - g1,b   -3.731 3.43 63.5 -1.089  0.6976 
# g2,a - g2,b   -4.122 1.32 52.1 -3.122  0.0151 
# g1,b - g2,b   -0.391 3.18 48.1 -0.123  0.9993 
# 
# Degrees-of-freedom method: satterthwaite 
# P value adjustment: tukey method for comparing a family of 4 estimates 

emmeans(model.aa, list(pairwise~cond+block),adjust="tukey",options=get_emm_option("emmeans"))

# $`emmeans of cond, block`
# cond block emmean   SE   df lower.CL upper.CL
# g1   a       71.7 2.59 73.5     66.5     76.8
# g2   a       72.1 2.63 72.7     66.8     77.3
# g1   b       75.8 2.69 79.2     70.4     81.1
# g2   b       76.2 2.74 79.0     70.7     81.6
# 
# Degrees-of-freedom method: satterthwaite 
# Confidence level used: 0.95 
# 
# $`pairwise differences of cond, block`
# contrast    estimate   SE   df t.ratio p.value
# g1,a - g2,a   -0.391 3.18 48.1 -0.123  0.9993 
# g1,a - g1,b   -4.122 1.32 52.1 -3.122  0.0151 
# g1,a - g2,b   -4.513 3.45 65.0 -1.307  0.5618 
# g2,a - g1,b   -3.731 3.43 63.5 -1.089  0.6976 
# g2,a - g2,b   -4.122 1.32 52.1 -3.122  0.0151 
# g1,b - g2,b   -0.391 3.18 48.1 -0.123  0.9993 
# 
# Degrees-of-freedom method: satterthwaite 
# P value adjustment: tukey method for comparing a family of 4 estimates 

