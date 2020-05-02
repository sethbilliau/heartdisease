Introduction
============

TODO

EDA
===

This is the code appendix for our EDA. The goals of this exercise were
to discover missingness, visualize the distribution of the response and
predictor variables, and assess the predictor variables for obvious
signs of multicollinearity.

Preprocessing
-------------

Read in the data and make note of missing values:

``` r
data_raw = read.csv("data/chd_risk.csv")
summary(data_raw)
```

    ##       age                     education      cigsPerDay        totChol     
    ##  Min.   :32.00   College or Higher : 473   Min.   : 0.000   Min.   :107.0  
    ##  1st Qu.:42.00   High School or GED:1253   1st Qu.: 0.000   1st Qu.:206.0  
    ##  Median :49.00   Some College      : 687   Median : 0.000   Median :234.0  
    ##  Mean   :49.58   Some High School  :1720   Mean   : 9.003   Mean   :236.7  
    ##  3rd Qu.:56.00   NA's              : 105   3rd Qu.:20.000   3rd Qu.:263.0  
    ##  Max.   :70.00                             Max.   :70.000   Max.   :696.0  
    ##                                            NA's   :29       NA's   :50     
    ##      sysBP           diaBP             BMI          heartRate     
    ##  Min.   : 83.5   Min.   : 48.00   Min.   :15.54   Min.   : 44.00  
    ##  1st Qu.:117.0   1st Qu.: 75.00   1st Qu.:23.07   1st Qu.: 68.00  
    ##  Median :128.0   Median : 82.00   Median :25.40   Median : 75.00  
    ##  Mean   :132.4   Mean   : 82.89   Mean   :25.80   Mean   : 75.88  
    ##  3rd Qu.:144.0   3rd Qu.: 89.88   3rd Qu.:28.04   3rd Qu.: 83.00  
    ##  Max.   :295.0   Max.   :142.50   Max.   :56.80   Max.   :143.00  
    ##                                   NA's   :19      NA's   :1       
    ##     glucose           sex             smoker     OnBPMeds    PrevStroke
    ##  Min.   : 40.00   female:2419   Nonsmoker:2144   No  :4061   No :4213  
    ##  1st Qu.: 71.00   male  :1819   Smoker   :2094   Yes : 124   Yes:  25  
    ##  Median : 78.00                                  NA's:  53             
    ##  Mean   : 81.97                                                        
    ##  3rd Qu.: 87.00                                                        
    ##  Max.   :394.00                                                        
    ##  NA's   :388                                                           
    ##   Hyp        Diab      CHD_Risk  
    ##  No :2922   No :4129   No :3594  
    ##  Yes:1316   Yes: 109   Yes: 644  
    ##                                  
    ##                                  
    ##                                  
    ##                                  
    ## 

``` r
length(data_raw$age)
```

    ## [1] 4238

Check for missingness
---------------------

Count number of missing predictors in each variable:

``` r
# Generate the number of missing values for each predictor
apply(is.na(data_raw), 2, sum)
```

    ##        age  education cigsPerDay    totChol      sysBP      diaBP        BMI 
    ##          0        105         29         50          0          0         19 
    ##  heartRate    glucose        sex     smoker   OnBPMeds PrevStroke        Hyp 
    ##          1        388          0          0         53          0          0 
    ##       Diab   CHD_Risk 
    ##          0          0

``` r
missing_preds = c("education", "cigsPerDay", "totChol", "BMI", 
                  "heartRate", "glucose", "OnBPMeds")
```

Visualizing the Response
------------------------

``` r
# Labels for ticks
N_label = paste("No (", round(mean(data_raw$CHD_Risk == "No"), 2)*100, "%)", sep="")
Y_label = paste("Yes (", round(mean(data_raw$CHD_Risk == "Yes"), 2)*100, "%)", sep="")

# Plot bar chart
p <- ggplot(data=data_raw, aes(x=as.factor(CHD_Risk))) +
  geom_histogram(stat="count",fill="red",
                 aes(y=..count../sum(..count..)),
                 alpha = 0.5) + 
    labs(title="Frequency of CHD_Risk") +
    xlab("CHD_Risk") +
    ylab("Frequency") +
    scale_x_discrete(breaks=c("No","Yes"), labels=c(N_label,Y_label)) +
    theme_bw()
```

    ## Warning: Ignoring unknown parameters: binwidth, bins, pad

``` r
p 
```

![](README_files/figure-markdown_github/ResponseBarChart-1.png)

Visualizing the Predictors
--------------------------

Visualize distribution of quantitative predictors conditional on the CHD
outcome:

``` r
quant_preds = c("age", "cigsPerDay", "totChol", "sysBP", 
                  "diaBP", "BMI", "heartRate", "glucose")

make_cond_hist = function(varname) {
  p1 = ggplot(data_raw, aes_string(x=varname)) + 
    geom_histogram(aes(y = ..density..), 
                   fill = "red", alpha = 0.5) + 
    labs(title=paste(varname, "given CHD_Risk")) +
    xlab(varname) +
    ylab("Density") + 
    facet_grid(. ~ CHD_Risk) + 
    theme_bw()
  return(p1)
}

graphs = lapply(quant_preds, make_cond_hist)
figure1 = ggarrange(graphs[[1]], graphs[[2]], graphs[[3]], graphs[[4]], 
          graphs[[5]], graphs[[6]], graphs[[7]], graphs[[8]],
          ncol = 2, nrow = 4)
annotate_figure(figure1,
                top = text_grob("Visualizing Quantitative Predictors given CHD_Risk (prevalence = 0.152)", face = "bold", size = 14)
)
```

![Quantitative EDA](images/quant_eda.jpg)

Visualizing the Qualitative predictors by showing their distributions
conditional on the outcome:

``` r
# Address Categorical predictors
cat_preds = c("education", "sex", "smoker", "OnBPMeds", 
              "PrevStroke", "Hyp", "Diab")
get_cond_prob_table = function(TABLE) { 
  col1 = TABLE[,1] / sum(TABLE[,1])
  col2 = TABLE[,2] / sum(TABLE[,2])
  return(cbind(No=col1, Yes=col2))
}
tab_education = get_cond_prob_table(table(data_raw$education, data_raw$CHD_Risk))
tab_sex = get_cond_prob_table(table(data_raw$sex, data_raw$CHD_Risk))
tab_smoker = get_cond_prob_table(table(data_raw$smoker, data_raw$CHD_Risk))
tab_OnBPMeds = get_cond_prob_table(table(data_raw$OnBPMeds, data_raw$CHD_Risk))
tab_PrevStroke = get_cond_prob_table(table(data_raw$PrevStroke, data_raw$CHD_Risk))
tab_Hyp = get_cond_prob_table(table(data_raw$Hyp, data_raw$CHD_Risk))
tab_Diab = get_cond_prob_table(table(data_raw$Diab, data_raw$CHD_Risk))

tab_prob_Yes = rbind(tab_education, tab_sex, tab_smoker, 
                     tab_OnBPMeds, tab_PrevStroke, tab_Hyp,
                     tab_Diab)
round(tab_prob_Yes,3)
```

    ##                       No   Yes
    ## College or Higher  0.115 0.111
    ## High School or GED 0.316 0.234
    ## Some College       0.171 0.140
    ## Some High School   0.399 0.514
    ## female             0.589 0.467
    ## male               0.411 0.533
    ## Nonsmoker          0.510 0.483
    ## Smoker             0.490 0.517
    ## No                 0.977 0.935
    ## Yes                0.023 0.065
    ## No                 0.996 0.983
    ## Yes                0.004 0.017
    ## No                 0.724 0.495
    ## Yes                0.276 0.505
    ## No                 0.981 0.938
    ## Yes                0.019 0.062

Check for collinearity with GVIF.

``` r
# Check for multicollinearity
mod.vif.lm <- lm(as.numeric(CHD_Risk) ~ ., data=data_raw)
vif(mod.vif.lm)
```

    ##                GVIF Df GVIF^(1/(2*Df))
    ## age        1.397737  1        1.182259
    ## education  1.124453  3        1.019742
    ## cigsPerDay 2.732416  1        1.653002
    ## totChol    1.116842  1        1.056808
    ## sysBP      3.767158  1        1.940917
    ## diaBP      3.000260  1        1.732126
    ## BMI        1.246685  1        1.116550
    ## heartRate  1.095015  1        1.046429
    ## glucose    1.638312  1        1.279966
    ## sex        1.223718  1        1.106218
    ## smoker     2.585357  1        1.607904
    ## OnBPMeds   1.111774  1        1.054407
    ## PrevStroke 1.017647  1        1.008785
    ## Hyp        2.051447  1        1.432287
    ## Diab       1.616622  1        1.271465

Because all values in the last column are less than
$3.1623 = \\sqrt{10}$, there is not significant/strong evidence of
multicollinearity.

Models
======

This is the code appendix for our modeling section.

Begin building model
--------------------

``` r
data = read.csv("data/chd_risk.csv")
# force smoker from categorical to numeric
data$smoker = unclass(data$smoker)
data_na = na.omit(data)
nrow(data_na)
```

    ## [1] 3656

``` r
nrow(data)
```

    ## [1] 4238

Fit a glm with individual predictors, smoker and cigPerDay are
multiplied based on first principles.

``` r
glm_all = glm(CHD_Risk ~ age + education + totChol + sysBP + diaBP + BMI + heartRate + glucose + sex + smoker:cigsPerDay + OnBPMeds + PrevStroke + Hyp + Diab, family = binomial, data = data_na)
summary(glm_all)
```

    ## 
    ## Call:
    ## glm(formula = CHD_Risk ~ age + education + totChol + sysBP + 
    ##     diaBP + BMI + heartRate + glucose + sex + smoker:cigsPerDay + 
    ##     OnBPMeds + PrevStroke + Hyp + Diab, family = binomial, data = data_na)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.9363  -0.5940  -0.4232  -0.2837   2.8682  
    ## 
    ## Coefficients:
    ##                              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                 -8.271975   0.699437 -11.827  < 2e-16 ***
    ## age                          0.061962   0.006742   9.191  < 2e-16 ***
    ## educationHigh School or GED -0.131590   0.177573  -0.741  0.45866    
    ## educationSome College       -0.135339   0.197504  -0.685  0.49319    
    ## educationSome High School    0.059549   0.164592   0.362  0.71750    
    ## totChol                      0.002362   0.001129   2.091  0.03650 *  
    ## sysBP                        0.015474   0.003810   4.061 4.88e-05 ***
    ## diaBP                       -0.004169   0.006441  -0.647  0.51745    
    ## BMI                          0.004476   0.012708   0.352  0.72471    
    ## heartRate                   -0.002966   0.004211  -0.704  0.48120    
    ## glucose                      0.007211   0.002235   3.226  0.00126 ** 
    ## sexmale                      0.534588   0.109948   4.862 1.16e-06 ***
    ## OnBPMedsYes                  0.165180   0.234415   0.705  0.48103    
    ## PrevStrokeYes                0.702450   0.491151   1.430  0.15266    
    ## HypYes                       0.233504   0.138183   1.690  0.09106 .  
    ## DiabYes                      0.025323   0.316114   0.080  0.93615    
    ## smoker:cigsPerDay            0.010064   0.002117   4.753 2.00e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3120.5  on 3655  degrees of freedom
    ## Residual deviance: 2752.1  on 3639  degrees of freedom
    ## AIC: 2786.1
    ## 
    ## Number of Fisher Scoring iterations: 5

Beginning analysis of deviance table. Decided not to do 1-term models
with every predictor because that would take forever, so I used the Wald
test predictors.

``` r
glm0 = glm(CHD_Risk ~ 1, family = binomial, data = data_na)
glm1 = glm(CHD_Risk ~ 1 + age, family = binomial, data = data_na)
glm2 = glm(CHD_Risk ~ 1 + sysBP, family = binomial, data = data_na)
glm3 = glm(CHD_Risk ~ 1 + sex, family = binomial, data = data_na)
glm4 = glm(CHD_Risk ~ 1 + smoker:cigsPerDay, family = binomial, data = data_na)

# age is the best
glm5 = glm(CHD_Risk ~ 1 + age + sysBP, family = binomial, data = data_na)
glm6 = glm(CHD_Risk ~ 1 + age + sex, family = binomial, data = data_na)
glm7 = glm(CHD_Risk ~ 1 + age + smoker:cigsPerDay, family = binomial, data = data_na)

# baseline age + sysBP
glm8 = glm(CHD_Risk ~ 1 + age + sysBP + sex, family = binomial, data = data_na)
glm9 = glm(CHD_Risk ~ 1 + age + sysBP + smoker:cigsPerDay, family = binomial, data = data_na)

# baseline age + sysBP + sex
glm10 = glm(CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay, family = binomial, data = data_na)

# m working model 
m = glm10

summary(glm10)
```

    ## 
    ## Call:
    ## glm(formula = CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay, 
    ##     family = binomial, data = data_na)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.5371  -0.5981  -0.4338  -0.2943   2.8106  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)       -8.251467   0.403019 -20.474  < 2e-16 ***
    ## age                0.068383   0.006346  10.776  < 2e-16 ***
    ## sysBP              0.018783   0.002127   8.829  < 2e-16 ***
    ## sexmale            0.537207   0.105146   5.109 3.24e-07 ***
    ## smoker:cigsPerDay  0.009354   0.002072   4.514 6.37e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3120.5  on 3655  degrees of freedom
    ## Residual deviance: 2785.6  on 3651  degrees of freedom
    ## AIC: 2795.6
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
# anova(m, glm10, test = "Chisq")
```

Continuing analysis of deviance - checking how each of the other 10
predictors adds to age, sex, sysBP, and smoker:cigsPerDay.

``` r
m = glm10
glm11 = glm(CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + education, family = binomial, data = data_na)
glm12 = glm(CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + totChol, family = binomial, data = data_na)
glm13 = glm(CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + diaBP, family = binomial, data = data_na)
glm14 = glm(CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + BMI, family = binomial, data = data_na)
glm15 = glm(CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + heartRate, family = binomial, data = data_na)
glm16 = glm(CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + glucose, family = binomial, data = data_na)
glm17 = glm(CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + OnBPMeds, family = binomial, data = data_na)
glm18 = glm(CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + PrevStroke, family = binomial, data = data_na)
glm19 = glm(CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + Hyp, family = binomial, data = data_na)
glm20 = glm(CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + Diab, family = binomial, data = data_na)

# Should use a for loop here to examine deviance of each model
x = summary(glm20)
print(x$call)
```

    ## glm(formula = CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + 
    ##     Diab, family = binomial, data = data_na)

``` r
print(x$deviance)
```

    ## [1] 2776.522

``` r
anova(glm16, m, test="Chisq")
```

    ## Analysis of Deviance Table
    ## 
    ## Model 1: CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + glucose
    ## Model 2: CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay
    ##   Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
    ## 1      3650     2766.5                          
    ## 2      3651     2785.6 -1  -19.102 1.239e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Baseline model: age, sysBP, sex, smoker:cigsPerDay, glucose.

Now,look for next predictor! Ignore diaBP and heartrate for now.

``` r
m = glm16
glm17 = glm(CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + glucose + education, family = binomial, data = data_na)
glm18 = glm(CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + glucose + totChol, family = binomial, data = data_na)
glm19 = glm(CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + glucose + BMI, family = binomial, data = data_na)
glm20 = glm(CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + glucose + OnBPMeds, family = binomial, data = data_na)
glm21 = glm(CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + glucose + PrevStroke, family = binomial, data = data_na)
glm22 = glm(CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + glucose + Hyp, family = binomial, data = data_na)
glm23 = glm(CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + glucose + Diab, family = binomial, data = data_na)

pred6 <- list(glm17, glm18, glm19, glm20, glm21, glm22, glm23)
for (model_i in pred6){
  print(model_i$call)
  print(model_i$deviance)
}
```

    ## glm(formula = CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + 
    ##     glucose + education, family = binomial, data = data_na)
    ## [1] 2763.365
    ## glm(formula = CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + 
    ##     glucose + totChol, family = binomial, data = data_na)
    ## [1] 2762.476
    ## glm(formula = CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + 
    ##     glucose + BMI, family = binomial, data = data_na)
    ## [1] 2766.02
    ## glm(formula = CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + 
    ##     glucose + OnBPMeds, family = binomial, data = data_na)
    ## [1] 2765.325
    ## glm(formula = CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + 
    ##     glucose + PrevStroke, family = binomial, data = data_na)
    ## [1] 2764.056
    ## glm(formula = CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + 
    ##     glucose + Hyp, family = binomial, data = data_na)
    ## [1] 2763.422
    ## glm(formula = CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + 
    ##     glucose + Diab, family = binomial, data = data_na)
    ## [1] 2766.465

``` r
anova(m, glm18, test = "Chisq")
```

    ## Analysis of Deviance Table
    ## 
    ## Model 1: CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + glucose
    ## Model 2: CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + glucose + 
    ##     totChol
    ##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)  
    ## 1      3650     2766.5                       
    ## 2      3649     2762.5  1   4.0507  0.04415 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Working model is glm18: age + sysBP + sex + smoker:cigsPerDay + glucose
+ totChol. Look for 7th predictors.

``` r
m = glm18
glm24 = glm(CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + glucose + totChol + education, family = binomial, data = data_na)
glm25 = glm(CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + glucose + totChol + OnBPMeds, family = binomial, data = data_na)
glm26 = glm(CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + glucose + totChol + PrevStroke, family = binomial, data = data_na)
glm27 = glm(CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + glucose + totChol + Hyp, family = binomial, data = data_na)
glm28 = glm(CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + glucose + totChol + diaBP, family = binomial, data = data_na)
glm29 = glm(CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + glucose + totChol + heartRate, family = binomial, data = data_na)


pred7 <- list(glm24, glm25, glm26, glm27, glm28, glm29)
for (model_i in pred7){
  print(model_i$call)
  print(model_i$deviance)
}
```

    ## glm(formula = CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + 
    ##     glucose + totChol + education, family = binomial, data = data_na)
    ## [1] 2758.95
    ## glm(formula = CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + 
    ##     glucose + totChol + OnBPMeds, family = binomial, data = data_na)
    ## [1] 2761.409
    ## glm(formula = CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + 
    ##     glucose + totChol + PrevStroke, family = binomial, data = data_na)
    ## [1] 2759.98
    ## glm(formula = CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + 
    ##     glucose + totChol + Hyp, family = binomial, data = data_na)
    ## [1] 2759.479
    ## glm(formula = CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + 
    ##     glucose + totChol + diaBP, family = binomial, data = data_na)
    ## [1] 2762.34
    ## glm(formula = CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + 
    ##     glucose + totChol + heartRate, family = binomial, data = data_na)
    ## [1] 2761.966

``` r
anova(m, glm24, test = "Chisq")
```

    ## Analysis of Deviance Table
    ## 
    ## Model 1: CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + glucose + 
    ##     totChol
    ## Model 2: CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + glucose + 
    ##     totChol + education
    ##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
    ## 1      3649     2762.5                     
    ## 2      3646     2758.9  3   3.5264   0.3174

Interaction terms

``` r
int1 = glm(CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + glucose + totChol + age:sysBP, family = binomial, data = data_na)
int2 = glm(CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + glucose + totChol + age:sex, family = binomial, data = data_na)
int3 = glm(CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + glucose + totChol + age:smoker:cigsPerDay, family = binomial, data = data_na)
int4 = glm(CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + glucose + totChol + age:glucose, family = binomial, data = data_na)
int5 = glm(CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + glucose + totChol + age:totChol, family = binomial, data = data_na)
int6 = glm(CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + glucose + totChol + sysBP:sex, family = binomial, data = data_na)
int7 = glm(CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + glucose + totChol + sysBP:smoker:cigsPerDay, family = binomial, data = data_na)
int8 = glm(CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + glucose + totChol + sysBP:glucose, family = binomial, data = data_na)
int9 = glm(CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + glucose + totChol + sysBP:totChol, family = binomial, data = data_na)
int10 = glm(CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + glucose + totChol + sex:smoker:cigsPerDay, family = binomial, data = data_na)
int11 = glm(CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + glucose + totChol + sex:glucose, family = binomial, data = data_na)
int12 = glm(CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + glucose + totChol + sex:totChol, family = binomial, data = data_na)
int13 = glm(CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + glucose + totChol + smoker:cigsPerDay:glucose, family = binomial, data = data_na)
int14 = glm(CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + glucose + totChol + smoker:cigsPerDay:totChol, family = binomial, data = data_na)
int15 = glm(CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + glucose + totChol + glucose:totChol, family = binomial, data = data_na)

inter1 <- list(int1, int2, int3, int4, int5, int6, int7, int8, int9, int10, int11, int12, int13, int14, int15)
for (model_i in inter1){
  print(model_i$call)
  print(model_i$deviance)
}
```

    ## glm(formula = CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + 
    ##     glucose + totChol + age:sysBP, family = binomial, data = data_na)
    ## [1] 2761.898
    ## glm(formula = CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + 
    ##     glucose + totChol + age:sex, family = binomial, data = data_na)
    ## [1] 2762.233
    ## glm(formula = CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + 
    ##     glucose + totChol + age:smoker:cigsPerDay, family = binomial, 
    ##     data = data_na)
    ## [1] 2761.905
    ## glm(formula = CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + 
    ##     glucose + totChol + age:glucose, family = binomial, data = data_na)
    ## [1] 2762.468
    ## glm(formula = CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + 
    ##     glucose + totChol + age:totChol, family = binomial, data = data_na)
    ## [1] 2760.629
    ## glm(formula = CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + 
    ##     glucose + totChol + sysBP:sex, family = binomial, data = data_na)
    ## [1] 2760.044
    ## glm(formula = CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + 
    ##     glucose + totChol + sysBP:smoker:cigsPerDay, family = binomial, 
    ##     data = data_na)
    ## [1] 2762.39
    ## glm(formula = CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + 
    ##     glucose + totChol + sysBP:glucose, family = binomial, data = data_na)
    ## [1] 2762.476
    ## glm(formula = CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + 
    ##     glucose + totChol + sysBP:totChol, family = binomial, data = data_na)
    ## [1] 2762.268
    ## glm(formula = CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + 
    ##     glucose + totChol + sex:smoker:cigsPerDay, family = binomial, 
    ##     data = data_na)
    ## [1] 2761.693
    ## glm(formula = CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + 
    ##     glucose + totChol + sex:glucose, family = binomial, data = data_na)
    ## [1] 2762.258
    ## glm(formula = CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + 
    ##     glucose + totChol + sex:totChol, family = binomial, data = data_na)
    ## [1] 2759.904
    ## glm(formula = CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + 
    ##     glucose + totChol + smoker:cigsPerDay:glucose, family = binomial, 
    ##     data = data_na)
    ## [1] 2761.937
    ## glm(formula = CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + 
    ##     glucose + totChol + smoker:cigsPerDay:totChol, family = binomial, 
    ##     data = data_na)
    ## [1] 2762.429
    ## glm(formula = CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + 
    ##     glucose + totChol + glucose:totChol, family = binomial, data = data_na)
    ## [1] 2759.187

``` r
anova(m, int15, test = "Chisq")
```

    ## Analysis of Deviance Table
    ## 
    ## Model 1: CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + glucose + 
    ##     totChol
    ## Model 2: CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + glucose + 
    ##     totChol + glucose:totChol
    ##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)  
    ## 1      3649     2762.5                       
    ## 2      3648     2759.2  1   3.2892  0.06974 .
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Model Fit
---------

Examine model fit

``` r
# 
summary(m)
```

    ## 
    ## Call:
    ## glm(formula = CHD_Risk ~ 1 + age + sysBP + sex + smoker:cigsPerDay + 
    ##     glucose + totChol, family = binomial, data = data_na)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.0074  -0.5966  -0.4315  -0.2864   2.8811  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)       -9.129843   0.475530 -19.199  < 2e-16 ***
    ## age                0.065896   0.006426  10.254  < 2e-16 ***
    ## sysBP              0.017534   0.002149   8.159 3.38e-16 ***
    ## sexmale            0.561446   0.106845   5.255 1.48e-07 ***
    ## glucose            0.007280   0.001677   4.342 1.41e-05 ***
    ## totChol            0.002272   0.001123   2.024    0.043 *  
    ## smoker:cigsPerDay  0.009613   0.002088   4.604 4.14e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3120.5  on 3655  degrees of freedom
    ## Residual deviance: 2762.5  on 3649  degrees of freedom
    ## AIC: 2776.5
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
3120.5 / 3655 # residual deviance / df
```

    ## [1] 0.853762

``` r
# hosmer-lemeshow function (source: lowbwt-03.R)
hosmerlem = function (y, yhat, g = 10) {
  cutyhat = cut(yhat, breaks = quantile(yhat, probs = seq(0, 1, 1/g)), 
                include.lowest = T)
  obs = xtabs(cbind(1 - y, y) ~ cutyhat)
  expect = xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
  chisq = sum((obs - expect)^2 / expect)
  P = 1 - pchisq(chisq, g - 2)
  c("X^2" = chisq, Df = g - 2, "P(>Chi)" = P)
}

# try different values of g to see if results are sensitive to choice
hosmerlem(m$y, fitted(m)) # g = 10
```

    ##       X^2        Df   P(>Chi) 
    ## 7.7444719 8.0000000 0.4588206

``` r
hosmerlem(m$y, fitted(m), g = 5) # g = 5
```

    ##       X^2        Df   P(>Chi) 
    ## 2.0660066 3.0000000 0.5588204

``` r
hosmerlem(m$y, fitted(m), g = 15) # g = 15
```

    ##        X^2         Df    P(>Chi) 
    ## 16.4053024 13.0000000  0.2279345

Diagnostics
-----------

``` r
# plot averaged residuals vs. fitted probabilities
binnedplot(fitted(m), residuals(m, type = "response"),
  xlab = "Averaged Fitted Probabilities",
  ylab = "Averaged Residuals",
  pch = 19, col.pts = "red", cex.pts = 1.5,
  main = "Averaged Residuals vs. Fitted Plot for Final Model")
abline(h = 0, lty = 2, col = "green")
```

![](README_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
# plot cook's distances
plot(cooks.distance(m), type = "h", lwd = 2,
  xlab = "Observation Index",
  ylab = "Cook's Distances",
  main = "Cook's Distances for Final Model")
abline(h = 1, lty = 2, col = "red")
```

![](README_files/figure-markdown_github/unnamed-chunk-10-1.png)
