model
================
Qi Yuchen
2019/12/9

# Data cleaning

``` r
# import
df = read_csv("Lawsuit.csv") %>% 
  janitor::clean_names() %>% 
  mutate(dept = factor(dept, levels = c(1:6), labels = c("Biochemistry/Molecular Biology", "Physiology ", "Genetics", "Pediatrics", "Medicine", "Surgery")),
         gender = factor(gender, levels = c(0, 1), labels = c("female","male")),
         clin = factor(clin, levels = c(1, 0), labels = c("clinical","research")),
         cert = factor(cert, levels = c(1, 0), labels = c("certified","not_certified")),
         rank = factor(rank, levels = c(1, 2, 3), labels = c("assistant", "associate", "full")),
         sal = (sal94 + sal95)/2)
```

    ## Parsed with column specification:
    ## cols(
    ##   ID = col_double(),
    ##   Dept = col_double(),
    ##   Gender = col_double(),
    ##   Clin = col_double(),
    ##   Cert = col_double(),
    ##   Prate = col_double(),
    ##   Exper = col_double(),
    ##   Rank = col_double(),
    ##   Sal94 = col_double(),
    ##   Sal95 = col_double()
    ## )

``` r
# consider sal94 only first 
df_94 = df %>%   
  mutate(log_sal94 = log(sal94)) %>% 
  dplyr::select(-sal95, -id, -sal94, -sal)

# from EDA
law_df <- 
    read_csv("Lawsuit.csv") %>% 
    janitor::clean_names() %>% 
    dplyr::select(-id) %>% 
    mutate(gender = factor(gender,labels = c("Female","Male")),
           gender = relevel(gender, ref = "Male"),
           dept = factor(dept, labels = c("bio","phy","gene","pedia","med","sur")),
           clin = factor(clin, labels = c("research","clinical")),
           cert = factor(cert, labels = c("not certified","certified")),
           rank = factor(rank,labels = c("assistant","associate","full")),
           )
```

    ## Parsed with column specification:
    ## cols(
    ##   ID = col_double(),
    ##   Dept = col_double(),
    ##   Gender = col_double(),
    ##   Clin = col_double(),
    ##   Cert = col_double(),
    ##   Prate = col_double(),
    ##   Exper = col_double(),
    ##   Rank = col_double(),
    ##   Sal94 = col_double(),
    ##   Sal95 = col_double()
    ## )

# Model

## main effects

### Confounders

First consider confounders

Adjusted vs crude coefficients of female

``` r
# form EDA
crude <- 
    lm(sal94 ~ gender, data = law_df) %>% 
    broom::tidy() %>% 
    filter(term == "genderFemale") %>% 
    pull(estimate)

adj <-
    law_df %>% 
    dplyr::select(dept,clin,cert,prate,exper,rank) %>% 
    map(~lm(sal94 ~ gender + .x, data = law_df) %>% broom::tidy()) %>% 
    map_dbl(~filter(.x,term == "genderFemale") %>% pull(estimate)) 

change <- round(100*(crude - adj)/crude,digits = 2)
change
```

    ##  dept  clin  cert prate exper  rank 
    ## 55.77 11.54 12.20 33.36 22.28 10.43

Rule of thumb, these covariate can all be confounders.

Then inlcude them all in the original model.

### model selection process

stepwise

``` r
fit_94 <- lm(log_sal94 ~ gender, data = df_94)

biggest <- formula(lm(log_sal94 ~ ., df_94))

step(fit_94, direction = 'both', scope = biggest)
```

    ## Start:  AIC=-389.6
    ## log_sal94 ~ gender
    ## 
    ##          Df Sum of Sq    RSS     AIC
    ## + dept    5    41.505 16.266 -710.40
    ## + prate   1    34.003 23.767 -619.42
    ## + clin    1    22.524 35.246 -516.57
    ## + cert    1    14.045 43.725 -460.30
    ## + exper   1     2.479 55.291 -399.05
    ## <none>                57.770 -389.60
    ## + rank    2     0.821 56.950 -389.33
    ## - gender  1     9.391 67.161 -352.29
    ## 
    ## Step:  AIC=-710.4
    ## log_sal94 ~ gender + dept
    ## 
    ##          Df Sum of Sq    RSS     AIC
    ## + exper   1     7.068  9.197 -857.20
    ## + rank    2     5.934 10.331 -824.87
    ## + cert    1     3.181 13.084 -765.20
    ## + clin    1     2.321 13.945 -748.57
    ## + prate   1     1.565 14.700 -734.81
    ## <none>                16.266 -710.40
    ## - gender  1     2.424 18.689 -676.14
    ## - dept    5    41.505 57.770 -389.60
    ## 
    ## Step:  AIC=-857.2
    ## log_sal94 ~ gender + dept + exper
    ## 
    ##          Df Sum of Sq    RSS     AIC
    ## + clin    1     2.042  7.155 -920.73
    ## + cert    1     1.863  7.335 -914.27
    ## + prate   1     1.754  7.443 -910.44
    ## + rank    2     1.242  7.955 -891.08
    ## <none>                 9.197 -857.20
    ## - gender  1     0.250  9.448 -852.19
    ## - exper   1     7.068 16.266 -710.40
    ## - dept    5    46.094 55.291 -399.05
    ## 
    ## Step:  AIC=-920.73
    ## log_sal94 ~ gender + dept + exper + clin
    ## 
    ##          Df Sum of Sq     RSS     AIC
    ## + cert    1    1.3871  5.7683 -974.97
    ## + rank    2    1.2251  5.9303 -965.74
    ## <none>                 7.1554 -920.73
    ## + prate   1    0.0458  7.1096 -920.40
    ## - gender  1    0.2552  7.4106 -913.58
    ## - clin    1    2.0420  9.1974 -857.20
    ## - exper   1    6.7895 13.9450 -748.57
    ## - dept    5   23.8372 30.9926 -548.13
    ## 
    ## Step:  AIC=-974.97
    ## log_sal94 ~ gender + dept + exper + clin + cert
    ## 
    ##          Df Sum of Sq     RSS      AIC
    ## + rank    2    1.2971  4.4713 -1037.45
    ## + prate   1    0.0731  5.6952  -976.30
    ## <none>                 5.7683  -974.97
    ## - gender  1    0.2374  6.0058  -966.44
    ## - cert    1    1.3871  7.1554  -920.73
    ## - clin    1    1.5664  7.3348  -914.27
    ## - exper   1    5.6731 11.4414  -798.22
    ## - dept    5   20.6451 26.4135  -587.86
    ## 
    ## Step:  AIC=-1037.45
    ## log_sal94 ~ gender + dept + exper + clin + cert + rank
    ## 
    ##          Df Sum of Sq     RSS      AIC
    ## - gender  1    0.0344  4.5056 -1037.45
    ## <none>                 4.4713 -1037.45
    ## + prate   1    0.0288  4.4425 -1037.13
    ## - rank    2    1.2971  5.7683  -974.97
    ## - cert    1    1.4591  5.9303  -965.74
    ## - clin    1    1.5676  6.0389  -961.01
    ## - exper   1    1.7013  6.1726  -955.29
    ## - dept    5   21.7617 26.2329  -585.65
    ## 
    ## Step:  AIC=-1037.45
    ## log_sal94 ~ dept + exper + clin + cert + rank
    ## 
    ##          Df Sum of Sq     RSS      AIC
    ## + prate   1    0.0441  4.4615 -1038.02
    ## <none>                 4.5056 -1037.45
    ## + gender  1    0.0344  4.4713 -1037.45
    ## - rank    2    1.5002  6.0058  -966.44
    ## - cert    1    1.4743  5.9799  -965.56
    ## - clin    1    1.5666  6.0722  -961.57
    ## - exper   1    1.8023  6.3079  -951.63
    ## - dept    5   23.9139 28.4195  -566.75
    ## 
    ## Step:  AIC=-1038.02
    ## log_sal94 ~ dept + exper + clin + cert + rank + prate
    ## 
    ##          Df Sum of Sq     RSS      AIC
    ## <none>                 4.4615 -1038.02
    ## - prate   1    0.0441  4.5056 -1037.45
    ## + gender  1    0.0190  4.4425 -1037.13
    ## - clin    1    0.2537  4.7152 -1025.58
    ## - rank    2    1.3891  5.8506  -971.27
    ## - cert    1    1.4895  5.9510  -964.83
    ## - exper   1    1.8398  6.3013  -949.90
    ## - dept    5    9.4082 13.8697  -751.99

    ## 
    ## Call:
    ## lm(formula = log_sal94 ~ dept + exper + clin + cert + rank + 
    ##     prate, data = df_94)
    ## 
    ## Coefficients:
    ##       (Intercept)    deptPhysiology        deptGenetics  
    ##          11.48002           -0.17743            0.16608  
    ##    deptPediatrics       deptMedicine        deptSurgery  
    ##           0.13592            0.48950            0.86148  
    ##             exper       clinresearch  certnot_certified  
    ##           0.01829           -0.15175           -0.19394  
    ##     rankassociate           rankfull              prate  
    ##           0.13551            0.22428           -0.02649

Stepwise using AIC does not include gender.

Criteria

``` r
best_1 <- function(model, ...) 
{
  subsets <- regsubsets(formula(model), model.frame(model), ...)
  subsets <- with(summary(subsets),
                  cbind(p = as.numeric(rownames(which)), which, rss, rsq, adjr2, cp, bic))
  
  return(subsets)
}  


full = lm(log_sal94 ~ ., df_94)

best_1(full) 
```

    ##   p (Intercept) deptPhysiology  deptGenetics deptPediatrics deptMedicine
    ## 1 1           1               0            0              0            0
    ## 2 2           1               0            0              0            0
    ## 3 3           1               0            0              0            0
    ## 4 4           1               0            0              0            1
    ## 5 5           1               0            0              0            1
    ## 6 6           1               1            0              0            1
    ## 7 7           1               1            0              0            1
    ## 8 8           1               1            0              0            1
    ##   deptSurgery gendermale clinresearch certnot_certified prate exper
    ## 1           0          0            0                 0     1     0
    ## 2           0          0            0                 0     1     1
    ## 3           1          0            0                 0     1     1
    ## 4           1          0            0                 0     1     1
    ## 5           1          0            0                 1     1     1
    ## 6           1          0            0                 1     1     1
    ## 7           1          0            0                 1     1     1
    ## 8           1          0            0                 1     1     1
    ##   rankassociate rankfull       rss       rsq     adjr2         cp
    ## 1             0        0 27.676485 0.5879079 0.5863169 1288.03705
    ## 2             0        0 16.737115 0.7507909 0.7488590  679.34780
    ## 3             0        0 13.248223 0.8027391 0.8004365  486.58076
    ## 4             0        0  8.500209 0.8734352 0.8714576  223.52333
    ## 5             0        0  7.164852 0.8933182 0.8912264  150.97714
    ## 6             0        0  6.253262 0.9068914 0.9046919  102.08776
    ## 7             0        1  5.495572 0.9181731 0.9159091   61.78979
    ## 8             1        1  5.000770 0.9255405 0.9231767   36.16751
    ##         bic
    ## 1 -220.2497
    ## 2 -345.9563
    ## 3 -401.4045
    ## 4 -511.6646
    ## 5 -550.7059
    ## 6 -580.6593
    ## 7 -608.8057
    ## 8 -627.8667

So is using criteria.

### Use VIF to see collinearity

``` r
vif(full)
```

    ##   deptPhysiology       deptGenetics    deptPediatrics      deptMedicine 
    ##          1.607184          1.629419          4.282664          6.379551 
    ##       deptSurgery        gendermale      clinresearch certnot_certified 
    ##          7.215586          1.443762          5.877635          1.329952 
    ##             prate             exper     rankassociate          rankfull 
    ##         16.626048          1.884661          1.508016          2.225837

prate’s VIF is more than 10, we may drop it.

``` r
fit = lm(log_sal94 ~gender+dept+clin+cert+exper+rank, data = df_94)

vif(fit)
```

    ##        gendermale   deptPhysiology       deptGenetics    deptPediatrics 
    ##          1.356319          1.607121          1.439383          1.894530 
    ##      deptMedicine       deptSurgery      clinresearch certnot_certified 
    ##          2.704003          2.392543          1.659126          1.327650 
    ##             exper     rankassociate          rankfull 
    ##          1.852094          1.499835          2.209568

Now each VIF \< 5, which is good.

# interaction term

``` r
## general test

# no prate
fit_conf = lm(log_sal94 ~gender+dept+clin+cert+exper+rank, data = df_94)

fit_int = lm(log_sal94 ~gender+dept+clin+cert+exper+gender*dept+gender*clin+gender*cert+gender*exper+gender*rank, data = df_94)

anova(fit_conf, fit_int)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: log_sal94 ~ gender + dept + clin + cert + exper + rank
    ## Model 2: log_sal94 ~ gender + dept + clin + cert + exper + gender * dept + 
    ##     gender * clin + gender * cert + gender * exper + gender * 
    ##     rank
    ##   Res.Df    RSS Df Sum of Sq      F Pr(>F)
    ## 1    249 4.4713                           
    ## 2    239 4.1932 10   0.27805 1.5848 0.1117

Maybe no interaction term.

The model now is log\_sal94 \~gender+dept+clin+cert+exper+rank

``` r
summary(fit)
```

    ## 
    ## Call:
    ## lm(formula = log_sal94 ~ gender + dept + clin + cert + exper + 
    ##     rank, data = df_94)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.34320 -0.07740 -0.02211  0.07833  0.89814 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       11.325799   0.034478 328.490  < 2e-16 ***
    ## gendermale         0.027211   0.019670   1.383    0.168    
    ## deptPhysiology    -0.176428   0.029190  -6.044 5.45e-09 ***
    ## deptGenetics       0.187044   0.036585   5.113 6.34e-07 ***
    ## deptPediatrics     0.199380   0.035795   5.570 6.59e-08 ***
    ## deptMedicine       0.539599   0.029584  18.240  < 2e-16 ***
    ## deptSurgery        0.931580   0.035616  26.157  < 2e-16 ***
    ## clinresearch      -0.204955   0.021936  -9.343  < 2e-16 ***
    ## certnot_certified -0.191938   0.021293  -9.014  < 2e-16 ***
    ## exper              0.017679   0.001816   9.734  < 2e-16 ***
    ## rankassociate      0.133262   0.023612   5.644 4.52e-08 ***
    ## rankfull           0.221400   0.026310   8.415 3.11e-15 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.134 on 249 degrees of freedom
    ## Multiple R-squared:  0.9334, Adjusted R-squared:  0.9305 
    ## F-statistic: 317.4 on 11 and 249 DF,  p-value: < 2.2e-16

Adjusted R squared is 0.93, indicating that the model fits well.

From this model, there is no “obvious” discrimination. However, there
may be mediators.

# stratified regression

``` r
stratified_dept = df_94 %>%
  group_by(dept) %>%
  summarize(
      n = n(),
      coef =  lm(log_sal94 ~ gender)$coef["gendermale"]
            )
stratified_dept %>% 
    knitr::kable()
```

| dept                           |  n |      coef |
| :----------------------------- | -: | --------: |
| Biochemistry/Molecular Biology | 50 | 0.1674974 |
| Physiology                     | 40 | 0.3184488 |
| Genetics                       | 21 | 0.3766389 |
| Pediatrics                     | 30 | 0.1309545 |
| Medicine                       | 80 | 0.1668338 |
| Surgery                        | 40 | 0.1345868 |

``` r
stratified_clin = df_94 %>%
  group_by(clin) %>%
  summarize(
      n = n(),
      coef =  lm(log_sal94 ~ gender)$coef["gendermale"]
            )
stratified_clin %>% 
    knitr::kable()
```

| clin     |   n |      coef |
| :------- | --: | --------: |
| clinical | 160 | 0.3804995 |
| research | 101 | 0.2752789 |

``` r
stratified_cert = df_94 %>%
  group_by(cert) %>%
  summarize(
      n = n(),
      coef =  lm(log_sal94 ~ gender)$coef["gendermale"]
            )
stratified_cert %>% 
    knitr::kable()
```

| cert           |   n |      coef |
| :------------- | --: | --------: |
| certified      | 188 | 0.3602442 |
| not\_certified |  73 | 0.2699972 |

``` r
stratified_rank = df_94 %>%
  group_by(rank) %>%
  summarize(
      n = n(),
      coef =  lm(log_sal94 ~ gender)$coef["gendermale"]
            )
stratified_rank %>% 
    knitr::kable()
```

| rank      |   n |      coef |
| :-------- | --: | --------: |
| assistant | 112 | 0.5205169 |
| associate |  64 | 0.2485783 |
| full      |  85 | 0.1149184 |
