SPPH 581c - Assignment 4
================
Patrick Daniele
February 20, 2017

<p>
I'm going to complete this as an R-Markdown document as some of the packages I use require R-Studio and a very up to date version of R.
</p>
<p>
Load Libraries
</p>
``` r
library(survival)
library(tidyverse)
```

    ## Loading tidyverse: ggplot2
    ## Loading tidyverse: tibble
    ## Loading tidyverse: tidyr
    ## Loading tidyverse: readr
    ## Loading tidyverse: purrr
    ## Loading tidyverse: dplyr

    ## Conflicts with tidy packages ----------------------------------------------

    ## filter(): dplyr, stats
    ## lag():    dplyr, stats

``` r
library(survminer)
library(ggfortify)
```

<p>
Incase you haven't seen it before, the "Tidyverse" is a set of packages that makes using R much better.
</p>
<p>
Import the data and basic tidying
</p>
``` r
data <- read.csv('C:/Users/pdaniele/Desktop/SPPH 581c/Assignment 4/alzheimers.csv', header=T)
str(data)
```

    ## 'data.frame':    102 obs. of  7 variables:
    ##  $ ID     : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ time   : int  5 6 8 3 22 1 7 9 3 12 ...
    ##  $ age    : int  46 35 30 30 36 32 36 31 48 47 ...
    ##  $ drug   : int  0 1 1 1 0 1 1 1 0 0 ...
    ##  $ censor : int  1 0 1 1 1 0 1 1 1 1 ...
    ##  $ entdate: Factor w/ 96 levels "1/13/1989","1/17/1989",..: 57 94 46 5 93 34 13 17 30 78 ...
    ##  $ enddate: Factor w/ 94 levels "1/10/1992","1/13/1990",..: 14 46 33 57 78 53 72 85 63 84 ...

``` r
#Note to self, dates are factors. I don't think we need them, so I won't do anything about them now.

#Change drug to a factor
data <- data %>% 
  mutate(drug=factor(drug))

# Change it to a tibble for easier processing
data <- as_data_frame(data)
data
```

    ## # A tibble: 102 × 7
    ##       ID  time   age   drug censor    entdate    enddate
    ##    <int> <int> <int> <fctr>  <int>     <fctr>     <fctr>
    ## 1      1     5    46      0      1  5/15/1990 10/14/1990
    ## 2      2     6    35      1      0  9/19/1989  3/20/1990
    ## 3      3     8    30      1      1  4/21/1991 12/20/1991
    ## 4      4     3    30      1      1   1/3/1991   4/4/1991
    ## 5      5    22    36      0      1  9/18/1989  7/19/1991
    ## 6      6     1    32      1      0  3/18/1991  4/17/1991
    ## 7      7     7    36      1      1 11/11/1989  6/11/1990
    ## 8      8     9    31      1      1 11/25/1989  8/25/1990
    ## 9      9     3    48      0      1  2/11/1991  5/13/1991
    ## 10    10    12    47      0      1  8/11/1989  8/11/1990
    ## # ... with 92 more rows

<p>
Three majors sections:
</p>
<ol>
<li>
Non-Parametric: Kaplan Meier Estimates
</li>
<li>
Semi-Parametric: Cox Proportional Hazards Model
</li>
<li>
Fully Parametric: Weibull or Fully Distributional Model
</li>
</ol>
<h1>
Non-Parametric
</h1>
``` r
## Overall
fit_KM <- survfit(Surv(time, censor) ~ 1, data)

fit_KM %>% 
  summary()
```

    ## Call: survfit(formula = Surv(time, censor) ~ 1, data = data)
    ## 
    ##  time n.risk n.event survival std.err lower 95% CI upper 95% CI
    ##     1    102      15   0.8529  0.0351       0.7869        0.925
    ##     2     85       5   0.8028  0.0395       0.7289        0.884
    ##     3     75      10   0.6957  0.0466       0.6102        0.793
    ##     4     63       4   0.6516  0.0486       0.5630        0.754
    ##     5     58       7   0.5729  0.0510       0.4812        0.682
    ##     6     51       2   0.5505  0.0514       0.4584        0.661
    ##     7     48       6   0.4816  0.0521       0.3896        0.595
    ##     8     41       4   0.4347  0.0520       0.3437        0.550
    ##     9     37       3   0.3994  0.0516       0.3100        0.515
    ##    10     34       3   0.3642  0.0509       0.2769        0.479
    ##    11     30       3   0.3278  0.0500       0.2431        0.442
    ##    12     27       3   0.2913  0.0487       0.2100        0.404
    ##    13     22       1   0.2781  0.0482       0.1980        0.391
    ##    14     21       1   0.2649  0.0477       0.1861        0.377
    ##    15     20       2   0.2384  0.0465       0.1627        0.349
    ##    22     17       1   0.2243  0.0458       0.1504        0.335
    ##    26     15       1   0.2094  0.0451       0.1373        0.319
    ##    30     14       1   0.1944  0.0443       0.1244        0.304
    ##    31     13       1   0.1795  0.0434       0.1118        0.288
    ##    32     12       1   0.1645  0.0422       0.0995        0.272
    ##    34     11       1   0.1496  0.0410       0.0874        0.256
    ##    35     10       1   0.1346  0.0395       0.0757        0.239
    ##    36      9       1   0.1197  0.0378       0.0644        0.222
    ##    43      8       1   0.1047  0.0359       0.0534        0.205
    ##    53      7       1   0.0897  0.0338       0.0429        0.188
    ##    54      6       1   0.0748  0.0313       0.0329        0.170
    ##    57      4       1   0.0561  0.0285       0.0207        0.152
    ##    58      3       1   0.0374  0.0244       0.0104        0.134

``` r
## Try different survival plot methods
#Extensions of GGPlot2

fit_KM %>% 
  ggsurvplot(risk.table = TRUE, ggtheme=theme_classic())
```

![](HW_04_censoring_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
fit_KM %>% 
  autoplot(conf.int=F)
```

![](HW_04_censoring_files/figure-markdown_github/unnamed-chunk-2-2.png)

``` r
## By Drug
fit_KM_drug <- survfit(Surv(time, censor) ~ drug, data)

fit_KM_drug %>% 
  summary()
```

    ## Call: survfit(formula = Surv(time, censor) ~ drug, data = data)
    ## 
    ##                 drug=0 
    ##  time n.risk n.event survival std.err lower 95% CI upper 95% CI
    ##     1     51       5   0.9020  0.0416       0.8239        0.987
    ##     2     46       3   0.8431  0.0509       0.7490        0.949
    ##     3     41       2   0.8020  0.0561       0.6992        0.920
    ##     4     38       2   0.7598  0.0606       0.6498        0.888
    ##     5     35       3   0.6947  0.0660       0.5766        0.837
    ##     6     32       1   0.6730  0.0675       0.5529        0.819
    ##     7     31       1   0.6513  0.0687       0.5296        0.801
    ##     8     30       2   0.6078  0.0706       0.4840        0.763
    ##     9     28       2   0.5644  0.0720       0.4396        0.725
    ##    10     26       2   0.5210  0.0727       0.3964        0.685
    ##    11     23       2   0.4757  0.0731       0.3520        0.643
    ##    12     21       2   0.4304  0.0728       0.3090        0.600
    ##    13     19       1   0.4077  0.0724       0.2879        0.577
    ##    14     18       1   0.3851  0.0718       0.2672        0.555
    ##    15     17       1   0.3624  0.0711       0.2468        0.532
    ##    22     15       1   0.3383  0.0703       0.2250        0.508
    ##    30     13       1   0.3123  0.0696       0.2018        0.483
    ##    31     12       1   0.2862  0.0685       0.1791        0.457
    ##    32     11       1   0.2602  0.0670       0.1571        0.431
    ##    34     10       1   0.2342  0.0652       0.1357        0.404
    ##    35      9       1   0.2082  0.0629       0.1151        0.376
    ##    36      8       1   0.1821  0.0602       0.0953        0.348
    ##    43      7       1   0.1561  0.0569       0.0764        0.319
    ##    53      6       1   0.1301  0.0531       0.0585        0.289
    ##    54      5       1   0.1041  0.0484       0.0418        0.259
    ##    57      4       1   0.0781  0.0427       0.0267        0.228
    ##    58      3       1   0.0520  0.0355       0.0136        0.198
    ## 
    ##                 drug=1 
    ##  time n.risk n.event survival std.err lower 95% CI upper 95% CI
    ##     1     51      10   0.8039  0.0556      0.70202        0.921
    ##     2     39       2   0.7627  0.0599      0.65388        0.890
    ##     3     34       8   0.5832  0.0719      0.45797        0.743
    ##     4     25       2   0.5366  0.0734      0.41043        0.701
    ##     5     23       4   0.4433  0.0740      0.31960        0.615
    ##     6     19       1   0.4199  0.0737      0.29775        0.592
    ##     7     17       5   0.2964  0.0697      0.18697        0.470
    ##     8     11       2   0.2425  0.0666      0.14155        0.416
    ##     9      9       1   0.2156  0.0644      0.11999        0.387
    ##    10      8       1   0.1886  0.0618      0.09928        0.358
    ##    11      7       1   0.1617  0.0585      0.07953        0.329
    ##    12      6       1   0.1347  0.0546      0.06087        0.298
    ##    15      3       1   0.0898  0.0517      0.02908        0.277
    ##    26      2       1   0.0449  0.0409      0.00752        0.268

``` r
## Try different survival plot methods
fit_KM_drug %>% 
  autoplot(conf.int=F)
```

![](HW_04_censoring_files/figure-markdown_github/unnamed-chunk-2-3.png)

``` r
fit_KM_drug %>% 
  ggsurvplot(risk.table = TRUE, ggtheme=theme_classic())
```

![](HW_04_censoring_files/figure-markdown_github/unnamed-chunk-2-4.png)

<h1>
Semi-Parametric
</h1>
``` r
coxfit1 <- coxph(Surv(time, censor) ~ drug, data)
coxfit1 %>% 
  summary()
```

    ## Call:
    ## coxph(formula = Surv(time, censor) ~ drug, data = data)
    ## 
    ##   n= 102, number of events= 82 
    ## 
    ##         coef exp(coef) se(coef)     z Pr(>|z|)   
    ## drug1 0.7766    2.1741   0.2365 3.284  0.00102 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##       exp(coef) exp(-coef) lower .95 upper .95
    ## drug1     2.174       0.46     1.368     3.456
    ## 
    ## Concordance= 0.603  (se = 0.035 )
    ## Rsquare= 0.099   (max possible= 0.997 )
    ## Likelihood ratio test= 10.66  on 1 df,   p=0.001097
    ## Wald test            = 10.78  on 1 df,   p=0.001025
    ## Score (logrank) test = 11.21  on 1 df,   p=0.0008131

``` r
autoplot(survfit(coxfit1, newdata=data.frame(drug=1)), conf.int=F)
```

    ## Warning in model.frame.default(Terms2, data = newdata, na.action =
    ## na.action, : variable 'drug' is not a factor

![](HW_04_censoring_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
## Need to Assess the Proportional Hazards Assumption
coxfit1 %>% 
  cox.zph()
```

    ##           rho chisq     p
    ## drug1 -0.0603 0.283 0.595

``` r
## Looks OK to me. Check Schoenfeld/Martingale Residuals next.
```

<h1>
Fully-Parametric (Accelerated Failure Time Models)
</h1>
``` r
#Exponential Distribution
fit_AFT_exp <- survreg(Surv(time, censor) ~ as.factor(drug) + age, data, dist='exponential')
fit_AFT_exp %>% 
  summary()
```

    ## 
    ## Call:
    ## survreg(formula = Surv(time, censor) ~ as.factor(drug) + age, 
    ##     data = data, dist = "exponential")
    ##                    Value Std. Error     z        p
    ## (Intercept)       5.9983     0.5910 10.15 3.35e-24
    ## as.factor(drug)1 -0.8945     0.2214 -4.04 5.35e-05
    ## age              -0.0878     0.0158 -5.54 2.95e-08
    ## 
    ## Scale fixed at 1 
    ## 
    ## Exponential distribution
    ## Loglik(model)= -275.4   Loglik(intercept only)= -300.2
    ##  Chisq= 49.64 on 2 degrees of freedom, p= 1.7e-11 
    ## Number of Newton-Raphson Iterations: 4 
    ## n= 102

``` r
#Weibull Distribution
fit_AFT_W <- survreg(Surv(time, censor) ~ as.factor(drug) + age, data, dist='weibull')
fit_AFT_W %>% 
  summary()
```

    ## 
    ## Call:
    ## survreg(formula = Surv(time, censor) ~ as.factor(drug) + age, 
    ##     data = data, dist = "weibull")
    ##                    Value Std. Error     z        p
    ## (Intercept)       5.9745     0.5161 11.58 5.49e-31
    ## as.factor(drug)1 -0.9135     0.1938 -4.71 2.44e-06
    ## age              -0.0863     0.0139 -6.22 4.85e-10
    ## Log(scale)       -0.1348     0.0837 -1.61 1.07e-01
    ## 
    ## Scale= 0.874 
    ## 
    ## Weibull distribution
    ## Loglik(model)= -274.2   Loglik(intercept only)= -297.8
    ##  Chisq= 47.22 on 2 degrees of freedom, p= 5.6e-11 
    ## Number of Newton-Raphson Iterations: 5 
    ## n= 102

``` r
#Lognormal
fit_AFT_lognorm <- survreg(Surv(time, censor) ~ as.factor(drug) + age, data, dist='lognormal')
fit_AFT_lognorm %>% 
  summary()
```

    ## 
    ## Call:
    ## survreg(formula = Surv(time, censor) ~ as.factor(drug) + age, 
    ##     data = data, dist = "lognormal")
    ##                    Value Std. Error      z        p
    ## (Intercept)       5.3276     0.6017  8.854 8.47e-19
    ## as.factor(drug)1 -0.8005     0.2176 -3.679 2.34e-04
    ## age              -0.0828     0.0162 -5.116 3.11e-07
    ## Log(scale)        0.0477     0.0784  0.609 5.43e-01
    ## 
    ## Scale= 1.05 
    ## 
    ## Log Normal distribution
    ## Loglik(model)= -273.7   Loglik(intercept only)= -290.5
    ##  Chisq= 33.66 on 2 degrees of freedom, p= 4.9e-08 
    ## Number of Newton-Raphson Iterations: 4 
    ## n= 102