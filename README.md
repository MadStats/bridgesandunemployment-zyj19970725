Predict Unemployed Level and Rate Using the Bridges Data
================

``` r
states=c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "WI")
fips.states=c("  01  ", "  02  ", "  04  ", "  05  ", "  06  ", "  08  ", "  09  ", "  10  ", "  12  ", "  13  ", "  55  ")
```

``` r
# read in the bridges data of the 11 states and select service type and average daily traffic 
bridge=data.frame()
for (i in states){
  u=paste0('https://www.fhwa.dot.gov/bridge/nbi/2018/delimited/',i,'18.txt')
  bridge.states=read.csv(u) %>% 
    select(COUNTY_CODE_003,SERVICE_LEVEL_005C,ADT_029) 
  bridge.states=mutate(bridge.states,state=rep(i,nrow(bridge.states)))
  bridge=rbind(bridge,bridge.states)
} 
```

    ## Warning in scan(file = file, what = what, sep = sep, quote = quote, dec = dec, :
    ## EOF within quoted string
    
    ## Warning in scan(file = file, what = what, sep = sep, quote = quote, dec = dec, :
    ## EOF within quoted string
    
    ## Warning in scan(file = file, what = what, sep = sep, quote = quote, dec = dec, :
    ## EOF within quoted string

``` r
# summarize counts of each service type for each county in each state 
service=bridge %>% 
  group_by(state,COUNTY_CODE_003,SERVICE_LEVEL_005C) %>% 
  summarise(n=n()) %>% 
  pivot_wider(names_from = SERVICE_LEVEL_005C, values_from = n,names_prefix = 'ServiceLevel')

# summarize the average daily traffic for each county in each state
ADT=bridge %>% 
  group_by(state,COUNTY_CODE_003) %>% 
  summarise(ADT=sum(ADT_029))

# join the two data sets
bridges=left_join(service,ADT)
```

    ## Joining, by = c("state", "COUNTY_CODE_003")

``` r
# read in the BLS data
BLS=read.csv('https://www.bls.gov/web/metro/laucntycur14.txt',skip = 6,sep = '|',header = F,colClasses = 'character')
colnames(BLS)=c("LAUS Area Code","State","County","Area Title","Period","Civilian_Labor_Force","Employed","Unemployed_Level","Unemployed_Rate")

BLS.Dec=data.frame()
BLS.Nov=data.frame()

# select the December data
for (i in 1:11){
  BLS.states.Dec=filter(BLS,State == fips.states[i] & Period=='   Dec-18  ') %>% 
    select(County,Unemployed_Level,Unemployed_Rate)
  BLS.states.Dec=mutate(BLS.states.Dec,state=rep(states[i],nrow(BLS.states.Dec)))
  BLS.Dec=rbind(BLS.Dec,BLS.states.Dec)
}

# select the November data
for (i in 1:11){
  BLS.states.Nov=filter(BLS,State == fips.states[i] & Period=='   Nov-18  ') %>% 
    select(County,pre_level=Unemployed_Level,pre_rate=Unemployed_Rate)
  BLS.states.Nov=mutate(BLS.states.Nov,state=rep(states[i],nrow(BLS.states.Nov)))
  BLS.Nov=rbind(BLS.Nov,BLS.states.Nov)  
}

# join the two BLS data
BLS.final=left_join(BLS.Dec,BLS.Nov)
```

    ## Joining, by = c("County", "state")

``` r
BLS.final$County=as.numeric(BLS.final$County)
BLS.final$Unemployed_Level=as.numeric(gsub(",", "", BLS.final$Unemployed_Level))
BLS.final$Unemployed_Rate=as.numeric(BLS.final$Unemployed_Rate)
BLS.final$pre_level=as.numeric(gsub(",", "", BLS.final$pre_level))
BLS.final$pre_rate=as.numeric(BLS.final$pre_rate)
```

``` r
# join the BLS data and the bridge data
data.final=left_join(bridges,BLS.final,by = c("COUNTY_CODE_003" = "County","state")) %>% 
  select(state,COUNTY_CODE_003,ServiceLevel1,ADT,Unemployed_Level,Unemployed_Rate,pre_level,pre_rate)
```

``` r
# fit the linear model
m.1=lm(Unemployed_Level~ServiceLevel1+ADT,data = data.final)
m.2=lm(Unemployed_Rate~ServiceLevel1+ADT,data = data.final)
m.3=lm(Unemployed_Level~ServiceLevel1+ADT+pre_level,data = data.final)
m.4=lm(Unemployed_Rate~ServiceLevel1+ADT+pre_rate,data = data.final)
```

``` r
summary(m.1)
```

    ## 
    ## Call:
    ## lm(formula = Unemployed_Level ~ ServiceLevel1 + ADT, data = data.final)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -17160.5   -625.3   -283.3    179.9  31395.2 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   5.281e+01  1.716e+02   0.308    0.758    
    ## ServiceLevel1 8.648e+00  1.280e+00   6.756 3.38e-11 ***
    ## ADT           9.699e-04  1.444e-05  67.187  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2926 on 595 degrees of freedom
    ##   (11 observations deleted due to missingness)
    ## Multiple R-squared:  0.9401, Adjusted R-squared:  0.9399 
    ## F-statistic:  4668 on 2 and 595 DF,  p-value: < 2.2e-16

``` r
summary(m.2)
```

    ## 
    ## Call:
    ## lm(formula = Unemployed_Rate ~ ServiceLevel1 + ADT, data = data.final)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -2.680 -1.014 -0.419  0.441 13.755 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    4.555e+00  1.071e-01  42.540  < 2e-16 ***
    ## ServiceLevel1 -2.432e-03  7.987e-04  -3.045  0.00243 ** 
    ## ADT            1.007e-08  9.008e-09   1.118  0.26406    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.826 on 595 degrees of freedom
    ##   (11 observations deleted due to missingness)
    ## Multiple R-squared:  0.01782,    Adjusted R-squared:  0.01452 
    ## F-statistic: 5.397 on 2 and 595 DF,  p-value: 0.004754

``` r
summary(m.3)
```

    ## 
    ## Call:
    ## lm(formula = Unemployed_Level ~ ServiceLevel1 + ADT + pre_level, 
    ##     data = data.final)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2132.0   -48.5    -9.4    22.8  5516.0 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   -2.461e+01  2.246e+01  -1.096    0.274    
    ## ServiceLevel1  8.500e-01  1.727e-01   4.921 1.12e-06 ***
    ## ADT           -6.850e-05  5.928e-06 -11.555  < 2e-16 ***
    ## pre_level      1.074e+00  5.809e-03 184.798  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 382.9 on 594 degrees of freedom
    ##   (11 observations deleted due to missingness)
    ## Multiple R-squared:  0.999,  Adjusted R-squared:  0.999 
    ## F-statistic: 1.931e+05 on 3 and 594 DF,  p-value: < 2.2e-16

``` r
summary(m.4)
```

    ## 
    ## Call:
    ## lm(formula = Unemployed_Rate ~ ServiceLevel1 + ADT + pre_rate, 
    ##     data = data.final)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8538 -0.2514 -0.0420  0.2284  3.7342 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    5.879e-01  7.395e-02   7.951 9.38e-15 ***
    ## ServiceLevel1 -3.519e-04  2.905e-04  -1.212    0.226    
    ## ADT           -6.583e-10  3.259e-09  -0.202    0.840    
    ## pre_rate       9.411e-01  1.495e-02  62.954  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.6596 on 594 degrees of freedom
    ##   (11 observations deleted due to missingness)
    ## Multiple R-squared:  0.872,  Adjusted R-squared:  0.8713 
    ## F-statistic:  1349 on 3 and 594 DF,  p-value: < 2.2e-16

## Analysis

The raw data I use is the bridges data of 11 states. To predict the
unemployed rate and the unemployed level, I select the service level and
the average daily traffic in the bridges data. The average daily traffic
somehow tells how many people drive to “work” everyday. For service
level, the one in interest is “service level 1” which means that the
bridge is for mainline use.

Since the average daily traffic and the service level is for each bridge
in the county, I summarize the average daily traffic and the counts of
the service level for the whole county. Thus the data is more
informative for the whole county and also it can be paired to the BLS
data.

After join the BLS data and the bridge data, my final data set has 609
rows and 8 columns. Then I fit my linear models. From the summary, I
found that average daily traffic has significant effect on unemployed
level but the effect on unemployed rate is not significant. When we do
not take the previous month data as predictor, the unemployed level is
estimated to increase 9.7 with 10000 increase of daily traffic and
increase 8.6 with one increase in mainline bridge. The analysis for the
unemployed rate is similar.

After adding the previous month data as predictor, it is not surprising
that they have significant effects. Also, the estimated coefficients are
near 1, suggesting that the unemployed level and unemployed rate do not
change much during the one month period.
