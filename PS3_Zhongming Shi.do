cd "C:\Users\ZhongmingShi\Desktop\cemfi\term5\development economics\ps3"
use dataUGA.dta,clear
set more off
set memory 20m, permanently
* This code is written by Zhongming Shi 
* Development Economics PS 3
* date: 27/2/2019

*Question 1.Consumption Insurance Tests

*1.

sort hh wave year
list hh year wave if year[_n]==year[_n+1] & hh[_n] == hh[_n+1] // list the observations that have the same year variable cross different waves
replace year=year-1 if year[_n]==year[_n+1] & hh[_n] == hh[_n+1] //change the value of year
replace year=year-1 if year[_n]==year[_n+1] & hh[_n] == hh[_n+1] //this may create new same value, so change again (6 observations)
br hh wave year 
  
bys hh  : gen id=_n


reg inctotal age age_sq familysize i.year i.ethnic sex urban 
predict residual_inc, residuals
reg ctotal age age_sq familysize i.year i.ethnic sex urban 
predict residual_cons, residuals
*generate residuals of consumption and income

by hh :gen d_inc=(residual_inc[_n]-residual_inc[_n-1])/(year[_n]-year[_n-1])
by hh: gen d_cons=(residual_cons[_n]-residual_cons[_n-1])/(year[_n]-year[_n-1])
//annualize residuals of consumption and income

br hh wave year d_inc d_cons id
 bys year: egen agg_cons=total(ctotal)
//generate total consumption by year

save data2.dta,replace

*collect beta for each group(individual) 
qui: statsby beta=_b[d_inc] phi=_b[agg_cons], by(hh) clear : regress d_cons d_inc agg_cons

save coefficient.dta,replace //save the result 








use coefficient.dta, clear
*since some households have not enough observation, and coefficient are set to be zero, I removed these results.
drop if beta==0 | beta==.

*I then remove the outlier
sum beta ,detail
keep if inrange(beta, r(p1), r(p99))

hist beta, color(green)
graph save beta.gph,replace

hist phi, color(red)
graph save phi.gph,replace

gr combine  beta.gph phi.gph, title("coefficients distribution of beta and phi")
gr save coefficients.gph,replace
gr save coefficients.png, replace

*reporting median and mean of each coefficients
tabstat beta phi, stat(min max median mean sd) c(s)

//The result statistics are diaplayer below:
/*

    variable |       min       max       p50      mean        sd
-------------+--------------------------------------------------
        beta | -34.59525  19.04328  .0712901  -.131337   4.23527
         phi |  -.020713  .0171962 -.0000315   -.00003  .0017527
----------------------------------------------------------------
*/


*------------------------------------Comments-------------------------------------------------------

*I run the regression of beta over constant and get the following result:
/*

      Source |       SS           df       MS      Number of obs   =     1,296
-------------+----------------------------------   F(0, 1295)      =      0.00
       Model |           0         0           .   Prob > F        =         .
    Residual |  23229.0769     1,295  17.9375111   R-squared       =    0.0000
-------------+----------------------------------   Adj R-squared   =    0.0000
       Total |  23229.0769     1,295  17.9375111   Root MSE        =    4.2353

------------------------------------------------------------------------------
        beta |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
       _cons |   -.131337   .1176464    -1.12   0.264    -.3621354    .0994614
------------------------------------------------------------------------------

As we can see, beta is not significant, which means we cannot reject the H0: beta==0. which means 
temporary income shock would not affect consumption. Hence we can draw the conclusion that there is 
a full risk sharing mechanism.

*/


*2.
*(a)
merge m:m hh using data2 //match original data
drop if beta==0 | beta==.

egen rank = xtile(inctotal), by(wave) nq(5) //generate quantile by waves based on total income
sort hh wave rank 
br hh wave inctotal rank

tabstat beta, by (rank) stat(mean median) c(s) //tab the statistics by quantile

*The result is as follows:
/*

    rank |      mean       p50
---------+--------------------
       1 | -1.231661  .1229867
       2 |  6.860339  .1318171
       3 | -.5493773   .098031
       4 |  14.81442  .0473157
       5 |   7.44951 -.0011448
---------+--------------------
   Total |  5.459025  .0712901
------------------------------
*/

*------------------------------Comment------------------------------------
/*As we can see, the median of beta decreases with rank, which means that the magnitude of 
insurance increases with income. The poor group has less insurance and consume more when facing
temporary income shock.
*/

*(b) Since we dont have data on wealth, this part is neglected.

*(c) 
*Since some hh have negative beta, this are like "over insurance" and I cannot find justification for that. 
*hence here I only consider the households whose beta are non negative.
keep if beta>=0
egen rank_beta = xtile(beta), nq(5) //generate quantile of beta 
tabstat inctotal, by(rank_beta) stat(mean median)
twoway scatter inctotal beta
gr save beta_income.png

*----------------comment------------------------------------------
/*

The income statistics based on different beta group are diplayed as below:

rank_beta |      mean       p50
----------+--------------------
        1 |  2161.079  1055.731
        2 |  1420.002  717.4996
        3 |  1221.541  645.4298
        4 |  948.7937  568.5674
        5 |  772.7499  481.1661
----------+--------------------
    Total |    1305.6  635.5824
-------------------------------

from which we can see that there is a monotonic pattern between income and beta. 
Among those who have highest insurance ( beta close to zero, or lowest rank) group, their
income are higher measured by either mean or median. Hence I can draw the conclustion that 
as income increasing, beta will get close to zero and agents are well insured.

*/

*3

use data2.dta,clear
reg d_cons d_inc agg_cons
/* The result is displayed below:

      Source |       SS           df       MS      Number of obs   =     6,412
-------------+----------------------------------   F(2, 6409)      =      2.26
       Model |  30805600.1         2    15402800   Prob > F        =    0.1043
    Residual |  4.3648e+10     6,409  6810454.93   R-squared       =    0.0007
-------------+----------------------------------   Adj R-squared   =    0.0004
       Total |  4.3679e+10     6,411  6813135.43   Root MSE        =    2609.7

------------------------------------------------------------------------------
      d_cons |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
       d_inc |   .0264303   .0136318     1.94   0.053    -.0002926    .0531532
    agg_cons |   .0000248   .0000285     0.87   0.384     -.000031    .0000805
       _cons |  -97.90935   111.0467    -0.88   0.378    -315.5979    119.7792
------------------------------------------------------------------------------

Apparently, d_income is significant, indicating that there is no full-risk sharing mechanism.
However, in (1) , beta is not significantly different from zero, indicating we can not reject the 
hypothesis that there is a full-risk sharing mechanism. The difference comes from the fact that 
the poor have higher weight than the rich one in the overall population. Thus when we put single beta
into the regression, we are estimating the "average effect", which is dominant by the poor and we get a 
positive coefficient. However, if we allow the parameter to vary across the household, the result is different
as now we are estimating the "marginal effect" depending on the household's income.

















