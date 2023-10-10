** Part a

*  Firstly, import the data and transfer them into stata format.
import sasxport5 "VIX_D.XPT"
save "VIX_D"
import sasxport5 "DEMO_D.XPT", clear

*  Merge two files together and print out the total sample size.
merge 1:1 seqn using "VIX_D.dta", keep(match)
count
*  We can see now the sample size now is 6980.


** Part b

*  Grouping each 10-year age bracket first.
egen age_group = cut(ridageyr), at(0(10)99)
*  Then create a table to check the number of people who wear glasses/contact lenses.
table (age_group) (viq220) (), missing statistic(proportion) nformat(%4.3f) 
*  From this table, the first column is the desired result, which is the proportion
*  of those who wear glasses/contact lenses for distance vision.


** Part c

*  Remove missing values and don't know values to ensure the response only has
*  2 logical values 1 and 2.
drop if viq220 == 9
drop if viq220 == .
*  Then fit the regression models and remove observations with missing values 
*  in given predictors. Before fitting the model, we should replace viq220 with 
*  values 2 to 0. After fitting the models, get their AIC values.
*  Model 1
drop if ridageyr == .
replace viq220 = 0 if viq220 == 2
logistic viq220 ridageyr
estat ic

*  Model 2
drop if ridreth1 == .
drop if riagendr == .
logistic viq220 ridageyr ridreth1 riagendr
estat ic

*  Model 3
drop if indfmpir == .
logistic viq220 ridageyr ridreth1 riagendr indfmpir
estat ic


** Part d

*  We can tell that the odds of men and women being wears of glasess/contact 
*  lenses for distance vision differs. The odds ration of predictor riagendr
*  is about 1.68, which means the odds of a woman (riagendr = 2) wearing glasses
*  /contact lenses for distance vision are 1.6797 times higher than the odds of 
*  a man (riagendr = 1). Next, carry out a test to check whether the proportion
*  of wearers of glasses/contact lenses for distance vision differs between 
*  men and women.

*  Perform a test for Proportions
prtest viq220, by(riagendr)
*  From the result we can see that the p-value is small, which means there are 
*  not enough evidence to accept the null. Therefore, we can conclude that the
*  proportion differs between men and women.






