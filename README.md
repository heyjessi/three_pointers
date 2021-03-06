# Analyzing Trends in the NCAA 3 Point Shooting from 2003 to 2019
## Authors: Seth Billiau, Jess Eng, Anna Li

### Abstract

The introduction of the three-pointer has undoubtably had an impact on team strategy in the college game. Since its debut in 1987, the popularity of the shot continued to grow until it comprised over one fourth of the total number of shots made in the 2008-2009 NCAA Men's Basketball season. At this point, however, the NCAA changed the three-point line just before the 2008-2009 season by pushing it back from 19 feet and 9 inches to 20 feet and 9 inches. The number of three-point attempts dropped from 35.2 percent of all field goal attempts in 2008 to 34.4 percent in 2009 -- the lowest percentage since 2000. Since then, however, three-point shots as a percentage of all field goal attempts have begun to increase once again making up over 37 percent of overall field goals in the 2018-19 season. In fact, three-pointers have rebounded so well that the  NCAA Playing Rules Oversight Panel in June 2019 decided to move the Division I three-point line again to 22 feet, 1¾ inches (the same distance used in international basketball), adding a further degree of difficulty to the shot. 

This paper, drawing upon data from 2003 to the present, analyzes how three-point line rule changes and other exogenous changes in the sport have been associated with changes in Division I NCAA teams three-point shooting tendencies. Specifically, we hope to answer the following questions: Is the 2008-2009 NCAA rule change associated with a statistically significant drop in three-point shots in the following years? Furthermore, if the rule change is associated with a significant decrease in three-point shots, when did the frequency of three-point shots begin to increase again? And what other changes in the sport may have led to this increase? 

Using a segmented, random slopes and random intercepts model, we found that the rate at which the number of attempted three point shots relative to the total number of three point shots changed over time exhibited three distinct non-zero trends between 2003-2006, 2007-2012, and 2012-2018. While initially increasing over time from 2003-2006, 3PAr decreased between 2007-2012. In 2013, 3PAr began to increase again at an accelerated rate - a rate even larger than the annual rate of growth between 2003-2006.  
	    
We hypothesize that the first breakpoint and subsequent decrease in 3PAr over time beginning in 2007 is due in part to the announcement of the 2007-2008 NCAA rule change. We also hypothesize that the second breakpoint in 2013 could be due to a lead-lag relationship between the NBA basketball and NCAA basketball. We are more confident in our prior claim than our latter claim, though further research could be done to flesh out both of these hypothesis. It would be interesting to do more research into other exogenous factors that may have led to the breakpoints. We would be especially interested in analyzing any possible lead-lag relationship between the NBA and NCAA more in depth in a future project. 

We provide a plot of the average team in our final model with appropriate breakpoints (labeled by hypothesized cause) below. 

![final_model](https://github.com/sethbilliau/stat139finalproject/blob/master/images/segmented_final.png)

Our full writeup can be found at writeup.pdf in this repository. 

Date Last Modified: December 12, 2019
