# Modeling match results in the Dutch Eredivisie using a hierarchical Bayesian Poisson model

## Quick summary
`r-bayesian-football-odds` is based on the works of [Rasmus Baath](http://www.sumsar.net/blog/2013/07/modeling-match-results-in-la-liga-part-one/). He predicted the results of the 50 last matches of the 2012/2013 Spanish LaLiga season. Originally, I slightly adapted his code to predict the results of the last two competition rounds of the 2018/2019 Dutch Eredivisie season. Later on, I added some new functionalities to the code such as parallel computation.

## The original works
Rasmus predicted the results of the 50 last matches of the 2012/2013 Spanish LaLiga season. He used data of the 2008/09-2012/13 seasons (5 seasons in total) to estimate his regression model in a [Bayesian](https://en.wikipedia.org/wiki/Bayes_estimator) way. See [this thread](https://stats.stackexchange.com/questions/252577/bayes-regression-how-is-it-done-in-comparison-to-standard-regression) for an intuitive explanation of the difference between the Bayesian approach and the conventional approaches of linear regression and maximum likelihood.

## My first application
I slightly adapted his code to predict the results of the last two competition rounds (that is, the last 18 matches) of the 2018/2019 Dutch Eredivisie season. These predictions are based on soccer match data of the 2014/15-2018/19 seasons (5 seasons in total). The source of these data is [here](http://www.football-data.co.uk/netherlandsm.php). Out of the three model specifications that Rasmus developed, I used the most sophisticated model that allowed for year-to-year variability in team skill (called "iteration 3" by Rasmus).

## The results
The results of the most recent version of the code can be found [here](https://pjastam.github.io/r-bayesian-football-odds/). Alternatively, click [here](http://htmlpreview.github.io/?https://github.com/pjastam/r-bayesian-football-odds/blob/1409c9e8bbcfe92d8526e2fc866e0f7a57549d60/NL_eredivisie_2014_2019.nb.html) if you want to dive into the results of my first application to the Dutch Eredivisie.

The latter weblink refers to the original output file of the [VERSION 1.0](https://github.com/pjastam/r-bayesian-football-odds/releases/tag/v1.0) code release. I also wrote [a blog post](https://www.pietstam.nl/posts/2019-05-10-de-bal-is-rond/) about my one-off betting experience (in Dutch). Short summary in English: 3 euro stake earned 4.15 euro, so a profit margin of 1.15 / 3 = 38%.

## Copyright
Rasmus Baath submitted his code to the [UseR 2013 data analysis contest](https://www.r-project.org/conferences/useR-2013/) and licensed it under the Creative Commons [Attribution 3.0 Unported license](http://creativecommons.org/licenses/by/3.0/). My contribution is licensed under the equivalent MIT license as Creative Commons [recommend against](https://creativecommons.org/faq/#can-i-apply-a-creative-commons-license-to-software) using Creative Commons licenses for software.
