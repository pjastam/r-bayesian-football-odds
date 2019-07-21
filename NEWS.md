# CHANGES IN r-bayesian-football-odds VERSION 1.2

## NEW FEATURES

- I added a last section to the Rmd file, showing the results of my one-off betting experience with VERSION 1.0.

## BUG FIXES

- The path to the data directory in the `raw_data` chunk was wrongly called "test". We replaced it by "data".

- We found that one record was missing in the N120182019.csv file, because the total amount of records in this file was one less than 306, which is the total number of matches in one football season. We added the missing record of the Zwolle-Groningen match at April 29th, 2019.

- If the contents of the data direcory are changed, it appears that the cache is not updated because `cache=1` or `cache=2` don't work with the `cache.extra` option, see https://github.com/yihui/knitr/issues/994 and https://yihui.name/knitr/demo/cache/#more-granular-cache. This problem is solved by splitting the code chunck into two code chunks, one of which does nothing more that scanning the status of the data directory.

# CHANGES IN r-bayesian-football-odds VERSION 1.1

## NEW FEATURES

- Parameterized the start year and end year when reading the raw data, plotting the histograms and estimating the team skill parameters.

- Added caching of the MCMC results. The cache updates automatically when the raw data, the processed data and/or the JAGS model descrition change. Otherwise, knitting uses the cached MCMC results.

- Added the hook hook_purl() to write the code chunks to an R script when knitting. See ?hook_purl and the example at https://stackoverflow.com/questions/27462018/knitr-do-not-purl-a-chunk#27488012.

## MINOR CHANGES

- Added chunck names.

- The script tag in the mathematical formulas is replaced by the $$ tag, in line with the original code by Rasmus.

- Changed order of sections in html output.

# CHANGES IN r-bayesian-football-odds VERSION 1.0

## NEW FEATURES

- first version of r-bayesian-football-odds: this is the version that I announced on my blog at https://pietstam.nl/blog/2019/05/10/bayesian-football-odds and posted about my one-off betting experience on LinkedIn (in Dutch): https://www.linkedin.com/pulse/de-bal-rond-piet-stam/  (short summary in English: 3 euro stake earned 4.15 euro, so a profit margin of 1.15 / 3 = 38%)
