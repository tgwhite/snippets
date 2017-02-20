
library(plyr)
library(dplyr)
library(reshape2)
library(stringr)
library(ggplot2)
library(WDI)
library(dtplyr)
library(data.table)
library(scales)

#  indicator                               name 
# "NY.GDP.PCAP.KD.ZG" "GDP per capita growth (annual %)" 

# indicator                    name 
# "NY.GDP.MKTP.KD.ZG" "GDP growth (annual %)" 

# get indicator to download from the world bank
growth_indicator = "NY.GDP.MKTP.KD.ZG"

# run the download and rename the growth variable 
gdp_growth_download = WDI(indicator = growth_indicator, start = 1970, end = 2015, extra = T)
setnames(gdp_growth_download, growth_indicator, 'gdp_growth')

# subset to OECD countries
oecd_growth = filter(gdp_growth_download, income == 'High income: OECD')

# get growth by decade 
oecd_growth$decade = with(oecd_growth, paste0(str_sub(year, 1, 2), str_sub(year, 3, 3), "0's"))

# create an indicator for country-years of economic growth greater than or equal to 3.5%
oecd_growth$growth_35 = with(oecd_growth, gdp_growth >= 3.5)

# run a simple logistic model that predicts high growth, using only a time trend and an indicator for each country
high_growth_model = glm(growth_35 ~ year + country, data = oecd_growth, family = binomial)

# predict the probability of achieving greater than or equal to 3.5% growth in each year of Trump's first term
growth_predictions = sapply(2017:2020, function(the_year){
	predict(high_growth_model, newdata = data.frame(year = the_year, country = 'United States'), type = 'response')
})

# multiply these annual probabilities together to get the overall probability of achieving 3.5% growth throughout Trump's first term
overall_prob_growth_greater_35 = prod(growth_predictions)


#### Simple output ####

### Overall distribution of economic growth across the OECD since 1970 ### 

# > summary(oecd_growth$gdp_growth)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -14.720   1.344   2.898   2.895   4.351  26.280     160 

### Mean and median economic growth in the OECD, by decade ### 

#   decade mean_growth median_growth
#    <chr>       <dbl>         <dbl>
# 1 1970's    4.270868      4.167882
# 2 1980's    2.974937      2.822948
# 3 1990's    2.952412      3.065175
# 4 2000's    2.456082      2.824718
# 5 2010's    1.731579      1.763026

### Mean and median economic growth in the U.S., by decade ### 

#   decade mean_growth median_growth
#    <chr>       <dbl>         <dbl>
# 1 1970's    3.542590      3.952037
# 2 1980's    3.142712      3.596069
# 3 1990's    3.232118      3.675639
# 4 2000's    1.817007      2.226377
# 5 2010's    2.166891      2.297244

### Simple estimates of the probability of the U.S. achieving 3.5% growth or greater for each year between 2017 and 2020 ### 

# > cat(percent(growth_predictions), '\n')
# 18.20% 17.53% 16.87% 16.24% 

### Overall probability of achieving 3.5% growth in every year from 2017 to 2020 ### 

# > cat(percent(overall_prob_growth_greater_35), '\n')
# 0.0874% 

