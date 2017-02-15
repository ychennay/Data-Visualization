#will open up a dialog box- pick the CSV file to upload it into R
df<- read.csv(file.choose())

#provides general summary statistics
summary(df)

#subset data to only numerical (vs. categorical columns)
numerical.df <- df[,sapply(df, is.numeric)]

#generate correlation matrix
cor(numerical.df, use="pairwise.complete.obs")

#visualize correlation matrix
install.packages('corrplot') #only if you do not have corrplot package installed on R
library('corrplot') #load the package
corrplot(cor(numerical.df, use="pairwise.complete.obs"), method='circle')

#writes the correlation matrix to a CSV file wherever your default working directory is
# if you cant find the file just search "correlation.csv" in your computer
write.csv(cor(numerical.df, use="pairwise.complete.obs"), "correlation.csv")



#run a regression and find significant variables
summary <- summary(lm(df$Additional.dollars.....montheach.month.your.clients.would.pay.for.LEED.or.Energy.Star.certification ~ 
             df$Property.Choice.Factors..Comfort + 
             df$Property.Choice.Factors.Location +
             df$Property.Choice.Factors..Reputation + 
             df$Property.Choice.Factors..Energy.efficiency + 
             df$Property.Choice.Factors..Appearance.and.aesthetics + 
             df$factor.ranking.Brand.reputation +
             df$factor.ranking.Ease.of.managing.their.properties + 
             df$factor.ranking..Cost.of.managing.their.properties + 
             df$factor.ranking.Tax.subsidies..rebates + 
             df$factor.ranking..Future.financing.options.from.banks..private.financing.companies..etc. + 
             df$factor.ranking.5..increase.to.the.property.value + 
             df$factor.ranking..20..increase.to.the.property.value + 
             df$factor.ranking..Decrease.in.their.building.s.CO2.and.other.greenhouse.gas.emissions.or.environmental.footprint +
             df$City.of.Los.Angeles.website +
             df$UCLA.website +
             df$Aggregate.real.estate.site.e.g..Zillow.com +
             df$Private.real.estate.firm.s.website,
           data=df))

capture.output(summary, file = "regression summary.txt")
