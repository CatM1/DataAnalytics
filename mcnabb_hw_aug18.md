---
title: "STA380.Ex1"
author: "Catherine McNabb"
date: "August 10, 2018"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

##Probability Practice

###Part A

Let RC stand for Random Clicker and TC stand for Truthful Clicker. We can calculate the fraction of people who are truthful clickers who answered yes using Bayes' Rule:

P(Yes) = .65
P(No) = .35

P(RC) = .3
P(TC) = .7

P(No|RC) = .5
P(Yes|RC) = .5

Ex: 100 people
65 answer yes, 30 are random clickers, and half of those are random clickers who answer yes. So, P(yes|RC) is .15.

That means 65-15 (50) people are truthful clickers who answered yes. 

That is 50/70 percent of the truthful clickers who answer yes, or 71%.

The Bayes Theorem formula that applies would go like this: 

P(yes|TC) = $$\frac{P(Yes) - P(yes|RC)}{P(TC)}$$
```{r}
truthful.yes = 50/70
print(truthful.yes)
```


###Part B

Let D stand for has disease; let ND stand for no disease.

P(D) = .000025
P(ND) = .999975

P(D|yes) = .993
P(ND|no) = .9999

P(yes|D) = $$\frac{P(D|yes)*P(D)}{P(D|yes)P(D) + P(ND)P(ND|no)}$$

P(yes|D) = $$\frac{(.993)(.000025)}{(.993)(.000025) + (.999975)(.0001)}$$

```{r}
probability = (.993*.000025)/((.993*.000025) + (.999975*.0001))
probability
```

The chance of getting a positive test when having the disease is 19.89%. 

This is pretty bad, because if you have the disease, you most definitely want to test positive for it so you can treat it. Even though the test seems pretty accurate, it's not accurate where it counts, which is diagnosing the disease for people who need it. 


## Exploratory Analysis: Green Buildings

First, I will read in the data to explore. 

```{r message=FALSE}
library(mosaic)
green = read.csv('greenbuildings.csv')
```


Then, I will extract the buildings with green ratings, so I can compare a green-only list to a not-green building list. 

```{r echo=FALSE}
green_only = subset(green, green_rating==1)
normal = subset(green,green_rating==0)
cat("dimensions are: ", dim(green_only))
```

Let's look at the distributions of the rent of the two lists, as well as the average rent per sq foot: 

```{r}
par(mfrow=c(1,2))

hist(green_only$Rent, 25)
median(green_only$Rent)

hist(normal$Rent, 25)
median(normal$Rent)
```

The distributions both have long tails, but the long tail of the rent price for normal buildings is MUCH longer and weirder than the long tail of the price for green buildings per square foot. Because of the weird distribution, I agree with the 'guru', and I also used median for the usual price of a square foot rather than the mean. 

As noted in the assignment, the green is about $2.60 more than the "normal" buildings.

Another problem with this data is that green buildings are probably built more in progressive cities like NY or SF where the price per square foot is more expensive anyways. So maybe we should compare the green only rent to the cluster rent price instead of the overall price. 

```{r}
median(green_only$Rent)-median(green_only$cluster_rent)
```

This shows a difference of $2.25, so let's go with that instead of the $2.60 figure.


Now, we compare the size of the buildings. Presumable, bigger size means more tenants and therefore more rent money. Ours is apparently planned to be 250,000 feet, but it would be interesting to know for similar buildings.

```{r}
green_only_size = mean(green_only$size)
green_only_size

normal_size = mean(normal$size)
normal_size
```

The size of a green building is much larger than a normal building on average. But, maybe the normal buildings have a higher leasing rate, so the extra size doesn't matter. Let's see. 

```{r}
green_only_leaserate = mean(green_only$leasing_rate)
green_only_leaserate

normal_leaserate = mean(normal$leasing_rate)
normal_leaserate
```

Nope! We can see this is not true. 
The green buildings are typically 89% leased, while the other buildings are just about 82% leased. If the green buildings are on average larger and the STILL are leasing at a much higher rate, then that bodes well for our building.

Perhaps also the electicty costs are cheaper for green buildings, because they tend to utilize natural lighting. 

```{r}
plot(green_only$Electricity_Costs)
plot(normal$Electricity_Costs)
```

Here, we can see that the plots look almost exactly the same, so we can assume this is generally NOT true. 

Back to the analysis from the 'Excel guru' though, I think most of what he said holds up. I think there are a couple additional considerations here that we came up with. First of all, the price is actually closer to only $2.20 difference per square foot instead of $2.60, so that makes it a lot more attractive to tenants even though we won't make as much. 
Second, and this is not backed up by statistical evidence here, but green buildings and taking climate change into consideration is more important with businesses than ever, so it could have a marginal impact on the bottom line for the business as well. I'd rather work in a green building and work with companies that are aware of their footprints. 
Additionally, I found that the green buildings are more likely to be leased than the normal buildings, so I am able to agree with the guru's assessment at hoping for a 90% full building to recoup the losses. 

Between the two analsyes and the trend toward "greener" decisions, I agree with the guru's assessment that the new building should indeed be green. 

#amenities#########

## Bootstrapping

Upload the needed libraries and set seed:
```{r warning=FALSE}
library(mosaic)
library(quantmod)
library(foreach)
set.seed(345)
```

Import the relevant info for the given tickers

```{r echo=FALSE}
ETFs = c("SPY", "TLT", "LQD","EEM","VNQ")
getSymbols(ETFs, from = "2006-01-01")
```

As with the stocks in class, ETFs can split and have dividends, so let's adjust for those. 

```{r echo=FALSE}
for(ticker in ETFs) {
  expr = paste0(ticker, "a = adjustOHLC(", ticker, ")")
  eval(parse(text=expr))
}
```

Look at all close to close changes to see variability of the ETFs.

```{r echo=FALSE}
par(mfrow=c(3,2))
plot(ClCl(SPYa))
plot(ClCl(TLTa))
plot(ClCl(LQDa))
plot(ClCl(EEMa))
plot(ClCl(VNQa))
```

As expected, there is a lot of variability around the Recession in 2008 for all types of ETFs. Otherwise, the emerging market equities and corporate bonds hold very little variability in the past 12 years. 

Now, I'll put the returns from the ETFs in one matrix and see what the data looks like using the "head" function.

```{r echo=FALSE}
all_returns = cbind(ClCl(SPYa),ClCl(TLTa),ClCl(LQDa),ClCl(EEMa),ClCl(VNQa))
head(all_returns)
all_returns = as.matrix(na.omit(all_returns))
```

Let's look at the relationship between the pairs of the ETFs.

```{r}
pairs(all_returns)

```

We can see that the emerging market equities seems to have a not very variable relationship with the other ETFs, which is inline with what we noted above. 

Now I'll pull a plot of all the returns combined in a matrix over time.

```{r}
plot(all_returns[,3], type='l')
```

We can see that again, the variability is really mostly around 2008-2009 when the global economy was pretty week. Otherwise, there are much smaller dips and hills in the dataset. 

Let's look at the correlations

```{r}
cor(all_returns)
```

Some of the ETFs are highly correlated and some are not. For example, the stock exchange and real estate market are, and that makes sense because they're both tied to the health of the US economy. However, treasury bonds are negatively correlated with the performance of the US stocks and emerging markets. This is probably because when the economy is doing well, people will invest in riskier funds, like those in emerging markets and the stock market, but when the economy is doing poorly, people will be more likely to invest in something dependable, like US Treasury bonds. 

Now, I have $100,000 to invest, and I need to dedide which portfolio to use. 

#### Portfolio #1 - Even split

I will be bootstrapping over the past 4 weeks (20 days), with equal weights on all 5 ETFs after setting the seed.

```{r echo=FALSE}
set.seed(3)
initial_wealth = 100000
sim.even = foreach(i=1:5000, .combine='rbind') %do% {
  total_wealth = initial_wealth
  n_days = 20
  wealthtracker = rep(0, n_days)
  for(today in 1:n_days) {
    weights = c(0.2, 0.2, 0.2, 0.2, 0.2)
    holdings = weights * total_wealth
    return.today = resample(all_returns, 1, orig.ids=FALSE)
    holdings = holdings + holdings*return.today
    total_wealth = sum(holdings)
    wealthtracker[today] = total_wealth
  }
  wealthtracker
}
head(sim.even)
```

Looking at the head values of the simulation, we can already see that the results are variable.

Now let's see the histogram:

```{r echo=FALSE}
hist(sim.even[,n_days]- initial_wealth, breaks=30)
```

Using boostrapping and then creating a histogram of gains and losses, we can see that the most likely outcome with this investment is a small gain. There is a longer tail on the right side of the histogram though, where we can see that there is also a chance of a big "win" in the market with this porfolio. Most likely though, you will either gain or lose a bit of money. 

```{r}
mean(sim.even[,n_days])
```

We can also see the mean for this histogram is \$100,991, which agrees with my assessment of the histogram above, where you are more likely to gain money, but probably just a bit. 

Now, looking at the 5% level belwo, we can see that 95% of the time you will have better returns than -\$6,000, which is good. Out of $100,000, that isn't that bad of a loss, but you of course still do not want any loss. The good news is that there is only a 5% chance of that loss happening and a 95% chance that there will be even less of a loss or a gain.

```{r}
quantile(sim.even[,n_days], 0.05) - initial_wealth
```


#### Portfolio #2 - Safe Option

For the "safe" option, I will invest 40% of my money into treasury bonds, 40% into investment-grade corporate bonds, and 20% into emerging market equities. 
```{r echo=FALSE}
set.seed(4)
initial_wealth = 100000
sim.safe = foreach(i=1:5000, .combine='rbind') %do% {
  total_wealth = initial_wealth
  n_days = 20
  wealthtracker = rep(0, n_days)
  for(today in 1:n_days) {
    weights = c(0.0, 0.4, 0.4, 0.2, 0.0)
     holdings = weights * total_wealth
    return.today = resample(all_returns, 1, orig.ids=FALSE)
    holdings = holdings + holdings*return.today
    total_wealth = sum(holdings)
    wealthtracker[today] = total_wealth
  }
  wealthtracker
}
head(sim.safe)
```

Looking at the head of the safe porfolio, I can't really tell a difference, so let's look at the histogram and see if there is something there. 

```{r}
hist(sim.safe[,n_days]- initial_wealth, breaks=30)
```

It definitely appears that you have more of a chance of gaining than losing. Moreover, there's a bit of a wacky long tail here, with just a few of the simulations gaining a LOT of money, but none losing a LOT of money. 

```{r}
mean(sim.safe[,n_days])
```

The mean gain for this safe simulation was $100,784, which is a bit less than the even split. This is actually as expected, because of the old saying "high risk, high reward". In this case, we went with "low risk, low reward". 

Now, looking at the 5% chance on the left tail, we can see that there is a 95% chance that we lose less than -\$3,000 or that we gain money. This is half the amount that we were at risk of losing at the same percentage for the evenly split porfolio, which again makes sense with the "low risk, low reward" mantra. This is the low risk part. 

```{r}
quantile(sim.safe[,n_days], 0.05) - initial_wealth
```

#### Porfolio #3 - Aggressive Option

For the "aggressive" option, I will invest 40% in US domestic equities, 40% in real estate, and 20% in emerging market equities. 

```{r echo=FALSE}
set.seed(5)
initial_wealth = 100000
sim.risk = foreach(i=1:5000, .combine='rbind') %do% {
  total_wealth = initial_wealth
  n_days = 20
  wealthtracker = rep(0, n_days)
  for(today in 1:n_days) {
    weights = c(0.4, 0.0, 0.0, 0.2, 0.4)
     holdings = weights * total_wealth
    return.today = resample(all_returns, 1, orig.ids=FALSE)
    holdings = holdings + holdings*return.today
    total_wealth = sum(holdings)
    wealthtracker[today] = total_wealth
  }
  wealthtracker
}
head(sim.risk)
```

At first glance, I can see at least one row where the end wealth is in the $800,000's, which I don't believe I saw for either of the previous porfolio scenarios. 

```{r}
hist(sim.risk[,n_days]- initial_wealth, breaks=30)
```

Looking at the risky histogram, it seems somewhat similar to the first scenario, where the histogram is somewhat normal, but instead of a few random marks way on the right side, there is more of a connected tail to the main histogram area. 

```{r}
mean(sim.risk[,n_days])
```

The mean return is the highest we've seen, actually at $100,972. I presume this is from the long tail mostly, because looking at the histogram, it looks like there's a chance to lose about \$200,000, which would be very bad. 

```{r}
quantile(sim.risk[,n_days], 0.05) - initial_wealth
```

This is confirmed at the 5% quantile, which says that there is a 5% chance of losing about \$10,500, which is much worse than the other options as expected. 

#### Summary

Looking at the mean returns and 5% risks, I think it would be easy for an investor to make a decision between the three portfolios depending on how much risk she is comfortable with. we saw that the "high risk, high reward" mantra held true between the three porfolios, and risk increased with the more aggressive porfolios. 

## Market Segmentation

Load the correct libraries. 

```{r echo=FALSE}
library(ggplot2)
library(LICORS)  # for kmeans++
library(foreach)
library(mosaic)
library(fpc)
```

Read in data. 

```{r echo=FALSE}
social_marketing = read.csv('social_marketing.csv')
```

#### K Means Clustering

Now, I'm going to set up the data to make the tweets the dependent variables, and create a k means plot with 7 clusters, because that's a reasonable amount of gropus to segment customers for marketing.

```{r echo=FALSE}
social = social_marketing[,c(2:37)]
cluster_all7 <- kmeans(social, centers=7, nstart=50)
plotcluster(social,cluster_all7$cluster)

```

Seven is clearly way too many clusters and non-sensical. It seems that there really should only be 4 or 5. We should proabably do 5 because segementing customers into 4 groups is not super helpful for marketing

```{r echo=FALSE}
set.seed(89)
cluster_all <- kmeans(social, centers=5, nstart=50)
plotcluster(social,cluster_all$cluster)
```

Five looks fine, with three very clear clusters and two around the center. 

Let's look at some plots to describe the clusters in ways which might be helpful for segmentation: 

```{r echo=FALSE}
par(mfrow=c(2,2))
plot(social[,"health_nutrition"], social[,"news"], 
     type="n", ylab="News", xlab="Health Nutrition")
points(social[,"health_nutrition"], social[,"news"], 
     col=rainbow(5)[cluster_all$cluster]) 
legend("topright",legend=c(1:5),col=rainbow(5),pch=1)

plot(social[,"chatter"], social[,"photo_sharing"], 
     type="n", ylab="photo sharing", xlab="chatter")
points(social[,"chatter"], social[,"uncategorized"], 
       col=rainbow(5)[cluster_all$cluster]) 
legend("topright",legend=c(1:5),col=rainbow(5),pch=1)

plot(social[,"eco"], social[,"cooking"], 
     type="n", ylab="cooking", xlab="eco")
points(social[,"eco"], social[,"cooking"], 
       col=rainbow(5)[cluster_all$cluster]) 
legend("topright",legend=c(1:5),col=rainbow(5),pch=1)

plot(social[,"food"], social[,"personal_fitness"], 
     type="n", ylab="personal fitness", xlab="food")
points(social[,"food"], social[,"personal_fitness"], 
       col=rainbow(5)[cluster_all$cluster]) 
legend("topright",legend=c(1:5),col=rainbow(5),pch=1)

```

Right away we can see that cluster 5 has a lot of tweets about health nutrition and personal fitness, and a good bit about food as well. This group seems likely to be a good fit for our product. Let's call cluster 5 "Fitness Folks", because it is often helpful to come up with a catchy name for each segmentation. Here, I am segmenting based on interests. 

Another group that pops out is cluster 4 - these people tweet a lot about cooking, but I think we can find more info about them. 

Cluster 3 could be interesting as well - these people are high in chatter and have a good amount of news as well, so they could be worth looking into for spam tweets. Let's look a bit more. 

```{r echo=FALSE}
par(mfrow=c(2,2))
plot(social[,"spam"], social[,"adult"], 
     type="n", ylab="adult", xlab="spam")
points(social[,"spam"], social[,"adult"], 
     col=rainbow(5)[cluster_all$cluster]) 
legend("topright",legend=c(1:5),col=rainbow(5),pch=1)

plot(social[,"religion"], social[,"art"], 
     type="n", ylab="art", xlab="religion")
points(social[,"religion"], social[,"art"], 
       col=rainbow(5)[cluster_all$cluster]) 
legend("topright",legend=c(1:5),col=rainbow(5),pch=1)

plot(social[,"outdoors"], social[,"computers"], 
     type="n", ylab="computers", xlab="outdoors")
points(social[,"outdoors"], social[,"computers"], 
       col=rainbow(5)[cluster_all$cluster]) 
legend("topright",legend=c(1:5),col=rainbow(5),pch=1)

plot(social[,"shopping"], social[,"fashion"], 
     type="n", ylab="fashion", xlab="shopping")
points(social[,"shopping"], social[,"fashion"], 
       col=rainbow(5)[cluster_all$cluster]) 
legend("topright",legend=c(1:5),col=rainbow(5),pch=1)
```

Cluster 4 stands out here for fashion and shopping. This fits in with the results above that showed they were interested in cooking. Perhaps these people are mostly lifestyle bloggers or "influencers" who try to tweet about the lastest trends. We'll go with that and call cluster 4 "Lifestyle Bloggers". 

Cluster 3 is not mostly spam as I suspected, but it is a lot of shopping accounts. These could be other brands that follow the NutrientH20 brand. We'll call cluster 3 "Corporate". 

That only leaves 1 and 2 to figure out. Cluster 3 seems to be all over the place, including a lot of adult and spam and art content, but also computers. For now, we'll consider Cluster 2 "Random/Spam". This even makes sense because cluster 2 was centered in the plot cluster graph. That leaves cluster 1, which did not seem to stand out in any of the graphs we have looked at. I want to try one more plot to check on cluster 1: 

```{r echo=FALSE}
par(mfrow=c(1,1))
plot(social[,"online_gaming"], social[,"college_uni"], 
     type="n", ylab="college", xlab="online gaming")
points(social[,"online_gaming"], social[,"college_uni"], 
     col=rainbow(5)[cluster_all$cluster]) 
legend("topright",legend=c(1:5),col=rainbow(5),pch=1)
```

As I suspected, a LOT of 1's popped up here. Let's consider them "Gamers". 

Okay, so we have our clusters now: 

1 - Gamers

2 - Random/Spam

3 - Corporate

4 - Lifestyle Bloggers

5 - Fitness Folks

This is more than enough information to do at least some targeted marketing. We know who could be "influencers" and convince others to use our brand, and we know hobbies of others, like the gamers. If our nutrientH20 product is a health drink, we also know that the fitness folks could be very helful to target, as they may be interested in a healthy product. 

And, for cluster 3, we know we need to investigate a little further before making decisions on them.

#### HClust

I think the K Means worked pretty well for us, but let's look at some other models just to be sure. 

Set up: 

```{r echo=FALSE}
social_scaled = scale(social)
social.pca = prcomp(social_scaled, scale=TRUE, rank=3)
```

Here, let's plot the variances that can be explained by each principal component.

```{r echo=FALSE}
plot(social.pca)
mtext(side=1, "Tweet Difference: Principal Components",  line=1, font=2)
```

We can see that most of the variance is explained in the first principal component, and then there's diminishing returns from there on out. Let's use two. 

```{r echo=FALSE}
social_scores = predict(social.pca)
plot(social_scores[,1:2], pch=21, col=heat.colors(120)[120:1], main="Tweet PC scores")
```

With this graph plotting the first two principal components, we can see that most points are clustered really close in a positive value. The rest are pretty evenly spread out in the highly negative area. 

Let's look at what might be grouping these points: 

```{r}
par(mfrow=c(2,1))
barplot(social.pca$rotation[,1], las=2)
barplot(social.pca$rotation[,2], las=2)
```

For the first graph, it seems that personal tweets - parenting, sports, religion are the most negative. These are personal, but they are also some of the most controversial subjects. That could be some sort of category - people who are active with very personal subjects are in this category of negative variance. 

Spam and adult tweets (which are spam) are the least negative, so they are not personal at all. I think these seem like pretty safe categories for the first principle component. 

For the second principle component, the same personal subjects are negative, but photo-sharing, cooking, fashion and beauty are the most positive. This is a hard distinction to make, because those are personal too. Perhaps the negative tweets are for friends, while the positive tweets are for the public at large. Again, more like what an "influencer" would tweet, while the negative group would be what any social media user would tweet. 

What these categories are basically is HOW people use twitter, whereas the kmeans is based more on interests. 

In conclusion, I would hand the k means clusters to the NutrientH20 group with the conclusions supported above in the k means section. 




