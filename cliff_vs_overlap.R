library(ggplot2)
library(dplyr)
library(DescTools)
library(tidyr)
library(effsize)
library(overlapping)
library(corrplot)
library(jtools)
library(patchwork)
library(diptest)
library(xtable)

#functions -----
overlap_distance = function(F1, F2) {
  #turns the overlap coefficient in to a similarity measures
  sim = as.numeric(overlap(list(F1, F2)), plot = FALSE)
  dissim = 1 - sim
  overlap(list(F1, F2), plot = FALSE)
  return(dissim)
}

#normal distributions
get_norm_distrib = function(m = NULL, sdev = NULL, n = 100){
  # generates one random normal distributions. If no values for mean or standard deviation are supplied, a random value between 0 and 10 is used
  # returns a list with both distributions
  
  if (missing(m) && missing(sdev)){
    rand_sd = runif(1, min = 0, max = 10)
    rand_mean = runif(1, min = 0, max = 10)
    dist = rnorm(n = n, mean = rand_mean, sd = rand_sd)
  }
  else if(missing(m)){
    rand_mean = runif(1, min = 0, max = 10)
    dist_1 = rnorm(n = n, mean = rand_mean, sd = sdev)
  }
  else if(missing(sdev)){
    rand_sd = runif(1, min = 0, max = 10)
    dist = rnorm(n = n, mean = m, sd = rand_sd)
  }
  else {
    dist = rnorm(n = n, mean = m, sd = sdev)
  }
  return(dist)
}

get_2_norm_distrib = function(m = NULL, sdev = NULL){
  # generates two random normal distributions. If no values for mean or standard deviation are supplied, a random value between 0 and 10 is used
  # returns a list with both distributions

  if (missing(m) && missing(sdev)){
    rand_sd_1 = runif(1, min = 0, max = 10)
    rand_mean_1 = runif(1, min = 0, max = 10)
    dist_1 = rnorm(n = 100, mean = rand_mean_1, sd = rand_sd_1)
    rand_sd_2 = runif(1, min = 0, max = 10)
    rand_mean_2 = runif(1, min = 0, max = 10)
    dist_2 = rnorm(n = 100, mean = rand_mean_2, sd = rand_sd_2)
  }
  else if(missing(m)){
    rand_mean_1 = runif(1, min = 0, max = 10)
    dist_1 = rnorm(n = 100, mean = rand_mean_1, sd = sdev)
    rand_mean_2 = runif(1, min = 0, max = 10)
    dist_2 = rnorm(n = 100, mean = rand_mean_2, sd = sdev)
  }
  else if(missing(sdev)){
    rand_sd_1 = runif(1, min = 0, max = 10)
    dist_1 = rnorm(n = 100, mean = m, sd = rand_sd_1)
    rand_sd_2 = runif(1, min = 0, max = 10)
    dist_2 = rnorm(n = 100, mean = m, sd = rand_sd_2)
  }
  else {
    dist_1 = rnorm(n = 100, mean = m, sd = sdev)
    dist_2 = rnorm(n = 100, mean = m, sd = sdev)
  }
  return(list(dist_1, dist_2))
}

get_bimodal_distrib = function(){
  #generates two distributions with random mean (different) and variance(same) and joins them into a bimodal distribution
  sd_rand = runif(1, min = 0, max = 10)
  distribs = get_2_norm_distrib(sdev = sd_rand)
  bimod_distrib = c(distribs[[1]], distribs[[2]])
  return(bimod_distrib)
}

get_measures = function(distrib_list){
  #takes a list with 2 distributions and calculates overlapping distances and estimate
  #of cliffs delta
  cliffs = cliff.delta(distrib_list[[1]], distrib_list[[2]])
  overlap = overlap_distance(distrib_list[[1]], distrib_list[[2]])
  return(c(cliffs$estimate, overlap))
}

get_bc = function(distrib){
  #source: https://www.frontiersin.org/journals/psychology/articles/10.3389/fpsyg.2013.00700/full
  n = length(distrib)
  skewness = Skew(distrib)  
  kurtosis = Kurt(distrib)
  BC = (skewness^2 + 1) / (kurtosis + (3 * (n - 1)^2) / ((n - 2) * (n - 3)))
  return(BC)
}

get_2_norm_distrib()

test = get_2_norm_distrib()
mean(test[[1]])
mean(test[[2]])
plot(density(test[[1]]))
plot(density(test[[2]]))
get_measures(test)

bimod = c(rnorm(n = 100, m = 0, sd = 1), rnorm(n = 100, m = 10, sd = 5))
plot(density(bimod))
get_bc(bimod)
get_bc(test[[1]])
get_bc(test[[2]])
dip.test(bimod)
dip.test(test[[1]])
dip.test(test[[2]])


bimod = get_bimodal_distrib()
get_bc(bimod)
plot(density(bimod))


### generate 1000 random normal distributions and compare ----
set.seed(42)

mean_1 = c()
mean_2 = c()
sd_1 = c()
sd_2 = c()
bc_1 = c()
bc_2 = c()
bimodality = c()
cliffs = c()
overlaps = c()
for (i in 1:500){
  distribs = get_2_norm_distrib()
  measures = get_measures(distribs)
  mean_1 = c(mean_1, mean(distribs[[1]]))
  mean_2 = c(mean_2, mean(distribs[[2]]))
  sd_1 = c(sd_1, sd(distribs[[1]]))
  sd_2 = c(sd_2, sd(distribs[[2]]))
  bc_1 = c(bc_1, get_bc(distribs[[1]]))
  bc_2 = c(bc_2, get_bc(distribs[[2]]))
  bimodality = c(bimodality, max(bc_1, bc_2))
  cliffs = c(cliffs, abs(measures[1]))
  overlaps = c(overlaps, measures[2])
}
rand_df = data.frame(mean_1, mean_2, sd_1, sd_2,bc_1, bc_2, bimodality, cliffs, overlaps)


### cliff vs overlap ----

### unimodal distrib ----

ggplot(data = rand_df, aes(y = overlaps, x = cliffs)) + 
  geom_point() + labs(y = "Overlap distance", x = "Cliffs delta")
#assymetric relationship
#if cliffs delta is large -> overlap distance is also large
#but if cliffs delta is small -> overlap distance can be large
# There is a limit for which values cliffs delta can take depending on the overlap distance

cor.test(rand_df$overlaps, rand_df$cliffs, method = "pearson") #medium positive correlation
cor.test(rand_df$overlaps, rand_df$cliffs, method = "spearman")#medium positive correlation


### diff effsize ----
rand_df = rand_df %>% mutate(diff_mean = abs(mean_1-mean_2), diff_sd = abs(sd_1 - sd_2), diff_distance = abs(overlaps - cliffs))

ggplot(data = rand_df, aes(y = diff_distance, x = diff_sd)) + 
  geom_point() + labs(y = "Difference in effect size", x = "Difference in standard deviation")
#As the difference in sd increases, the differences between the effectsizes increases as well. For standard deviations up to 25, thewre is generally low difference between the effectsizes
cor.test(rand_df$diff_distance, rand_df$diff_sd, method = "spearman") #medium positive correlation

ggplot(data = rand_df, aes(y = diff_distance, x = diff_mean)) + 
  geom_point() + labs(y = "Difference in effect size", x = "Difference in mean")
# As the difference in mean increases, the difference between the effect sizes decrease.  
cor.test(rand_df$diff_distance, rand_df$diff_mean, method = "spearman") #negative small correlation

### overlap ----

p1 = ggplot(data = rand_df, aes(y = overlaps, x = diff_sd)) + 
  geom_point() + labs(y = "overlapping distance", x = "Difference in standard deviation")
# the difference in standard deviation sets a limit for the values the overlap distance can take.
# for large difference in standard deviations, the overlapping distance has to be large
cor.test(rand_df$overlaps, rand_df$diff_sd, method = "spearman") # medium positive
print(p1)

p2 = ggplot(data = rand_df, aes(y = overlaps, x = diff_mean)) + 
  geom_point() + labs(y = "overlapping distance", x = "Difference in mean")
# similar to with standard deviation but not as clear cut
cor.test(rand_df$overlaps, rand_df$diff_mean, method = "spearman") # small postive
print(p2)

### cliffs ----
p3 = ggplot(data = rand_df, aes(y = cliffs, x = diff_sd)) + 
  geom_point() + labs(y = "Cliff's Delta", x = "Difference in standard deviation")
#opposite to the relationship between overlap and standard deviation.
# for large differences in standard deviation cliffs delta is small
cor.test(rand_df$cliffs, rand_df$diff_sd, method = "spearman") # negligible correlation
print(p3)

p4 = ggplot(data = rand_df, aes(y = cliffs, x = diff_mean)) + 
  geom_point() + labs(y = "Cliff's Delta", x = "Difference in mean")
# Clear linear relationship, as the difference in mean increases, cliffs delta increases.
# Heteroscedastic
cor.test(rand_df$cliffs, rand_df$diff_mean, method = "spearman") # large postive
print(p4)

p5 = ggplot(data = rand_df, aes(y = cliffs, x = overlaps)) + 
  geom_point() + labs(y = "Cliff's Delta", x = "overlap distance")
cor.test(rand_df$cliffs, rand_df$overlaps, method = "spearman")

p1 + p2 + p3 + p4

p5


#model

### bimodality

set.seed(123)

mean_1 = c()
mean_2 = c()
sd_1 = c()
sd_2 = c()
bc_1 = c()
bc_2 = c()
cliffs = c()
overlaps = c()
for (i in 1:500){
  bimod_distrib = get_bimodal_distrib()
  unimod_distrib = get_norm_distrib(n = 200)
  distribs = list(bimod_distrib, unimod_distrib)
  measures = get_measures(distribs)
  mean_1 = c(mean_1, mean(distribs[[1]]))
  mean_2 = c(mean_2, mean(distribs[[2]]))
  sd_1 = c(sd_1, sd(distribs[[1]]))
  sd_2 = c(sd_2, sd(distribs[[2]]))
  bc_1 = c(bc_1, get_bc(distribs[[1]]))
  bc_2 = c(bc_2, get_bc(distribs[[2]]))
  cliffs = c(cliffs, abs(measures[1]))
  overlaps = c(overlaps, measures[2])
}
bimod_df = data.frame(mean_1, mean_2, sd_1, sd_2,bc_1, bc_2, cliffs, overlaps)



#### diff effsize
bimod_df = bimod_df %>% mutate(diff_mean = abs(mean_1-mean_2), diff_sd = abs(sd_1 - sd_2), diff_bimod = abs(bc_1 - bc_2), diff_distance = abs(overlaps - cliffs))

ggplot(data = bimod_df, aes(y = diff_distance, x = diff_sd)) + 
  geom_point() + labs(y = "Difference in effect size", x = "Difference in standard deviation")
#As the difference in sd increases, the differences between the effectsizes increases as well. For standard deviations up to 25, thewre is generally low difference between the effectsizes

cor.test(bimod_df$diff_distance, bimod_df$diff_sd, method = "spearman") #medium positive correlation

ggplot(data = bimod_df, aes(y = diff_distance, x = diff_mean)) + 
  geom_point() + labs(y = "Difference in effect size", x = "Difference in mean")
# As the difference in mean increases, the difference between the effect sizes decrease.  
cor.test(bimod_df$diff_distance, bimod_df$diff_mean, method = "spearman") #negative small correlation

ggplot(data = bimod_df, aes(y = diff_distance, x = diff_bimod)) + 
  geom_point() + labs(y = "Difference in effect size", x = "Difference in modality")
# As the difference in mean increases, the difference between the effect sizes decrease.  
cor.test(bimod_df$diff_distance, bimod_df$diff_bimod, method = "spearman") #negative small correlation


### overlap ----

b1 = ggplot(data = bimod_df, aes(y = overlaps, x = diff_sd)) + 
  geom_point() + labs(y = "overlapping distance", x = "Difference in standard deviation")
# the difference in standard deviation sets a limit for the values the overlap distance can take.
# for large difference in standard deviations, the overlapping distance has to be large
cor.test(bimod_df$overlaps, bimod_df$diff_sd, method = "spearman") # medium positive
print(b1)

b2 = ggplot(data = bimod_df, aes(y = overlaps, x = diff_mean)) + 
  geom_point() + labs(y = "overlapping distance", x = "Difference in mean")
# similar to with standard deviation but not as clear cut
cor.test(bimod_df$overlaps, bimod_df$diff_mean, method = "spearman") # small postive
print(b2)

b3 = ggplot(data = bimod_df, aes(y = overlaps, x = diff_bimod)) + 
  geom_point() + labs(y = "overlapping distance", x = "Difference in modality")
# similar to with standard deviation but not as clear cut
cor.test(bimod_df$overlaps, bimod_df$diff_bimod, method = "spearman") # small postive
print(b3)

### cliffs ----
b4 = ggplot(data = bimod_df, aes(y = cliffs, x = diff_sd)) + 
  geom_point() + labs(y = "Cliff's Delta", x = "Difference in standard deviation")
cor.test(bimod_df$cliffs, bimod_df$diff_sd, method = "spearman") # negligible correlation
print(b4)#cliffs delta is fairly insensitive to differences in standard deviations

b5 = ggplot(data = bimod_df, aes(y = cliffs, x = diff_mean)) + 
  geom_point() + labs(y = "Cliff's Delta", x = "Difference in mean")
# Clear linear relationship, as the difference in mean increases, cliffs delta increases.
# Heteroscedastic
cor.test(bimod_df$cliffs, bimod_df$diff_mean, method = "spearman") # large postive
print(b5)


b6 = ggplot(data = bimod_df, aes(y = cliffs, x = diff_bimod)) + 
  geom_point() + labs(y = "Cliff's Delta", x = "Difference in modality")
# Clear linear relationship, as the difference in mean increases, cliffs delta increases.
# Heteroscedastic
cor.test(bimod_df$cliffs, bimod_df$diff_bimod, method = "spearman") # large postive
print(b6)

b7 = ggplot(data = bimod_df, aes(y = cliffs, x = overlaps)) + 
  geom_point() + labs(y = "Cliff's Delta", x = "overlap distance")
cor.test(rand_df$cliffs, rand_df$overlaps, method = "spearman")

b1 + b2 + b3 + b4 + b5 + b6 + plot_layout(ncol = 2)

b1 + b4 + b2 + b5 + b3 + b6 + plot_layout(ncol = 2)

b7 


#### derive cohens'd benchmarks

test = get_2_norm_distrib(sdev = 10)
cohen.d(test[[1]], test[[2]])$estimate
cliff.delta(test[[1]], test[[2]])$estimate
overlap_distance(test[[1]], test[[2]])
test_df = data.frame(test[[1]], test[[2]])
colnames(test_df) = c("test1", "test2")

ggplot(data = test_df) + 
  geom_density(aes(x = test1, fill = "test1", alpha = 0.5)) + 
  geom_density(aes(x = test2, fill = "test2", alpha = 0.5))

cliff = c()
over = c()
cohen = c()
for (i in 1:500){
  distribs = get_2_norm_distrib(sdev = 5)
  dist_1 = distribs[[1]]
  dist_2 = distribs[[2]]
  cliff = c(cliff, cliff.delta(dist_1, dist_2)$estimate)
  cohen = c(cohen, cohen.d(dist_1, dist_2)$estimate)
  over = c(over, overlap_distance(dist_1, dist_2))
}

effsizes = data.frame(cliff, over, cohen)


effsizes$cliff = abs(effsizes$cliff)
effsizes$cohen = abs(effsizes$cohen)

effsizes = effsizes[effsizes$cohen < 1, ]

plot(effsizes$cohen, effsizes$cliff)
plot(effsizes$cohen, effsizes$over)

#create a model of cliff based on cohen and infer cliff for values of cohen(0.2, 0.5, 0.8)
benchmarks = data.frame(cohen = c(0.2, 0.5, 0.8))
mdl_cliff = lm(data = effsizes, cliff ~ cohen)
summary(mdl_cliff)
pred_cliff = predict(mdl_cliff, benchmarks)

#same but for overlapping distance
mdl_overlap = lm(data = effsizes, over ~ cohen)
summary(mdl_overlap)
pred_overlap = predict(mdl_overlap, benchmarks)


labs = c("small", "mediun", "large")
out_tab = data.frame(labs, benchmarks, pred_cliff, pred_overlap )
head(out_tab)

print(xtable(out_tab), type = "latex")
