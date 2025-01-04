library(ggplot2)
library(dplyr)
library(DescTools)
library(readr)
library(tidyr)
library(effsize)
library(overlapping)
library(phonR)
library(geometry)
library(scales)
library(gt)
library(corrplot)
library(sf)
library(patchwork)

default_colors = hue_pal()(10)
print(default_colors)
C_col = default_colors[1]
D_col = default_colors[6]
B_col = default_colors[4]

CD_cols = c(C_col, D_col)
BD_cols = c(B_col, D_col)
BCD_cols = c(B_col, C_col, D_col)

C = read.csv("clean_data/C_clean")
D = read.csv("clean_data/D_clean")
B = read.csv("clean_data/B_clean")

### comparing C and D, colored and white speakers with word lists

CD = rbind(C, D)
BD = rbind(B, D)
BCD = rbind(B, CD)
       
       
#summary statistics
sum_CD = CD %>% group_by(Set) %>% summarize(
  n_instances = n(),
  n_speakers = length(unique(Speaker)),
  n_vowels = length(unique(Vowel)),
  mean_F1 = mean(F1),
  sd_F1 = SD(F1),
  skew_F1 = Skew(F1),
  mean_F2 = mean(F2),
  sd_F2 = SD(F2),
  skew_F2 = Skew(F2),
  median_F1 = median(F1),
  median_F2 = median(F2),
  median_F1_norm = median(F1_norm),
  median_F2_norm = median(F2_norm)
)

head(sum_CD)

### distributions

##boxplots
ggplot(data = CD,
       map = aes(y = F1_norm, fill = Set, color = Set)) + geom_boxplot(color = "black") + labs(title = "distribution of F1 across C and D")
ggplot(data = CD,
       map = aes(y = F2_norm, fill = Set, color = Set)) + geom_boxplot(color = "black") + labs(title = "distribution of F2 across C and D")
#per vowel
ggplot(data = CD,
       map = aes(
         y = F1,
         x = Vowel,
         fill = Set,
         color = Set
       )) + geom_boxplot(color = "black") + labs(title = "distribution of F1 across vowels for C and D")
ggplot(data = CD,
       map = aes(
         y = F2,
         x = Vowel,
         fill = Set,
         color = Set
       )) + geom_boxplot(color = "black") + labs(title = "distribution of F2 across vowels C and D")

## density
#f1
ggplot(data = CD,
       map = aes(x = F1, fill = Set)) + geom_density(alpha = 0.5) +
  labs(title = "Density of F1 for C and D", x = "F1(Hz)", y = "Density")
ggplot(data = CD,
       map = aes(x = F1_norm, fill = Set)) + geom_density(alpha = 0.5) +
  labs(title = "Density of F1 for C and D", x = "F1(SD)", y = "Density")
#f2
ggplot(data = CD,
       map = aes(x = F2, fill = Set)) + geom_density(alpha = 0.5) +
  labs(title = "Density of F1 for C and D", x = "F2(Hz)", y = "Density")
ggplot(data = CD,
       map = aes(x = F2_norm, fill = Set)) + geom_density(alpha = 0.5) +
  labs(title = "Density of F2 for C and D", x = "F1(SD)", y = "Density")




###vowels spaces
ggplot(data = CD,
       map = aes(
         y = F1,
         x = F2,
         fill = Set,
         colour = Set
       )) + geom_point(alpha = 0.3) + stat_ellipse() +
  geom_text(
    data = CD %>% group_by(Set) %>% summarize(F1 = median(F1), F2 = median(F2)),
    aes(y = F1, x = F2, label = Set),
    alpha = 1,
    size = 10,
    fontface = "bold"
  ) +
  scale_y_reverse() + scale_x_reverse() + labs(title = 'Vowel space of C and D in Hz') + theme(legend.position = "none")

# C
ggplot(data = C,
       map = aes(
         y = F1,
         x = F2,
         fill = Vowel,
         colour = Vowel
       )) + geom_point(alpha = 0.1) + stat_ellipse(alpha = 1) +
  geom_text(
    data = C %>% group_by(Vowel) %>% summarize(F1 = median(F1), F2 = median(F2)),
    aes(y = F1, x = F2, label = Vowel),
    alpha = 1,
    size = 10,
    fontface = "bold"
  )  +
  scale_x_reverse() + scale_y_reverse() +
  labs(title = 'Vowel space of C in HZ', x = "F2 (Hz)", y = "F1 (Hz)") +
  theme(legend.position = "none")

p_c = ggplot(data = C,
       map = aes(
         y = F1_norm,
         x = F2_norm,
         fill = Vowel,
         colour = Vowel
       )) + geom_point(alpha = 0.1) + stat_ellipse(alpha = 1) +
  geom_text(
    data = C %>% group_by(Vowel) %>% summarize(F1 = median(F1_norm), F2 = median(F2_norm)),
    aes(y = F1, x = F2, label = Vowel),
    alpha = 1,
    size = 10,
    fontface = "bold"
  )  +
  scale_x_reverse() + scale_y_reverse() +
  labs(title = 'Vowel space of C in standard deviations of speakers', x = "F2 (SD)", y = "F1 (SD)") +
  theme(legend.position = "none")


#D
ggplot(data = D,
       map = aes(
         y = F1,
         x = F2,
         fill = Vowel,
         colour = Vowel
       )) + geom_point(alpha = 0.1) + stat_ellipse(alpha = 1) +
  geom_text(
    data = D %>% group_by(Vowel) %>% summarize(F1 = median(F1), F2 = median(F2)),
    aes(y = F1, x = F2, label = Vowel),
    alpha = 1,
    size = 10,
    fontface = "bold"
  )  +
  scale_x_reverse() + scale_y_reverse() +
  labs(title = 'Vowel space of D in HZ', x = "F2 (Hz)", y = "F1 (Hz)") +
  theme(legend.position = "none")

p_d = ggplot(data = D,
       map = aes(
         y = F1_norm,
         x = F2_norm,
         fill = Vowel,
         colour = Vowel
       )) + geom_point(alpha = 0.1) + stat_ellipse(alpha = 1) +
  geom_text(
    data = D %>% group_by(Vowel) %>% summarize(F1 = median(F1_norm), F2 = median(F2_norm)),
    aes(y = F1, x = F2, label = Vowel),
    alpha = 1,
    size = 10,
    fontface = "bold"
  )  +
  scale_x_reverse() + scale_y_reverse() +
  labs(title = 'Vowel space of D in standard deviations of speakers', x = "F2 (SD)", y = "F1 (SD)") +
  theme(legend.position = "none")

p_c + p_d
#summaries
Desc(C$F1)
Desc(log(C$F1))
Desc(C$F2)
Desc(D$F1)
Desc(log(D$F1))
Desc(D$F2)
#multimodal
#can't transform to normality -> non parametric tests required


#### Distribution approach

#Mann whitney U test assumes that the distributions have similar shape, and measures
wilcox.test(C$F1, D$F1) # p < 0.001
wilcox.test(C$F1_norm, D$F1_norm) # p > 0.05 ->  no significant effect

wilcox.test(C$F2, D$F2) # p > 0.05 -> no significant effect
wilcox.test(C$F2_norm, D$F2_norm) # p  > 0.05 -> no significant effect
# considering the entire vowel space, the data does not support a significant difference in overlap of distribution between F1 and F2
# for white and coloured speakers, after accounting for biological differences.

#mann whitney U assumes independence within the groups, but we have multiple vowels per speaker -> violates the assumption of independence
#for Effectsize, making use of cliffs delta is a good idea, as it does not assume independence within the groups, only between
cliff.delta(C$F1_norm, D$F1_norm) #effect size is not different from zero, 0.0046 estimated
cliff.delta(C$F2_norm, D$F2_norm) # effect size is not different from zero, 0.0031 estimated
# the probability of observing a f2 or f1 lower or higher for the two groups is not different from zero

# to address independence problem, and get a better idea of variations within the space, we can look at each vowel independently

# looking at each vowel individually
vowels = unique(D$Vowel)
lci_f1_norm = c()
uci_f1_norm = c()
lci_f2_norm = c()
uci_f2_norm = c()
est_f1_norm = c()
est_f2_norm = c()
for (vowel in vowels) {
  D_f1_norm = D$F1_norm[D$Vowel == vowel]
  D_f2_norm = D$F2_norm[D$Vowel == vowel]
  C_f1_norm = C$F1_norm[C$Vowel == vowel]
  C_f2_norm = C$F2_norm[C$Vowel == vowel]
  
  lci_f1_norm = c(lci_f1_norm, as.numeric(cliff.delta(D_f1_norm, C_f1_norm)$conf.int[1]))
  uci_f1_norm = c(uci_f1_norm, as.numeric(cliff.delta(D_f1_norm, C_f1_norm)$conf.int[2]))
  est_f1_norm = c(est_f1_norm, as.numeric(cliff.delta(D_f1_norm, C_f1_norm)$estimate))
  
  lci_f2_norm = c(lci_f2_norm, as.numeric(cliff.delta(D_f2_norm, C_f2_norm)$conf.int[1]))
  uci_f2_norm = c(uci_f2_norm, as.numeric(cliff.delta(D_f2_norm, C_f2_norm)$conf.int[2]))
  est_f2_norm = c(est_f2_norm, as.numeric(cliff.delta(D_f2_norm, C_f2_norm)$estimate))

}

df_cliff = data.frame(vowels, lci_f1_norm, uci_f1_norm, est_f1_norm, lci_f2_norm, uci_f2_norm, est_f2_norm)
head(df_cliff)
 
plot_data = df_cliff %>% pivot_longer(
  cols = starts_with("lci") | starts_with("uci") | starts_with("est"),
  names_to = c("measure", "formant"),
  names_sep = "_",
  values_to = "value") %>%
  pivot_wider(
    names_from = measure,
    values_from = value)

plot_data = plot_data %>% mutate(formant = case_when(
  formant == "f1" ~ "F1",
  formant == "f2" ~ "F2"
))
plot_data = plot_data %>% rename(Formant = formant)          
          
cliffs_plot_CD = ggplot(data = plot_data, aes(y = est, x = vowels, fill = Formant)) +
         geom_bar(stat = 'identity', position = 'dodge2') + 
         geom_errorbar(aes(ymin = lci, ymax = uci), position = position_dodge(width = 0.9), width = 0.2) + 
         labs(y = "Cliff's Delta", x = "Vowels", title = "Comparison of Vowels in C and D using Cliff's Delta")


#positive cliff delta -> values of C tends to be larger

# a key take away here is that alltough we do not find a difference when looking at the entire vowelspace, but looking at the vowels individually, there are distinct differences
# going in multiple directions, which must result in these differences evening each other out when combined.
# therefore, we have to create an index, averaging the differences per vowel to get a distance measure
sum_delta = sum(abs(df_cliff$est_f1_norm)) + sum(abs(df_cliff$est_f2_norm))
index_cliff = sum_delta / (2 * nrow(df_cliff))
print(index_cliff)


#the distance between the vowel spaces measured as the average cliffs delta differences of f1 and f2 for the vowels is 0.44
# we could call this a "distinction" index, it summarizes the magnitude of differences in formant values and summarizes the effect size of differences in vowels.
#the larger the distinction index, the more the vowels in the space tends to be displaced from one another.
# If two vowels space have a large distinction index, it means the vowels in the space are very distinct from each other.

#to calculate the contribution of each vowel, we can add together the absolute values of the cliff deltas and scale to a percentage
head(df_cliff)
df_cliff = df_cliff %>% mutate(contribution = (abs(est_f1_norm) + abs(est_f2_norm)) /
                                 (sum(abs(est_f1_norm)) + sum(abs(est_f2_norm))))
contrib_plot_CD = ggplot(data = df_cliff,
       map = aes(y = contribution, x = vowels, fill = vowels)) + 
  geom_bar(stat = 'identity') + 
  labs(title = "Contribution of vowels to Cliffs Index",
       x = "Vowels",
       y = "Contribution Score") + 
  theme(legend.position = "none") 
#from this, we can see that a, æ and æu contribute the most to any difference, with o, ø and æy contributing the least.

cliffs_plot_CD / contrib_plot_CD

#Cliffs Delta measures the dominance of one distribution over the other, this has the advantage that a directionality is given for the distance between each vowel compared to overlap
#it measures overlap indirectly -> 1 & ->-1 implies no overlap / ->0<- implies identical distributions
#overlap coefficient = percentage of area overlapping
#cliffs delta = probability of observing larger or smaller values

#we implement the calculations above as functions to make future analysis easier
get_cliff_delta = function(D1, D2) {
  #this function takes two dataframes with vowels and their F1 and F2 values, and creates
  #a new dataframe with calculated p values and cliffs delta for each vowel
  vowels = unique(D$Vowel)
  lci_F1_norm = c()
  uci_F1_norm = c()
  lci_F2_norm = c()
  uci_F2_norm = c()
  est_F1_norm = c()
  est_F2_norm = c()
  for (vowel in vowels) {
    D1_F1_norm = D1$F1_norm[D1$Vowel == vowel]
    D1_F2_norm = D1$F2_norm[D1$Vowel == vowel]
    D2_F1_norm = D2$F1_norm[D2$Vowel == vowel]
    D2_F2_norm = D2$F2_norm[D2$Vowel == vowel]
    
    lci_F1_norm = c(lci_F1_norm, as.numeric(cliff.delta(D1_F1_norm, D2_F1_norm)$conf.int[1]))
    uci_F1_norm = c(uci_F1_norm, as.numeric(cliff.delta(D1_F1_norm, D2_F1_norm)$conf.int[2]))
    est_F1_norm = c(est_F1_norm, as.numeric(cliff.delta(D1_F1_norm, D2_F1_norm)$estimate))
    
    lci_F2_norm = c(lci_F2_norm, as.numeric(cliff.delta(D1_F2_norm, D2_F2_norm)$conf.int[1]))
    uci_F2_norm = c(uci_F2_norm, as.numeric(cliff.delta(D1_F2_norm, D2_F2_norm)$conf.int[2]))
    est_F2_norm = c(est_F2_norm, as.numeric(cliff.delta(D1_F2_norm, D2_F2_norm)$estimate))
    
  }
  cliff = data.frame(vowels, lci_F1_norm, uci_F1_norm, est_F1_norm, lci_F2_norm, uci_F2_norm, est_F2_norm)
  return(cliff)
}


get_plot_format_cliff = function(df){
  plot_data = df %>% pivot_longer(
    cols = starts_with("lci") | starts_with("uci") | starts_with("est"),
    names_to = c("measure", "formant"),
    names_sep = "_",
    values_to = "value") %>%
    pivot_wider(
      names_from = measure,
      values_from = value)
  return(plot_data)
}


get_index_cliff = function(df) {
  #takes a dataframe with cliffs deltas for F1 and F2 for multiple vowels
  #calculates an index as the average F1 and F2 for all vowels.
  sum_delta = sum(abs(df$est_F1_norm)) + sum(abs(df$est_F2_norm))
  index_cliff = sum_delta / (2 * nrow(df_cliff))
  return(index_cliff)
}

add_contribution_cliff = function(df) {
  #calculates the contribution of each vowel to the index
  df = df %>% mutate(contribution = (abs(est_F1_norm) + abs(est_F2_norm)) /
                       (sum(abs(est_F1_norm)) + sum(abs(est_F2_norm))))
  return(df)
}


### Using overlapping index

overlap_distance = function(F1, F2) {
  #turns the overlap coefficient in to a similarity measures
  #plots the overlapping distributions
  sim = as.numeric(overlap(list(F1, F2)), plot = FALSE)
  dissim = 1 - sim
  overlap(list(F1, F2), plot = FALSE)
  return(dissim)
}



overlap_distance(C$F1_norm, D$F1_norm) #0.102
overlap_distance(C$F2_norm, D$F2_norm) #0.123,

vowels = unique(D$Vowel)
f1_distances = c()
f2_distances = c()
for (vowel in vowels) {
  f1_overlap = overlap_distance(C$F1_norm[C$Vowel == vowel], D$F1_norm[D$Vowel == vowel])
  f1_distances = c(f1_distances, f1_overlap)
  f2_overlap = overlap_distance(C$F2_norm[C$Vowel == vowel], D$F2_norm[D$Vowel == vowel])
  f2_distances = c(f2_distances, f2_overlap)
}

df_overlap_CD = data.frame(vowels, f1_distances, f2_distances)
head(df_overlap_CD)

index_overlap = (sum(df_overlap_CD$f1_distances) + sum(df_overlap_CD$f2_distances)) /
  (length(vowels) * 2)
print(index_overlap) # Overlap index for the vowel spaces 0.43

df_overlap_CD = df_overlap_CD %>% rename(F1 = f1_distances, F2 = f2_distances)

# plotting the overlap distance for each vowel
plot_overlap_vowel_CD = ggplot(
  data = df_overlap_CD %>% pivot_longer(
    cols = c(F1, F2),
    names_to = "Formant",
    values_to = "overlap_distance"
  ),
  map = aes(y = overlap_distance, x = vowels, fill = Formant)
) + geom_bar(stat = 'identity', position = "dodge2") +
  ylim(c(0, 1)) + 
  labs(y = "Overlap Distance", x = "Vowels", title = "Comparison of Vowels using Overlap Distance")
#similar to the plot of cliffs delta, but we lack direction!
plot(plot_overlap_vowel_CD)

#similarly, we can calculate contribution as with cliffs delta
df_overlap_CD = df_overlap_CD %>% mutate(contribution = (F1 + F2) /
                                     (sum(F1) + sum(F2)))
plot_contrib_overlap_CD = ggplot(data = df_overlap_CD,
       map = aes(y = contribution, x = vowels, fill = vowels)) + geom_bar(stat = 'identity') +
  labs(title = "Contribution of Vowels to Overlap Index") + 
  theme(legend.position = "none")

plot_overlap_vowel_CD / plot_contrib_overlap_CD

#what is the difference in vowel contribution between overlap index and distinction index?
df_overlap_CD$contribution_diff = df_overlap_CD$contribution - df_cliff$contribution
ggplot(data = df_overlap_CD,
       map = aes(y = contribution_diff, x = vowels)) + geom_bar(stat = 'identity') + labs(title = "difference in conitrbution of vowels to overlap and distinciton index")
#postive = contributes more to the overlap index


plot(df_overlap$contribution, df_cliff$contribution)
cor(df_overlap$contribution, df_cliff$contribution)

#does the dominance and overlap index correlate generally?
plot(df_overlap$f1_distances , abs(df_cliff$est_f1_norm))
cor(df_overlap$f1_distances, abs(df_cliff$est_f1_norm)) #0.85 -> very highly correlated

plot(df_overlap$f2_distances , abs(df_cliff$est_f2_norm))
cor(df_overlap$f2_distances, abs(df_cliff$est_f2_norm)) #0.87 -> very highly correlated

#the point where we would see great differences between overlap and cliffs delta would be the vowels have the same median but different variance.

#The overlap index gives the percentage of overlap between the vowelspace
#The distinction index summarizes the magnitude of the differences between vowels. The strength of the distinction index is that one can analyse the direction of the effect for single vowels formant values

#With regards to the difference between C and D (white and coloured), we can conclude that there is no statistical significant difference between the vowel spaces as a whole, but
#on a vowel level there are significant differences in f1 and f2 values for all vowels except /i/ and /y/


#implement the overlap index as functions

get_overlap = function(D1, D2) {
  #takes two dataframe with vowels and F1 and F2 values and calculates the overlap distance between each vowel
  vowels = unique(D1$Vowel)
  f1_distances = c()
  f2_distances = c()
  for (vowel in vowels) {
    f1_overlap = overlap_distance(D1$F1_norm[D1$Vowel == vowel], D2$F1_norm[D2$Vowel == vowel])
    f1_distances = c(f1_distances, f1_overlap)
    f2_overlap = overlap_distance(D1$F2_norm[D1$Vowel == vowel], D2$F2_norm[D2$Vowel == vowel])
    f2_distances = c(f2_distances, f2_overlap)
  }
  df = data.frame(vowels, f1_distances, f2_distances)
  return(df)
}

get_index_overlap = function(df) {
  #takes a dataframe with overlap distances for each value and calculates an index
  index_overlap = (sum(df$f1_distances) + sum(df$f2_distances)) / (nrow(df) * 2)
  return(index_overlap)
}

add_contribution_overlap = function(df) {
  #adds a column which quantifies the contribution to the overlap index for each vowel
  df = df %>% mutate(contribution = (f1_distances + f2_distances) /
                                       (sum(f1_distances) + sum(f2_distances)))
  return(df)
}


#### geometric based approach

# Convex hull to visualize vowel spaces
#convex hull is here based on medians

C_vowel_medians = C %>% group_by(Vowel) %>% summarize(
  F1 = median(F1),
  F2 = median(F2),
  F1_norm = median(F1_norm),
  F2_norm = median(F2_norm)
)
C_vowel_medians = as.data.frame(C_vowel_medians)

indices = chull(C_vowel_medians$F2, C_vowel_medians$F1)
indices = c(indices, indices[1])
hull_C = convhulln(C_vowel_medians[indices, c("F1", "F2")], "FA")
hull_area_C = hull_C$area
print(hull_area_C)#3424.479
hull_points_C = hull_C$p


ggplot(data = C,
       map = aes(
         y = F1,
         x = F2,
         fill = Vowel,
         color = Vowel
       )) + geom_point(alpha = 0.1) +
  geom_polygon(
    data = hull_points_C,
    linetype = "dashed",
    color = "blue",
    fill = NA
  ) +
  geom_text(
    data = C %>% group_by(Vowel) %>% summarize(F1 = median(F1), F2 = median(F2)),
    aes(y = F1, x = F2, label = Vowel),
    alpha = 1,
    size = 10,
    fontface = "bold"
  ) +
  scale_x_reverse() + scale_y_reverse() +
  labs(title = 'Vowel space of C in HZ', x = "F2 (Hz)", y = "F1 (Hz)") +
  theme(legend.position = "none")


indices_norm = chull(C_vowel_medians$F2_norm, C_vowel_medians$F1_norm)
indices_norm = c(indices_norm, indices_norm[1])
hull_C_norm = convhulln(C_vowel_medians[indices_norm, c("F1_norm", "F2_norm")], "FA")
hull_area_C_norm = hull_C_norm$area
print(hull_area_C_norm) #8.92
hull_points_C_norm = hull_C_norm$p


ggplot(data = C,
       map = aes(
         y = F1_norm,
         x = F2_norm,
         fill = Vowel,
         color = Vowel
       )) + geom_point(alpha = 0.1) +
  geom_polygon(
    data = hull_points_C_norm,
    linetype = "dashed",
    color = "blue",
    fill = NA
  ) +
  geom_text(
    data = C %>% group_by(Vowel) %>% summarize(F1 = median(F1_norm), F2 = median(F2_norm)),
    aes(y = F1, x = F2, label = Vowel),
    alpha = 1,
    size = 10,
    fontface = "bold"
  ) +
  scale_x_reverse() + scale_y_reverse() +
  labs(title = 'Vowel space of C in HZ', x = "F2 (Hz)", y = "F1 (Hz)") +
  theme(legend.position = "none")

#put it into a function
get_hull_hz = function(df) {
  medians = df %>% group_by(Vowel) %>% summarize(F1 = median(F1), F2 = median(F2))
  medians = as.data.frame(medians)
  indices = chull(medians$F2, medians$F1)
  indices = c(indices, indices[1])
  hull = convhulln(medians[indices, c("F1", "F2")], "FA")
  return(hull)
}

get_hull_norm = function(df) {
  medians = df %>% group_by(Vowel) %>% summarize(F1_norm = median(F1_norm), F2_norm = median(F2_norm))
  medians = as.data.frame(medians)
  indices = chull(medians$F2, medians$F1)
  indices = c(indices, indices[1])
  hull = convhulln(medians[indices, c("F1_norm", "F2_norm")], "FA")
  return(hull)
}


#D
hull_D_hz = get_hull_hz(D)
hull_area_D = hull_D_hz$area
print(hull_area_D)# 4275
hull_points_D = hull_D_hz$p

ggplot(data = D,
       map = aes(
         y = F1,
         x = F2,
         fill = Vowel,
         color = Vowel
       )) + geom_point(alpha = 0.1) +
  geom_polygon(
    data = hull_points_D,
    linetype = "dashed",
    color = "blue",
    fill = NA
  ) +
  geom_text(
    data = D %>% group_by(Vowel) %>% summarize(F1 = median(F1), F2 = median(F2)),
    aes(y = F1, x = F2, label = Vowel),
    alpha = 1,
    size = 10,
    fontface = "bold"
  ) +
  scale_x_reverse() + scale_y_reverse() +
  labs(title = 'Vowel space of D in HZ', x = "F2 (Hz)", y = "F1 (Hz)") +
  theme(legend.position = "none")


hull_D_norm = get_hull_norm(D)
hull_area_D_norm = hull_D_norm$area
print(hull_area_D_norm) #9.11
hull_points_D_norm = hull_D_norm$p

hull_area_diff_hz = abs(hull_area_C - hull_area_D)
hull_area_diff_norm = abs(hull_area_C_norm - hull_area_D_norm)


ggplot(data = D,
       map = aes(
         y = F1_norm,
         x = F2_norm,
         fill = Vowel,
         color = Vowel
       )) + geom_point(alpha = 0.1) +
  geom_polygon(
    data = hull_points_D_norm,
    linetype = "dashed",
    color = "blue",
    fill = NA
  ) +
  geom_text(
    data = D %>% group_by(Vowel) %>% summarize(F1 = median(F1_norm), F2 = median(F2_norm)),
    aes(y = F1, x = F2, label = Vowel),
    alpha = 1,
    size = 10,
    fontface = "bold"
  ) +
  scale_x_reverse() + scale_y_reverse() +
  labs(title = 'Vowel space of D in SD with Convex Hull', x = "F2 (SD)", y = "F1 (SD)") +
  theme(legend.position = "none")


# C and D
cvx_hull_cd = ggplot(data = CD,
       map = aes(
         y = F1_norm,
         x = F2_norm,
         fill = Set,
         color = Set
       ))  +
  geom_text(
    data = CD %>% group_by(Set, Vowel) %>% summarize(F1 = median(F1_norm), F2 = median(F2_norm)),
    aes(y = F1, x = F2, label = Vowel),
    alpha = 1,
    size = 10,
    fontface = "bold"
  ) +
  geom_polygon(
    data = hull_points_C_norm,
    linetype = "solid",
    color = C_col,
    fill = NA
  ) +
  geom_polygon(
    data = hull_points_D_norm,
    linetype = "solid",
    color = D_col,
    fill = NA
  ) +
  scale_x_reverse() + scale_y_reverse() +
  labs(title = 'Convex Hulls of C and D', x = "F2 (SD)", y = "F1 (SD)") +
  theme()


#difference in vowel space =  difference in area of the convex hull of C and D
diff_area = abs(hull_area_C - hull_area_D)
diff_area_norm = abs(hull_area_C_norm - hull_area_D_norm)
print(diff_area) #851 Hz^2
print(diff_area_norm)# 0.198 SD^2

#difference in vowel space = intersected area as part of union area

get_iou_distance = function(hull_points_1, hull_points_2){
  #takes two dataframes with hull points and caclulates 1 - intersect under area
  hull_1 = st_sfc(st_polygon(list(hull_points_1)))
  hull_2 = st_sfc(st_polygon(list(hull_points_2)))
  
  intersection = st_intersection(hull_1, hull_2)
  union = st_union(hull_1, hull_2)
  
  intersection_area = st_area(intersection)
  union_area = st_area(union)
  iou = intersection_area / union_area
  return(1-iou)
}

iou_CD = get_iou_distance(hull_points_C_norm, hull_points_D_norm)
print(iou_CD)
# Each vowel has a Centroid in the vowel space. By comparing the euclidean distance between the Centroids, we arrive at a distance measure.
# To compare the dispersion of vowels within the vowel space, we can calculate the average distance of each vowels Centroid to the entire vowel spaces centroid

#we use the median F1 and F2 for the vowels as centroids

F1_C_center = median(C$F1_norm)
F2_C_center = median(C$F2_norm)
F1_D_center = median(D$F1_norm)
F2_D_center = median(D$F2_norm)


C_centroid = C %>% group_by(Vowel) %>% summarise(F1_C = median(F1_norm), F2_C = median(F2_norm))
D_centroid = D %>% group_by(Vowel) %>% summarise(F1_D = median(F1_norm), F2_D = median(F2_norm))

CD_centroid = merge(C_centroid, D_centroid, by = "Vowel")
CD_centroid = CD_centroid %>%
  mutate(
    euclid = sqrt((F1_C - F1_D) ^ 2 + (F2_C - F2_D) ^ 2),
    dist_cent_C = sqrt((F1_C - F1_C_center) ^
                         2 + (F2_C - F2_C_center) ^ 2),
    dist_cent_D = sqrt((F1_D - F1_D_center) ^
                         2 + (F2_D - F2_D_center) ^ 2)
  )


Desc(CD_centroid$euclid)
mean_euclid_dist_CD = mean(CD_centroid$euclid)
print(mean_euclid_dist_CD)
#the average distance between vowel centroids in C and D is 0.42 Standard deviations


vowel_disp_C = mean(CD_centroid$dist_cent_C)
vowel_disp_D = mean(CD_centroid$dist_cent_D)
print(vowel_disp_C) #1.20 SD
print(vowel_disp_D) #1.32 SD

# the vowels realisations are more dispersed on D than on C

# To look at the importance of every vowel for the distance between the C and D
# we can again normalize the distances per vowel to get a score as contribution to total distance.

CD_centroid = CD_centroid %>% mutate(contribution = euclid / sum(euclid))
CD_centroid$Vowel = as.factor(CD_centroid$Vowel)
contrib_euclid_plot_CD = ggplot(data = CD_centroid, aes(x = Vowel, y = contribution, fill = Vowel)) + geom_bar(stat = 'identity') +
  labs(title = "Relative Euclidean Distance between Vowels") + theme(legend.position = "none")

cvx_hull_cd + contrib_euclid_plot_CD

# we can now compare the contribution of vowels to the different distance measures
contrib_cliff = df_cliff %>% select(vowels, contribution)
contrib_overlap = df_overlap_CD %>% select(vowels, contribution)
contrib_euclid = CD_centroid %>% select(Vowel, contribution)
colnames(contrib_euclid) = c("vowels", "euclid")
contribs = contrib_euclid %>% left_join(contrib_overlap, by = join_by(vowels)) %>% left_join(contrib_cliff, by = join_by(vowels))
colnames(contribs) = c("vowels", "euclid", "overlap", "cliff")


ggplot(
  data = contribs %>% pivot_longer(
    cols = c(euclid, cliff, overlap),
    names_to = "measure",
    values_to = "values"
  ),
  map = aes(y = values, x = vowels, fill = measure)
) + geom_bar(stat = 'identity', position = "dodge") +
  labs(title = "Contribution of vowels to euclidean, cliffs, and overlap distance between C and D")

cm = cor(contribs %>% select(!vowels)) #the measures all correlate, overlap and 
contrib_CD_corrplot = corrplot(cm,
                               method = 'color',
                               addCoef.col = "black",
                               number.cex = 0.8,
                               tl.col = "black",
                               tl.srt = 45)




#put it into functions

get_centroid = function(D1, D2) {
  F1_D1_center = median(D1$F1_norm)
  F2_D1_center = median(D1$F2_norm)
  F1_D2_center = median(D2$F1_norm)
  F2_D2_center = median(D2$F2_norm)
  
  
  D1_centroid = D1 %>% group_by(Vowel) %>% summarise(F1_D1 = median(F1_norm), F2_D1 = median(F2_norm))
  D2_centroid = D2 %>% group_by(Vowel) %>% summarise(F1_D2 = median(F1_norm), F2_D2 = median(F2_norm))
  
  centroid = merge(D1_centroid, D2_centroid, by = "Vowel")
  
  centroid = centroid %>%
    mutate(
      euclid = sqrt((F1_D1 - F1_D2) ^ 2 + (F2_D1 - F2_D2) ^ 2),
      dist_cent_D1 = sqrt((F1_D1 - F1_D1_center) ^ 2 + (F2_D1 - F2_D1_center) ^
                            2),
      dist_cent_D2 = sqrt((F1_D2 - F1_D2_center) ^ 2 + (F2_D2 - F2_D2_center) ^
                            2)
    )
  return(centroid)
}

get_euclidean_measures = function(centroid) {
  mean_euclid_dist = mean(centroid$euclid)
  vowel_disp_D1 = mean(centroid$dist_cent_D1)
  vowel_disp_D2 = mean(centroid$dist_cent_D2)
  df_vals = data.frame(mean_euclid_dist, vowel_disp_D1, vowel_disp_D2)
  return(df_vals)
}

get_euclidean_contribution = function(centroid) {
  centroid = centroid %>% mutate(contribution = euclid / sum(euclid))
  return(centroid)
}


centroid = get_centroid(C, D)
get_euclidean_measures(centroid)
get_euclidean_contribution(centroid)

###summary of C vs D
index_cliff_CD = index_cliff
index_overlap_CD = index_overlap
hull_area_diff_hz_CD = hull_area_diff_hz
hull_area_diff_norm_CD = hull_area_diff_norm


print(index_cliff_CD) #0.44 #average distinctness of the vowels (probability of larger or smaller values)
print(index_overlap_CD) #0.435 #average distribution overlap of the vowels (percentage of not shared area)
print(mean_euclid_dist_CD) #0.42 SD, average euclidean distance between centroids
print(hull_area_diff_hz_CD) #851.14 Hz^2
print(hull_area_diff_norm_CD) #0.198 SD^2
print(iou_CD) #0.1976017
print(vowel_disp_C)#1.20 SD
print(vowel_disp_D)#1.32 SD

sum_tab_cd = data.frame(index_cliff_CD, index_overlap_CD, mean_euclid_dist_CD, hull_area_diff_norm_CD, vowel_disp_C, vowel_disp_D)
gt(sum_tab_cd)

# looking at the three distance measures we have used, we can conclude that they have different strengths
# cliffs delta is a common effect size measure and quantifies how distinct the distributions are
# Cliffs delta gives a direction of difference between the distributions, and is related to
# mann-whitney U-test as it uses the U statistic -> logical effect size if testing for significance
# it has also a fairly intuitive interpretation as the probability of observing larger or smaller values
# Cliffs delta does however assume the distributions to have fairly similar shape = weakness

#overlap is not sensitive to the shape of the distribution, but does not take the distinctness of the distributions into account
#overlap has a very intuitive interpretation = percentage of overlapping area of distribution.
#Is not associated with any significance tests. Lacks direction.
#Overlap was used in previous study with similar data -> good for comparability

# Euclidean distance has a intuitive spatial interpretation as the distance between the vowels centroids,
# but it does not take the intravariability of the vowels into account
# As an effect size, it is not restricted to 0 and 1, which makes it worse for comparison with other effects
# It does not allow for significance testing
# It allows a good and comprehensible way of quantifying the dispersion of vowel realisations within a vowel space.


# how to go forward?
# Since we want to test to what extent there is a difference between two groups, using cliffs delta is the best way to go.
#average euclidean distance could be used to quantify the dispersion of the vowel realisation within the vowel space.
# use the convex hull to visualize vowel space


### B VS D ----
# look at the effect of Wordlist vs reading (B = white + reading, D = white + wordlist)

#summary statistics
sum_BD = BD %>% group_by(Set) %>% summarize(
  n_instances = n(),
  n_speakers = length(unique(Speaker)),
  n_vowels = length(unique(Vowel)),
  mean_F1 = mean(F1),
  sd_F1 = SD(F1),
  skew_F1 = Skew(F1),
  mean_F2 = mean(F2),
  sd_F2 = SD(F2),
  skew_F2 = Skew(F2),
  median_F1 = median(F1),
  median_F2 = median(F2),
  median_F1_norm = median(F1_norm),
  median_F2_norm = median(F2_norm)
)
head(sum_BD)
gt(sum_BD)

### distributions

##boxplots
ggplot(data = BD,
       map = aes(y = F1_norm, fill = Set, color = Set)) + geom_boxplot(color = "black") + labs(title = "distribution of F1 across B and D") + scale_fill_manual(values = BD_cols)
ggplot(data = BD,
       map = aes(y = F2_norm, fill = Set, color = Set)) + geom_boxplot(color = "black") + labs(title = "distribution of F2 across B and D") + scale_fill_manual(values = BD_cols)

#per vowel
ggplot(data = BD,
       map = aes(
         y = F1_norm,
         x = Vowel,
         fill = Set,
         color = Set
       )) + geom_boxplot(color = "black") + labs(title = "distribution of F1 across vowels for B and D") + scale_fill_manual(values = BD_cols)

ggplot(data = BD,
       map = aes(
         y = F2_norm,
         x = Vowel,
         fill = Set,
         color = Set
       )) + geom_boxplot(color = "black") + labs(title = "distribution of F2 across vowels for B and D") + scale_fill_manual(values = BD_cols)

## density
#F1
ggplot(data = BD,
       map = aes(x = F1, fill = Set)) + geom_density(alpha = 0.5) +
  labs(title = "Density of F1 for B and D", x = "F1(Hz)", y = "Density") + scale_fill_manual(values = BD_cols)
ggplot(data = BD,
       map = aes(x = F1_norm, fill = Set)) + geom_density(alpha = 0.5) +
  labs(title = "Density of F1 for B and D", x = "F1(SD)", y = "Density") + scale_fill_manual(values = BD_cols)
#F2
ggplot(data = BD,
       map = aes(x = F2, fill = Set)) + geom_density(alpha = 0.5) +
  labs(title = "Density of F1 for B and D", x = "F2(Hz)", y = "Density") + scale_fill_manual(values = BD_cols)
ggplot(data = BD,
       map = aes(x = F2_norm, fill = Set)) + geom_density(alpha = 0.5) +
  labs(title = "Density of F2 for B and D", x = "F1(SD)", y = "Density") + scale_fill_manual(values = BD_cols)



###vowels spaces
ggplot(data = BD,
       map = aes(
         y = F1_norm,
         x = F2_norm,
         fill = Set,
         colour = Set
       )) + geom_point(alpha = 0.3) + 
  stat_ellipse() +
  geom_text(
    data = BD %>% group_by(Set) %>% summarize(F1 = median(F1_norm), F2 = median(F2_norm)),
    aes(y = F1, x = F2, label = Set),
    alpha = 1,
    size = 10,
    fontface = "bold"
  )  + 
  scale_y_reverse() + scale_x_reverse() + labs(title = 'Vowel space of B and D in Hz') + theme(legend.position = "none") + 
  scale_fill_manual(values = BD_cols) + 
  scale_color_manual(values = BD_cols)


#B
ggplot(data = B,
       map = aes(
         y = F1,
         x = F2,
         fill = Vowel,
         colour = Vowel
       )) + geom_point(alpha = 0.1) + stat_ellipse(alpha = 1) +
  geom_text(
    data = B %>% group_by(Vowel) %>% summarize(F1 = median(F1), F2 = median(F2)),
    aes(y = F1, x = F2, label = Vowel),
    alpha = 1,
    size = 10,
    fontface = "bold"
  )  +
  scale_x_reverse() + scale_y_reverse() +
  labs(title = 'Vowel space of C in HZ', x = "F2 (Hz)", y = "F1 (Hz)") +
  theme(legend.position = "none")
B_plot = ggplot(data = B,
       map = aes(
         y = F1_norm,
         x = F2_norm,
         fill = Vowel,
         colour = Vowel
       )) + geom_point(alpha = 0.1) + stat_ellipse(alpha = 1) +
  geom_text(
    data = B %>% group_by(Vowel) %>% summarize(F1 = median(F1_norm), F2 = median(F2_norm)),
    aes(y = F1, x = F2, label = Vowel),
    alpha = 1,
    size = 10,
    fontface = "bold"
  )  +
  scale_x_reverse() + scale_y_reverse() +
  labs(title = 'Vowel space of B in standard deviations of speakers', x = "F2 (SD)", y = "F1 (SD)") +
  theme(legend.position = "none")

#D
ggplot(data = D,
       map = aes(
         y = F1,
         x = F2,
         fill = Vowel,
         colour = Vowel
       )) + geom_point(alpha = 0.1) + stat_ellipse(alpha = 1) +
  geom_text(
    data = D %>% group_by(Vowel) %>% summarize(F1 = median(F1), F2 = median(F2)),
    aes(y = F1, x = F2, label = Vowel),
    alpha = 1,
    size = 10,
    fontface = "bold"
  )  +
  scale_x_reverse() + scale_y_reverse() +
  labs(title = 'Vowel space of D in HZ', x = "F2 (Hz)", y = "F1 (Hz)") +
  theme(legend.position = "none")
D_plot = ggplot(data = D,
       map = aes(
         y = F1_norm,
         x = F2_norm,
         fill = Vowel,
         colour = Vowel
       )) + geom_point(alpha = 0.1) + stat_ellipse(alpha = 1) +
  geom_text(
    data = D %>% group_by(Vowel) %>% summarize(F1 = median(F1_norm), F2 = median(F2_norm)),
    aes(y = F1, x = F2, label = Vowel),
    alpha = 1,
    size = 10,
    fontface = "bold"
  )  +
  scale_x_reverse() + scale_y_reverse() +
  labs(title = 'Vowel space of D in standard deviations of speakers', x = "F2 (SD)", y = "F1 (SD)") +
  theme(legend.position = "none")

B_plot + D_plot


#summaries
Desc(B$F1_norm)
Desc(B$F2_norm)
Desc(D$F1_norm)
Desc(D$F2_norm)



cliff.delta(B$F1_norm, D$F1_norm) #effect size is not different from zero, 0.0046 estimated
cliff.delta(B$F2_norm, D$F2_norm) # effect size is not different from zero, 0.0031 estimated
# the probability of observing a f2 or f1 lower or higher for the two groups is not different from zero


### cliffs index
df_cliff_BD = get_cliff_delta(B, D)

plot_data = get_plot_format_cliff(df_cliff_BD)

plot_cliff_BD = ggplot(data = plot_data, aes(y = est, x = vowels, fill = formant)) + 
  geom_bar(stat = 'identity', position = 'dodge') + 
  geom_errorbar(aes(ymin = lci, ymax = uci), position = position_dodge(width = 0.9), width = 0.2) + 
  labs(y = "Cliffs Delta", title = "Comparison of Cliff's Delta for B and D")
#positive cliffs delta -> more likely to observe larger values for B

index_cliff_BD = get_index_cliff(df_cliff_BD) #0.407
print(index_cliff_BD) #0.407

df_cliff_BD = add_contribution_cliff(df_cliff_BD)

contrib_cliff_BD = ggplot(data = df_cliff_BD, aes(y = contribution, x = vowels, fill = vowels)) + 
  geom_bar(stat = 'identity') + theme(legend.position = "none") + 
  labs(y = "contribution", title = 'Contribution of Vowels to Cliffs Index of B and D')

plot_cliff_BD / contrib_cliff_BD

### overlap index
df_overlap_BD = get_overlap(B, D)

plot_overlap_BD = ggplot(data = df_overlap_BD %>% pivot_longer(cols = c("f1_distances", "f2_distances"), names_to = "Formants", values_to = "vals"),
       aes(y = vals, x = vowels, fill = Formants)) + 
  geom_bar(stat = 'identity', position = "dodge") + 
  labs(y = "overlap distance", title = "Overlap Distances Between the Vowels of B and D ") + scale_fill_discrete(
    labels = c("f1_distances" = "F1", "f2_distances" = "F2") # Custom labels
)
plot_overlap_BD

index_overlap_BD = get_index_overlap(df_overlap_BD) #0.52
print(index_overlap_BD)

df_overlap_BD = add_contribution_overlap(df_overlap_BD)

contrib_overlap_BD = ggplot(data = df_overlap_BD, aes(y = contribution, x = vowels, fill = vowels)) + 
  geom_bar(stat = 'identity') + theme(legend.position = "none") + labs(y = "contribution", title = 'Contribution of Vowels to Overlap Index of B and D')

plot_overlap_BD / contrib_overlap_BD

### hull area
hull_area_B = get_hull_norm(B)$area
hull_area_D = get_hull_norm(D)$area

hull_area_diff_norm_BD = abs(hull_area_B - hull_area_D) #0.8771627 SD^2

hull_points_B = get_hull_norm(B)$p
hull_points_D = get_hull_norm(D)$p

iou_BD = get_iou_distance(hull_points_B, hull_points_D) 
print(iou_BD) ## 0.339

hull_plot_BD = ggplot(data = BD,
       map = aes(
         y = F1_norm,
         x = F2_norm,
         fill = Set,
         color = Set
       ))  +
  geom_text(
    data = BD %>% group_by(Set, Vowel) %>% summarize(F1 = median(F1_norm), F2 = median(F2_norm), .groups = "drop"),
    aes(y = F1, x = F2, label = Vowel),
    alpha = 1,
    size = 10,
    fontface = "bold"
  ) +
  geom_polygon(
    data = hull_points_B,
    linetype = "solid",
    color = B_col,
    fill = NA
  ) +
  geom_polygon(
    data = hull_points_D,
    linetype = "solid",
    color = D_col,
    fill = NA
  ) +
  scale_color_manual(values = BD_cols) + 
  scale_x_reverse() + scale_y_reverse() +
  labs(title = 'Convex Hulls of B and D', x = "F2 (SD)", y = "F1 (SD)") +
  theme()



BD_centroid = get_centroid(B, D)

euclid_measures = get_euclidean_measures(BD_centroid)
mean_euclid_dist_BD = euclid_measures$mean_euclid_dist
vowel_disp_B = euclid_measures$vowel_disp_D1
vowel_disp_D = euclid_measures$vowel_disp_D2

print(mean_euclid_dist_BD) #0.63 SD
print(vowel_disp_B) #1.048 SD
print(vowel_disp_D) #1.32 SD

BD_centroid = get_euclidean_contribution(BD_centroid)

contrib_plot_euclid_BD = ggplot(data = BD_centroid, aes(y = contribution, x = Vowel, fill = Vowel)) + 
  geom_bar(stat = 'identity') + theme(legend.position = "none") + labs(title = 'contribution of vowels to euclidean distance')


hull_plot_BD + contrib_plot_euclid_BD

#compare contribution of vowels across measures
contrib_cliff = df_cliff_BD %>% select(vowels, contribution)
contrib_overlap = df_overlap_BD %>% select(vowels, contribution)
contrib_euclid = BD_centroid %>% select(Vowel, contribution)
colnames(contrib_euclid) = c("vowels", "euclid")
contribs = contrib_euclid %>% left_join(contrib_overlap, by = join_by(vowels)) %>% left_join(contrib_cliff, by = join_by(vowels))
colnames(contribs) = c("vowels", "euclid", "overlap", "cliff")

cm = cor(contribs %>% select(!vowels)) #the measures all correlate, overlap and 
contrib_CD_corrplot = corrplot(cm,
                               method = 'color',
                               addCoef.col = "black",
                               number.cex = 0.8,
                               tl.col = "black",
                               tl.srt = 45)

ggplot(
  data = contribs %>% pivot_longer(
    cols = c(euclid, cliff, overlap),
    names_to = "measure",
    values_to = "values"
  ),
  map = aes(y = values, x = vowels, fill = measure)
) + geom_bar(stat = 'identity', position = "dodge") +
  labs(title = "contribution of vowels to euclidean, cliffs, and overlap distance for B and D")



### plot all three vowel spaces
hull_points_C = hull_points_C_norm

ggplot(data = BCD,
       map = aes(
         y = F1_norm,
         x = F2_norm,
         fill = Set,
         color = Set
       ))  +
  geom_text(
    data = BCD %>% group_by(Set, Vowel) %>% summarize(F1 = median(F1_norm), F2 = median(F2_norm), .groups = "drop"),
    aes(y = F1, x = F2, label = Vowel),
    alpha = 1,
    size = 10,
    fontface = "bold"
  ) +
  geom_polygon(
    data = hull_points_B,
    linetype = "solid",
    color = B_col,
    fill = NA
  ) +
  geom_polygon(
    data = hull_points_D,
    linetype = "solid",
    color = D_col,
    fill = NA
  ) +
  geom_polygon(
    data = hull_points_C,
    linetype = "solid",
    color = C_col,
    fill = NA
  ) +
  scale_color_manual(values = BCD_cols) + 
  scale_x_reverse() + scale_y_reverse() +
  labs(title = 'Comparison of Vowel Spaces of B and D by Convexhull area', x = "F2 (SD)", y = "F1 (SD)") +
  theme()


#### summary of results table ----

cd = data.frame(index_cliff_CD, index_overlap_CD, mean_euclid_dist_CD, iou_CD)
colnames(cd) = c("Cliffs Index", "Overlap Index", "Mean Euclidean Distance (sd)","Intersect over Union Distance")

bd = data.frame(index_cliff_BD, index_overlap_BD, mean_euclid_dist_BD, iou_BD)
colnames(bd) = c("Cliffs Index", "Overlap Index", "Mean Euclidean Distance (sd)", "Intersect over Union Distance")

cd_bd = rbind(cd, bd)
cd_bd = round(cd_bd, 3)

gt(round(cd, 3))
gt(round(bd, 3))

gt(cd_bd)


