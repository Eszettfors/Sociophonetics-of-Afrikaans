library(dplyr)
library(tidyr)
library(phonR)
library(phonTools)
library(DescTools)
library(ggplot2)
library(lme4)
library(lmerTest)
library(MuMIn)
library(influence.ME)
library(patchwork)
library(jtools)

A = read.csv("clean_data/A_clean")
B = read.csv("clean_data/B_clean")
C = read.csv("clean_data/C_clean")
D = read.csv("clean_data/D_clean")


#### summary of all datasets
abcd = rbind(A,B,C,D)

sum_abcd = abcd %>% group_by(Set) %>% summarize(n_instances = n(), 
                                                n_speakers = length(unique(Speaker)), 
                                                n_vowels = length(unique(Vowel)), 
                                                median_F1 = median(F1), median_F2 = median(F2),
                                                skew_F1 = Skew(F1), skew_F2 = Skew(F2),
                                                median_F1_norm = median(F1_norm), median_F2_norm = median(F2_norm),
                                                skew_F1_norm = Skew(F1_norm), skew_F2_norm = Skew(F2_norm))
sum_abcd = sum_abcd %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

# the normalized formants are less skewed = good!
# B has a much lower median F2 -> this is resolved with normalization
# The summary of A is misleading as the dataset is split over two speaker groups -> normalization unreliable.


### explore A
length(unique(A$Speaker)) #387 unique speakers


Desc(A$F1) 
qqnorm(A$F1)
qqline(A$F1)

Desc(A$F2)
qqnorm(A$F2)
qqline(A$F2)

#### words in the reading
unique(A$Word)
unique(A$Vowel)

#summary per speaker
A_speakers = A %>% group_by(Speaker) %>% summarise(Mean_F1 = mean(F1), Mean_F2 = mean(F2), n_vowels = as.numeric(length(unique(Vowel))))
table(A_speakers$n_vowels) #no indivudal covers the entire vowelspace

ggplot(data = A_speakers %>% pivot_longer(cols = c(Mean_F1, Mean_F2), names_to = "Formants", values_to = "Frequency"),
       map = aes(y = Frequency, x = Formants)) + geom_boxplot() + labs(title = "distribution of speakers by mean F1 and F2 A")

#summary per vowel
Vowels_A = A %>% group_by(Vowel) %>% summarise(n_recordings = n(), n_speakers = length(unique(Speaker)), Mean_F1= mean(F1),Mean_F2 =  mean(F2))
head(Vowels_A)
#front vowels average values are looking very suspect, deviating from distriubtion of B,C and D

ggplot(data = Vowels_A %>% pivot_longer(cols = c(Mean_F1, Mean_F2), names_to = "Formants", values_to = "Frequency"),
       map = aes(y = Frequency, x = Vowel, fill = Formants, colour = Formants)) + geom_bar(stat = 'identity', position = 'dodge')


#distributions across vowels
ggplot(data = A %>% pivot_longer(cols = c(F1, F2), names_to = "Formants", values_to = "Frequency"),
       map = aes(y = Frequency, x = Vowel, fill = Formants, colour = Formants)) + geom_boxplot(position = 'dodge', color = "black")
ggplot(data = A %>% pivot_longer(cols = c(F1_norm, F2_norm), names_to = "Formants", values_to = "Frequency"),
       map = aes(y = Frequency, x = Vowel, fill = Formants, colour = Formants)) + geom_boxplot(position = 'dodge', color = "black")


#plot vowel space
A_mono = A %>% filter(Type != "pure_diphthong")
ggplot(data = A_mono %>% filter(F1 < 1000),
       map = aes(y = F1, x = F2, fill = Vowel, colour = Vowel)) + geom_point(alpha = 0.1) + stat_ellipse(alpha = 1) + 
              geom_text(data = A_mono %>% group_by(Vowel) %>% summarize(F1 = median(F1), F2 = median(F2)), aes(y = F1, x = F2, label=Vowel),alpha = 1, size = 10, fontface = "bold")  + 
              scale_x_reverse() + scale_y_reverse() + labs(title = 'Vowel space of monophthongs for А', x = "F2 (Hz)", y = "F1 (Hz)") + theme(legend.position = "none")
ggplot(data = A_mono,
      map = aes(y = F1_norm, x = F2_norm, fill = Vowel, colour = Vowel)) + geom_point(alpha = 0.1) + stat_ellipse(alpha = 1) + 
      geom_text(data = A_mono %>% group_by(Vowel) %>% summarize(F1 = mean(F1_norm), F2 = mean(F2_norm)), aes(y = F1, x = F2, label=Vowel),alpha = 1, size = 10, fontface = "bold")  + 
      scale_x_reverse() + scale_y_reverse() + labs(title = 'Vowel space of monophthongs for А', x = "F2 (sd)", y = "F1 (sd)") + theme(legend.position = "none")
#normalization does not work for A due to the back vowels being pronounced by one group of speakers, and the front and middle vowels by another group. 

A_glide = A %>% filter(Type == "gliding_monophthong",  F1 < 1000, F2 < 3000)
ggplot(data = A_glide,
       map = aes(y = F1, x = F2, fill = Vowel, colour = Vowel)) + geom_point(alpha = 0.1) + stat_ellipse() + 
        geom_text(data = A_glide %>% group_by(Vowel) %>% summarize(F1 = mean(F1), F2 = mean(F2)), aes(y = F1, x = F2, label=Vowel),alpha = 1, size = 10, fontface = "bold") +
        scale_y_reverse() + scale_x_reverse() + labs(title = 'Vowel space of gliding monophthong for А') + theme(legend.position = "none")

A_diph = A %>% filter(Type == "pure_diphthong",  F1 < 1000, F2 < 3000)
ggplot(data = A_diph,
       map = aes(y = F1, x = F2, fill = Vowel, colour = Vowel)) + geom_point(alpha = 0.1) + stat_ellipse() + 
        geom_text(data = A_diph %>% group_by(Vowel) %>% summarize(F1 = mean(F1), F2 = mean(F2)), aes(y = F1, x = F2, label=Vowel),alpha = 1, size = 10, fontface = "bold") +
        scale_y_reverse() + scale_x_reverse() + labs(title = 'Vowel space of gliding monophthong for А') + theme(legend.position = "none")


### Explore B

B$Vowel = as.factor(B$Vowel)
length(unique(B$Speaker)) #82 unique speakers

Desc(B$F1_norm) 
qqnorm(B$F1_norm)
qqline(B$F1_norm) 

Desc(B$F2_norm)
qqnorm(B$F2_norm)
qqline(B$F2_norm)

B_speakers = B %>% group_by(Speaker) %>% summarise(Median_F1 = median(F1), Median_F2 = median(F2), n_vowels = as.factor(length(unique(Vowel))))

ggplot(data = B_speakers %>% pivot_longer(cols = c(Median_F1, Median_F2), names_to = "Formants", values_to = "Frequency"),
       map = aes(y = Frequency, x = Formants)) + geom_boxplot() + labs(title = "distribution of speakers by median F1 and F2")


#summary per vowel
Vowels_B = B %>% group_by(Vowel) %>% summarise(n_recordings = n(),n_speakers = length(unique(Speaker)), Median_F1= median(F1),Median_F2 =  median(F2), Median_F1_norm = median(F1_norm),Median_F2_norm =  median(F2_norm)) 
head(Vowels_B)

ggplot(data = Vowels_B %>% pivot_longer(cols = c(Median_F1, Median_F2), names_to = "Formants", values_to = "Frequency"),
       map = aes(y = Frequency, x = Vowel, fill = Formants, colour = Formants)) + geom_bar(stat = 'identity', position = 'dodge')


#distributions across vowels
ggplot(data = B %>% pivot_longer(cols = c(F1, F2), names_to = "Formants", values_to = "Frequency"),
       map = aes(y = Frequency, x = Vowel, fill = Formants, colour = Formants)) + geom_boxplot(position = 'dodge', color = "black") 

ggplot(data = B %>% pivot_longer(cols = c(F1_norm, F2_norm), names_to = "Formants", values_to = "Frequency"),
       map = aes(y = Frequency, x = Vowel, fill = Formants, colour = Formants)) + geom_boxplot(position = 'dodge', color = "black")

ggplot(data = B %>% pivot_longer(cols = c(F1_norm, F2_norm), names_to = "Formants", values_to = "Frequency"),
       map = aes(y = Frequency, x = Vowel, fill = Formants, colour = Formants)) + geom_violin(position = 'dodge', color = "black")


#plot vowel space
B_mono = B %>% filter(Type != "pure_diphthong")
ggplot(data = B_mono,
       map = aes(y = F1, x = F2, fill = Vowel, colour = Vowel)) + geom_point(alpha = 0.1) + stat_ellipse(alpha = 1) + 
  geom_text(data = B_mono %>% group_by(Vowel) %>% summarize(F1 = median(F1), F2 = median(F2)), aes(y = F1, x = F2, label=Vowel),alpha = 1, size = 10, fontface = "bold")  + 
  scale_x_reverse() + scale_y_reverse() + labs(title = 'Vowel space of monophthongs for B', x = "F2 (Hz)", y = "F1 (Hz)") + 
  theme(legend.position = "none")
ggplot(data = B_mono,
       map = aes(y = F1_norm, x = F2_norm, fill = Vowel, colour = Vowel)) + geom_point(alpha = 0.1) + stat_ellipse(alpha = 1) + 
  geom_text(data = B_mono %>% group_by(Vowel) %>% summarize(F1 = mean(F1_norm), F2 = mean(F2_norm)), aes(y = F1, x = F2, label=Vowel),alpha = 1, size = 10, fontface = "bold")  + 
  scale_x_reverse() + scale_y_reverse() + labs(title = 'Vowel space of monophthongs for B', x = "F2 (sd)", y = "F1 (sd)") + 
  theme(legend.position = "none")



B_glide = B %>% filter(Type == "gliding_monophthong")
ggplot(data = B_glide,
       map = aes(y = F1, x = F2, fill = Vowel, colour = Vowel)) + geom_point(alpha = 0.1) + stat_ellipse() + 
  geom_text(data = B_glide %>% group_by(Vowel) %>% summarize(F1 = mean(F1), F2 = mean(F2)), aes(y = F1, x = F2, label=Vowel),alpha = 1, size = 10, fontface = "bold") +
  scale_y_reverse() + scale_x_reverse() + labs(title = 'Vowel space of gliding monophthong for white Afrikaans speakers') + theme(legend.position = "none")

B_diph = B %>% filter(Type == "pure_diphthong")
ggplot(data = B_diph,
       map = aes(y = F1, x = F2, fill = Vowel, colour = Vowel)) + geom_point(alpha = 0.1) + stat_ellipse() + 
  geom_text(data = B_diph %>% group_by(Vowel) %>% summarize(F1 = mean(F1), F2 = mean(F2)), aes(y = F1, x = F2, label=Vowel),alpha = 1, size = 10, fontface = "bold") +
  scale_y_reverse() + scale_x_reverse() + labs(title = 'Vowel space of gliding monophthong for white Afrikaans speakers') + theme(legend.position = "none")

#overall, looks much better, fewer outliers, more clear vowel space


###explore C
C$Vowel = as.factor(C$Vowel)
length(unique(C$Speaker)) 
Desc(C$F1_norm)
qqnorm(C$F1_norm)
qqline(C$F1_norm)

Desc(C$F2_norm) 
qqnorm(C$F2_norm)
qqline(C$F2_norm)

C_speakers = C %>% group_by(Speaker) %>% summarise(Median_F1= median(F1),Median_F2 =  median(F2), Median_F1_norm = median(F1_norm),Median_F2_norm =  median(F2_norm), n_vowels = as.numeric(length(unique(Vowel))))
head(C_speakers)

ggplot(data = C_speakers %>% pivot_longer(cols = c(Median_F1, Median_F2), names_to = "Formants", values_to = "Frequency"),
       map = aes(y = Frequency, x = Formants)) + geom_boxplot(color = "black") + labs(title = "distribution of speakers by median F1 and F2 C")
#there are potential outliers to look into

#summary per vowel
Vowels_C = C %>% group_by(Vowel) %>% summarise(n_recordings = n(),n_speakers = length(unique(Speaker)), Median_F1= median(F1),Median_F2 =  median(F2), Median_F1_norm = median(F1_norm),Median_F2_norm =  median(F2_norm))
head(Vowels_C)


ggplot(data = Vowels_C %>% pivot_longer(cols = c(Median_F1, Median_F2), names_to = "Formants", values_to = "Frequency"),
       map = aes(y = Frequency, x = Vowel, fill = Formants, colour = Formants)) + geom_bar(stat = 'identity', position = 'dodge')


#distributions across vowels
ggplot(data = C %>% pivot_longer(cols = c(F1_norm, F2_norm), names_to = "Formants", values_to = "Frequency"),
       map = aes(y = Frequency, x = Vowel, fill = Formants, colour = Formants)) + geom_boxplot(position = 'dodge', color = "black")

ggplot(data = C %>% pivot_longer(cols = c(F1_norm, F2_norm), names_to = "Formants", values_to = "Frequency"),
       map = aes(y = Frequency, x = Vowel, fill = Formants, colour = Formants)) + geom_violin(position = 'dodge', color = "black")


#plot vowel space
C_mono = C %>% filter(Type != "pure_diphthong", F1 < 1250)
ggplot(data = C_mono,
       map = aes(y = F1, x = F2, fill = Vowel, colour = Vowel)) + geom_point(alpha = 0.1) + stat_ellipse(alpha = 1) + 
       geom_text(data = C_mono %>% group_by(Vowel) %>% summarize(F1 = median(F1), F2 = median(F2)), aes(y = F1, x = F2, label=Vowel),alpha = 1, size = 10, fontface = "bold")  + 
       scale_x_reverse() + scale_y_reverse() + labs(title = 'Vowel space of monophthongs for C', x = "F2(Hz)", y = "F1(Hz)") + theme(legend.position = "none")

ggplot(data = C_mono,
        map = aes(y = F1_norm, x = F2_norm, fill = Vowel, colour = Vowel)) + geom_point(alpha = 0.1) + stat_ellipse(alpha = 1) + 
        geom_text(data = C_mono %>% group_by(Vowel) %>% summarize(F1 = median(F1_norm), F2 = median(F2_norm)), aes(y = F1, x = F2, label=Vowel),alpha = 1, size = 10, fontface = "bold")  + 
        scale_x_reverse() + scale_y_reverse() + labs(title = 'Vowel space of monophthongs for C', x = "F2(sd)", y = "F1(sd)") + 
        theme(legend.position = "none") 

C_glide = C %>% filter(Type == "gliding_monophthong")
ggplot(data = C_glide,
       map = aes(y = F1, x = F2, fill = Vowel, colour = Vowel)) + geom_point(alpha = 0.1) + stat_ellipse() + 
  geom_text(data = C_glide %>% group_by(Vowel) %>% summarize(F1 = median(F1), F2 = median(F2)), aes(y = F1, x = F2, label=Vowel),alpha = 1, size = 10, fontface = "bold") +
  scale_y_reverse() + scale_x_reverse() + labs(title = 'Vowel space of gliding monophthong for coloured Afrikaans speakers C') + theme(legend.position = "none")

C_diph = C %>% filter(Type == "pure_diphthong")
ggplot(data = C_diph,
       map = aes(y = F1, x = F2, fill = Vowel, colour = Vowel)) + geom_point(alpha = 0.1) + stat_ellipse() + 
       geom_text(data = C_diph %>% group_by(Vowel) %>% summarize(F1 = median(F1), F2 = median(F2)), aes(y = F1, x = F2, label=Vowel),alpha = 1, size = 10, fontface = "bold") +
       scale_y_reverse() + scale_x_reverse() + labs(title = 'Vowel space of gliding monophthong for coloured Afrikaans speakers C') + 
       theme(legend.position = "none")

###explore D
D$Vowel = as.factor(D$Vowel)
length(unique(D$Speaker)) #9 unique speakers

Desc(D$F1_norm) 
qqnorm(D$F1_norm)
qqline(D$F1_norm) 

Desc(D$F2_norm)
qqnorm(D$F2_norm)
qqline(D$F2_norm)


D_speakers = D %>% group_by(Speaker) %>% summarise(Median_F1= median(F1),Median_F2 = median(F2), Median_F1_norm = median(F1_norm), Median_F2_norm = median(F2_norm), n_vowels = as.factor(length(unique(Vowel))))

ggplot(data = D_speakers %>% pivot_longer(cols = c(Median_F1, Median_F2), names_to = "Formants", values_to = "Frequency"),
       map = aes(y = Frequency, x = Formants)) + geom_boxplot() + labs(title = "distribution of speakers by median F1 and F2 ")

#summary per vowel
Vowels_D = D %>% group_by(Vowel) %>% summarise(n_recordings = n(),n_speakers = length(unique(Speaker)), Median_F1= median(F1),Median_F2 = median(F2), Median_F1_norm = median(F1_norm), Median_F2_norm = median(F2_norm)) 
head(Vowels_D)


ggplot(data = Vowels_D %>% pivot_longer(cols = c(Median_F1, Median_F2), names_to = "Formants", values_to = "Frequency"),
       map = aes(y = Frequency, x = Vowel, fill = Formants, colour = Formants)) + geom_bar(stat = 'identity', position = 'dodge')


#distributions across vowels
ggplot(data = D %>% pivot_longer(cols = c(F1, F2), names_to = "Formants", values_to = "Frequency"),
       map = aes(y = Frequency, x = Vowel, fill = Formants, colour = Formants)) + geom_boxplot(position = 'dodge', color = "black")

ggplot(data = D %>% pivot_longer(cols = c(F1_norm, F2_norm), names_to = "Formants", values_to = "Frequency"),
       map = aes(y = Frequency, x = Vowel, fill = Formants, colour = Formants)) + geom_boxplot(position = 'dodge', color = "black")

ggplot(data = D %>% pivot_longer(cols = c(F1_norm, F2_norm), names_to = "Formants", values_to = "Frequency"),
       map = aes(y = Frequency, x = Vowel, fill = Formants, colour = Formants)) + geom_violin(position = 'dodge', color = "black")

#plot vowel space
D_mono = D %>% filter(Type != "pure_diphthong")
ggplot(data = D_mono,
        map = aes(y = F1, x = F2, fill = Vowel, colour = Vowel)) + geom_point(alpha = 0.1) + stat_ellipse(alpha = 1) + 
        geom_text(data = D_mono %>% group_by(Vowel) %>% summarize(F1 = median(F1), F2 = median(F2)), aes(y = F1, x = F2, label=Vowel),alpha = 1, size = 10, fontface = "bold")  + 
        scale_x_reverse() + scale_y_reverse() + 
        labs(title = 'Vowel space of monophthongs for D', x = "F2 (Hz)", y = "F1 (Hz)") + 
        theme(legend.position = "none")

ggplot(data = D_mono,
        map = aes(y = F1_norm, x = F2_norm, fill = Vowel, colour = Vowel)) + geom_point(alpha = 0.1) + stat_ellipse(alpha = 1) + 
        geom_text(data = D_mono %>% group_by(Vowel) %>% summarize(F1 = median(F1_norm), F2 = median(F2_norm)), aes(y = F1, x = F2, label=Vowel),alpha = 1, size = 10, fontface = "bold")  + 
        scale_x_reverse() + scale_y_reverse() + 
        labs(title = 'Vowel space of monophthongs for white female Afrikaans speakers D', x = "F2 (sd)", y = "F1 (sd)") + 
        theme(legend.position = "none")



D_glide = D %>% filter(Type == "gliding_monophthong")
ggplot(data = D_glide,
       map = aes(y = F1, x = F2, fill = Vowel, colour = Vowel)) + geom_point(alpha = 0.1) + stat_ellipse() + 
       geom_text(data = D_glide %>% group_by(Vowel) %>% summarize(F1 = median(F1), F2 = median(F2)), aes(y = F1, x = F2, label=Vowel),alpha = 1, size = 10, fontface = "bold") +
       scale_y_reverse() + scale_x_reverse() + labs(title = 'Vowel space of gliding monophthong for white female Afrikaans speakers D') + theme(legend.position = "none")

D_diph = D %>% filter(Type == "pure_diphthong")
ggplot(data = D_diph,
       map = aes(y = F1, x = F2, fill = Vowel, colour = Vowel)) + geom_point(alpha = 0.1) + stat_ellipse() + 
       geom_text(data = D_diph %>% group_by(Vowel) %>% summarize(F1 = median(F1), F2 = median(F2)), aes(y = F1, x = F2, label=Vowel),alpha = 1, size = 10, fontface = "bold") +
       scale_y_reverse() + scale_x_reverse() + labs(title = 'Vowel space of gliding monophthong for white female Afrikaans speakers D') + theme(legend.position = "none")


####all datasets

#comparing distributions across datasets
ggplot(
  data = abcd %>% filter(F1 < 1250),
  map = aes(
    y = F1,
    x = Vowel,
    fill = Set,
    color = Set
  )
) + geom_boxplot(color = "black") + labs(title = "distribution of F1 across all datasets")
ggplot(data = abcd,
       map = aes(
         y = F2,
         x = Vowel,
         fill = Set,
         color = Set
       )) + geom_boxplot(color = "black") + labs(title = "distribution of F2 across all datasets")

bp1 = ggplot(data = abcd,
       map = aes(
         y = F1_norm,
         x = Vowel,
         fill = Set,
         color = Set
       )) + 
  geom_boxplot(color = "black") + 
  geom_hline(yintercept = 0, linetype = 'dashed') + 
  labs(y = "F1 (SD)") + 
  theme_apa()

bp2 = ggplot(data = abcd,
       map = aes(
         y = F2_norm,
         x = Vowel,
         fill = Set,
         color = Set
       )) + 
  geom_boxplot(color = "black") + 
  geom_hline(yintercept = 0, linetype = 'dashed') + 
  labs(y = "F2 (SD)") + 
  theme_apa()

bp1/bp2


#Vowel space across datasets
ggplot(data = abcd %>% filter(F1_norm < 3 & F1_norm > -3 ),
       map = aes(y = F1, x = F2, fill = Set, colour = Set)) + geom_point(alpha = 0.01) + stat_ellipse() + 
  geom_text(data = abcd %>% group_by(Set) %>% summarize(F1 = mean(F1), F2 = mean(F2)), aes(y = F1, x = F2, label=Set),alpha = 1, size = 10, fontface = "bold") +
  scale_y_reverse() + scale_x_reverse() + labs(title = 'Vowel space of A, B, C and D in Hz') + theme(legend.position = "none")

ggplot(data = abcd %>% filter(F1_norm < 3 & F1_norm > -3 ),
      map = aes(y = F1_norm, x = F2_norm, fill = Set, colour = Set)) + geom_point(alpha = 0.01) + stat_ellipse() + 
      geom_text(data = abcd %>% group_by(Set) %>% summarize(F1 = mean(F1_norm), F2 = mean(F2_norm)), aes(y = F1, x = F2, label=Set),alpha = 1, size = 10, fontface = "bold") +
      scale_y_reverse() + scale_x_reverse() + labs(title = 'Vowel space of A, B, C and D normalized') + theme(legend.position = "none")



head(abcd)
abcd$Group = as.factor(abcd$Group)
abcd$Method = as.factor(abcd$Method)

#modelling, investigate effect of group and method
#remove A from the analysis due to unreliable values

BCD = abcd %>% filter(Set != "A")

BCD %>% group_by(Set) %>% summarise(length(unique(Speaker)))
attach(BCD)

Desc(F1) # poisson distribution
Desc(F2) # bimodal

Desc(F1_norm) #bimodal
Desc(F2_norm) #trimodal
# multimodal distributions, due to the sets and vowels constituting subgroups

# what we can do is to use the raw F1 and F2 and account for speaker variability using a mixed effect model instead of normalization
# we use speaker and vowel as random effect to isolate the effect of speaker and group.
# We can then build one model for each vowel

mdl_F1 = lmer(data = BCD, F1 ~ Group * Method + (1|Vowel) + (1|Speaker))
summary(mdl_F1)
# Speaker accounts for 0.25% of variance in the data
# there is a significant effect for both group and method (white and word list are associated with 
# a reduction in F1 of 50.90 and 47.89 Hz)
AIC(mdl_F1) #55275.88
res = resid(mdl_F1)
plot(res, fitted(mdl_F1))
qqnorm(res)
qqline(res) #upper and lower quantiles deviating from normal -> try tansform

r.squaredGLMM(mdl_F1) #low marginal R2, 0.6 conditional R2
# --> large part of the variance can be explained by the entire model, the fixed effects
# only account for a very small part of the total variacne, 
# the fixed effect of gender and method are not especially important alltough significant predictors.

#log transform

Desc(log(F1)) #looks better

mdl_F1_log = lmer(data = BCD, log(F1) ~ Group + Method + (1|Vowel) + (1|Speaker))
summary(mdl_F1_log)

AIC(mdl_F1_log) #AIC = -2537 -> great improvement
res = resid(mdl_F1_log)
qqnorm(res)
qqline(res)#decent improvement

r.squaredGLMM(mdl_F1_log) # R2m and R2C increased, R2C = 0.72

#sqrt transform
Desc(sqrt(F1)) #looks better

mdl_F1_sqr = lmer(data = BCD, sqrt(F1) ~ Group + Method + (1|Vowel) + (1|Speaker))
summary(mdl_F1_sqr)

AIC(mdl_F1_sqr) #AIC = 19670 -> worse than log transform
res = resid(mdl_F1_sqr)
qqnorm(res)
qqline(res)#decent improvement

r.squaredGLMM(mdl_F1_sqr) # small improvement but worse than log transform

#log transform gives the best model


#outlier detection
inf = influence(mdl_F1_log, group = "Speaker")
D = cooks.distance(inf)
threshold = 4/(nrow(BCD)-length(fixef(mdl_F1_log)))
plot(D, type = "h", main = "Cox's D for Random Effects", ylab = "Cox's D")
abline(h = threshold)

D = as.data.frame(D)
D$Speaker = rownames(D)
rownames(D) = 1:nrow(D)
colnames(D) = c("D", "Speaker")
outliers = D %>% filter(D > threshold)
outliers = merge(BCD, outliers, by = "Speaker")
outliers = outliers %>% group_by(Speaker) %>% summarise(F1 = median(F1), F2 = median(F2), Set = unique(Set))
tab = table(outliers$Set) # all speakers of D counts as outliers, most of the B speakers as well.

#  remove all outliers
outliers = D %>% filter(D > threshold)
outliers = merge(BCD, outliers, by = "Speaker")
outliers = outliers %>% group_by(Speaker) %>% summarise(F1 = median(F1), F2 = median(F2), Set = unique(Set))
tab = table(outliers$Set) 
print(tab) # # from D are outliers


BCD_no_out = BCD %>% filter(!(Speaker %in% outliers$Speaker))

#model without outliers
attach(BCD_no_out)
Desc(log(F1)) #does not look better

mdl_F1_log_no_out = lmer(data = BCD_no_out, log(F1) ~ Group + Method + (1|Vowel) + (1|Speaker))
summary(mdl_F1_log_no_out)

AIC(mdl_F1_log_no_out) #AIC = -2510 ->  improvement
res = resid(mdl_F1_log_no_out)
qqnorm(res)
qqline(res)#small improvement

r.squaredGLMM(mdl_F1_log) #no real improvement
# removing outler is not worth it, since D values are very scarce

##F2 
attach(BCD)
Desc(F2)

mdl_F2 = lmer(data = BCD, F2 ~ Group + Method + (1 |Vowel) + (1|Speaker))
summary(mdl_F2) # white is associated with 109 HZ increase in F2, and Wordlist is associated with a 420 Hz increase in F2
r.squaredGLMM(mdl_F2) #R2m = 0.06, R2c = 0.725
AIC(mdl_F2) #65794.58
res = resid(mdl_F2)
qqnorm(res)
qqline(res) #looks decent

#log transform
Desc(log(F2))

mdl_F2_log = lmer(data = BCD, log(F2) ~ Group + Method + (1 |Vowel) + (1|Speaker))
summary(mdl_F2_log) # white is associated with 109 HZ increase in F2, and Wordlist is associated with a 420 Hz increase in F2
r.squaredGLMM(mdl_F2_log) #R2m = 0.034, R2c = 0.743 - improvement
AIC(mdl_F2_log) #-1202.576
res = resid(mdl_F2_log)
qqnorm(res)
qqline(res) # no real improvement

# sqrt transform
Desc(sqrt(F2))

mdl_F2_sqrt = lmer(data = BCD, sqrt(F2) ~ Group + Method + (1 |Vowel) + (1|Speaker))
summary(mdl_F2_sqrt) # white is associated with 109 HZ increase in F2, and Wordlist is associated with a 420 Hz increase in F2
r.squaredGLMM(mdl_F2_sqrt) #R2m = 0.047, R2c = 0.738 - decrease
AIC(mdl_F2_sqrt) #25688
res = resid(mdl_F2_sqrt)
qqnorm(res)
qqline(res) # no real improvement


# White and wordlist is assocaited with a decrease in F1 and an increase in F2
# the increase in F2 is substantial wordlist, but we have seen that the average F2 for the set B(the only set with wordlist)
# is much lower. Since B is so much larger than D, it likely removes the effect of D


### only look at gender, C vs D

CD = as.data.frame(BCD %>% filter(Set != "B"))
attach(CD)
Desc(log(F1))
Desc(F2)

mdl_F1 = lmer(data = CD, log(F1) ~ Group + (1 |Vowel) + (1|Speaker))
summary(mdl_F1) #significant effect, -51 Hz - white
AIC(mdl_F1) #40541.19
r.squaredGLMM(mdl_F1)
res = resid(mdl_F1)
qqnorm(res)
qqline(res)






### density plots

plot_f1 = ggplot(data = abcd, aes(x = F1_norm, fill = Set)) +
  geom_density(alpha = 0.5) +
  geom_hline(yintercept = 0) + 
  labs(x = "F1 (SD)") + 
  theme_apa(legend.pos = "topright")
 
plot_f2 = ggplot(data = abcd, aes(x = F2_norm, fill = Set)) +
  geom_density(alpha = 0.5) +
  geom_hline(yintercept = 0) + 
  labs(x = "F2 (SD)") + 
  theme_apa(legend.pos = "topright")

plot_f1 + plot_f2


### vowel spaces



p_A = ggplot(data = A,
       map = aes(
         y = F1_norm,
         x = F2_norm,
         fill = Vowel,
         colour = Vowel
       )) + geom_point(alpha = 0.1) + stat_ellipse(alpha = 1) +
  geom_text(
    data = A %>% group_by(Vowel) %>% summarize(F1 = median(F1_norm), F2 = median(F2_norm)),
    aes(y = F1, x = F2, label = Vowel),
    alpha = 1,
    size = 10,
    fontface = "bold"
  )  +
  scale_x_reverse() + 
  scale_y_reverse() + 
  labs(title = 'Vowel space of A', x = "F2 (SD)", y = "F1 (SD)") +
  theme_apa(legend.pos = "none")

p_B = ggplot(data = B,
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
  scale_x_reverse() + 
  scale_y_reverse() + 
  labs(title = 'Vowel space of B', x = "F2 (SD)", y = "F1 (SD)") +
  theme_apa(legend.pos = "none")

p_C = ggplot(data = C,
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
  scale_x_reverse() + 
  scale_y_reverse() + 
  labs(title = 'Vowel space of C', x = "F2 (SD)", y = "F1 (SD)") +
  theme_apa(legend.pos = "none")

p_D = ggplot(data = D,
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
  scale_x_reverse() + 
  scale_y_reverse() + 
  labs(title = 'Vowel space of D', x = "F2 (SD)", y = "F1 (SD)") +
  theme_apa(legend.pos = "none")

p_A + p_B + p_C + p_D
