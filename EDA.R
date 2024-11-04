library(dplyr)
library(tidyr)
library(phonR)
library(phonTools)
library(DescTools)
library(ggplot2)


A = read.csv("clean_data/A_clean")
B = read.csv("clean_data/B_clean")
C = read.csv("clean_data/C_clean")
D = read.csv("clean_data/D_clean")

### explore A
A$Vowel = as.factor(A$Vowel)
A$Gender = as.factor(A$Gender)

length(unique(A$Speaker)) #404 unique speakers
Desc(A %>% group_by(Speaker) %>% select(Age))
#mean age = 35.64 ; min =  17 ; max = 84 ; left skew, 0.69 (multimodal)

Desc(A$F1) #Mean = 530 - 532 ; min = 96 ; max = 2259 ; left skew, 1.31
qqnorm(A$F1)
qqline(A$F1) #heavy skew

Desc(A$F2)
#Mean = 1700 ; min = 356 ; max = 3580; no skew -> skew = -0.01
qqnorm(A$F2)
qqline(A$F2)#very small skew; assume normal distribution

#### words in the reading
unique(A$Word)
unique(A$Vowel)

#summary per speaker
A_speakers = A %>% group_by(Speaker) %>% summarise(Mean_F1 = mean(F1), Mean_F2 = mean(F2), n_vowels = as.numeric(length(unique(Vowel))), Gender = unique(Gender), Age = unique(Age))
table(A_speakers$Gender) # 252 f vs 152 m
table(A_speakers$n_vowels) #no indivudal covers the entire vowelspace
barplot(table(A_speakers$n_vowels))
boxplot(A_speakers$Age) # median approx 33, 2nd and 3d quartile 20 - 50
#age contains NA

ggplot(data = A_speakers %>% pivot_longer(cols = c(Mean_F1, Mean_F2), names_to = "Formants", values_to = "Frequency"),
       map = aes(y = Frequency, x = Formants)) + geom_boxplot() + labs(title = "distribution of speakers by mean F1 and F2 A")
#distribution per speaker heavily skewed for f2

#Difference in gender?
ggplot(data = A_speakers %>% pivot_longer(cols = c(Mean_F1, Mean_F2), names_to = "Formants", values_to = "Frequency"),
       map = aes(y = Frequency, x = Formants, fill = Gender, colour = Gender)) + geom_bar(stat = 'identity', position = 'dodge') + labs(title = "mean formant values for male and female")
#women score higher in F1 and F2 on average compared to men - significant?

#F1 is not normally distributed -> wilcox test
wilcox.test(A$F1[A$Gender == 'f'], A$F1[A$Gender == 'm']) #p>0.001
#F2 is normally distributed -> t.test
t.test(A$F2[A$Gender == 'f'], A$F2[A$Gender == 'm']) #p>0.001
#significant differenece in F1 and F2 for men and women -> using normalization required


#summary per vowel
Vowels_A = A %>% group_by(Vowel) %>% summarise(n_recordings = n(), n_speakers = length(unique(Speaker)), Mean_F1= mean(F1),Mean_F2 =  mean(F2))
# unproportional number of recordings for the schwa: 16060
table(Vowels_A$Vowel,Vowels_A$n_speakers)
# three vowels were only pronounced by 87 speakers, why? (back vowels) - ɔ, o, u
# dataset B only contain 87 speakers -> do the 87 in this dataset contain all vowels?
back_vowel_speakers = unique(A$Speaker[A$Vowel %in% c("ɔ","o", "u")])
Vowels_A_sub = A %>% group_by(Vowel) %>% filter(Speaker %in% back_vowel_speakers) %>% summarise(n_recordings = n(), n_speakers = length(unique(Speaker)), Mean_F1= mean(F1),Mean_F2 =  mean(F2)) 
#the recordings of back vowel belong to speakers which do not pronounce the front vowel and vise versa -> low quality since front and back vowel formants inherently are different


ggplot(data = Vowels_A %>% pivot_longer(cols = c(Mean_F1, Mean_F2), names_to = "Formants", values_to = "Frequency"),
       map = aes(y = Frequency, x = Vowel, fill = Formants, colour = Formants)) + geom_bar(stat = 'identity', position = 'dodge')

#distributions across vowels
ggplot(data = A %>% pivot_longer(cols = c(F1, F2), names_to = "Formants", values_to = "Frequency"),
       map = aes(y = Frequency, x = Vowel, fill = Formants, colour = Formants)) + geom_boxplot(position = 'dodge', color = "black")

#plot vowel space
A_mono = A %>% filter(Type == "pure_monophthong", F1 < 1000, F2 < 3000)
ggplot(data = A_mono,
       map = aes(y = F1, x = F2, fill = Vowel, colour = Vowel)) + geom_point(alpha = 0.1) + stat_ellipse(alpha = 1) + 
              geom_text(data = A_mono %>% group_by(Vowel) %>% summarize(F1 = mean(F1), F2 = mean(F2)), aes(y = F1, x = F2, label=Vowel),alpha = 1, size = 10, fontface = "bold")  + 
              scale_x_reverse() + scale_y_reverse() + labs(title = 'Vowel space of monophthongs for А', x = "F1 (Hz)", y = "F1 (Hz)") + theme(legend.position = "none")
A_mono = A %>% filter(Type == "pure_monophthong")
ggplot(data = A_mono,
       map = aes(y = F1_norm, x = F2_norm, fill = Vowel, colour = Vowel)) + geom_point(alpha = 0.1) + stat_ellipse(alpha = 1) + 
  geom_text(data = A_mono %>% group_by(Vowel) %>% summarize(F1 = mean(F1_norm), F2 = mean(F2_norm)), aes(y = F1, x = F2, label=Vowel),alpha = 1, size = 10, fontface = "bold")  + 
  scale_x_reverse() + scale_y_reverse() + labs(title = 'Vowel space of monophthongs for А') + theme(legend.position = "none")
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
#outlier detection will be very necessary.
#F2 can't be higher than 3000 and F1 higher than 1000 e.g?



### Explore B

B$Vowel = as.factor(B$Vowel)
length(unique(B$Speaker)) #87 unique speakers

Desc(B$F1) #Mean = 521.37 ; min = 62 ; max = 1745 ; left skew, 0.87 (unimodal)
qqnorm(B$F1)
qqline(B$F1) #heavy skew -> outliers

Desc(B$F2)
#Mean = 1302 ; min = 317 ; max = 3092; skew = 0.36 (bimodal)
qqnorm(B$F2)
qqline(B$F2)#fairly heavy skew, not as many outliers

B_speakers = B %>% group_by(Speaker) %>% summarise(Mean_F1 = mean(F1), Mean_F2 = mean(F2), n_vowels = as.factor(length(unique(Vowel))))
#no age and gender data available
table(B_speakers$n_vowels) #two speakers only cover one vowel -> remove from analysis

ggplot(data = B_speakers %>% pivot_longer(cols = c(Mean_F1, Mean_F2), names_to = "Formants", values_to = "Frequency"),
       map = aes(y = Frequency, x = Formants)) + geom_boxplot() + labs(title = "distribution of speakers by mean F1 and F2")


#summary per vowel
Vowels_B = B %>% group_by(Vowel) %>% summarise(n_recordings = n(),n_speakers = length(unique(Speaker)), Mean_F1= mean(F1),Mean_F2 =  mean(F2)) 
table(Vowels_B$n_speakers, Vowels_B$Vowel) #almost all vowels covered by all speakers

ggplot(data = Vowels_B %>% pivot_longer(cols = c(Mean_F1, Mean_F2), names_to = "Formants", values_to = "Frequency"),
       map = aes(y = Frequency, x = Vowel, fill = Formants, colour = Formants)) + geom_bar(stat = 'identity', position = 'dodge')


#distributions across vowels
ggplot(data = B %>% pivot_longer(cols = c(F1, F2), names_to = "Formants", values_to = "Frequency"),
       map = aes(y = Frequency, x = Vowel, fill = Formants, colour = Formants)) + geom_boxplot(position = 'dodge', color = "black")


#plot vowel space
B_mono = B %>% filter(Type == "pure_monophthong")
ggplot(data = B_mono,
       map = aes(y = F1, x = F2, fill = Vowel, colour = Vowel)) + geom_point(alpha = 0.1) + stat_ellipse(alpha = 1) + 
  geom_text(data = B_mono %>% group_by(Vowel) %>% summarize(F1 = mean(F1), F2 = mean(F2)), aes(y = F1, x = F2, label=Vowel),alpha = 1, size = 10, fontface = "bold")  + 
  scale_x_reverse() + scale_y_reverse() + labs(title = 'Vowel space of monophthongs for white Afrikaans speakers', x = "F2 (Hz)", y = "F1 (Hz)") + 
  theme(legend.position = "none")
ggplot(data = B_mono,
       map = aes(y = F1_norm, x = F2_norm, fill = Vowel, colour = Vowel)) + geom_point(alpha = 0.1) + stat_ellipse(alpha = 1) + 
  geom_text(data = B_mono %>% group_by(Vowel) %>% summarize(F1 = mean(F1_norm), F2 = mean(F2_norm)), aes(y = F1, x = F2, label=Vowel),alpha = 1, size = 10, fontface = "bold")  + 
  scale_x_reverse() + scale_y_reverse() + labs(title = 'Vowel space of monophthongs for white Afrikaans speakers', x = "F2 (SD)", y = "F1 (SD)") + 
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
length(unique(C$Speaker)) #336 unique speakers

Desc(C$F1) #Mean = 481.75 ; min = 146 ; max = 1789 ; left skew, 1.21 (unimodal)
qqnorm(C$F1)
qqline(C$F1) #Poisson distribution

Desc(C$F2) #Mean = 1602 ; min = 331 ; max = 3098; skew = -0.01 (multimodal)
qqnorm(C$F2)
qqline(C$F2)#skewness left and right

C_speakers = C %>% group_by(Speaker) %>% summarise(Mean_F1 = mean(F1), Mean_F2 = mean(F2), n_vowels = as.numeric(length(unique(Vowel))), Gender = unique(Gender), Age = unique(Age))

boxplot(C_speakers$Age) #median at about 25, 2nd 3rd quartile 20 - 40 ; C tends to be younger than B
table(C_speakers$Gender) #199 f vs 134 m
table(C_speakers$n_vowels)
View(C_speakers) #many speakers only cover a few vowels -> remove?

ggplot(data = C_speakers %>% pivot_longer(cols = c(Mean_F1, Mean_F2), names_to = "Formants", values_to = "Frequency"),
       map = aes(y = Frequency, x = Formants)) + geom_boxplot(color = "black") + labs(title = "distribution of speakers by mean F1 and F2 C")


#summary per vowel
Vowels_C = C %>% group_by(Vowel) %>% summarise(n_recordings = n(),n_speakers = length(unique(Speaker)), Mean_F1= mean(F1),Mean_F2 =  mean(F2)) 
table(Vowels_C$Vowel, Vowels_C$n_speakers)
#almost all vowels covered by all speakers ; almost only one recording per speaker per vowel

ggplot(data = Vowels_C %>% pivot_longer(cols = c(Mean_F1, Mean_F2), names_to = "Formants", values_to = "Frequency"),
       map = aes(y = Frequency, x = Vowel, fill = Formants, colour = Formants)) + geom_bar(stat = 'identity', position = 'dodge')


#distributions across vowels
ggplot(data = C %>% pivot_longer(cols = c(F1, F2), names_to = "Formants", values_to = "Frequency"),
       map = aes(y = Frequency, x = Vowel, fill = Formants, colour = Formants)) + geom_boxplot(position = 'dodge', color = "black")


#plot vowel space
C_mono = C %>% filter(Type == "pure_monophthong")
ggplot(data = C_mono,
       map = aes(y = F1, x = F2, fill = Vowel, colour = Vowel)) + geom_point(alpha = 0.1) + stat_ellipse(alpha = 1) + 
       geom_text(data = C_mono %>% group_by(Vowel) %>% summarize(F1 = mean(F1), F2 = mean(F2)), aes(y = F1, x = F2, label=Vowel),alpha = 1, size = 10, fontface = "bold")  + 
       scale_x_reverse() + scale_y_reverse() + labs(title = 'Vowel space of monophthongs for coloured female Afrikaans speakers C', x = "F2(Hz)", y = "F1(Hz)") + theme(legend.position = "none")

ggplot(data = C_mono,
        map = aes(y = F1_norm, x = F2_norm, fill = Vowel, colour = Vowel)) + geom_point(alpha = 0.1) + stat_ellipse(alpha = 1) + 
        geom_text(data = C_mono %>% group_by(Vowel) %>% summarize(F1 = mean(F1_norm), F2 = mean(F2_norm)), aes(y = F1, x = F2, label=Vowel),alpha = 1, size = 10, fontface = "bold")  + 
        scale_x_reverse() + scale_y_reverse() + labs(title = 'Vowel space of monophthongs for coloured female Afrikaans speakers C', x = "F2(SD)", y = "F1(SD)") + 
        theme(legend.position = "none") 

C_glide = C %>% filter(Type == "gliding_monophthong")
ggplot(data = C_glide,
       map = aes(y = F1, x = F2, fill = Vowel, colour = Vowel)) + geom_point(alpha = 0.1) + stat_ellipse() + 
  geom_text(data = C_glide %>% group_by(Vowel) %>% summarize(F1 = mean(F1), F2 = mean(F2)), aes(y = F1, x = F2, label=Vowel),alpha = 1, size = 10, fontface = "bold") +
  scale_y_reverse() + scale_x_reverse() + labs(title = 'Vowel space of gliding monophthong for coloured Afrikaans speakers C') + theme(legend.position = "none")

C_diph = C %>% filter(Type == "pure_diphthong")
ggplot(data = C_diph,
       map = aes(y = F1, x = F2, fill = Vowel, colour = Vowel)) + geom_point(alpha = 0.1) + stat_ellipse() + 
       geom_text(data = C_diph %>% group_by(Vowel) %>% summarize(F1 = mean(F1), F2 = mean(F2)), aes(y = F1, x = F2, label=Vowel),alpha = 1, size = 10, fontface = "bold") +
       scale_y_reverse() + scale_x_reverse() + labs(title = 'Vowel space of gliding monophthong for coloured Afrikaans speakers C') + 
       theme(legend.position = "none")

###explore D
D$Vowel = as.factor(D$Vowel)
length(unique(D$Speaker)) #9 unique speakers

Desc(D$F1) #Mean = 442 ; min = 214 ; max = 1084 ; left skew, 1.02 (unimodal)
qqnorm(D$F1)
qqline(D$F1) #Poisson distribution

Desc(D$F2) #Mean = 1749 ; min = 431 ; max = 3209; skew = 0.14 (bimodal)
qqnorm(D$F2)
qqline(D$F2)#skewness left and right


D_speakers = D %>% group_by(Speaker) %>% summarise(Mean_F1 = mean(F1), Mean_F2 = mean(F2), n_vowels = as.factor(length(unique(Vowel))), Gender = unique(Gender), Age = unique(Age))
#no age available, all are female. 

ggplot(data = C_speakers %>% pivot_longer(cols = c(Mean_F1, Mean_F2), names_to = "Formants", values_to = "Frequency"),
       map = aes(y = Frequency, x = Formants)) + geom_boxplot() + labs(title = "distribution of speakers by mean F1 and F2 D")

#summary per vowel
Vowels_D = D %>% group_by(Vowel) %>% summarise(n_recordings = n(),n_speakers = length(unique(Speaker)), Mean_F1= mean(F1),Mean_F2 =  mean(F2)) 
#almost all vowels covered by all speakers ; almost only two recording per speaker per vowel


ggplot(data = Vowels_D %>% pivot_longer(cols = c(Mean_F1, Mean_F2), names_to = "Formants", values_to = "Frequency"),
       map = aes(y = Frequency, x = Vowel, fill = Formants, colour = Formants)) + geom_bar(stat = 'identity', position = 'dodge')


#distributions across vowels
ggplot(data = D %>% pivot_longer(cols = c(F1, F2), names_to = "Formants", values_to = "Frequency"),
       map = aes(y = Frequency, x = Vowel, fill = Formants, colour = Formants)) + geom_boxplot(position = 'dodge', color = "black")


#plot vowel space
D_mono = D %>% filter(Type == "pure_monophthong")
ggplot(data = D_mono,
        map = aes(y = F1, x = F2, fill = Vowel, colour = Vowel)) + geom_point(alpha = 0.1) + stat_ellipse(alpha = 1) + 
        geom_text(data = D_mono %>% group_by(Vowel) %>% summarize(F1 = mean(F1), F2 = mean(F2)), aes(y = F1, x = F2, label=Vowel),alpha = 1, size = 10, fontface = "bold")  + 
        scale_x_reverse() + scale_y_reverse() + 
        labs(title = 'Vowel space of monophthongs for white female Afrikaans speakers D', x = "F2 (Hz)", y = "F1 (Hz)") + 
        theme(legend.position = "none")

ggplot(data = D_mono,
        map = aes(y = F1_norm, x = F2_norm, fill = Vowel, colour = Vowel)) + geom_point(alpha = 0.1) + stat_ellipse(alpha = 1) + 
        geom_text(data = D_mono %>% group_by(Vowel) %>% summarize(F1 = mean(F1_norm), F2 = mean(F2_norm)), aes(y = F1, x = F2, label=Vowel),alpha = 1, size = 10, fontface = "bold")  + 
        scale_x_reverse() + scale_y_reverse() + 
        labs(title = 'Vowel space of monophthongs for white female Afrikaans speakers D', x = "F2 (SD)", y = "F1 (SD)") + 
        theme(legend.position = "none")



D_glide = D %>% filter(Type == "gliding_monophthong")
ggplot(data = D_glide,
       map = aes(y = F1, x = F2, fill = Vowel, colour = Vowel)) + geom_point(alpha = 0.1) + stat_ellipse() + 
       geom_text(data = D_glide %>% group_by(Vowel) %>% summarize(F1 = mean(F1), F2 = mean(F2)), aes(y = F1, x = F2, label=Vowel),alpha = 1, size = 10, fontface = "bold") +
       scale_y_reverse() + scale_x_reverse() + labs(title = 'Vowel space of gliding monophthong for white female Afrikaans speakers D') + theme(legend.position = "none")

D_diph = D %>% filter(Type == "pure_diphthong")
ggplot(data = D_diph,
       map = aes(y = F1, x = F2, fill = Vowel, colour = Vowel)) + geom_point(alpha = 0.1) + stat_ellipse() + 
       geom_text(data = D_diph %>% group_by(Vowel) %>% summarize(F1 = mean(F1), F2 = mean(F2)), aes(y = F1, x = F2, label=Vowel),alpha = 1, size = 10, fontface = "bold") +
       scale_y_reverse() + scale_x_reverse() + labs(title = 'Vowel space of gliding monophthong for white female Afrikaans speakers D') + theme(legend.position = "none")


####summary of all datasets
A$Set = "A"
B$Set = "B"
C$Set = "C"
D$Set = "D"
abcd = rbind(A,B,C,D)

sum_abcd = abcd %>% group_by(Set) %>% summarize(n_instances = n(), n_speakers = length(unique(Speaker)), n_vowels = length(unique(Vowel)), mean_F1 = mean(F1), sd_F1 = SD(F1), skew_F1 = Skew(F1), mean_F2 = mean(F2), sd_F2 = SD(F2), skew_F2 = Skew(F2))
head(sum_abcd)
#The mean F2 of Dataset B is much lower than the other datasets, why???

#comparing distributions across datasets
ggplot(data = abcd,
       map = aes(y = F1, x = Vowel, fill = Set, color = Set)) +geom_boxplot(color = "black") + labs(title = "distribution of F1 across all datasets")
ggplot(data = abcd,
       map = aes(y = F2, x = Vowel, fill = Set, color = Set)) +geom_boxplot(color = "black") + labs(title = "distribution of F2 across all datasets")

#Vowel space across datasets
ggplot(data = abcd %>% filter(F1_norm < 3 & F1_norm > -3 ),
       map = aes(y = F1, x = F2, fill = Set, colour = Set)) + geom_point(alpha = 0.01) + stat_ellipse() + 
  geom_text(data = abcd %>% group_by(Set) %>% summarize(F1 = mean(F1), F2 = mean(F2)), aes(y = F1, x = F2, label=Set),alpha = 1, size = 10, fontface = "bold") +
  scale_y_reverse() + scale_x_reverse() + labs(title = 'Vowel space of A, B, C and D in Hz') + theme(legend.position = "none")

ggplot(data = abcd %>% filter(F1_norm < 3 & F1_norm > -3 ),
      map = aes(y = F1_norm, x = F2_norm, fill = Set, colour = Set)) + geom_point(alpha = 0.01) + stat_ellipse() + 
      geom_text(data = abcd %>% group_by(Set) %>% summarize(F1 = mean(F1_norm), F2 = mean(F2_norm)), aes(y = F1, x = F2, label=Set),alpha = 1, size = 10, fontface = "bold") +
      scale_y_reverse() + scale_x_reverse() + labs(title = 'Vowel space of A, B, C and D normalized') + theme(legend.position = "none")



