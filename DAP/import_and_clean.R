library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(DescTools)
library(VennDiagram)

A = read_excel("data/A. BA Noordewinde.xlsx")
B = read_excel("data/B. WIT Noordewinde.xlsx")
C = read_excel("data/C. BRUIN WOORDELYS.xlsx")
D = read_excel("data/D. WIT WOORDELYS.xlsx")
ipa_key = read.csv("data/transcription.csv", sep = "\t")


### A - coloured reading
colnames(A) = c("Group", "Speaker", "Vowel", "Word", "F1", "F2")
nrow(A) #39661 data points

speakers_A = unique(A$Speaker) 
length(speakers_A) #404 unique speakers
ID = 1:length(speakers_A)
key_speakers_A = data.frame(ID, speakers_A) # 1-316, gender indicated by first letter1-118 male, 119-316 female, 317 - 349 are male, 350 - 403 are female, 404 is male; all ranges are inclusive
key_speakers_A = key_speakers_A %>% mutate(Gender = case_when(ID < 119 ~ "m", ID > 118 & ID < 317 ~'f', ID >316 & ID <350 ~ "m", ID>349 & ID != 404 ~'f', ID == 404 ~'m')) # creates a gender data frame
key_speakers_A = key_speakers_A %>% mutate(Age = str_extract(speakers_A, "\\d+")) #extracts number from every speaker, if there is no number to extract, returns NA
key_speakers_A$ Age = as.numeric(key_speakers_A$Age)
#192-202, number does not indicate age -> NA ; 12, wrong number extracted -> adjust
key_speakers_A[192:202, c("Age")] = NA
key_speakers_A[12, c("Age")] = 19
table(is.na(key_speakers_A$Age)) #Age missing for 69 speakers. 

A = merge(A, key_speakers_A %>% select(!ID), by.x = "Speaker", by.y = "speakers_A") #Merge A on speaker
A = A %>% mutate(Group = "Coloured") #change bruin to coloured


nrow(A) #39661 data points, nothing lost

### B - Whites reading
colnames(B) = c("Group", "Speaker", "Vowel", "Word", "F1", "F2")
B = B[-1,] # remove row with columns
nrow(B) # 20176 data points
B$F1 = as.numeric(B$F1)
B$F2 = as.numeric(B$F2) # make formants numeric

Speakers_B = unique(B$Speaker) #87 unique speakers
ID = 1:length(Speakers_B)
key_speakers_B = data.frame(ID, Speakers_B) # no info on gender and Age, partly no first name -> impossible to deduce any information about speakers
B$Gender = NA
B$Age = NA

B = B %>% mutate(Group = "White")
nrow(B) #20176 data points, no dataloss

### C - coloured reading

colnames(C) = c("Group", "Speaker", "Vowel", "Word", "F1", "F2")
nrow(C) #4771 data points

speakers_C = unique(C$Speaker) 
length(speakers_C) #650 unique speakers ??? unlikely with so few data points, might be doublets
ID = 1:length(speakers_C)
key_speakers_C = data.frame(ID, speakers_C) #doublets present, e.g. 25_M_Christiana_82_Vr_Woordely is the same as 25MChristiana82VrWoordelys
# is IPienaar20Vr the same person as IPienaar25Vr recorded at different times or sisters with first name both starting with I?

#some speakers have same name followed by "woordelys1 or 2"
#some are denoted as Oud1VrWoordelys or Oud2VrWoordelys etc, = old one women Wordlist
#gender is consistently denoted as ml/Ml/ML = male, vr/Vr/VR = female
#first letter is sometimes number but never denotes age
#if the age is followed by v or m, this indicates gender

#try to unify by removing "_"
C = C %>% mutate(Speaker = Speaker %>% str_replace_all("_", ""))

# remove first letter
C = C %>% mutate(Speaker = Speaker %>% str_replace("^\\d+", ""))

# remove wordelys/lys 
C = C %>% mutate(Speaker = Speaker %>% str_replace("Woordelys.*", ""))
C = C %>% mutate(Speaker = Speaker %>% str_replace("lys.*", ""))

length(unique(C$Speaker)) #336 speakers left
speakers_C = unique(C$Speaker) 
ID = 1:length(speakers_C)
key_speakers_C = data.frame(ID, speakers_C)

#extract age
key_speakers_C = key_speakers_C %>% mutate(Age = speakers_C %>% str_extract("\\d+")) # 179-192 does not denote age but is a list indicating young, 250 - 258 indicates old
key_speakers_C$Age[179:192] = rep(NA, length(key_speakers_C$Age[179:192]))
key_speakers_C$Age[250:258] = rep(NA, length(key_speakers_C$Age[250:258]))

#extract gender
key_speakers_C = key_speakers_C %>% mutate(Gender = speakers_C %>% str_extract("(ml|Ml|ML|vr|Vr|VR|(?<=\\d)[mv])"))

#manual extraction of gender based on name, 218 could be male or female
ms = c(88, 98,99,113, 129, 173, 193,206,207,264,267, 289, 291, 304, 311, 327)
vs = c(75)

key_speakers_C = key_speakers_C %>% 
        mutate(Gender = case_when(
                      ID %in% ms ~"m",
                      ID %in% vs ~"f",
                      Gender == "ml" | Gender == "Ml" | Gender == "Ml" | Gender == "m" ~ "m",
                      Gender == "vr" | Gender == "Vr" | Gender == "VR" | Gender == "v" ~ "f"))

key_speakers_C$Age = as.numeric(key_speakers_C$Age)
length(unique(key_speakers_C$speakers_C)) # 336 unique speakers
table(is.na(key_speakers_C$Age)) # missing age values for 66 speakers
table(is.na(key_speakers_C$Gender)) #missing gender values for 3 speakers

C = merge(C, key_speakers_C %>% select(!ID), by.x = "Speaker", by.y = "speakers_C") #Merge A on speaker
C = C %>% mutate(Group = "Coloured") #change bruin to coloured


nrow(C) #4771 data points, nothing lost

### D - white wordlist

colnames(D) = c("Group", "Speaker", "Vowel", "Word", "F1", "F2")
nrow(D) # 265 data points

Speakers_D = unique(D$Speaker) #9 unique speakers
ID = 1:length(Speakers_D)
key_speakers_D = data.frame(ID, Speakers_D) # -> can deduce gender from name, but not age; all names are female

D$Gender = "f"
D$Age = NA
D = D %>% mutate(Group = "White")


nrow(D) #265 data points, no dataloss


#### IPA
unique(A$Vowel) %in% unique(ipa_key$vowel_trans)
unique(ipa_key$vowel_trans) # A contains an ø, which isn't in the transcription key
A %>% filter(Vowel == "ø") # ø corresponds to eu orthographically
A_wv = A %>% group_by(Word) %>% summarise(vowel = unique(Vowel))
A[A$Vowel == "ø", "Vowel"] = "eu"

A = merge(A, ipa_key, by.x = "Vowel", by.y = "vowel_trans") # merge with ipa_keys and replace to get vowels
A$Vowel = NULL
A = A %>% rename(c(Vowel = vowel, Example = example, Type = type)) #change column names according to convention

unique(B$Vowel)
unique(ipa_key$vowel_trans)
unique(B$Vowel) %in% unique(ipa_key$vowel_trans) # æ and ø not present in ipa_key
B %>% filter(Vowel == "æ")
odd_letter = B %>% filter(Vowel == "æ")
unique(odd_letter$Word)# "erken"    "lekker"   "regkry"   "sterkste" "trek"     "uittrek"  "werd"
A[A$Word == "trek", ] # represented by /ɛ/ = E in the first data frame -> make unison
B$Vowel[B$Vowel == "æ"] = "E"
B$Vowel[B$Vowel == "ø"] = "eu"


B = merge(B, ipa_key, by.x = "Vowel", by.y = "vowel_trans") #change to IPA symbols
B$Vowel = NULL
B = B %>% rename(c(Vowel = vowel, Example = example, Type = type)) #change column names according to convention

unique(C$Vowel) %in% unique(ipa_key$vowel_trans) # all vowels present in key
C = merge(C, ipa_key, by.x = "Vowel", by.y = "vowel_trans") # merge with ipa_keys and replace to get vowels
C$Vowel = NULL
C = C %>% rename(c(Vowel = vowel, Example = example, Type = type)) #change column names according to convention

unique(D$Vowel)
unique(D$Vowel) %in% unique(ipa_key$vowel_trans) # all vowels present

D = merge(D, ipa_key, by.x = "Vowel", by.y = "vowel_trans") #change to IPA symbols
D$Vowel = NULL
D = D %>% rename(c(Vowel = vowel, Example = example, Type = type)) #change column names according to convention

unique(ipa_key$vowel)
length(unique(ipa_key$vowel)) #16 unique vowels
length(unique(A$Vowel)) #14 vowels
length(unique(B$Vowel)) #15 vowels
length(unique(C$Vowel)) #15 vowels
length(unique(C$Vowel)) #15 vowels

#what is missing from the ipa_key?
unique(ipa_key$vowel) %in% unique(A$Vowel) #"œ", "æ" # missing "bus", but present in B,C and D?
unique(ipa_key$vowel) %in% unique(C$Vowel) #æ missing æ = ek(not present the data set and also not in previous study)

wds = unique(B$Word[B$Vowel == 'œ'])
A$Vowel[A$Word %in% wds]
B$Vowel[B$Word %in% wds]

missing_vowel_A = A %>% filter(Word %in%  wds) %>% summarize(word = unique(Word), vowel = unique(Vowel))
missing_vowel_B = B %>% filter(Word %in%  wds) %>% summarize(word = unique(Word), vowel = unique(Vowel))
#the white speakers have 'œ', 'u' and 'ə' , while the coloured have 'ə'
#the sound œ seems to be completely missing from the first dataframe


#add method
A$Method = "Reading"
B$Method = "Reading"
C$Method = "Wordlist"
D$Method = "Wordlist"

A_W = unique(A$Word) 
B_W = unique(B$Word)
C_W = unique(C$Word)
D_W = unique(D$Word)

length(A_W) #69
length(B_W) #74
length(C_W) #15
length(D_W) #15

B_W %in% A_W
A_W %in% B_W
B_W[!B_W %in% A_W]

#which words in B and A are not in Northwind?
nws = readLines("data/nws_afr.txt")
nws = unlist(strsplit(nws, " "))

not_in_reading = B_W[!B_W %in% nws] # where did these words come from?

B %>% filter(!Word %in% not_in_reading) %>% summarise(vowels = length(unique(Vowel)))
A %>% filter(!Word %in% not_in_reading) %>% summarise(vowels = length(unique(Vowel))) #only removing "beurt" reduces the size of the vowel space

#arrange columns
A = A %>% select(c("Speaker","Group","Method", "Gender", "Age", "Word", "Vowel", "Example", "Type", "F1", "F2" ))
B = B %>% select(c("Speaker","Group","Method", "Gender", "Age", "Word", "Vowel", "Example", "Type", "F1", "F2" ))
C = C %>% select(c("Speaker","Group","Method", "Gender", "Age", "Word", "Vowel", "Example", "Type", "F1", "F2" ))
D = D %>% select(c("Speaker","Group","Method", "Gender", "Age", "Word", "Vowel", "Example", "Type", "F1", "F2" ))

#check NA in target variable(F1, F2)
A[is.na(A$F1),] #some NAs
A[is.na(A$F2),] #some NAs

B[is.na(B$F1),] #ok
B[is.na(B$F2),] #ok

C[is.na(C$F1),] #ok
C[is.na(C$F2),] #ok

D[is.na(D$F1),] #ok
D[is.na(D$F2),] #ok

#the Nas were missing in the source data, not lost during cleaning -> remove
A = A[!is.na(A$F1),]


##adding normalization

lobanov = function(df, formant){
  lobanovs = c()
  for (i in 1:nrow(df)){
    speaker = df[i, "Speaker"]
    speaker_mean = mean(df[df$Speaker == speaker, formant])
    speaker_sd = SD(df[df$Speaker == speaker, formant])
    norm = (df[i, formant] - speaker_mean)/speaker_sd
    lobanovs = c(lobanovs, norm)
  }
  return(lobanovs)
}
A$F1_norm = lobanov(A, "F1")
A$F2_norm = lobanov(A, "F2")


B$F1_norm = lobanov(B, "F1")
B$F2_norm = lobanov(B, "F2")

C$F1_norm = lobanov(C, "F1")
C$F2_norm = lobanov(C, "F2")

D$F1_norm = lobanov(D, "F1")
D$F2_norm = lobanov(D, "F2")


#check NA in target variable(F1_norm, F2_norm)
A[is.na(A$F1_norm),] #ok
A[is.na(A$F2_norm),] #ok

B[is.na(B$F1_norm),] #2 na
B[is.na(B$F2_norm),] #2 na

C[is.na(C$F1_norm),] #2 na
C[is.na(C$F2_norm),] #2 na

D[is.na(D$F1_norm),] #ok
D[is.na(D$F2_norm),] #ok

na_speakers_B = B$Speaker[is.na(B$F1_norm)]
B[B$Speaker == na_speakers_B,] #only one entry -> SD = 0 -> NA when normalizing
### exclude speakers from the analysis which do not cover enough vowels?

A %>% group_by(Vowel) %>% summarize(n_speakers = length(unique(Speaker)))
B %>% group_by(Vowel) %>% summarize(n_speakers = length(unique(Speaker)))
C %>% group_by(Vowel) %>% summarize(n_speakers = length(unique(Speaker)))
D %>% group_by(Vowel) %>% summarize(n_speakers = length(unique(Speaker)))

A_speakers = A %>% group_by(Speaker) %>% summarize(n_vowels = length(unique(Vowel)))
table(A_speakers$n_vowels)
B_speakers = B %>% group_by(Speaker) %>% summarize(n_vowels = length(unique(Vowel)))
table(B_speakers$n_vowels)
C_speakers = C %>% group_by(Speaker) %>% summarize(n_vowels = length(unique(Vowel)))
table(C_speakers$n_vowels)
D_speakers = D %>% group_by(Speaker) %>% summarize(n_vowels = length(unique(Vowel)))
table(D_speakers$n_vowels)

write.csv(A,"clean_data/A_clean", row.names = FALSE)
write.csv(B,"clean_data/B_clean", row.names = FALSE)
write.csv(C,"clean_data/C_clean", row.names = FALSE)
write.csv(D,"clean_data/D_clean", row.names = FALSE)


