
#################################################################################
### Dependencies


require(lsa)
require(readxl)
require(tidyr)
require(dplyr)

#################################################################################
### Set directory

setwd("")


#################################################################################
### Initiate functions

recodeit <- function(m){
  m[m == 1] <- 1
}


normalit <- function(m){
  (m - min(m))/(max(m) - min(m))
}


#################################################################################
### Import strengthsFinder data - should be simple table of names and strengths
### strengths should be ranked from 1-5 (1 being top strength)

#import
df <- read_excel("Strengths Matrix.xlsx",sheet = "Rdata")
names(df)[1] <- "Name"
Names <- df$Name


#add summary categories (count of strengths in each category)
df$executing <- rowSums(df[c("Achiever",	"Arranger",	"Belief",	"Consistency",	"Deliberative",	"Discipline",	"Focus",	"Responsibility",	"Restorative")]/
                        df[c("Achiever",	"Arranger",	"Belief",	"Consistency",	"Deliberative",	"Discipline",	"Focus",	"Responsibility",	"Restorative")],na.rm = TRUE)
df$influencing <- rowSums(df[c("Activator",	"Command",	"Communication",	"Competition",	"Maximiser",	"Self-Assurance",	"Significance",	"Woo")]/
                          df[c("Activator",	"Command",	"Communication",	"Competition",	"Maximiser",	"Self-Assurance",	"Significance",	"Woo")],na.rm = TRUE)
df$relationshipBuilding <- rowSums(df[c("Adaptability",	"Connectedness",	"Developer",	"Empathy",	"Harmony",	"Includer",	"Individualisation",	"Positivity",	"Relator")]/
                            df[c("Adaptability",	"Connectedness",	"Developer",	"Empathy",	"Harmony",	"Includer",	"Individualisation",	"Positivity",	"Relator")],na.rm = TRUE)
df$strategicThinking <- rowSums(df[c("Analytical",	"Context",	"Futuristic",	"Ideation",	"Input",	"Intellection",	"Learner",	"Strategic")]/
                            df[c("Analytical",	"Context",	"Futuristic",	"Ideation",	"Input",	"Intellection",	"Learner",	"Strategic")],na.rm = TRUE)


#normalise summary categories
df$executing <- normalit(df$executing)
df$influencing <- normalit(df$influencing)
df$relationshipBuilding <- normalit(df$relationshipBuilding)
df$strategicThinking <- normalit(df$strategicThinking)


#convert to matrix
m <- as.matrix(df[,2:ncol(df)])

#recode individual strength rankings into scores
m[m == 1] <- 1
m[m == 2] <- 0.8
m[m == 3] <- 0.6
m[m == 4] <- 0.4
m[m == 5] <- 0.2
m[is.na(m)] <- 0


#calc cosine similarity measures for each person in the data
x <- cosine(t(m))


#create dataset ready for export
mat <- cbind(Names,as.data.frame(x))
names(mat) <- c("Name",Names)
mat <- mat %>% gather(key = Name2, value = Simscore,-Name)
mat <- mat %>% filter(Name != Name2)
mat <- mat %>% filter(!is.na(Simscore))
mat <- mat %>% arrange(Name,desc(Simscore))


write.csv(mat,"similarityScores.csv")


