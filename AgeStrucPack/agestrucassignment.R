setwd( "/home/chris/Desktop/AgeStrucPack")

ExtraData = read.csv(file="chicago-pop-data.csv", header=TRUE, sep=",") #####Load Population and Sq Area Data
AgeStrucData = read.csv(file="Chicago_Age_Structure.csv")

Population = ExtraData$X2010.Population
AgeGroups = 6

n = ceiling(0.01*sum(Population))###########number of individuals total
NeighborhoodVec=sample(1:length(ExtraData$Neighborhood), n, replace=TRUE, prob = Population/sum(Population)) ####Who is in what neighborhood


###Make age struc dist
agedist_male = matrix(data = 0, ncol = AgeGroups,nrow  = length(Population))
agedist_female = matrix(data = 0, ncol = AgeGroups,nrow = length(Population))
totaldist = matrix(data = 0, ncol = AgeGroups, nrow = length(Population))

genderdist = matrix(data = 0, ncol = 2, nrow = length(Population))

for (i in 1:length(Population))
{
  
  totalmalescounted = sum(AgeStrucData$Male_15.17.[i],AgeStrucData$Male_18.24.[i],AgeStrucData$Male_25.34.[i],AgeStrucData$Male_35.49.[i],AgeStrucData$Male_50..[i])
  totalfemalescounted = sum(AgeStrucData$F_15.17.[i],AgeStrucData$Female_18.24.[i],AgeStrucData$Female_25.34.[i],AgeStrucData$Female_35.49.[i],AgeStrucData$Female_50..[i])
  totalcounted = totalfemalescounted + totalmalescounted
  below15 = 1 - totalcounted
  
  males = c(below15/2,AgeStrucData$Male_15.17.[i],AgeStrucData$Male_18.24.[i],AgeStrucData$Male_25.34.[i],AgeStrucData$Male_35.49.[i],AgeStrucData$Male_50..[i])
  females = c(below15/2,AgeStrucData$F_15.17.[i],AgeStrucData$Female_18.24.[i],AgeStrucData$Female_25.34.[i],AgeStrucData$Female_35.49.[i],AgeStrucData$Female_50..[i])
  
  agedist_male[i,] = males
  agedist_female[i,] = females
  totaldist = agedist_female + agedist_male
  
  genderdist[i,1] = sum(males)
  genderdist[i,2] = sum(females)
  
}

####### Marriage distribution

marriagecats = 1:2 #1 married #2 single
agecats = 1:AgeGroups
cohabitingvec = c(0,0,0.16,0.55,0.71,0.65)
singlevec = c(1,1,0.84,0.45,0.29,0.35)

marriagedist = matrix(data = c(cohabitingvec,singlevec),ncol = length(marriagecats), nrow = length(agecats))

gender_vec = vector(length = n)
age_vec = vector(length = n)
marriage_vec = vector(length = n)


####### Assign gender, age, and marriage status

for(i in 1:n){
  decider = runif(1)
    if(decider < genderdist[NeighborhoodVec[i],1]){
    gender_vec[i] = 1 #male  
    }
    else{
    gender_vec[i] = 2 #female
    }
  
    if(gender_vec[i] == 1){ #if male
      age_vec[i] = sample(agecats,1,prob=agedist_male[NeighborhoodVec[i],])
    }
    else { #if female
      age_vec[i] = sample(agecats,1,prob=agedist_female[NeighborhoodVec[i],])
    }
    
    #Assign marriage status from marriage dist
    marriage_vec[i] = sample(marriagecats,1,prob = marriagedist[age_vec[i],])
   
 
}

