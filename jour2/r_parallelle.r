specimens <- list(seq(1,50,1))
jours <- 10
nb.times <- length(specimens)*jours

# premiere table
table.specimen <- data.frame(ID=integer(), jour=integer(), hauteur = numeric(), masse = numeric(), BMI=numeric())

for(i in specimens){ # une table par specimen
  table.temp <- data.frame(ID=integer(), jour=integer(), hauteur = numeric(), masse = numeric(), BMI=numeric())
  for(j in 1:jours){ # 10 jours de mesures
    haut <- runif(n = 1, min = 5.4, max = 6.2)
    mass <- sample(98:160, 1)
    BMI <- haut/mass
    temp2 <- data.frame(ID=i, jour=j, hauteur = haut, masse = mass, BMI=BMI)
    table.temp <- rbind(table.temp, temp2)
  }
  table.specimen <- rbind(table.specimen, table.temp)
}

# deuxieme table
table.alloc1 <- as.data.frame(matrix(ncol=5, nrow=length(specimens)*jours)) # nombre de lignes = nb specimens *nb jours
names(table.alloc1) <- c("ID", "jour", "hauteur", "masse", "BMI")
ligne <- 1
for(i in specimens){ # une table par specimen
  for(j in 1:jours){ # 10 jours de mesures
    haut <- runif(n = 1, min = 5.4, max = 6.2)
    mass <- sample(98:160, 1)
    BMI <- haut/mass
    table.alloc1[ligne,] <- c(i,j,haut,mass,BMI)

    ligne <- ligne+1
  }
}

# 3e table
table.alloc2 <- data.frame(ID=rep(specimens, jours), 
                           jour=rep(1:jours, length(specimens)), 
                           hauteur = replicate(1,runif(min= 5.4, max= 6.2,n=nb.times)), 
                           masse = replicate(1,sample(98:160, nb.times, replace = TRUE)), 
                           BMI=numeric(length = nb.times))
table.alloc2$BMI <- table.alloc2$hauteur/table.alloc2$masse

for(indiv in specimens){
  df.subset <- subset(table.alloc2, ID == indiv, select = c(names(table.alloc2)))
  plot(df.subset)
}

for(j in 1:jours){
  df.subset <- subset(table.alloc2, jour == j, select = c(names(table.alloc2)))
  lm.model <- as.numeric(lm(BMI~hauteur, data = df.subset)$coefficients[2])
  print(lm.model)
}

library(doParallel)

print.coeff <- function(indiv){
  df.subset <- subset(table.alloc2, ID == indiv, select = c(names(table.alloc2)))
  lm.model <- as.numeric(lm(BMI~hauteur, data = df.subset)$coefficients[2])
  print(lm.model)
}


foreach(indiv=specimens, .combine=rbind) %dopar% {print.coeff(indiv=indiv)}

ncores = 2
registerDoParallel(cores=ncores)# Shows the number of Parallel Workers to be used

# ici, nous avons la manière de specifier le nombre de coeurs a utiliser lorsque la valeur est dependante du fichier
# de soumission
#ncores = Sys.getenv("SLURM_CPUS_PER_TASK") 

#registerDoParallel(cores=ncores)# Shows the number of Parallel Workers to be used

foreach(indiv=specimens, .combine=rbind) %dopar% {print.coeff(indiv=indiv)}


