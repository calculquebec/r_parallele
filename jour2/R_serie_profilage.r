specimens <- list(seq(1,50,1))
jours <- 10

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

nb.times <- length(specimens)*jours
table.alloc2 <- data.frame(ID=rep(specimens, jours), 
                           jour=rep(1:jours, length(specimens)), 
                           hauteur = replicate(1,runif(min= 5.4, max= 6.2,n=nb.times)), 
                           masse = replicate(1,sample(98:160, nb.times, replace = TRUE)), 
                           BMI=numeric(length = nb.times))
table.alloc2$BMI <- table.alloc2$hauteur/table.alloc2$masse



library(profvis)

profvis({
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
})

profvis({
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
    })

profvis({
    nb.times <- length(specimens)*jours
table.alloc2 <- data.frame(ID=rep(specimens, jours), 
                           jour=rep(1:jours, length(specimens)), 
                           hauteur = replicate(1,runif(min= 5.4, max= 6.2,n=nb.times)), 
                           masse = replicate(1,sample(98:160, nb.times, replace = TRUE)), 
                           BMI=numeric(length = nb.times))
table.alloc2$BMI <- table.alloc2$hauteur/table.alloc2$masse
})


