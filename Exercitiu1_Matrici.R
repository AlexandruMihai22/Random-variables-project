#--------a)

frepcomgen <- function(m, n) {
  x <- matrix(0, 2, n)
  y <- matrix(0, 2, m)
  sum_prob <- 1
  x[1, 1] <- sample(1:5, 1)
  x[2, 1] <- round(runif(1, 0, sum_prob), digits = 2)
  sum_prob <- sum_prob - x[2, 1]  #scadem probabilitatea adaugata din suma totala
  for (i in 2:(n - 1)) {
    x[1, i] <- sample(1:5, 1) + x[1, i - 1]
    x[2, i] <- round(runif(1, 0, sum_prob), digits = 2)
    sum_prob <- sum_prob - x[2, i] #actualizam suma ramasa 
    
  }
  x[2, n] = sum_prob
  
  #analog pentru y 
  sum_prob <- 1 
  y[1, 1] <- sample(1:5, 1)
  y[2, 1] <- round(runif(1, 0, sum_prob), digits = 2)
  sum_prob <- sum_prob - y[2, 1]
  for (i in 2:(m - 1)) {
    y[1, i] <- sample(1:5, 1) + y[1, i - 1]
    y[2, i] <- round(runif(1, 0, sum_prob), digits = 2)
    sum_prob <- sum_prob - y[2, i]
  }
  y[2, m] = sum_prob
  
  matrice <- matrix(0, n, m)
  x <- x[, order(x[1, ])]  #Ordonam matricile dupa prima linie
  y <- y[, order(y[1, ])]
  
  aux_x <- x
  aux_y <- y
  
  for (i in 1:n) {
    pozitie_libera <- sample(1:m, 1) #alegem random pozitia care nu va fi completata
    for (j in 1:m)
    {
      if (j != pozitie_libera)
      {
        matrice[i, j] <- round(runif(1, 0, min(aux_x[2, i],aux_y[2, j])),
                               digits = 2)
        aux_x[2, i] <- aux_x[2, i] - matrice[i, j] #actualizam suma liniei 
        aux_y[2, j] <- aux_y[2, j] - matrice[i, j]  
      }
    }
  }
  
  All_matrices <- list(x, y, matrice) #cream o lista cu toate cele 3 matrici
  return(All_matrices)
  
}

#----------b)

fcomplrepcom <- function(matrice) {
  while (1) {   #cat timp exista zerouri de inlocuit
    found  <- 0
    n = length(matrice[, 1])
    m = length(matrice[1, ])
    
    i <- 1
    j <- 1
    aux_i <- i
    aux_j <- j
    while (aux_i <= n && aux_j <= m)  #pentru matricea patratica
      
    {
      nr_of_zeros <- 0
      poz <- 0
      for (count in 1:m)
        if (matrice[aux_i, count] == 0) {
          nr_of_zeros <- nr_of_zeros + 1 #numaram zerourile de pe linie
          poz <- count
          
        }
      if (nr_of_zeros == 1)
      {
        matrice[aux_i, poz] = x[2, aux_i] - sum(matrice[aux_i, ])
        found <- 1
      }
      nr_of_zeros <- 0
      poz <- 0
      for (count in 1:n)
        if (matrice[count, aux_j] == 0) {  
          nr_of_zeros <- nr_of_zeros + 1  #numaram zerourile de pe coloana
          poz <- count
          
        }
      if (nr_of_zeros == 1)
      { #scadem suma celorlalte elemente de pe coloana din suma totala
        matrice[poz, aux_j] = y[2, aux_j] - sum(matrice[, aux_j])
        found <- 1
      }
      aux_i <- aux_i + 1
      aux_j <- aux_j + 1
      
    }
    
    while (aux_i <= n) #verificam liniile ramase
    {
      nr_of_zeros <- 0
      poz <- 0
      for (count in 1:m)
        if (matrice[aux_i, count] == 0) {
          nr_of_zeros <- nr_of_zeros + 1
          poz <- count
          
        }
      if (nr_of_zeros == 1)
      {
        matrice[aux_i, poz] = x[2, aux_i] - sum(matrice[aux_i, ])
        found <- 1
      }
      aux_i <- aux_i + 1
    }
    
    
    while (aux_j <= m)  #verificam coloanele ramase
    {
      nr_of_zeros <- 0
      poz <- 0
      for (count in 1:n)
        if (matrice[count, aux_j] == 0) {
          nr_of_zeros <- nr_of_zeros + 1
          poz <- count
          
        }
      if (nr_of_zeros == 1)
      {
        matrice[poz, aux_j] = y[2, aux_j] - sum(matrice[, aux_j])
        found <- 1
      }
      aux_j <- aux_j + 1
      
      
    }
    
    if (found == 0) #Daca nu mai sunt zerouri de inlocuit, se returneaza matricea
      
      return(matrice)
  }
}



#--------c)
m <- 4
n <- 5
All_matrices <- frepcomgen(m,n)
x <- All_matrices[[1]]
y <- All_matrices[[2]]
matrice <- All_matrices[[3]]
matrice <- fcomplrepcom(matrice)

#-----1) Cov(5X+9,-3Y-2)

f_x <- 5*x + 9
f_y <- -3*y - 2


med_x <- 0
med_y <- 0
for (i in 1:length(x[1, ]))
  med_x <- med_x + f_x[1, i] * f_x[2, i] #calculam media pentru x
for (i in 1:length(y[1, ]))
  med_y <- med_y + f_y[1, i] * f_y[2, i]
med_xy <- 0

for (i in 1:length(x[1, ]))
  for (j in 1:length(y[1, ]))
    med_xy <- med_xy + ((f_x[1, i] * f_y[1, j]) * (f_x[2, i] * f_y[2, j]))
cov_xy <- med_xy - med_x * med_y


#-----2) P(0<X<0.8/Y>0.3) = P(0<X<0.8) intersectatat P(Y>0.3) / P(Y>0.3) =
#      = P(0<X<0.8) * P(Y>0.3) / P(Y>0.3) = P(0<X<0.8)        

#P(0<X<0.8)

sum_x <- 0
for (i in 1:length(x[1, ]))
  if (x[1, i] > 0 && x[1, i] < 0.8) #Cautam elementele care respecta conditia
    sum_x <- sum_x + x[2, i]
P_2 <- sum_x


#-----3) P(X>0.2,Y<1.7) = P(X>0.2) intersectat P(Y<1.7) = P(X>0.2) * P(Y<1.7)

sum_x <- 0
for (i in 1:length(x[1, ]))
  if (x[1, i] > 0.2) #Cautam elementele care respecta conditia
    sum_x <- sum_x + x[2, i]

sum_y <- 0
for (i in 1:length(y[1, ]))
  if (y[1, i] < 1.7) #Cautam elementele care respecta conditia
    sum_y <- sum_y + y[2, i]

P_3 <- sum_x* sum_y



#--------d)

fverind <- function(x, y, matrice) {
  found <- 1
  for (i in 1:length(matrice[, 1])) #calculam toate produsele x[2, i] * y[2, j]
    for (j in 1:length(matrice[1, ]))
      if (x[2, i] * y[2, j] != matrice[i, j])
        found <- 0
  if (found == 1)
    print("Sunt independente")
  else
    print("Nu sunt independente")
  
}

fverind(x, y, matrice)

fvernecor <- function(x, y) {
  
  med_x <- 0
  med_y <- 0
  for (i in 1:length(x[1, ]))
    med_x <- med_x + x[1, i] * x[2, i] #calculam media lui x
  for (i in 1:length(y[1, ]))
    med_y <- med_y + y[1, i] * y[2, i]
  med_xy <- 0
  
  for (i in 1:length(x[1, ]))
    for (j in 1:length(y[1, ]))
      med_xy <- med_xy + ((x[1, i] * y[1, j]) * (x[2, i] * y[2, j]))
  cov_xy <- med_xy - med_x * med_y  #calculam cov dupa formula
  if (cov_xy == 0)
    print("Sunt necorelate")
  else
    print("Sunt corelate")
}

fvernecor(x, y)



