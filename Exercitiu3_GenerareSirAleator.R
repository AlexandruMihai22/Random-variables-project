#Functia urmatoare are rolul de a calcula prima valoare din vector.
calcPrimaValoare <- function(){
  tries <- 0
  #O sa folosesc aceasta variabila pentru a tine cont de cate ori s-a incercat calcularea si nu a reusit.
  #daca la iesirea din "while" calcularea a reusit tries o sa fie = 3
  #altfel, tries o sa fie = 2
  
  val <- 0 #valoarea care o sa fie returnata 
  
  while (tries < 2) {
    oraChar<-format(Sys.time(), "%X")
    ore<-0
    min<-0
    sec<-0
    if (substring(oraChar, 2, 2) == ":"){
      valDeCal = 
        as.integer(substring(oraChar,0,1)) * 10000 + as.integer(substring(oraChar,3,4)) * 100 + as.integer(substring(oraChar,6,7))
      ore = as.integer(substring(oraChar,0,1))
      min = as.integer(substring(oraChar,3,4))
      sec = as.integer(substring(oraChar,6,7))
    } else {
      valDeCal = 
        as.integer(substring(oraChar,0,1)) * 10000 + as.integer(substring(oraChar,4,5)) * 100 + as.integer(substring(oraChar,7,8))
      ore = as.integer(substring(oraChar,0,1))
      min = as.integer(substring(oraChar,4,5))
      sec = as.integer(substring(oraChar,7,8))
    }
    #Dupa aceste procesari in valDeCalc o sa fie retinuta cifra cu care urmeaza sa lucram
    
    if ((valDeCal %% 17) == 0){
              tries = 3
              #Generez o singura valoare cu ajutorul lui rnorm(1, nr_min, nr_sec ) si o retin.
              val = rnorm(1, min, sec)
            }
    else if ((valDeCal %% 17) == 3){
              tries = 3
              #Generez o singura valoare cu ajutorul lui rpois(1, minute+runif(1,-1,1))
              val = rpois(1, min + runif(1, -1, 1))
            }
    else if ((valDeCal %% 17) == 5){
              tries = 3
              #Distributie exponentiala cu numarul de minute.
              val = rexp(1, min)
            }
    else if ((valDeCal %% 17) == 7){
              tries = 3
              #Distributie binomiala cu ora sist si 1/nr_minute + runif(1,0,5)
              val = rbinom(1, ore, 1/min)  + runif(1,0,5)
            }
    else if ((valDeCal %% 17) == 8){
              tries = 3
              val = runif(1,-5,7)
            }
    else if ((valDeCal %% 17) == 11){
              tries = 3
              #Distributie gamma si Distributie hypergeometrica
              val = rgamma(1, ore) - rhyper(1, sec, min, ore)
            }
    else{
              #Incercam inca o data, dupa ce programul asteapta o secunda.
              tries = tries + 1
              Sys.sleep(1)
            }
  }
  if (tries == 2) {
    #Daca niciuna dintre variante nu s-a indeplinit de doua ori, atunci:
    val = rnorm(1,0,1)
  }
  
  return(val)
}

generareSirAleator <- function(n) {
  #creez un sir gol.
  sir <- c(calcPrimaValoare())
  
  for (i in 2:n) {
    #xn=a* (xn-1)+b
    #a este o valoare generata cu func??ia rexp din R de parametru 5
    #b este o valoare generata cu func??ia rnorm din R de parametri 2 ??i 1
    newVal <- rexp(1,5) * sir[length(sir)] + rnorm(1,2,1)
    
    sir <- append(sir,newVal)
  }
  
  return(sir)
}

hist(
  generareSirAleator(
  readline(prompt="Enter n: ")
  )
  )


