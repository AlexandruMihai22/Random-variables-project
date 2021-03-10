exponentialPdf <- function(val) {
  if (val <= 0) stop("Lambda trebuie sa fie strict pozitiv!")
  return(
    function(x) (
    if (x < 0) 0
    else val * exp(-val * x)
    )
  )
}

f_2<-function(f,g){
  
  my.c<-readline(prompt = "Introduceti optiunea 1/2/3/4: ")
  if(my.c ==1||my.c==2||my.c==3||my.c==4){}
  else print("Optiunea nu este valabila")
  
  my.c<-as.integer(my.c)
  
  if(my.c==1) {
    
    tryCatch (
      {
        # Calculam formula de convolutie pentru functiile f si g
        function(z) (integrate(Vectorize(function(y) (f(y) * g(z - y))), -Inf, Inf)$value)
      },
      error = function(e)
      {
        stop("Nu s-a putut aplica formula de convolutie pe f si g!")
      })
    
  }
  
  else if(my.c==2){
    
    tryCatch (
      {
        # Calculam formula de convolutie pentru functiile f si g
        function(z) (integrate(Vectorize(function(y) (f(y) * g(y - z))), -Inf, Inf)$value)
      },
      error = function(e)
      {
        stop("Nu s-a putut aplica formula de convolutie pe f si g!")
      })
    
  }
  else if(my.c==3){
    
    
  }
  else if(my.c==4){
    
  }
  
  
}

rez = f_2(exponentialPdf(2),exponentialPdf(3))
print(rez(3))