
data("Indometh")
df=Indometh
z<-df[,1]
x<-df[,2]
y<-df[,3]

my.0 = length(z)

my.1=median(x) #cea mai mare valoare medie
my.2=median(y)

my.3=mean(x) #media aritmeticÄƒ
my.4=mean(y)

duration = Indometh$time #durata
my.5=var(duration) #varianÈ›a

concentrations = Indometh$conc
my.7=var(concentrations)

my.6=quantile(duration) #raspunsurile se gasesc la 25%, 50% si respectiv 75%


show<-function(){
  print("Numarul de probe din setul de date este: ")
  print(my.0)
  print ("Mediana timpilor este: ")
  print(my.1)
  print ("Mediana concentratiilor este: ")
  print(my.2)
  print ("Media aritmetica a datelor din coloana timp este: ")
  print(my.3)
  print ("Media aritmetica a datelor din coloana concentratiilor este: ")
  print(my.4)
  print ("Variatia prezenta in coloana timp din setul de date este: ")
  print(my.5)
  print ("Variatia prezenta in coloana concetratiilor din setul de date este: ")
  print(my.7)
  print ("Valorile care împart intervalul în 4 parti sunt: ")
  print(my.6)
  
  boxplot(df) #grafic caracteristic
}

show()
