#RÉPLICA ARTÍCULO
#Kevin Steven García - 1533173
#Alejandro Vargas - 1525953
install.packages("Planesmuestra")
library(Planesmuestra)
install.packages("AcceptanceSampling")
library(AcceptanceSampling)

#MIL-STD-105E
f_milstd105e<-function(N,L,NCA,type){
  # Encontrar el numero de linea de intervalo del lote
  if (missing(N)){
    stop("El lote debe ser igual o mayor que 2")
  } else {
    data(lot_size)
    lot_interval<-findInterval(N,lot_size[,1])+1
    # Asigna el nivel de inspeccion si no es declarado explicitamente
    if (missing(L)){
      stop("Es necesario el argumento nivel o L")
    } else {
      # Encuentra la letra correspondiente al tama?o del lote y nivel de inspeccion
      data(code_letter)
      code_l<-code_letter[lot_interval,L]
    }
    # Asigna el valor por defecto de NCA si no es declarado
    # Intepolando el valor dado de NCA a los valores de tabla
    if (missing(NCA)){
      stop("Es necesario el argumento NCA")
    } else {
      data(NCA_values)
      if (any(NCA==NCA_values)){
        NCA2<-NCA
      } else {
        NCA_interval<-findInterval(NCA,NCA_values)
        NCA2<-NCA_values[NCA_interval]
      }
    } # Asigna el tipo de inpeccion si no es declarado
    if (missing(type)){
      stop("Es necesario el argumento type del tipo de inspeccion")
    }
    if(type=="n"){
      T_ins<-"Normal"
    } else {
      if(type=="r"){
        T_ins<-"Reducida"
      } else {
        T_ins<-"Rigurosa"
      }
    }
    # Leer los planes de inspeccion
    # La informacion queda
    data(milstd105eplans)
    # Busca con los argumetos code_l, T, NCA2 los valores n y c para el
    # Plan de inspecci?n
    code1<-milstd105eplans[milstd105eplans$code_letter==as.vector(code_l),]
    T1<-code1[code1$T==type,]
    NCA3<-T1[T1$NCA==NCA2,]
    c<-NCA3$c
    muestra<-NCA3$n
    # Objetos con los nombres de los argumentos y los argumentos
    argumentos_nombres<-c("Lote","Tipo de Inspeccion",
                          "Nivel de Inspeccion", "Nivel de Calidad Aceptable")
    argumentos_plan<-c(N,T_ins,L,NCA)
    # Objeto con los parametros del plan
    resultados_nombres<-c("Codigo Letra","Nivel de Calidad Aceptable", "Muestra",
                          "Numero de Aceptacion", "Numero de Rechazo")
    resultados_plan<-c(as.vector(code_l),NCA2,muestra,c,c+1)
    # Integrar los resultados en un data frame
    names(argumentos_plan)<-argumentos_nombres
    names(resultados_plan)<-resultados_nombres
    # Presentar resultados
    print(argumentos_plan)
    print(resultados_plan)
  }
} 

f_milstd105e(N=1300,L="II",type="n",NCA=1) #Tamaño de muestra 3, nivel de aceptación 1%
f_milstd105e(N=1300,L="II",type="n",NCA=4)#Tamaño de muestra 10, nivel de aceptación 4% 

f_milstd105e(N=600,L="II",type="n",NCA=1)

#Curva caracteristica de operación(requiere c,n y NCA)
f_DR.CO<-function(c,n,NCA){
  if (missing(NCA)){
    stop("El nivel de calidad aceptable debe de ser definido,
         para calcular la probabilidad de aceptación")
  }
  if (missing(n)){
    stop("La muestra debe de ser un número entero y positivo")
  }
  if (missing(c)){
    stop("El numero de aceptación debes ser entero e igual o mayor que cero")
  }
  beta.x<-pbinom(c,n,NCA/100)
  prob.x<-seq(0,0.13,by=0.001)
  n.1<-pbinom(c,n,prob.x)
  plot(prob.x, n.1, type = "l", lwd = 2,
       col = 2, cex = 2, bg = NA,
       xlab = "p", ylab = expression(1- alpha),
       xlim = c(0,0.13), ylim = c(0,1),
       main = "Curva de Operación",add=T)
  # Agrega opciones de graficas de bajo nivel
  segments(x0=0.0,y0=beta.x,x1=p,y1=beta.x,col="blue",lwd=2)
  segments(x0=p,y0=0.0,x1=p,y1=beta.x,col="blue",lwd=2)
  segments(x0=-0.0,y0=0,x1=0.1,y1=0,col="black",lwd=1)
  segments(x0=-0.0,y0=0,x1=0,y1=1,col="black",lwd=1)
  text(0.07,0.9, expression(paste(beta)),cex = 1, col="black")
  text(0.075,0.9, expression(" = "),cex = 1, col="black")
  text(0.09,0.9, round(beta.x,3),cex = 1, col="black")
  grid(10, 10, lwd = 1)
  structure(cbind("c"=c, "n"=n, "p"=p, "beta"=beta.x))
} 

#Curva característica de operación(requiere NCA, NCL, n y c)
f_CO.NCA.NCL<-function (NCA, NCL, n, c) {
  if (missing(NCL)) {
    stop("No existe el nivel de Calidad Limite")
  }
  else {
    if (missing(NCA)) {
      stop("No existe el nivel de Calidad Aceptable")
    }
    else {
      if (missing(n)) {
        stop("Tiene que definir la muestra")
      }
      else {
        if (missing(c)) {
          stop("Tiene que defir el numero de aceptacion")
        }
        else {
          beta.NCL <- pbinom(c, n, NCL)
          alpha.NCA <- pbinom(c, n, NCA)
          prob.x <- seq(0, 0.125, by = 0.001)
          n.1 <- pbinom(c, n, prob.x)
          plot(prob.x, n.1, type = "l", lwd = 2, 
               col = 2, cex = 2, bg = "grey", xlab = "NCA - NCL", 
               ylab = "Pa", xlim = c(0, 0.13), ylim = c(0, 
                                                        1), main = "Curva característica de operación")
          segments(x0 = 0, y0 = beta.NCL, x1 = NCL, y1 = beta.NCL, 
                   col = "blue", lwd = 2)
          segments(x0 = NCL, y0 = 0, x1 = NCL, y1 = beta.NCL, 
                   col = "blue", lwd = 2)
          segments(x0 = 0, y0 = alpha.NCA, x1 = NCA, 
                   y1 = alpha.NCA, col = "blue", lwd = 2)
          segments(x0 = NCA, y0 = 0, x1 = NCA, y1 = alpha.NCA, 
                   col = "blue", lwd = 2)
          segments(x0 = -0, y0 = 0, x1 = 0.125, y1 = 0, 
                   col = "black", lwd = 1)
          segments(x0 = -0, y0 = 0, x1 = 0, y1 = 1, col = "black", 
                   lwd = 1)
          text(NCL * 0.95, beta.NCL * 1.6, expression(paste(beta)), 
               cex = 1, col = "black")
          text(NCL, beta.NCL * 1.6, expression(" = "), 
               cex = 1, col = "black")
          text(NCL * 1.05, beta.NCL * 1.6, round(beta.NCL, 
                                                 3), cex = 1, col = "black")
          text(NCA * 1.3, alpha.NCA, expression(paste(alpha)), 
               cex = 1, col = "black")
          text(NCA * 1.4, alpha.NCA, expression(" = "), 
               cex = 1, col = "black")
          text(NCA * 1.55, alpha.NCA, round(1 - alpha.NCA, 
                                            3), cex = 1, col = "black")
          grid(10, 10, lwd = 1)
          structure(cbind(c = c, n = n, NCA = NCA, NCL = NCL, 
                          beta = beta.NCL, alpha = 1 - alpha.NCA))
        }
      }
    }
  }
}

f_CO.NCA.NCL(0.04,0.123,125,10) #Curva operación figura 1
f_DR.CO(10,125,0.04) #Curva operación figura 1

#Curvas caracteristicas figura 2
# Fijar en una matriz la secuencia de
# los valores de los "no conformes"
p.mat<-matrix(rep(seq(0,0.12,by=0.001),2),ncol=2,byrow=FALSE)
# Integrar los resultados en una matriz
n90<-pbinom(10,125,seq(0,0.12,by=0.001))
n69<-pbinom(0,18,seq(0,0.12,by=0.001))
OC.mat<-cbind(n90,n69)
# Plotear ambas matrices
x11()
matplot(p.mat, OC.mat, type = "l", lty = 1, lwd = 2, lend = par("lend"),
        pch = NULL,
        col = 1:6, cex = 2, bg = NA,
        xlab = "p", ylab = expression(pa),
        xlim = NULL, ylim = NULL,
        add = FALSE, verbose
        = getOption("verbose"))
text(0.0795,0.83, "n=125",cex = 1, col="black")
text(0.079,0.8, "c=10",cex = 1, col="black")
text(0.0395,0.63, "n=18",cex = 1, col="black")
text(0.039,0.6, "c=0",cex = 1, col="black")
grid(10, 10, lwd = 0) 



#Tabla tamaños de muestra y número de aceptación
f_milstd105e(N=1300,L="II",type="n",NCA=1) #Tamaño de muestra 125, c=3 y NCA 1%
f_milstd105e(N=1300,L="II",type="n",NCA=4)#Tamaño de muestra 10, c=10 y NCA 4% 
b=0.1
n=(log(b)/log(1-RQL(0.09977,3,125))) #Tamaño de muestra 42, c=0 y NCA 1%
n=(log(b)/log(1-RQL(0.09977,10,125))) #Tamaño de muestra 18, c=0 y NCA 4%

f_CO.NCA.NCL(0.001,0.01,18,0) #Gráfica 

#Construcción curva AOQL
P<-seq(0,1,0.01)
pa<-pbinom(0,50,P)
AOQ<-pa*P
x11()
plot(P,AOQ,type="l",xlim = c(0,0.08),ylim = c(0,0.008))

max(AOQ)

#Curvas para determinar valores AOQL's
p=seq(0,1,0.01)
n=0:140
Pa=c()
max_AOQ=c()
c=0:15
a=F
for (j in 1:length(c)) {
  max_AOQ=c()
  for (i in n) {
    pa=c()
    pa=pbinom(j,n[i],p)
    AOQ=p*pa
    max_AOQ=c(max_AOQ,max(AOQ))
  }
  if(a==F){
    x11()
    plot(n,max_AOQ,type = "l",ylim = c(0,0.10))
    a=T
  }
  
  lines(n,max_AOQ,type = "l",ylim = c(0,0.10),add=T)
}

#GRÁFICAS CURVAS CARACTERISTICAS DE OPERACIÓN
#PAG 17
n=c(5,3,2)
P=seq(0,1,0.01)
a=T
for (i in 1:length(n)) {
  Pa=pbinom(0,n[i],P)
  if(a==T){
  plot(P,Pa,type = "l",xlim=c(0,0.7))
  abline(h=0.1,col="red")
  a="K"
  }
  lines(P,Pa,add=T)
}

#PAG 18
n=c(13,8,5,3,2)
P=seq(0,1,0.01)
a=T
for (i in 1:length(n)) {
  Pa=pbinom(0,n[i],P)
  if(a==T){
    plot(P,Pa,type = "l",xlim=c(0,0.7))
    abline(h=0.1,col="red")
    a="K"
  }
  lines(P,Pa,add=T)
}

#PAG 19
n=c(32,20,13,8,5,3)
P=seq(0,1,0.01)
a=T
for (i in 1:length(n)) {
  Pa=pbinom(0,n[i],P)
  if(a==T){
    plot(P,Pa,type = "l",xlim=c(0,0.7))
    abline(h=0.1,col="red")
    a="K"
  }
  lines(P,Pa,add=T)
}

#PAG 20
n=c(32,20,13,8,5,3)
P=seq(0,1,0.01)
a=T
for (i in 1:length(n)) {
  Pa=pbinom(0,n[i],P)
  if(a==T){
    plot(P,Pa,type = "l",xlim=c(0,0.7))
    abline(h=0.1,col="red")
    a="K"
  }
  lines(P,Pa,add=T)
}

#PAG 21
n=c(80,50,32,20,13,8,5)
P=seq(0,1,0.01)
a=T
for (i in 1:length(n)) {
  Pa=pbinom(0,n[i],P)
  if(a==T){
    plot(P,Pa,type = "l",xlim=c(0,0.7))
    abline(h=0.1,col="red")
    a="K"
  }
  lines(P,Pa,add=T)
}

#PAG 22
n=c(80,32,20,13,7,5)
P=seq(0,1,0.01)
a=T
for (i in 1:length(n)) {
  Pa=pbinom(0,n[i],P)
  if(a==T){
    plot(P,Pa,type = "l",xlim=c(0,0.7))
    abline(h=0.1,col="red")
    a="K"
  }
  lines(P,Pa,add=T)
}

#PAG 23
n=c(125,50,32,20,13,10,6)
P=seq(0,1,0.01)
a=T
for (i in 1:length(n)) {
  Pa=pbinom(0,n[i],P)
  if(a==T){
    plot(P,Pa,type = "l",xlim=c(0,0.7))
    abline(h=0.1,col="red")
    a="K"
  }
  lines(P,Pa,add=T)
}

#PAG 24
n=c(125,50,29,16,11,7)
P=seq(0,1,0.01)
a=T
for (i in 1:length(n)) {
  Pa=pbinom(0,n[i],P)
  if(a==T){
    plot(P,Pa,type = "l",xlim=c(0,0.7))
    abline(h=0.1,col="red")
    a="K"
  }
  lines(P,Pa,add=T)
}

#PAG 25
n=c(125,75,47,27,19,11)
P=seq(0,1,0.01)
a=T
for (i in 1:length(n)) {
  Pa=pbinom(0,n[i],P)
  if(a==T){
    plot(P,Pa,type = "l",xlim=c(0,0.7))
    abline(h=0.1,col="red")
    a="K"
  }
  lines(P,Pa,add=T)
}

#PAG26
n=c(200,73,42,29,13,9)
P=seq(0,1,0.01)
a=T
for (i in 1:length(n)) {
  Pa=pbinom(0,n[i],P)
  if(a==T){
    x11()
    plot(P,Pa,type = "l",xlim=c(0,0.7))
    abline(h=0.1,col="red")
    a="K"
  }
  lines(P,Pa,add=T)
}

#PAG27
n=c(189,86,50,29,15,9)
P=seq(0,1,0.01)
a=T
for (i in 1:length(n)) {
  Pa=pbinom(0,n[i],P)
  if(a==T){
    x11()
    plot(P,Pa,type = "l",xlim=c(0,0.7))
    abline(h=0.1,col="red")
    a="K"
  }
  lines(P,Pa,add=T)
}

#PAG28
n=c(189,77,46,29,15,9)
P=seq(0,1,0.01)
a=T
for (i in 1:length(n)) {
  Pa=pbinom(0,n[i],P)
  if(a==T){
    x11()
    plot(P,Pa,type = "l",xlim=c(0,0.7))
    abline(h=0.1,col="red")
    a="K"
  }
  lines(P,Pa,add=T)
}

#PAG29
n=c(170,96,56,29,15,9)
P=seq(0,1,0.01)
a=T
for (i in 1:length(n)) {
  Pa=pbinom(0,n[i],P)
  if(a==T){
    x11()
    plot(P,Pa,type = "l",xlim=c(0,0.7))
    abline(h=0.1,col="red")
    a="K"
  }
  lines(P,Pa,add=T)
}

#PAG 30
n=c(156,64,29,15,9)
P=seq(0,1,0.01)
a=T
for (i in 1:length(n)) {
  Pa=pbinom(0,n[i],P)
  if(a==T){
    x11()
    plot(P,Pa,type = "l",xlim=c(0,0.7))
    abline(h=0.1,col="red")
    a="K"
  }
  lines(P,Pa,add=T)
}

#Función para encontrar RQL
RQL=function(alfa,c,n){
  a=seq(0,1,0.000001)
  t=pbinom(c,n,a)
  s=c()
  for (i in 1:length(t)) {
    if(t[i]>=alfa){
      s=c(s,i)
    }
    
  }
  max1=max(s)
  return(a[max1])
}

RQL(0.09977,2,80)

b=0.1
n=(log(b)/log(1-RQL(0.09977,10,125)))

f_milstd105e(N=15000,L="II",type="n",NCA=4) #Tamaño de muestra 3, nivel de aceptación 1%
RQL(0.099,21,315)
n=(log(b)/log(1-RQL(0.099,21,315)))
