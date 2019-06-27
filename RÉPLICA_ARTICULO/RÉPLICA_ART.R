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

#Construcción curva AOQL Figura 3
P<-seq(0,1,0.0001)
pa<-pbinom(0,50,P)
AOQ<-pa*P
x11()
plot(P,AOQ,type="l",xlim = c(0,0.08),ylim = c(0,0.008),add=T,ylab="AOQ - Calidad saliente")
abline(h=max(AOQ),col="red",lty=2,lwd=2)
abline(v=0.02,col="red",lwd=2,lty=2)
text(0.0445,0.0065, "n=50",cex = 1, col="black")
text(0.0442,0.00626, "c=0",cex = 1, col="black")
text(0.049,0.006, "Tamaño del lote=10000",cex = 1, col="black")
text(0.015,0.0075, "AOQL=0.0073",cex = 1, col="black")


#Curvas para determinar valores AOQL's Figura 4
p=seq(0,1,0.001)
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
    plot(n,max_AOQ,type = "l",ylim = c(0,0.1),xlim=c(140,0),xlab="Tamaño de muestra",ylab="AOQL")
    text(125,0.095, "c=15",cex = 1, col="black")
    text(40,0.027, "c=0",cex = 1, col="black")
    #abline(v=c(20,80),h=c(0.0108,0.0174,0.0437,0.072))
    a=T
  }
  
  lines(n,max_AOQ,type = "l",ylim = c(0,0.1),add=T)
}

#GRÁFICAS CURVAS CARACTERISTICAS DE OPERACIÓN
#PAG 17
# Fijar en una matriz la secuencia de
# los valores de los "no conformes"
j<-seq(0,0.75,0.125)
p.mat<-matrix(rep(j,3),ncol=3,byrow=FALSE)
# Integrar los resultados en una matriz
n1<-phyper(0,j*8,(1-j)*8,5)
n2<-phyper(0,j*8,(1-j)*8,3)
n3<-phyper(0,j*8,(1-j)*8,2)
OC.mat<-cbind(n1,n2,n3)
# Plotear ambas matrices
x11()
matplot(p.mat, OC.mat, type = "l", lty = 1, lwd = 2, lend = par("lend"),
        pch = NULL,
        col = 1:6, cex = 2, bg = NA,
        xlab = "p", ylab = expression(pa),
        xlim = NULL, ylim = NULL,
        add = FALSE, verbose
        = getOption("verbose"))
text(0.2,0.26, "n=5",cex = 1, col="black")
text(0.3,0.33, "n=3",cex = 1, col="black")
text(0.38,0.4, "n=2",cex = 1, col="black")
grid(10, 10, lwd = 0) 


#PAG 18
# Fijar en una matriz la secuencia de
# los valores de los "no conformes"
j<-seq(0,0.75,0.0666)
p.mat<-matrix(rep(j,5),ncol=5,byrow=FALSE)
# Integrar los resultados en una matriz
n1<-phyper(0,j*15,(1-j)*15,13)
n2<-phyper(0,j*15,(1-j)*15,8)
n3<-phyper(0,j*15,(1-j)*15,5)
n4<-phyper(0,j*15,(1-j)*15,3)
n5<-phyper(0,j*15,(1-j)*15,2)
OC.mat<-cbind(n1,n2,n3,n4,n5)
# Plotear ambas matrices
x11()
matplot(p.mat, OC.mat, type = "l", lty = 1, lwd = 2, lend = par("lend"),
        pch = NULL,
        col = 1:6, cex = 2, bg = NA,
        xlab = "p", ylab = expression(pa),
        xlim = NULL, ylim = NULL,
        add = FALSE, verbose
        = getOption("verbose"))
text(0.2,0.3, "n=5",cex = 1, col="black")
text(0.3,0.36, "n=3",cex = 1, col="black")
text(0.38,0.4, "n=2",cex = 1, col="black")
text(0.14,0.25, "n=8",cex = 1, col="black")
text(0.08,0.2, "n=13",cex = 1, col="black")
grid(10, 10, lwd = 0) 

#PAG 19
# Fijar en una matriz la secuencia de
# los valores de los "no conformes"
j<-seq(0,0.76,0.04)
p.mat<-matrix(rep(j,6),ncol=6,byrow=FALSE)
# Integrar los resultados en una matriz
n0<-phyper(0,j*25,(1-j)*25,20)
n1<-phyper(0,j*25,(1-j)*25,13)
n2<-phyper(0,j*25,(1-j)*25,8)
n3<-phyper(0,j*25,(1-j)*25,5)
n4<-phyper(0,j*25,(1-j)*25,3)
n5<-phyper(0,j*25,(1-j)*25,2)
OC.mat<-cbind(n0,n1,n2,n3,n4,n5)
# Plotear ambas matrices
x11()
matplot(p.mat, OC.mat, type = "l", lty = 1, lwd = 2, lend = par("lend"),
        pch = NULL,
        col = 1:6, cex = 2, bg = NA,
        xlab = "p", ylab = expression(pa),
        xlim = NULL, ylim = NULL,
        add = FALSE, verbose
        = getOption("verbose"))
text(0.23,0.3, "n=5",cex = 1, col="black")
text(0.3,0.36, "n=3",cex = 1, col="black")
text(0.38,0.4, "n=2",cex = 1, col="black")
text(0.16,0.25, "n=8",cex = 1, col="black")
text(0.11,0.2, "n=13",cex = 1, col="black")
text(0.07,0.15, "n=20",cex = 1, col="black")
grid(10, 10, lwd = 0) 

#PAG 20
# Fijar en una matriz la secuencia de
# los valores de los "no conformes"
j<-seq(0,0.56,0.02)
p.mat<-matrix(rep(j,6),ncol=6,byrow=FALSE)
# Integrar los resultados en una matriz
n0<-phyper(0,j*50,(1-j)*50,32)
n1<-phyper(0,j*50,(1-j)*50,20)
n2<-phyper(0,j*50,(1-j)*50,13)
n3<-phyper(0,j*50,(1-j)*50,8)
n4<-phyper(0,j*50,(1-j)*50,5)
n5<-phyper(0,j*50,(1-j)*50,3)
OC.mat<-cbind(n0,n1,n2,n3,n4,n5)
# Plotear ambas matrices
x11()
matplot(p.mat, OC.mat, type = "l", lty = 1, lwd = 2, lend = par("lend"),
        pch = NULL,
        col = 1:6, cex = 2, bg = NA,
        xlab = "p", ylab = expression(pa),
        xlim = NULL, ylim = NULL,
        add = FALSE, verbose
        = getOption("verbose"))
text(0.25,0.45, "n=3",cex = 1, col="black")
text(0.15,0.3, "n=8",cex = 1, col="black")
text(0.2,0.36, "n=5",cex = 1, col="black")
text(0.11,0.25, "n=13",cex = 1, col="black")
text(0.08,0.2, "n=20",cex = 1, col="black")
text(0.05,0.15, "n=32",cex = 1, col="black")
grid(10, 10, lwd = 0) 

#PAG 21
j<-seq(0,0.46,1/90)
p.mat<-matrix(rep(j,7),ncol=7,byrow=FALSE)
n0<-phyper(0,j*90,(1-j)*90,80)
n1<-phyper(0,j*90,(1-j)*90,50)
n2<-phyper(0,j*90,(1-j)*90,32)
n3<-phyper(0,j*90,(1-j)*90,20)
n4<-phyper(0,j*90,(1-j)*90,13)
n5<-phyper(0,j*90,(1-j)*90,8)
n6<-phyper(0,j*90,(1-j)*90,5)
OC.mat<-cbind(n0,n1,n2,n3,n4,n5,n6)
x11()
matplot(p.mat, OC.mat, type = "l", lty = 1, lwd = 2, lend = par("lend"),
        pch = NULL,
        col = 1:6, cex = 2, bg = NA,
        xlab = "p", ylab = expression(pa),
        xlim = NULL, ylim = NULL,
        add = FALSE, verbose
        = getOption("verbose"))
text(0.19,0.38, "n=5",cex = 1, col="black")
text(0.15,0.3, "n=8",cex = 1, col="black")
text(0.11,0.25, "n=13",cex = 1, col="black")
text(0.08,0.20, "n=20",cex = 1, col="black")
text(0.058,0.15, "n=32",cex = 1, col="black")
text(0.042,0.1, "n=50",cex = 1, col="black")
text(0.025,0.07, "n=80",cex = 1, col="black")
grid(10, 10, lwd = 0) 

#PAG 22
j<-seq(0,0.46,1/150)
p.mat<-matrix(rep(j,6),ncol=6,byrow=FALSE)
n0<-phyper(0,j*150,(1-j)*150,80)
n1<-phyper(0,j*150,(1-j)*150,32)
n2<-phyper(0,j*150,(1-j)*150,20)
n3<-phyper(0,j*150,(1-j)*150,13)
n4<-phyper(0,j*150,(1-j)*150,7)
n5<-phyper(0,j*150,(1-j)*150,5)
OC.mat<-cbind(n0,n1,n2,n3,n4,n5)
x11()
matplot(p.mat, OC.mat, type = "l", lty = 1, lwd = 2, lend = par("lend"),
        pch = NULL,
        col = 1:6, cex = 2, bg = NA,
        xlab = "p", ylab = expression(pa),
        xlim = NULL, ylim = NULL,
        add = FALSE, verbose
        = getOption("verbose"))
text(0.19,0.38, "n=5",cex = 1, col="black")
text(0.165,0.3, "n=7",cex = 1, col="black")
text(0.11,0.25, "n=13",cex = 1, col="black")
text(0.09,0.20, "n=20",cex = 1, col="black")
text(0.065,0.15, "n=32",cex = 1, col="black")
text(0.035,0.07, "n=80",cex = 1, col="black")
grid(10, 10, lwd = 0) 

#PAG 23
j<-seq(0,0.4,1/280)
p.mat<-matrix(rep(j,7),ncol=7,byrow=FALSE)
n0<-phyper(0,j*280,(1-j)*280,125)
n1<-phyper(0,j*280,(1-j)*280,50)
n2<-phyper(0,j*280,(1-j)*280,32)
n3<-phyper(0,j*280,(1-j)*280,20)
n4<-phyper(0,j*280,(1-j)*280,13)
n5<-phyper(0,j*280,(1-j)*280,10)
n6<-phyper(0,j*280,(1-j)*280,6)
OC.mat<-cbind(n0,n1,n2,n3,n4,n5,n6)
x11()
matplot(p.mat, OC.mat, type = "l", lty = 1, lwd = 2, lend = par("lend"),
        pch = NULL,
        col = 1:6, cex = 2, bg = NA,
        xlab = "p", ylab = expression(pa),
        xlim = NULL, ylim = NULL,
        add = FALSE, verbose
        = getOption("verbose"))
text(0.145,0.45, "n=6",cex = 1, col="black")
text(0.105,0.38, "n=10",cex = 1, col="black")
text(0.097,0.3, "n=13",cex = 1, col="black")
text(0.076,0.25, "n=20",cex = 1, col="black")
text(0.055,0.20, "n=32",cex = 1, col="black")
text(0.042,0.15, "n=50",cex = 1, col="black")
text(0.025,0.07, "n=125",cex = 1, col="black")
grid(10, 10, lwd = 0) 

#PAG 24
j<-seq(0,0.36,1/500)
p.mat<-matrix(rep(j,6),ncol=6,byrow=FALSE)
n0<-phyper(0,j*500,(1-j)*500,125)
n1<-phyper(0,j*500,(1-j)*500,50)
n2<-phyper(0,j*500,(1-j)*500,29)
n3<-phyper(0,j*500,(1-j)*500,16)
n4<-phyper(0,j*500,(1-j)*500,11)
n5<-phyper(0,j*500,(1-j)*500,7)
OC.mat<-cbind(n0,n1,n2,n3,n4,n5)
x11()
matplot(p.mat, OC.mat, type = "l", lty = 1, lwd = 2, lend = par("lend"),
        pch = NULL,
        col = 1:6, cex = 2, bg = NA,
        xlab = "p", ylab = expression(pa),
        xlim = NULL, ylim = NULL,
        add = FALSE, verbose
        = getOption("verbose"))
text(0.13,0.45, "n=7",cex = 1, col="black")
text(0.1,0.38, "n=11",cex = 1, col="black")
text(0.095,0.25, "n=16",cex = 1, col="black")
text(0.06,0.20, "n=29",cex = 1, col="black")
text(0.044,0.15, "n=50",cex = 1, col="black")
text(0.027,0.07, "n=125",cex = 1, col="black")
grid(10, 10, lwd = 0) 

#PAG 25
j<-seq(0,0.25,1/1200)
p.mat<-matrix(rep(j,6),ncol=6,byrow=FALSE)
n0<-phyper(0,j*1200,(1-j)*1200,125)
n1<-phyper(0,j*1200,(1-j)*1200,75)
n2<-phyper(0,j*1200,(1-j)*1200,47)
n3<-phyper(0,j*1200,(1-j)*1200,27)
n4<-phyper(0,j*1200,(1-j)*1200,19)
n5<-phyper(0,j*1200,(1-j)*1200,11)
OC.mat<-cbind(n0,n1,n2,n3,n4,n5)
x11()
matplot(p.mat, OC.mat, type = "l", lty = 1, lwd = 2, lend = par("lend"),
        pch = NULL,
        col = 1:6, cex = 2, bg = NA,
        xlab = "p", ylab = expression(pa),
        xlim = NULL, ylim = NULL,
        add = FALSE, verbose
        = getOption("verbose"))
text(0.08,0.45, "n=11",cex = 1, col="black")
text(0.06,0.38, "n=19",cex = 1, col="black")
text(0.057,0.25, "n=27",cex = 1, col="black")
text(0.039,0.20, "n=47",cex = 1, col="black")
text(0.032,0.15, "n=75",cex = 1, col="black")
text(0.027,0.07, "n=125",cex = 1, col="black")
grid(10, 10, lwd = 0) 

#PAG26
j<-seq(0,0.3,1/3200)
p.mat<-matrix(rep(j,6),ncol=6,byrow=FALSE)
#CON HIPERGEOMETRICA
n0<-phyper(0,j*3200,(1-j)*3200,200)
n1<-phyper(0,j*3200,(1-j)*3200,73)
n2<-phyper(0,j*3200,(1-j)*3200,42)
n3<-phyper(0,j*3200,(1-j)*3200,23)
n4<-phyper(0,j*3200,(1-j)*3200,13)
n5<-phyper(0,j*3200,(1-j)*3200,9)
OC.mat<-cbind(n0,n1,n2,n3,n4,n5)
x11()
matplot(p.mat, OC.mat, type = "l", lty = 1, lwd = 2, lend = par("lend"),
        pch = NULL,
        col = 1:6, cex = 2, bg = NA,
        xlab = "p", ylab = expression(pa),
        xlim = NULL, ylim = NULL,
        add = FALSE, verbose
        = getOption("verbose"))
text(0.095,0.45, "n=9",cex = 1, col="black")
text(0.08,0.38, "n=13",cex = 1, col="black")
text(0.064,0.27, "n=23",cex = 1, col="black")
text(0.044,0.20, "n=42",cex = 1, col="black")
text(0.032,0.15, "n=73",cex = 1, col="black")
text(0.022,0.07, "n=200",cex = 1, col="black")
grid(10, 10, lwd = 0) 

#CON BINOMIAL
n0<-pbinom(0,200,j)
n1<-pbinom(0,73,j)
n2<-pbinom(0,42,j)
n3<-pbinom(0,23,j)
n4<-pbinom(0,13,j)
n5<-pbinom(0,9,j)
OC.mat<-cbind(n0,n1,n2,n3,n4,n5)
x11()
matplot(p.mat, OC.mat, type = "l", lty = 1, lwd = 2, lend = par("lend"),
        pch = NULL,
        col = 1:6, cex = 2, bg = NA,
        xlab = "p", ylab = expression(pa),
        xlim = NULL, ylim = NULL,
        add = FALSE, verbose
        = getOption("verbose"))
text(0.095,0.45, "n=9",cex = 1, col="black")
text(0.08,0.38, "n=13",cex = 1, col="black")
text(0.064,0.27, "n=23",cex = 1, col="black")
text(0.044,0.20, "n=42",cex = 1, col="black")
text(0.032,0.15, "n=73",cex = 1, col="black")
text(0.022,0.07, "n=200",cex = 1, col="black")
grid(10, 10, lwd = 0) 

#PAG27
j<-seq(0,0.3,0.001)
p.mat<-matrix(rep(j,6),ncol=6,byrow=FALSE)
#CON BINOMIAL
n0<-pbinom(0,189,j)
n1<-pbinom(0,86,j)
n2<-pbinom(0,50,j)
n3<-pbinom(0,29,j)
n4<-pbinom(0,15,j)
n5<-pbinom(0,9,j)
OC.mat<-cbind(n0,n1,n2,n3,n4,n5)
x11()
matplot(p.mat, OC.mat, type = "l", lty = 1, lwd = 2, lend = par("lend"),
        pch = NULL,
        col = 1:6, cex = 2, bg = NA,
        xlab = "p", ylab = expression(pa),
        xlim = NULL, ylim = NULL,
        add = FALSE, verbose
        = getOption("verbose"))
text(0.095,0.45, "n=9",cex = 1, col="black")
text(0.07,0.38, "n=15",cex = 1, col="black")
text(0.055,0.27, "n=29",cex = 1, col="black")
text(0.04,0.20, "n=50",cex = 1, col="black")
text(0.03,0.15, "n=86",cex = 1, col="black")
text(0.022,0.07, "n=189",cex = 1, col="black")
grid(10, 10, lwd = 0) 

#PAG28
j<-seq(0,0.3,0.001)
p.mat<-matrix(rep(j,6),ncol=6,byrow=FALSE)
n0<-pbinom(0,189,j)
n1<-pbinom(0,77,j)
n2<-pbinom(0,46,j)
n3<-pbinom(0,29,j)
n4<-pbinom(0,15,j)
n5<-pbinom(0,9,j)
OC.mat<-cbind(n0,n1,n2,n3,n4,n5)
x11()
matplot(p.mat, OC.mat, type = "l", lty = 1, lwd = 2, lend = par("lend"),
        pch = NULL,
        col = 1:6, cex = 2, bg = NA,
        xlab = "p", ylab = expression(pa),
        xlim = NULL, ylim = NULL,
        add = FALSE, verbose
        = getOption("verbose"))
text(0.095,0.45, "n=9",cex = 1, col="black")
text(0.07,0.38, "n=15",cex = 1, col="black")
text(0.055,0.27, "n=29",cex = 1, col="black")
text(0.04,0.20, "n=46",cex = 1, col="black")
text(0.03,0.15, "n=77",cex = 1, col="black")
text(0.022,0.07, "n=189",cex = 1, col="black")
grid(10, 10, lwd = 0) 

#PAG29
j<-seq(0,0.3,0.001)
p.mat<-matrix(rep(j,6),ncol=6,byrow=FALSE)
n0<-pbinom(0,170,j)
n1<-pbinom(0,96,j)
n2<-pbinom(0,56,j)
n3<-pbinom(0,29,j)
n4<-pbinom(0,15,j)
n5<-pbinom(0,9,j)
OC.mat<-cbind(n0,n1,n2,n3,n4,n5)
x11()
matplot(p.mat, OC.mat, type = "l", lty = 1, lwd = 2, lend = par("lend"),
        pch = NULL,
        col = 1:6, cex = 2, bg = NA,
        xlab = "p", ylab = expression(pa),
        xlim = NULL, ylim = NULL,
        add = FALSE, verbose
        = getOption("verbose"))
text(0.095,0.45, "n=9",cex = 1, col="black")
text(0.07,0.38, "n=15",cex = 1, col="black")
text(0.055,0.27, "n=29",cex = 1, col="black")
text(0.037,0.20, "n=56",cex = 1, col="black")
text(0.027,0.15, "n=96",cex = 1, col="black")
text(0.008,0.07, "n=170",cex = 1, col="black")
grid(10, 10, lwd = 0) 

#PAG 30
j<-seq(0,0.3,0.001)
p.mat<-matrix(rep(j,5),ncol=5,byrow=FALSE)
n0<-pbinom(0,156,j)
n1<-pbinom(0,64,j)
n2<-pbinom(0,29,j)
n3<-pbinom(0,15,j)
n4<-pbinom(0,9,j)
OC.mat<-cbind(n0,n1,n2,n3,n4)
x11()
matplot(p.mat, OC.mat, type = "l", lty = 1, lwd = 2, lend = par("lend"),
        pch = NULL,
        col = 1:6, cex = 2, bg = NA,
        xlab = "p", ylab = expression(pa),
        xlim = NULL, ylim = NULL,
        add = FALSE, verbose
        = getOption("verbose"))
text(0.095,0.45, "n=9",cex = 1, col="black")
text(0.07,0.38, "n=15",cex = 1, col="black")
text(0.055,0.27, "n=29",cex = 1, col="black")
text(0.033,0.20, "n=64",cex = 1, col="black")
text(0.024,0.08, "n=170",cex = 1, col="black")
grid(10, 10, lwd = 0) 

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

RQL(0.09977,0,18)

b=0.1
n=(log(b)/log(1-RQL(0.09977,10,125)))

f_milstd105e(N=15000,L="II",type="n",NCA=4) #Tamaño de muestra 3, nivel de aceptación 1%
RQL(0.099,21,315)
n=(log(b)/log(1-RQL(0.099,21,315)))

OC2c(5,0,1,type = "hypergeom",8)
x <- OC2c(5,0, type="hypergeom", N=8, pd=seq(0,0.7, 0.125))
plot(x)
