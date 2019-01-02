library(car)
library(carData)
library(readxl)
library(corrplot)
library(Rcmdr)

#CODA 
#Factor Analysis
#Load data
lagos<- read.table("C:/Users/Maksym/Desktop/UPM/Analisis Estadistico/af/lagos.txt", header=TRUE, sep="\t", na.strings="NA", dec=".", strip.white=TRUE)

#Correlation matixx

library(lattice, pos=19)
library(survival, pos=19)

library(Formula, pos=19)

library(ggplot2, pos=19)

library(Hmisc, pos=19)

rcorr.adjust(lagos[,c("Carbono","Conductividad","NH4","NO2","NO3","Oxígeno","pH",
                      
                      +    "PO4","Profundidad","SiO2","Temperatura")], type="pearson", use="pairwise.complete")

#AF
.FA <- factanal(~Carbono+Conductividad+NH4+NO2+NO3+Oxígeno+pH+PO4+Profundidad+SiO2+Temperatura, factors=2, rotation="varimax", scores="none", data=lagos)

+    print(.FA)

factanal(x = ~Carbono + Conductividad + NH4 + NO2 + NO3 + Oxígeno +     pH + PO4 + Profundidad + SiO2 + Temperatura, factors = 2,     data = lagos, scores = "none", rotation = "varimax")

#Loadings sorting
print(.FA$loadings, cutoff = .0, sort = TRUE)

#Saturation Graph
.FA <-factanal(~Carbono+Conductividad+NO2+Oxígeno+PO4+SiO2+Temperatura+NO3+NH4+pH+Profundidad, factors=2, rotation="varimax", scores= "none", data=lagos)

plot(.FA$loadings, type="n")
text(.FA$loadings,labels=row.names(.FA$loadings),cex=.7)


#KMO calculation
R2 = cor(lagos[,3:10])^2 # matriz de correl. al cuadrado
R2.suma = sum(R2)-dim(R2)[1] # suma correl. al cuadrado
RP2 = partial.cor(lagos[,3:13])$R^2
RP2.suma = sum(RP2) # suma correl. parciales al cuadrado
kmo = R2.suma/(R2.suma + RP2.suma) # calcula KMO
kmo # escribe resultado KMO

----------------------------------------------------------------------
  # Correspondence analysis
  #Load Data
  Dataset <- read.table("C:/Users/Maksym/Desktop/UPM/Analisis Estadistico/af/coches.txt", header=TRUE, sep="\t", na.strings="NA", dec=".", strip.white=TRUE)

#Libraries
install.packages("FactoMineR")
library(FactoMineR) # carga el paquete FactoMineR

#Excluding 1st row
rownames(Dataset)=Dataset[,1]
Dataset <- Dataset[ ,2:dim(Dataset)[2]]

#Performing correspondence analysis
res <- CA(Dataset, ncp=5, graph = TRUE) # aplica AC DatoAC, creando el objeto “res”

#Looking at eigenvalues
res$eig

#Looking at columns coordinates
res$col

#Looking at row coordinates
res$row

#Rversion
version
