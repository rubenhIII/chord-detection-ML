rm(list=ls(all=TRUE))
library(seewave)
library(tuneR)
library("e1071")

#------------------------------
# Frequencies and chords

NotaNombres<-c("DO","DO#","RE","RE#","MI","FA",
		"FA#","SOL","SOL#","LA","LA#","SI")

# Function to build the chords templates
Acorde<-function(notabase,tipo="M"){
   nota<-toupper(notabase)
   if(!nota%in%NotaNombres)
      stop("Nombre de nota no valido")
   for(i in 1:length(NotaNombres)){
      if(nota==NotaNombres[i]){
          if(tipo=="M")
             notas<-NotaNombres[(i-1+c(0,4,7))%%12+1]
          if(tipo=="m")
             notas<-NotaNombres[(i-1+c(0,3,7))%%12+1]
          if(tipo=="d")
             notas<-NotaNombres[(i-1+c(0,3,6))%%12+1]
          if(tipo=="a")
             notas<-NotaNombres[(i-1+c(0,4,8))%%12+1]
          if(tipo=="sus2")
             notas<-NotaNombres[(i-1+c(0,2,7))%%12+1]
          if(tipo=="sus4")
             notas<-NotaNombres[(i-1+c(0,5,7))%%12+1]
          }
      }
   names(notas)<-c(paste(nota,tipo,sep=""),rep("",length(notas)-1))
   return(notas)
   }

# Build every chord over every note
Acordes<-list()
for(i in 1:length(NotaNombres))
   Acordes[[i]]<-Acorde(NotaNombres[i])
for(i in 1:length(NotaNombres))
   Acordes[[i+length(NotaNombres)]]<-Acorde(NotaNombres[i],"m")
for(i in 1:length(NotaNombres))
   Acordes[[i+length(NotaNombres)*2]]<-Acorde(NotaNombres[i],"d")
for(i in 1:length(NotaNombres))
   Acordes[[i+length(NotaNombres)*3]]<-Acorde(NotaNombres[i],"a")
for(i in 1:length(NotaNombres))
   Acordes[[i+length(NotaNombres)*4]]<-Acorde(NotaNombres[i],"sus2")
for(i in 1:length(NotaNombres))
   Acordes[[i+length(NotaNombres)*5]]<-Acorde(NotaNombres[i],"sus4")
names(Acordes)<-c(NotaNombres,paste(NotaNombres,"m",sep=""),paste(NotaNombres,"dim",sep=""),
   paste(NotaNombres,"aug",sep=""),paste(NotaNombres,"sus2",sep=""),paste(NotaNombres,"sus4",sep=""))
#names(Acordes)<-c(NotaNombres,paste(NotaNombres,"m",sep=""))

#------------------------------
# Execute functions in ./Tools
#------------------------------

#Test
featureTypes<-c("MelFCC","ZCR","CSH","SpectralCentroid",
      "SpectralFlux","DOMF")
rutaT<-file.choose()
Test<-CalculateFeature(rutaT,22050,featureTypes,2048,512)

#Training
rutas<-sort(choose.files())
acordes<-length(rutas)

es<-numeric()

ues<-numeric()
legs<-character()
legsM<-list()

for(k in 1:acordes){
   ruta<-rutas[k]
   r<-CalculateFeature(ruta,22050,featureTypes,2048,512)
   nr<-dim(r)[1]
   SampNumTrain<-nr
   mr<-sample(1:nr,SampNumTrain)
   r<-r[mr,]
   if(k==1){
      rs<-r
   }
   else{
      rs<-rbind(rs,r)
   }	
   nom<-basename(ruta)
   partes<-unlist(strsplit(nom,".",fixed=TRUE))
   nom<-partes[1]
   partes<-unlist(strsplit(nom,"_"))
   indice<-as.numeric(partes[2])+12*as.numeric(partes[3])

   nombreUnico<-paste(sort(toupper(partes[4:6])),collapse="")
   if(!(nombreUnico%in%legs)){
      ues<-c(ues,indice)
      legs<-c(legs,nombreUnico)
      legsM[[length(ues)]]<-nombreUnico
   }else{
      posIndA<-which(nombreUnico==legs)
      indice<-ues[posIndA]
      legsM[[posIndA]]<-c(legsM[[posIndA]],"otroNombre")
      }

   e<-rep(indice,SampNumTrain)
   if(k==1){
      es<-e
   }else{
      es<-c(es,e)
   }
}


#Training Individual

nT<-dim(rs)[2]
Training<-(rs)
#colnames(Training)<-c(paste("v",(1:nT),sep=""))
eTraining<-(es)
colnames(Test)<-paste("v",(1:nT),sep="")

#Training con Python

TrainingTot<-cbind(Training,Training2)
TestTot<-cbind(Test,Test2)
nT<-dim(TrainingTot)[2]
colnames(TrainingTot)<-c(paste("v",(1:nT),sep=""))
colnames(TestTot)<-paste("v",(1:nT),sep="")

rm("ePred")
types<-c("BagCART","C50","DecisionTree","Multinomial",
	"randomForest","SVM","GBM","naiveBayes","FDA",
	"Nnet","KNN","MDA","LDA")

ePred<-SupervisedLearning(TrainingTot,eTraining,TestTot,types)



#-----Impresión-------------

colores2<-as.numeric(eTraining)
paleta<-rainbow(max(colores2))
paleta[1]<-"#FF0000FF"
paleta[2]<-"#00FF00FF"
paleta[3]<-"#0000FFFF"
paleta[4]<-"#FFFFFFFF"

x11()
MostrarResultadosAcordes(eTraining,Test,ePred2,paleta)

#----Limpieza con Cadenas Markovianas---

#ePredLimp<-GMMF2(as.numeric(ePred$randomForest))
#ePredLimp2<-factor(ePredLimp$MAP)
#ePredLimp2<-cbind(ePred,ePredLimp2)

resPred<-lapply(ePred,GMMF2Factor)
FmaxN2<-(FmaxN[seq(1,length(FmaxN),length.out=579)])
ePred2<-cbind(as.data.frame(resPred),FmaxN2)

x11()
MostrarResultadosAcordes(eTraining,Test,ePredLimp2,paleta)
