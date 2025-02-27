SupervisedLearning<-function(Training,eTraining,Test,types){
   #Esta funcion es un wrapper de todas las demas
   tipos<-c("BagCART","C50","DecisionTree","Multinomial",
	"randomForest","SVM","GBM","naiveBayes","FDA",
	"PDA","Nnet","KNN","MDA","QDA","LDA")
   if(sum(!types%in% tipos))
      stop(paste("Tipos disponibles: ",tipos,sep=' ', collapse=''))
   n<-length(types)
   types<-unique(types)
   if(length(types)!=n)
      warning("Se eliminaron tipos repetidos")

   # prepando datos
   nT<-dim(Training)[2]
   dataTrain<-data.frame(cbind(Training,eTraining))
   colnames(dataTrain)<-c(paste("v",(1:nT),sep=""),"class")
   dataTrain$class<-as.factor(dataTrain$class)

   dataTest<-data.frame(Test)
   colnames(dataTest)<-paste("v",(1:nT),sep="")

   #Comienza metodos
   ePred<-data.frame(rep(0,dim(Test)[1]))
   for(type in types){
      if(type=="BagCART")
         et<-BagCART(dataTrain,dataTest)
      if(type=="C50")
         et<-c50(dataTrain,dataTest,trials=10)
      if(type=="DecisionTree")
         et<-DecisionTree(dataTrain,dataTest,prune=TRUE,minsplit=5)
      if(type=="Multinomial")
         et<-Multinomial(dataTrain,dataTest)
      if(type=="randomForest")
         et<-RandomF(dataTrain,dataTest)
      if(type=="SVM")
         et<-SVM(dataTrain,dataTest)
      if(type=="GBM")
         et<-GBM(dataTrain,dataTest)
      if(type=="naiveBayes")
         et<-NaiveBayes(dataTrain,dataTest)
      if(type=="FDA")
         et<-FDA(dataTrain,dataTest)
      if(type=="PDA")
         et<-PDA(Training, eTraining,dataTest)
      if(type=="Nnet")
         et<-Nnet(dataTrain,dataTest)
      if(type=="KNN")
         et<-KNN(Training, eTraining, Test,k=5)
      if(type=="MDA")
         et<-MDA(dataTrain,dataTest)
      if(type=="QDA")
         et<-QDA(dataTrain,dataTest)
      if(type=="LDA")
         et<-LDA(dataTrain,dataTest)

      nt<-colnames(ePred)
      ePred<-cbind(ePred,et)
      colnames(ePred)<-c(nt,type)
      }
   return(ePred[,-1])
   }




BagCART<-function(dataTrain,dataTest){
   LIB<-"ipred"
   if (!LIB %in% installed.packages()) install.packages(LIB)
   library("ipred")

   modeloBCART<-bagging(class~.,data=dataTrain)

   resBCART<-predict(modeloBCART,dataTest)

   return(resBCART)
   }

c50<-function(dataTrain,dataTest,...){
   LIB<-"C50"
   if (!LIB %in% installed.packages()) install.packages(LIB)
   library("C50")

   modeloC50<-C5.0(class~.,data=dataTrain,...)

   resC50<-predict(modeloC50,dataTest)

   return(resC50)
   }

DecisionTree<-function(dataTrain,dataTest,prune=FALSE,...){
   LIB<-"rpart"
   if (!LIB %in% installed.packages()) install.packages(LIB)
   library("rpart")

   modeloDtree<-rpart(class~.,data=dataTrain,method="class")

   # En algunos casos tal vez sea necesario "podar" el arbol
   if(prune){
      fit<-modeloDtree
      modeloDtree<- prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
      }

   resDtree<-predict(modeloDtree,dataTest)
   resDtree<-colnames(resDtree)[apply(resDtree,1,which.max)]
   return(resDtree)
   }

Multinomial<-function(dataTrain,dataTest,...){
   LIB<-"nnet"
   if (!LIB %in% installed.packages()) install.packages(LIB)
   library("nnet")

   modeloBCART<-multinom(class~.,data=dataTrain,MaxNWts =10000000)

   resMult<-predict(modeloBCART,dataTest)

   return(resMult)
   }

RandomF<-function(dataTrain,dataTest,...){
   LIB<-"randomForest"
   if (!LIB %in% installed.packages()) install.packages(LIB)
   library("randomForest")

   modeloRF<-randomForest(class~.,data=dataTrain)

   resRF<-predict(modeloRF,dataTest)

   return(resRF)
   }

SVM<-function(dataTrain,dataTest,...){
   LIB<-"e1071"
   if (!LIB %in% installed.packages()) install.packages(LIB)
   library("e1071")

   modeloSVM<-svm(class~.,data=dataTrain,kernel="radial")

   resSVM<-predict(modeloSVM,dataTest)

   return(resSVM)
   }

GBM<-function(dataTrain,dataTest,...){
   LIB<-"gbm"
   if (!LIB %in% installed.packages()) install.packages(LIB)
   library("gbm")

   modeloGBM<-gbm(class~.,data=dataTrain,
	distribution="multinomial",shrinkage=0.01,
	interaction.depth=4,n.trees=100)

   resGBM<-predict(modeloGBM,dataTest,n.trees=100)
   resGBM<-levels(dataTrain$class)[apply(resGBM,1,which.max)]

   return(resGBM)
   }

NaiveBayes<-function(dataTrain,dataTest,...){
   LIB<-"e1071"
   if (!LIB %in% installed.packages()) install.packages(LIB)
   library("e1071")

   modelonB<-naiveBayes(class~.,data=dataTrain)

   resnB<-predict(modelonB,dataTest)

   return(resnB)
   }

FDA<-function(dataTrain,dataTest,...){
   LIB<-"mda"
   if (!LIB %in% installed.packages()) install.packages(LIB)
   library("mda")

   modeloFDA<-fda(class~.,data=dataTrain)

   resFDA<-predict(modeloFDA,dataTest)

   return(resFDA)
   }

PDA<-function(Training, eTraining,dataTest,...){
   LIB<-"penalizedLDA"
   if (!LIB %in% installed.packages()) install.packages(LIB)
   library("penalizedLDA")

   modeloPDA<-PenalizedLDA(Training, eTraining,lambda=0.14,K=3)

   resPDA<-predict(modeloPDA,dataTest)
   resPDA<-(levels(as.factor(eTraining)))[resPDA$ypred[,3]]

   return(resPDA)
   }

Nnet<-function(dataTrain,dataTest,...){
   LIB<-"nnet"
   if (!LIB %in% installed.packages()) install.packages(LIB)
   library("nnet")

   modeloNnet<-nnet(class~.,data=dataTrain,size=6,MaxNWts =10000000)

   resNnet<-predict(modeloNnet,dataTest)
   resNnet<-levels(dataTrain$class)[apply(resNnet,1,which.max)]

   return(resNnet)
   }

KNN<-function(Training, eTraining,Test,...){
   library("class")

   resknn<-knn(Training,Test,eTraining,...)

   return(resknn)
   }

MDA<-function(dataTrain,dataTest,...){
   LIB<-"mda"
   if (!LIB %in% installed.packages()) install.packages(LIB)
   library("mda")

   modeloMDA<-mda(class~.,data=dataTrain)

   resMDA<-predict(modeloMDA,dataTest)

   return(resMDA)
   }

QDA<-function(dataTrain,dataTest,...){
   library("MASS")

   modeloQDA<-qda(class~.,data=dataTrain)

   resQDA<-predict(modeloQDA,dataTest)$class

   return(resQDA)
   }

LDA<-function(dataTrain,dataTest,...){
   library("MASS")

   modeloLDA<-lda(class~.,data=dataTrain)

   resLDA<-predict(modeloLDA,dataTest)$class

   return(resLDA)
   }






