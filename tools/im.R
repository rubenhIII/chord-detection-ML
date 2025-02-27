plotIM<-function(M,normalizar=TRUE,ylab=""){
   part<- par()$mai
   if(normalizar)
      M<-(M-min(M))/(max(M)-min(M))
   nr<-dim(M)[1]
   nc<-dim(M)[2]
   if(ylab=="")
      par(mai=c(0,0,0,0))
   else
      par(mai=c(0,0.8,0,0),las=2)
   plot(c(1,nc),c(1,nr), type='n', axes = FALSE, xlab = "", ylab = "",yaxt="n")
   mtext(ylab,2)
   rasterImage(M, 1, 1, nc, nr,interpolate = FALSE)
   par(mai=part)
   }

ecualizaIM<-function(Im,type=c("empirica","GMM","KDE"),gmmNmodels=2){
   if(!type%in%c("empirica","GMM","KDE"))
      stop("Metodo de ecualizacion desconocido")
   nr<-dim(Im)[1]
   nc<-dim(Im)[2]
   if(type=="empirica"){
      T<-function(x){
         # F empirica
         F<-sum(Im<x)/(nr*nc)
         return(F)
         }
      }

   if(type=="GMM"){
      library("mixtools")
      xi<-as.vector(Im)
      Mx <- normalmixEM(xi,k=gmmNmodels)
      GMM_F <- function(x){
         m<-length(x)
         res<-numeric()
         for(i in 1:m){
            r <- Mx$lambda[1]*pnorm(x[i],Mx$mu[1],Mx$sigma[1])+Mx$lambda[2]*pnorm(x[i],Mx$mu[2],Mx$sigma[2])
            res<-c(res,r)
            }
         return(res)
         }
      T<-function(x) return(GMM_F(x))
      }

   if(type=="KDE"){
      xi<-as.vector(Im)
      d<-density(xi)
      f <- approxfun(d,yleft=0,yright=0)
      kernelF <- function(x){
         m<-length(x)
         res<-numeric()
         for(i in 1:m){
            r<-integrate(f,0,x[i],stop.on.error = FALSE)
            res<-c(res,min(r$value,1))
            }
         return(res)
         }
      T<-function(x) return(kernelF(x))
      }

   res<-matrix(0, nrow=nr,ncol=nc)

   for(i in 1:nr)
   for(j in 1:nc)
      res[i,j]<-T(Im[i,j])

   return(res)
   }

leerTransfQ<-function(ruta){
   library("tools")
   x<-read.table(ruta,header=FALSE,sep=".")
   r<-x$V1+x$V2/99
   r<-matrix(r,nrow=12,byrow=TRUE)
   r<-t(r)
   return(r)
   }

MostrarResultadoAcordes<-function(eTraining,Test,ePred){
   colores2<-as.numeric(eTraining)
   paleta<-rainbow(max(colores2))
   nc<-length(eTraining)
   R32<-array(0,dim=c(2,nc,3))
   for(j in 1:nc){
      R32[1,j,]<-col2rgb(paleta[colores2[j]])
      R32[2,j,]<-col2rgb(paleta[colores2[j]])
      }
   colores<-as.numeric(ePred)
   nc<-dim(Test)[1]
   nr<-dim(Test)[2]
   R3<-array(0,dim=c(nr,nc,3))
   for(i in 1:nr)
   for(j in 1:nc)
      R3[i,j,]<-col2rgb(paleta[colores[j]])

   par(mfcol=c(3,1),mai=c(0,0,0,0))
   plotIM(R32)
   plotIM(t(Test))
   abline(v=1:nc,col=paleta[colores],lty=3)
   plotIM(R3)
   }


MostrarResultadosAcordes<-function(eTraining,Test,ePred,paleta){
   colores2<-as.numeric(eTraining)
   #paleta<-rainbow(max(colores2))
   nc<-length(eTraining)
   R32<-array(0,dim=c(2,nc,3))
   for(j in 1:nc){
      R32[1,j,]<-col2rgb(paleta[colores2[j]])
      R32[2,j,]<-col2rgb(paleta[colores2[j]])
      }
   nc<-dim(Test)[1]
   nr<-2#dim(Test)[2]
   Npred<-dim(ePred)[2]
   R3<-array(0,dim=c(nr,nc,3))
   par(mfcol=c(Npred+1,1),mai=c(0.82,0,0,0))
   plotIM(R32)

   for(k in 1:Npred){
      colores<-as.numeric(as.character(ePred[,k]))
      for(i in 1:nr)
      for(j in 1:nc)
         R3[i,j,]<-col2rgb(paleta[colores[j]])
      plotIM(R3,ylab=colnames(ePred)[k])
      }
   }


