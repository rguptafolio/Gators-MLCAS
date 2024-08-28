rm(list = ls(all.names = TRUE))
getwd()
setwd("~/")
getwd()
setwd("C:/Users/diego.leitao/Documents/R (1)")
library(dplyr)
library(openxlsx)
library(olsrr) #stepwise regression
library(mctest) #multicollinearity
library(GGally) #multicollinearity
library(Metrics) #MAE
library(stringr)
library(hydroGOF) #model parameters

###################
##### Data  #######
###################

data<-read.xlsx("vegetation_indices_v2.xlsx",
                na.strings="NA")
View(data)
str(data)



data<-data|>
  dplyr::select(Imagename, Row, Range, Experiment, Location,
                Red,Green,Blue,GLI,NGRDI,NDVI,GNDVI,SAVI,NDRE,RedEdge,DeepBlue,Nir,DVI,GDVI,RDVI,TDVI,
                   SCCI,EVI,TVI,VARIG,GARI,GCI,GLI2,TGI,NLI,MNLI,GSAVI,OSAVI,GOSAVI,MSAVI2,MSR,GRVI,WDRVI,
                   SR,BCC,BGI,BI,BRVI,CIVE,ExB,ExG,ExGR,ExR,GCC,GR,HI,HUE,I_KAW,IOR,I_PCA,MGRVI,MPRI,MVARI,
                   NDI,NGBDI,RCC,RGBVI,PRI,SCI,SI,VARI,VDVI,VEG,VIgreen,vNDVI,WI,yieldPerAcre)|>
  na.omit()

data[, 6:71] <- lapply(data[, 6:71], as.numeric)

val_data<-read.xlsx("validation_vegetation_indices_v2.xlsx",
                    na.strings="NA")
View(val_data)
str(val_data)

val_data[, 6:71] <- lapply(val_data[, 6:71], as.numeric)

val_data<-val_data|>
  dplyr::select(Imagename, Row, Range, Experiment, Location,
                Red,Green,Blue,GLI,NGRDI,NDVI,GNDVI,SAVI,NDRE,RedEdge,DeepBlue,Nir,DVI,GDVI,RDVI,TDVI,
                SCCI,EVI,TVI,VARIG,GARI,GCI,GLI2,TGI,NLI,MNLI,GSAVI,OSAVI,GOSAVI,MSAVI2,MSR,GRVI,WDRVI,
                SR,BCC,BGI,BI,BRVI,CIVE,ExB,ExG,ExGR,ExR,GCC,GR,HI,HUE,I_KAW,IOR,I_PCA,MGRVI,MPRI,MVARI,
                NDI,NGBDI,RCC,RGBVI,PRI,SCI,SI,VARI,VDVI,VEG,VIgreen,vNDVI,WI)|>
  na.omit()

test_data<-read.xlsx("test_vegetation_indices_v2.xlsx",
                     na.strings="NA")

test_data<-test_data|>
  dplyr::select(Imagename, Row, Range, Experiment, Location,
                Red,Green,Blue,GLI,NGRDI,NDVI,GNDVI,SAVI,NDRE,RedEdge,DeepBlue,Nir,DVI,GDVI,RDVI,TDVI,
                SCCI,EVI,TVI,VARIG,GARI,GCI,GLI2,TGI,NLI,MNLI,GSAVI,OSAVI,GOSAVI,MSAVI2,MSR,GRVI,WDRVI,
                SR,BCC,BGI,BI,BRVI,CIVE,ExB,ExG,ExGR,ExR,GCC,GR,HI,HUE,I_KAW,IOR,I_PCA,MGRVI,MPRI,MVARI,
                NDI,NGBDI,RCC,RGBVI,PRI,SCI,SI,VARI,VDVI,VEG,VIgreen,vNDVI,WI)|>
  na.omit()

test_data[, 6:71] <- lapply(test_data[, 6:71], as.numeric)

#########################
##### Regression ########
#########################


colnames(data)

lm1<-lm(yieldPerAcre ~ Red+Green+Blue+GLI+NGRDI+NDVI+GNDVI+SAVI+NDRE+RedEdge+DeepBlue+Nir+DVI+GDVI+RDVI+TDVI+
          SCCI+EVI+TVI+VARIG+GARI+GCI+GLI2+TGI+NLI+MNLI+GSAVI+OSAVI+GOSAVI+MSAVI2+MSR+GRVI+WDRVI+SR+BCC+BGI+BI+BRVI+
          CIVE+ExB+ExG+ExGR+ExR+GCC+GR+HI+HUE+I_KAW+IOR+I_PCA+MGRVI+MPRI+MVARI+NDI+NGBDI+RCC+RGBVI+PRI+SCI+SI+VARI+
          VDVI+VEG+VIgreen+vNDVI+WI,data=data)
ols_step_backward_aic(lm1,details=TRUE)
lm1_model<-lm(yieldPerAcre ~ Green+Blue+GLI+NGRDI+NDVI+NDRE+RedEdge+DeepBlue+Nir+DVI+RDVI+
                SCCI+VARIG+GARI+GCI+NLI+MNLI+GSAVI+OSAVI+GOSAVI+MSR+WDRVI+SR+BGI+BI+BRVI+
                CIVE+ExB+GR+IOR+I_PCA+MGRVI+MVARI+NGBDI+RGBVI+PRI+VEG+vNDVI,data=data)
lm1_model
summary(lm1_model) #0.356
ggpairs(X)
omcdiag(lm1_model)
imcdiag(lm1_model)

AIC(lm1_model)
RSS <- c(crossprod(lm1_model$residuals))
MSE <- RSS / length(lm1_model$residuals)
RMSE <- sqrt(MSE)
RMSE
mae(data$yieldPerAcre, predict(lm1_model))
g1<-ggplot(data, aes(x=predict(lm1_model), y= yieldPerAcre)) +
  geom_point(size=2) +
  geom_abline(intercept=0, slope=1,linewidth=2,linetype="dashed") +
  geom_smooth(method="lm",aes(x=yieldPerAcre, y=predict(lm1_model),color="Model 1"),fill="gray2",
              colour="red", linewidth=2)+
  labs(x='Predicted Values', y='Actual Values', title='Model 1')+
  theme_classic()+
  theme(axis.text.x = element_text(color='black', size = 20,face="bold"),
        axis.text.y = element_text(color='black', size = 20,face="bold"),  
        axis.title.x = element_text(color='black', size = 20,face="bold"),
        axis.title.y = element_text(color='black', size = 20,face="bold"),
        plot.title = element_text(color='black', size = 20,face="bold"))
g1<-g1+theme(plot.margin = margin(0.5,1,0.5,0.5,unit="cm"))
g1

predict_train<-predict(lm1_model,data)
data$predict_train<-predict_train
predict_val<-predict(lm1_model,val_data)
val_data$predict_val<-predict_val
predict_test<-predict(lm1_model,test_data)
test_data$predict_test<-predict_test

write.xlsx(test_data, 'test_prediction_Diego.xlsx')

#######################################################################################################################