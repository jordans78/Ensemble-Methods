######################################################################################################################################################################
#                                                                                                                                                                    #
#                                                                                                                                                                    #
#                                                                                                                                                                    #
#                Customs Revenues Prediction Using Ensemble Methods (Statistical Modeling vs Machine Learning)                                                       #
                                                                                                                                                                # 



# Import library ----------------------------------------------

        Sys.setlocale("LC_TIME", "English")
        library(forecast)
        library(forecastHybrid)  
        library(tidyverse)
        library(ggplot2)
        library(fpp2)
        library(seasonal)
        library(readxl)



# (I.) Data Preprocessing ------------------------------------------------------


      # 1.Converting into ts-----------------------------------------------------------------------
      
      
      
      
      
      
      INPUT_MATRIX_FINAL <- read_excel("Data/INPUT_MATRIX_FINAL.xlsx", 
                                       col_types = c("numeric", "numeric", "date", 
                                                     "text", "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric"))
      
      
      
      
      INPUT_MATRIX_FINAL_MONTLY<-INPUT_MATRIX_FINAL%>%
        dplyr::select(Month,Year,Customs_duties)%>%
        dplyr:: group_by(Month,Year)%>%
        dplyr::summarise(Customs_duties=sum(Customs_duties,na.rm = TRUE))%>%
      dplyr::arrange((Year))
      
      
      
      
      library(psych)
      DESCRIPTIVE_S<-data.frame(describe.by(INPUT_MATRIX_FINAL_MONTLY))
      View(DESCRIPTIVE_S)
      autoplot(INPUT_MATRIX_FINAL_MONTLY,facet=TRUE)+ggtitle("MY DATA")
      
      
      library(urca)
      INPUT_MATRIX_FINAL_MONTLY$Customs_duties%>% ur.kpss() %>% summary()
      lag(INPUT_MATRIX_FINAL_MONTLY$Customs_duties)%>% ur.kpss() %>% summary()
      
      
      INPUT_MONTLY_TS_INPUT <-ts(matrix(as.integer(INPUT_MATRIX_FINAL_MONTLY$Customs_duties)),
                           start =c (2014 ,1) ,
                           end =c (2020 ,1) ,
                           frequency =12)
      
      
      
      
      # 
      
      # 2.Seasonal models --------------------------------------------------------
      
      library(tsibble)
      library(gglagplot)
      library(gridExtra)
      
      # 3.Collection of customs duties--------------------------------------------------------
      
      PLOT_COLLECTION<-autoplot(INPUT_MONTLY_TS_INPUT) +
        ggtitle("Monthly collection of customs duties") +
        ylab("MKD million") +
        xlab("Year")
      
      
      # 4.Seasonal sub-series --------------------------------------------------------
      
      PLOT_SUBS<-ggsubseriesplot(INPUT_MONTLY_TS_INPUT) +
       # ggtitle("Seasonal plot of monthly collection of customs duties ")+
        ylab("MKD million") 
      
      # 5.Polar seasonal plot----------------------------------------------------
      
      PLOT_SEAS<-ggseasonplot(INPUT_MONTLY_TS_INPUT, polar=TRUE) +
        ylab("MKD million") +
        ggtitle("")
      # 6.Lag plot----------------------------------------------------
      
      PLOT_LAG<-gglagplot(INPUT_MONTLY_TS_INPUT,seasonal = TRUE,set.lags = 1:12)+
      ggtitle(" Lagged scatterplots") 
      # 7.Box plot----------------------------------------------------
      INPUT_MATRIX_FINAL_MONTLY_BOX<-data.frame(INPUT_MATRIX_FINAL_MONTLY)
      INPUT_MATRIX_FINAL_MONTLY_BOX[1:2]<-NULL
      
      BOX_PLOT<-INPUT_MATRIX_FINAL_MONTLY_BOX %>%
        ggplot( aes(x="", y=Customs_duties)) + 
        geom_boxplot() +
        ylab("MKD million")+
        xlab("") 
      BOX_PLOT
      
      
      grid.arrange(BOX_PLOT,PLOT_SUBS,nrow=2)
                   
      
      grid.arrange(PLOT_SEAS,PLOT_LAG,nrow=1, ncol=2)
      
      # 8.ACF and PACF--------------------------------------------------
      
        ACF_PLOT<-ggAcf(INPUT_MONTLY_TS_INPUT,lag.max =24)+ggtitle("")
        ggAcf(INPUT_MONTLY_TS_INPUT,lag.max =24,plot = FALSE)
        PACF_PLOT<-ggPacf(INPUT_MONTLY_TS_INPUT,lag.max =24)+ggtitle("")
        ggPacf(INPUT_MONTLY_TS_INPUT,lag.max =24,plot = FALSE)
        grid.arrange(ACF_PLOT,PACF_PLOT)
      
      
        INPUT_MONTLY_TS_INPUT[,1] %>%
          stl(s.window="periodic",robust=TRUE) %>%
          autoplot()+ ylab("MKD million") +
          xlab("Year")
      
      
      # 9.Seasonal adjustment--------------------------------------------------
        
        #stl(INPUT_MONTLY_TS_INPUT[,1], s.window="periodic",robust=TRUE)
      
      
      
        
        INPUT_MONTLY_TS<-seasadj(stl(INPUT_MONTLY_TS_INPUT[,1], s.window="periodic",robust=TRUE))  
        INPUT_MONTLY_TS1<-seasadj(stl(INPUT_MONTLY_TS_INPUT[,1], s.window="periodic",robust=TRUE)) %>% tsclean()
        INPUT_MONTLY_TS_INPUT[,1]
        
      
      
      
        autoplot( INPUT_MONTLY_TS)
        autoplot(INPUT_MONTLY_TS1)
      
      
      
      # Seasonally adjusted
      
      autoplot(INPUT_MONTLY_TS_INPUT, series="Data") +
        #autolayer(trendcycle(INPUT_MONTLY_TS), series="Trend") +
        autolayer(INPUT_MONTLY_TS, series="Seasonally Adjusted") +
        xlab("Year") + ylab("MKD million") +
        ggtitle(" ") +
        scale_colour_manual(values=c("gray","blue","red"),
                            breaks=c("Data","Seasonally Adjusted"))
      
      
      
      
      
      autoplot(INPUT_MONTLY_TS_INPUT)
      autoplot(INPUT_MONTLY_TS)
      
      
      # Total observation 73
      # 20 % of 73 is 14
      # Test set 59
      # Traning set 14
      
      
      
      # 10.Splitting database Training set and test set ---------------------------------------------------------------------
      
        MONTLY_TS_TRAINING_SET<-subset(INPUT_MONTLY_TS,start=1,end=58)
        MONTLY_TS_TEST_SET<-subset(INPUT_MONTLY_TS,start=59,end=73)


# (II.) Modeling --------------------------------------------------------

      # 11.Statistical modeling with ARIMA---------------------------------------------------------------------
      ARIMA_forecast<-auto.arima(MONTLY_TS_TRAINING_SET,biasadj=TRUE,stepwise = FALSE,approximation = FALSE,D=1,d=1)
      #ARIMA_forecast<-auto.arima(MONTLY_TS_TRAINING_SET)
      summary(ARIMA_forecast)
      accuracy(forecast(ARIMA_forecast,h=15),x=MONTLY_TS_TEST_SET)
      checkresiduals(ARIMA_forecast)
      autoplot(forecast(ARIMA_forecast,h=15))+ylab("MKD million")+ 
      xlab("Year")+ggtitle("") 
      
      
      # 12.Machine learning------------------------------------------------------------------
      
      Fit_forecast<-nnetar(MONTLY_TS_TRAINING_SET,lambda = "auto",P=12) #,lambda = "auto") #,lambda="auto",repeats = 500) #,p=11) #,lambda="auto") #,p=25,P=12,size=10,repeats = 10)
      NNETAR_forecast<-forecast(Fit_forecast,PI=TRUE,h=15)
      #summary(NNETAR_forecast)
      summary(NNETAR_forecast$model[[1]])
      accuracy(forecast(NNETAR_forecast,h=15),x=MONTLY_TS_TEST_SET)
      #checkresiduals(NNETAR_forecast)
      #autoplot(forecast(NNETAR_forecast,PI=TRUE,h=15))
      
      autoplot(NNETAR_forecast)+ylab("MKD million")+ 
        xlab("Year")+ggtitle("") 
      
      checkresiduals(NNETAR_forecast,lag=11, df=11)
      
      Fit_forecast
      
      
      test_res<-NNETAR_forecast$residuals
      accuracy(test_res)
      
      Box.test(test_res,lag=11, type= c("Ljung-Box"))
      
      
      
      
      # 13.Ensemble---------------------------------------------------------------------
      
      AP_Ensemble_01 <- hybridModel(MONTLY_TS_TRAINING_SET,models="na",errorMethod = c("RMSE"),weights = c("insample.errors"), lambda = "auto")
      summary(AP_Ensemble_01$auto.arima)
      summary(AP_Ensemble_01$nnetar)
      summary(AP_Ensemble_01)
      
      #accuracy(AP_Ensemble_01,h=15,x=MONTLY_TS_TEST_SET)
      accuracy(forecast(AP_Ensemble_01,h=15),x=MONTLY_TS_TEST_SET)
      checkresiduals(AP_Ensemble_01$auto.arima)
      checkresiduals(AP_Ensemble_01$nnetar)
      checkresiduals(AP_Ensemble_01)
      
      autoplot(forecast(AP_Ensemble_01,h=15))+ylab("MKD million")+ 
        xlab("Year")+ggtitle("") 
      
      
      
      test_res2<-AP_Ensemble_01$residuals
      
      Box.test(test_res2,lag=11, type= c("Ljung-Box"))
      
      
      # Test on same graphs
      
      
      autoplot(MONTLY_TS_TRAINING_SET) +
        forecast::autolayer(ARIMA_forecast, PI=FALSE, series = "AutoARIMA") +
        ggtitle("Forecast 2018-2019") +
        guides(colour=guide_legend(title = "Forecast"))
      
      
      
      
      
      