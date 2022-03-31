################meta data##################################
### Created by - Sai Lalitha N #############3
#### last modified - 9/20/2021
#### Created - 4/1/2021
##### 9 models are available in the module as of date
###############code starts here#################################
library(shiny)
library(tidyr)
library(forecast)
library(prophet)
library(dygraphs)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(shinyWidgets)
library(shinyalert)
library(rhandsontable)
library(timeSeries)
library(shinyBS)
library(shinyscreenshot)
# install.packages("shinyscreenshot")

# readline("this is the error debugging")

##############for deprecation messages##supress unnecessary warnings ###
options(shiny.deprecation.messages=FALSE)
storeWarn<- getOption("warn")
options(warn = -1)
options(scipen = 999) ### scientific notation idsable for deafult, options(scipen = 0)

help<-div(
  br(),
  box(
  h4("Models available for univariate forecast",style="color:#046bb5;"),
  # p('TSLM allows you to put in an external regressor.it complies regression of the seasonal components. STL on the other hand will find the trend (using loess). Then break out the seasonal'),
  p(strong('Simple exponential smoothening'),': Forecasts produced using exponential smoothing methods are weighted averages of past observations, with the weights decaying exponentially as the observations get older. It only estimates the level component'),
  p(strong('ETS state space models'),': error trend and seasonality state space models maintain a latent state vector with recent information about level, trend and seasonality evolving and adding the factor at each step'),
  p(strong('ARIMA'),': Auto-Regressive Integrated Moving Average makes use of lagged moving averages to smooth time series data'),
  p(strong('STLM'),':  applies an STL decomposition, models the seasonally adjusted data, re-seasonalizes, and returns the forecasts'),
  p(strong('TSLM'),': used to fit linear models to time series, allows variables "trend" and "season" which are created on the fly from the time series characteristics of the data'),
  p(strong('NNAR'),': fits a neural network model to a time series with lagged values of the time series as inputs. Sequential neural network model works like a nonlinear autoregressive model for time series'),
  p(strong('SPLINE'),': cubic smoothing spline model is equivalent to an ARIMA(0,2,2) model but with a restricted parameter space. The advantage of the spline model over the full ARIMA model is that it provides a smooth historical trend as well as a linear forecast function'),
  p(strong('PROPHET'),': Prophet is based on an additive model where non-linear trends are fit with yearly, weekly, and daily seasonality( if customized for a case, can add holidays)'),
  p(strong('RWF'),': random walk, equivalent to an ARIMA(0,1,0) model with an optional drift coefficient')
  ),
  box(
  h4('Univariate vs Multivariate Time series',style="color:#046bb5;"),
  p(strong('Univariate time series'),': Only one variable is varying over time and will be used for forecasting based on historical data'),
  p(strong('Multivariate time series'),': A Multivariate time series has more than one time-dependent variable. Each variable depends not only on its past values but also has some dependency on other variables which will be used to forecast our required variable')
  )
  )
inputs<-div(
  br(),
  box(width="60%",height="auto",
      fluidRow(
        strong('I. Basic inputs',style="font-size: 14px;color:#046bb5;"),
        p('Upload an input file to perform the trending analysis on',
          style="font-size: 12px;color:grey;margin-top:2px;"),
        column(7,fileInput("input_file", "Choose CSV File", accept = ".csv"),style="margin-bottom:-16px;margin-left:-16px;"),
        column(1,circleButton("data_ip_format",icon=icon("info"),status = "default",size = "xs"),style="margin-left: -618px;padding-top: 30px;")
      ),
      fluidRow(
        fluidRow(
          column(3,
                 p('select the product/account/market name',
                   style="font-size: 12px;color:grey;"),
                 
                 selectInput(inputId = "prod",selected="sel1",
                             label = "Selection",choices = c("sel1","sel2","sel3")
                 )
          ),
          column(3,
                 p('select the frequency of the input data',
                   style="font-size: 12px;color:grey;"),
                 selectInput("freq","Data Frequency",c("Weekly","Monthly","Quarterly","Yearly"),selected="Monthly")
          )))
  ),
  # c(52,12,4,1),selected=12)
  # ,actionButton("info_train_prd","","On hover",icon = icon("info-circle"),style="background-color:transparent;")
  
  # ,p('# of missing values in the data',
  #      style="font-size: 12px;color:grey;")
  #   ,p('Replace missing values with',
  #      style="font-size: 12px;color:grey;")
  #   ,p(' # of outliers in the data',
  #      style="font-size: 12px;color:grey;")
  #   ,p('Replace outliers with',
  #      style="font-size: 12px;color:grey;")
  br(),
  box(width="60%",height="auto",
      fluidRow(
        strong('II. Forecast parameters',style="font-size: 14px;color:#046bb5;")
        ,p('If data needs to be filtered for relevancy or anyother reason,select the date range',
           style="font-size: 12px;color:grey;"),
        fluidRow(
          column(3,airDatepickerInput("st_dt","Start date filter",view = "months",autoClose=TRUE,minView = "months", dateFormat = "yyyy-mm"
                                      ,value=NA,update_on='close')),
          column(3,airDatepickerInput("end_dt","End date filter",view = "months",autoClose=TRUE,minView = "months", dateFormat = "yyyy-mm"
                                      ,value=NA,update_on='close'))
        )
        ,p('Select number of periods to forecast',
           style="font-size: 12px;color:grey;")
        ,selectInput("fcst_prd","Forecast period(# months)",choices = c(1:15),selected=1)
      )),br(),
  box(width="60%",height="auto",
      strong('III. Cross validation inputs (Optional)',style="font-size: 14px;color:#046bb5;"),
      fluidRow(
        column(7,p("Training parameters for customization",style="font-size: 12px;color:grey;")),
        column(1,circleButton("info_train",icon=icon("info"),status = "default",size = "xs",hover=T),style="margin-left: -630px;
               margin-top: -16px;"),
        style="margin:-1px;")
      ,airDatepickerInput("date_train","Select training period",view = "months",autoClose=TRUE,minView = "months", dateFormat = "yyyy-mm",range=TRUE,separator=" to ",
                          value=c(Sys.Date(),Sys.Date()-60),update_on='close')
      ,selectInput("n_step","#Steps for finding best model",choices = c(1:6),selected=1)
      ,br()
  ),br(),
  box(width="60%",height="auto",
      strong('IV. Model selection',style="font-size: 14px;color:#046bb5;"),
      fluidRow(
        column(width=8,
               radioButtons("model_type",label="Select level of model",choices=c("Basic trends","Advanced models"),inline=T),
               conditionalPanel(
                 condition = "input.model_type == 'Advanced models'",
                 checkboxGroupInput("model","",
                                    c("ARIMA","STLM","NNETAR","TSLM","PROPHET","RWF","SPLINE"),selected=c("ARIMA","STLM"),inline=TRUE)
               ),
               conditionalPanel(
                 condition = "input.model_type == 'Basic trends'",
                 div(
                   checkboxGroupInput("model_basic","",
                                      c("ETS","SES","Double exponential","6point exp-smootheining","12point exp-smoothening",
                                        "Logarithmic"),selected=c("ETS"),inline=TRUE)
                   
                   ,p('Currently ETS and SES are available, other models will soon be available in simple models ',
                      style="font-size: 10px;color:grey;")
                 )
               ))
        ,style="padding:22px 0px 0px 0px;"
      ))#upload and selection close
  ,br(),
  fluidRow(actionButton("apply","Apply",style="background-color: #046bb5;color:white;border-color:#046bb5;box-shadow: 0 2px 5px 1px rgb(64 60 67 / 16%);align:center;"),
           style="padding-left: 60px;padding-right: 60px;")
  ,style="font-family: Google Sans,arial,sans-serif;font-size: 12px;padding:2px 5px;"
  ##font size applies to outer heading of inputs
  
)

forecast<-div(
  
  br(),
  progressBar(id = "pb4", value = 0, display_pct = T),
  
  # div(
  fluidRow( 
    column(11,box(height="80%",width="70%",dygraphOutput("plot")))
    ,tags$head(tags$style(HTML('.box{border: 1px solid black;}')))
  ),
  box(width="60%",height="auto",
      fluidRow(
        column(10,
               h3("Forecast"),
               rHandsontableOutput("forecast_vals",height = "auto",width = "auto"),
               tags$style(type="text/css", "#forecast_vals th {word-wrap:break-word;
                          font-weight:bold;color:#f7f9fa;background-color:#0460A9;}")
               )),
      fluidRow(
        column(10,h3("Test Period Forecast"),
               rHandsontableOutput("forecast_test",height = "60%",width = "auto"),
               tags$style(type="text/css", "#forecast_test th {word-wrap:break-word;
                          font-weight:bold;color:#f7f9fa;background-color:#0460A9;}")
               
               ))),
  box(title="Error Metrics",width="60%",height="auto",
      # h4("Error on Test data"),
      fluidRow(
        p('Models are sorted based on their performance with first being the best model',
          style="font-size: 10px;color:grey;margin-top:-7px;"),
        rHandsontableOutput("models_acc",height = "auto",width = "auto"),
        tags$style(type="text/css", "#models_acc th {word-wrap:break-word;
                   font-weight:bold;color:#f7f9fa;background-color:#0460A9;}")
        
        )
  ),
  br(),
  # span(h3(textOutput("best_model")),style="color:blue"),
  fluidRow(
    column(3,
           screenshotButton(filename = "forecast",label="Save as image",style="background: #046bb5;
                            color: whitesmoke; 
                            box-shadow: 0 2px 5px 1px rgb(64 60 67 / 16%);
                            font-family: Google Sans,arial,sans-serif;border: 1px dotted darkgrey;")
           ),column(3,
                    downloadButton('downloadReport','Download tables',icon("download"),style="background: #046bb5;
                                   color: whitesmoke; 
                                   box-shadow: 0 2px 5px 1px rgb(64 60 67 / 16%);
                                   font-family: Google Sans,arial,sans-serif;border: 1px dotted darkgrey;")
                    ))
  ,br()
  ,style="color:black;font-family: arial,sans-serif;font-size: 13px;"
                    )#####close maintab

# sidebar<-div(
#   fluidRow(
#     span(
#       selectInput(inputId = "prod",selected="sel1",
#                   label = "Selection",choices = c("sel1","sel2","sel3")
#       ),
#       selectInput("freq","Data Frequency",c("Weekly","Monthly","Quarterly","Yearly"),selected="Monthly")
#       # c(52,12,4,1),selected=12)
#       # ,actionButton("info_train_prd","","On hover",icon = icon("info-circle"),style="background-color:transparent;")
#       ,selectInput("fcst_prd","Forecast period",choices = c(1:15),selected=1),
#       airDatepickerInput("st_dt","start date filter",view = "months",autoClose=TRUE,minView = "months", dateFormat = "yyyy-mm"
#        ,value=c(Sys.Date()),update_on='close'),
#       airDatepickerInput("end_dt","end date filter",view = "months",autoClose=TRUE,minView = "months", dateFormat = "yyyy-mm"
#                          ,value=c(Sys.Date()),update_on='close')
#     ),
#     
#     fluidRow(
#       column(10,p("Training parameters for customization",style="font-size: 10px;color:grey;")),
#       column(1,circleButton("info_train",icon=icon("info"),status = "default",size = "xs",hover=T)),
#       style="margin:-1px;")
#     ,airDatepickerInput("date_train","Select date range for training",view = "months",autoClose=TRUE,minView = "months", dateFormat = "yyyy-mm",range=TRUE,separator=" to ",
#                         value=c(Sys.Date(),Sys.Date()-60),update_on='close')
#     ,selectInput("n_step","#Steps for finding best model",choices = c(1:6),selected=1)
#     ,br()),
#   
#   fluidRow(actionButton("apply","Apply",style="background-color: #046bb5;color:white;border-color:#046bb5;box-shadow: 0 2px 5px 1px rgb(64 60 67 / 16%);align:center;"),
#            style="padding-left: 60px;padding-right: 60px;")
#   ,style="font-family: Google Sans,arial,sans-serif;font-size: 12px;padding:2px 5px;"
#   ##font size applies to outer heading of inputs
# )

body<-fluidPage(
  useShinyalert(),
  tags$style(HTML("
                  
                  /* .selectize-input { font-size: 13px; line-height: 25px;}
                  .selectize-dropdown { font-size: 13px; line-height: 22px; }*/
                  
                  .content-wrapper, .right-side {
                  background-color: #ffffff;
                  }
                  
                  body {
                  font-family: Google Sans,arial,sans-serif;
                  -moz-transform: scale(0.95, 0.95); /* Moz-browsers */
                  zoom: 0.95; /* Other non-webkit browsers */
                  zoom: 95%; /* Webkit browsers */
                  }
                  
                  
                  .tabbable > .nav > li[class=active]    > a                  
                  {background-color: white  
                  color:#0088cc;
                  box-shadow: 0 4px 0px 0px rgb(0 136 204 / 40%);
                  font-family: Google Sans,arial,sans-serif;
                  font-weight: bold;
                  font-size: medium;
                  } 
                  .tabbable > .nav > li > a                  
                  {background-color:white;  
                  color:#0088cc;
                  box-shadow: 0 2px 5px 1px rgb(64 60 67 / 16%);
                  font-family:Google Sans,arial,sans-serif;
                  font-weight: bold;
                  font-size: medium;
                  } 

                  .box
                  {
                  padding:0px 10px 3px 3px;
                  box-shadow: 0 2px 5px 1px rgb(64 60 67 / 16%);
                  font-family: Google Sans,arial,sans-serif;
                  border: 1px dotted darkgrey;
                  }
                  .box-body{
                  margin-left:20px;
                  padding:5px 3px 0px 2px;
                  }
                  .btn.btn-circle-xs{background-color:#046bb5;color:white}
                  /* for model selection froup margin */
                  .form-group {
                  margin-bottom: 4px;
                  }
                  
                  .btn-file {
                  background-color:#046bb5;
                  color:white;
                  }
                  
                  .progress.shiny-file-input-progress {
                  visibility: visible;
                  background-color:#f0f0f0;
                  }
                  .progress {
                  height: 16px;
                  ")))

ui <- fluidPage(
  div(
    titlePanel(
      title =div(
        img(src='logo.png', align = "right",width='13%',height='8%',style="margin-top:0px;padding-top:0px;"),
       p(span("Smart Forecast",style="color:#009deb;font-family:Bahnschrift;font-size:29px;"), span("- Divinition",style="color:#0088cc;font-family:Sans-serif;font-size:14px;"))
        # ,h4("...Forecast any data",style="color:black;font-style:italic;font-size:10px;padding-left:200px;padding-top:0px;margin-top:-7px;margin-bottom: 23px;")
      )
    ),
    sidebarLayout(position = "right",
                  sidebarPanel(
                    #   sidebar,
                    #   width=2
                    #   ,style="box-shadow: 0 1px 6px 0 rgb(32 33 36 / 68%);margin-top:80px;"
                  ),##sidebarclose
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Help",help),
                      tabPanel("Inputs",inputs),
                      tabPanel("Forecast",forecast)
                    ),width=12
                  )
    ) ##sidebarlayout close         
    ,body),##div close
  style="padding-left:25px;padding-right:20px;"
) ##ui close

########################3bnackend server function #############################3
server <- function(input,output,session) {
  # set.seed(9)
  # addTooltip(session=session,id="info_train_prd",title = "Training period used for calculating the error metrics, default value is selected if not modified")
  observeEvent(input$data_ip_format, {
    
    showModal(
      modalDialog(title = "Data input format",size="s",easyClose = TRUE,
                  # p('format of input file  edited ',style="color:#0d99bf;font-family:Verdana, Geneva, sans-serif;margin-top:2px"),
                  span(
                    img(src='input_format.png', align = "center",width='90%',height='90%'),
                    style="color:#000000;font-family:Verdana, Geneva, sans-serif;"
                  )
      ))
  })
  observeEvent(input$info_train, {
    
    showModal(
      modalDialog(title = "Training parameters",size="s",easyClose = TRUE,
                  tags$li(p('Date range selected will be used for training the model in cross validation process and number of steps to predict in the process is taken from n-step input',style="font-family:Verdana, Geneva, sans-serif;margin-top:2px")),
                  tags$li(p('Training parameters, if not selceted 15% data and 1-step ahead are used for testing the model accuracy',style="font-family:Verdana, Geneva, sans-serif;margin-top:2px")),
                  tags$li(p('selecting testing period less than n-step+1 for cross validation will by default change it to n-step+1 values',style="font-family:Verdana, Geneva, sans-serif;"))
                  
      ))
  })
  tableData <- reactiveValues()
  data<-reactiveValues()
  data_fil<-reactiveValues()
  models<-reactiveValues()
  observeEvent(input$input_file,
               {
                 inFile <- input$input_file
                 tableData$data<-read.csv(inFile$datapath,stringsAsFactors=F)
               })
  
  observeEvent(input$input_file,{  
    data_df <- data.frame(tableData$data)
    colnames(data_df)<-c("date",colnames(data_df)[-1])
    print(data_df)
    ########## READING DATA FROM FILE###########
    # data_df<-read.csv("C:/other/data.csv",stringsAsFactors=FALSE)
    # ####converting numeric to date only MDY formats
    data_df$date<-as.Date.character(data_df$date,tryFormats = c("%m/%d/%Y","%m-%d-%Y"))
    data_df<-data_df[order(data_df$date),]
    if(ncol(data_df[-1])==1){
      data_df[,-1]<-lapply(data_df[-1],as.numeric)
    }else{
      data_df[,-1]<-apply(data_df[,-1],2,as.numeric)}
    ##pass data to other set of observe events
    data$data<-data_df
    ###list of products###and other params
    prods<-colnames(data_df)[-1]
    updateSelectInput(session,inputId = "prod",
                      label = "Selection",
                      choices = prods,
                      selected=NA)
  })
  observeEvent(input$prod,{
    req(input$input_file)
    data_df<-data.frame(data$data)
    prods<-input$prod
    
    data_df <- data_df %>% select(c(colnames(data_df)[1],prods[1]))
    # print(data_df)
    min=data_df[head(which(data_df[,-1]!=0),1),1]
    max=data_df[tail(which(data_df[,-1]!=0),1),1]
    print(max)
    updateAirDateInput(session,"st_dt","start date filter"
                       ,value=min,options=list(minDate = min,maxDate = max,autoClose=TRUE))
    updateAirDateInput(session,"end_dt","end date filter"
                       ,value=max,options=list(minDate = min,maxDate = max,autoClose=TRUE))
  })
  observeEvent(c(input$st_dt,input$end_dt),{
    min_fil<-input$st_dt
    max_fil<-input$end_dt
    data_df_fil<-data.frame(data$data)
    data_df_fil <- data_df_fil %>% dplyr::filter(between(data_df_fil$date,min_fil,max_fil))
    data_fil$data<-data_df_fil
    print(min_fil)
    updateAirDateInput(session, "date_train","Select date range",
                       value=c(min_fil,max_fil),options=list(minDate = min_fil,maxDate = max_fil,range=TRUE,autoClose=TRUE))
    
  })
  
  observeEvent(input$apply,{ 
    req(input$input_file)
    data_df<-data.frame(data_fil$data) 
    prods<-input$prod 
    n<-as.integer(input$fcst_prd)
    frequency<-input$freq
    freq<-ifelse(frequency=="weekly",52,ifelse(frequency=="Monthly",12,ifelse(frequency=="Quarterly",4,1)))
    model_ad<-input$model
    model_bsc<-input$model_basic
    models$val<-c(model_ad,model_bsc)
    print(models$val)
    ##########breaking into products and then running next steps
    for (i in 1:length(prods))
    {
      p<-0
      print(prods[i])
      print(p)
      max_p<-length(models$val)
      ##selecting the i column from data along with date
      ts_kq<-data_df %>% select(c(colnames(data_df)[1],prods[i]))
      colnames(ts_kq)<-c("date","nbrx")
      ts_kq<-data.frame(ts_kq[which(ts_kq$nbrx>0),])
      actuals_unfil<-ts_kq ##unfiltered data
      ##############data for training for metrics calculation
      
      ###if the end date for training and actual data is same, choose 10% of data for training any way
      end_dt<-input$date_train[2]
      test_n<-NA
      if(input$date_train[2]==max(actuals_unfil[,1])){
        end_dt<- min(tail(ts_kq[,1],floor(0.15*nrow(ts_kq))+1)) 
        test_n<-floor(0.15*nrow(ts_kq))
      } else if(max(actuals_unfil[,1])-input$date_train[2] < ((as.integer(input$n_step)+1)*28))
      { 
        #end_dt<-tail(ts_kq[,1],as.integer(input$n_step)+2) ###remove n+1 steps so select n+2th 1st value
        end_dt<- min(tail(ts_kq[,1],floor(0.15*nrow(ts_kq))+1))
      } 
      
      print(paste("total points",nrow(ts_kq)))
      
      
      ts_kq<- ts_kq %>% dplyr::filter(between(ts_kq$date,input$date_train[1],end_dt))
      
      # max_ac<-actuals_unfil[tail(which(actuals_unfil[,-1]!=0),1),1]
      # max_train<-ts_kq[tail(which(ts_kq[,-1]!=0),1),1]
      
      test_n<-coalesce(test_n,nrow(actuals_unfil)-nrow(ts_kq)) ##incase date for training is selected
      initial<-nrow(ts_kq)
      
      print(paste("initial",initial))
      print(paste("test data number of months considered",test_n))
      n_step<-as.integer(input$n_step)
      
      
      yr<-lubridate::year(min(ts_kq$date))
      mth<-lubridate::month(min(ts_kq$date))
      # print(ts_kq)
      ts_kq_df<-ts(ts_kq[2],frequency = freq,start = c(yr,mth))
      
      #########actuals on which model output will be cal after training
      yr_ac<-lubridate::year(min(actuals_unfil$date))
      mth_ac<-lubridate::month(min(actuals_unfil$date))
      actuals_unfil_ts<-ts(actuals_unfil[2],frequency = freq,start = c(yr_ac,mth_ac))
      # print(ts_kq_df)
      # print(ts_kq)
      
      # print(max_ac)
      # print(max_train)
      
      showNotification("Forecasting for the selected models",type="default")
      modelname<-models$val
      yr<-lubridate::year(tail(actuals_unfil$date,1))
      mth<-lubridate::month(tail(actuals_unfil$date,1))
      ##################spline##e cubic smoothing spline model
      if("SPLINE" %in% modelname)
      {
        tryCatch(
          {
            ######training on entire data and predicting for selected period
            spline<-splinef(actuals_unfil_ts)
            spline_fcst<-forecast(spline,h=n)
            
            ####cross validation accuracy predicting n step ahead
            f<-function(x,h){forecast(splinef(x),h=h)}
            error_spline<-tsCV(actuals_unfil_ts,f,h=n_step,initial = initial)
            err<-if(n_step>1){
              tail(error_spline,test_n)[,n_step][1:(length(tail(error_spline,test_n)[,n_step])-n_step)]
            } else {
              tail(error_spline,test_n)[1:(length(tail(error_spline,test_n))-n_step)]
            }
            acc_spline_test<-accuracy(ts(tail(actuals_unfil[,-1],test_n-n_step) -err), tail(actuals_unfil[,-1],test_n-n_step))
            rownames(acc_spline_test)<-"spline"
            
            comp_spline<-ts(tail(actuals_unfil[,-1],test_n-n_step)-err,frequency = freq,end = c(yr,mth))
            
            ######forecast output
            forecast_spline=spline_fcst$mean
            
            showNotification("Spline forecast done",type="message")
            print("spline done")
            p<-p+1
            updateProgressBar(session = session, id = "pb4", value = (p/max_p)*100)
          },#try catch,
          warning=function(w){
            message("data warning! spline")
            print(w)
          },
          error= function(e){
            message("no spline model fits the data")
            print(e)
          }
        )#closse try catch
      }#close if
      #######rwf###naive and random walk forecast;equivalent to an ARIMA(0,1,0) model with an optional drift coefficient
      if("RWF" %in% modelname)
      {
        tryCatch(
          {
            #######random walk forecast
            
            ######training on entire data and predicting for selected period
            rwf<-rwf(actuals_unfil_ts,drift=TRUE)
            rwf_fcst<-forecast(rwf,h=n)
            
            # ###cross validation accuracy predicting n step ahead
            f<-function(x,h){forecast(rwf(x,drift=TRUE),h=h)}
            error_rwf<-tsCV(actuals_unfil_ts,f,h=n_step,initial = initial)
            err<-if(n_step>1){
              tail(error_rwf,test_n)[,n_step][1:(length(tail(error_rwf,test_n)[,n_step])-n_step)]
            } else {
              tail(error_rwf,test_n)[1:(length(tail(error_rwf,test_n))-n_step)]
            }
            acc_rwf_test<-accuracy(ts(tail(actuals_unfil[,-1],test_n-n_step) -err), tail(actuals_unfil[,-1],test_n-n_step))
            rownames(acc_rwf_test)<-"rwf"
            
            comp_rwf<-ts(tail(actuals_unfil[,-1],test_n-n_step) -err,frequency = freq,end = c(yr,mth))
            ######forecast output
            forecast_rwf=rwf_fcst$mean
            
            showNotification("random walk forecast done",type="message")
            print("rwf done")
            p<-p+1
            updateProgressBar(session = session, id = "pb4", value = (p/max_p)*100)
          },#try catch,
          warning=function(w){
            message("data warning! rwf")
            print(w)
          },
          error= function(e){
            message("no rwf model fits the data")
            print(e)
          }
        )#closse try catch
      }#close if
      
      ############ses#####
      
      if("SES" %in% modelname)
      {
        tryCatch(
          {
            #######simple exp smoothening
            
            ######training on entire data and predicting for selected period
            simpex<-ses(actuals_unfil_ts)
            simpex_fcst<-forecast(simpex,h=n)
            
            # ###cross validation accuracy predicting n step ahead
            f<-function(x,h){forecast(ses(x),h=h)}
            error_ses<-tsCV(actuals_unfil_ts,f,h=n_step,initial = initial)
            err<-if(n_step>1){
              tail(error_ses,test_n)[,n_step][1:(length(tail(error_ses,test_n)[,n_step])-n_step)]
            } else {
              tail(error_ses,test_n)[1:(length(tail(error_ses,test_n))-n_step)]
            }
            
            acc_simpex_test<-accuracy(ts(tail(actuals_unfil[,-1],test_n-n_step) - err), tail(actuals_unfil[,-1],test_n-n_step))
            rownames(acc_simpex_test)<-"ses"
            
            comp_ses<-ts(tail(actuals_unfil[,-1],test_n-n_step) -err,frequency = freq,end = c(yr,mth))
            ######forecast output
            forecast_simpex=simpex_fcst$mean
            
            showNotification("Simple exponential smoothening done",type="message")
            print("ses done")
            p<-p+1
            updateProgressBar(session = session, id = "pb4", value = (p/max_p)*100)
          },#try catch,
          warning=function(w){
            message("data warning! ses")
            print(w)
          },
          error= function(e){
            message("no ses model fits the data")
            print(e)
          }
        )#closse try catch
      }#close if
      
      ############ets#####
      modelname<-models$val
      if("ETS" %in% modelname)
      {
        tryCatch(
          {
            # ########training and predicting for test period
            # ets_kq<-ets(ts_kq_df)
            # ets_fsct_kq<-forecast(ets_kq,h=test_n)
            ######training on entire data and predicting for selected period
            ets <- ets(actuals_unfil_ts)
            ets_fsct<-forecast(ets, h=n)
            
            # ###cross validation accuracy predicting n step ahead
            f<-function(x,h){forecast(ets(x),h=h)}
            error_ets<-tsCV(actuals_unfil_ts,f,h=n_step,initial = initial)
            err<-if(n_step>1){
              tail(error_ets,test_n)[,n_step][1:(length(tail(error_ets,test_n)[,n_step])-n_step)]
            } else {
              tail(error_ets,test_n)[1:(length(tail(error_ets,test_n))-n_step)]
            }
            
            acc_ets_test<-accuracy(ts(tail(actuals_unfil[,-1],test_n-n_step) - err), tail(actuals_unfil[,-1],test_n-n_step))
            rownames(acc_ets_test)<-"ets"
            
            comp_ets<-ts(tail(actuals_unfil[,-1],test_n-n_step) -err,frequency = freq,end = c(yr,mth))
            
            ####forecast op   
            forecast_ets=ets_fsct$mean
            showNotification("exponential smoothening state space model done",type="message")
            print("ets done")
            p<-p+1
            updateProgressBar(session = session, id = "pb4", value = (p/max_p)*100)
          },#try catch,
          warning=function(w){
            message("data warning! ets")
            print(w)
          },
          error= function(e){
            message("no ets model fits the data")
            print(e)
          }
        )#closse try catch
      }#close if
      
      #########auto arima####333
      if("ARIMA" %in% modelname)
      {
        tryCatch(
          {
            # ########training and predicting for test period
            # arima_kq<-auto.arima(ts_kq_df,approximation = FALSE,stepwise = FALSE)
            # arima_fcst_kq <- forecast(arima_kq, h = test_n)
            ######training on entire data and predicting for selected period
            arima_model<-auto.arima(actuals_unfil_ts,approximation = FALSE,stepwise = FALSE)
            arima_fcst <- forecast(arima_model, h = n)
            print("data for checking manually fit of curve")
            
            # comp_ar<-arima_fcst_kq$mean
            
            # ###cross validation accuracy predicting n step ahead
            f<-function(x,h){forecast(auto.arima(x,approximation = FALSE,stepwise = FALSE),h=h)}
            error_ar<-tsCV(actuals_unfil_ts,f,h=n_step,initial = initial)
            err<-if(n_step>1){
              tail(error_ar,test_n)[,n_step][1:(length(tail(error_ar,test_n)[,n_step])-n_step)]
            } else {
              tail(error_ar,test_n)[1:(length(tail(error_ar,test_n))-n_step)]
            }
            acc_ar_test<-accuracy(ts(tail(actuals_unfil[,-1],test_n-n_step) - err), tail(actuals_unfil[,-1],test_n-n_step))
            rownames(acc_ar_test)<-"arima"
            comp_ar<-ts(tail(actuals_unfil[,-1],test_n-n_step) -err,frequency = freq,end = c(yr,mth))
            
            ####forecast op 
            forecast_ar=arima_fcst$mean
            showNotification("Auto regressive integrated moving average done",type="message")
            print("arima done")
            p<-p+1
            updateProgressBar(session = session, id = "pb4", value = (p/max_p)*100)
          },#try catch,
          warning=function(w){
            message("data warning! arima")
            print(w)
          },
          error= function(e){
            message("no arima model fits the data")
            print(e)
          }
        )#closse try catch
      }#close if
      ############stl models (seasonal trend decomposition as base for ets/arima model)
      if("STLM" %in% modelname)
      {
        tryCatch(
          {
            # ########training and predicting for test period
            # stl_kq<-stlm(ts_kq_df,robust = TRUE)
            # fcst_stl_kq<-forecast(stl_kq,test_n)
            ######training on entire data and predicting for selected period
            stl<-stlm(actuals_unfil_ts,robust = TRUE)
            fcst_stl<-forecast(stl,n)
            
            
            # ###cross validation accuracy predicting n step ahead
            f<-function(x,h){forecast(stlm(x,robust = TRUE),h=h)}
            error_stl<-tsCV(actuals_unfil_ts,f,h=n_step,initial = initial)
            err<-if(n_step>1){
              tail(error_stl,test_n)[,n_step][1:(length(tail(error_stl,test_n)[,n_step])-n_step)]
            } else {
              tail(error_stl,test_n)[1:(length(tail(error_stl,test_n))-n_step)]
            }
            acc_stlm_test<-accuracy(ts(tail(actuals_unfil[,-1],test_n-n_step) - err), tail(actuals_unfil[,-1],test_n-n_step))
            rownames(acc_stlm_test)<-"stlm"
            comp_stlm<-ts(tail(actuals_unfil[,-1],test_n-n_step) -err,frequency = freq,end = c(yr,mth))
            
            ####forecast op 
            forecast_stl=fcst_stl$mean
            
            showNotification("STLM is done",type="message")
            print("stl done")
            p<-p+1
            updateProgressBar(session = session, id = "pb4", value = (p/max_p)*100)
          },#try catch,
          warning=function(w){
            message("data warning! arima")
            print(w)
          },
          error= function(e){
            message("no stlm model fits the data")
            print(e)
          }
        )
      }#end if statement
      ######### neural network time series
      if("NNETAR" %in% modelname)
      {
        tryCatch(
          {
            # set.seed(9)
            # ########training and predicting for test period
            # fit_kq <- nnetar(ts_kq_df,repeats = 3000)
            # forecast_nnetar_kq<-forecast(fit_kq,h=test_n,bootstrap = TRUE)
            ######training on entire data and predicting for selected period
            fit<- nnetar(actuals_unfil_ts,repeats = 4000,lambda = 0)
            forecast_nnetar<-forecast(fit,h=n,bootstrap = TRUE)
            
            #####cross validation acuuracy measure
            # set.seed(1801)
            f<-function(x,h){forecast(nnetar(x,repeats = 4000,lambda = 0),h=h,bootstrap = TRUE
            )}
            error_nn<-tsCV(actuals_unfil_ts,f,h=n_step,initial = initial)
            err<-if(n_step>1){
              tail(error_nn,test_n)[,n_step][1:(length(tail(error_nn,test_n)[,n_step])-n_step)]
            } else {
              tail(error_nn,test_n)[1:(length(tail(error_nn,test_n))-n_step)]
            }
            acc_nnetar_test<-accuracy(ts(tail(actuals_unfil[,-1],test_n-n_step) - err), tail(actuals_unfil[,-1],test_n-n_step))
            rownames(acc_nnetar_test)<-"nnetar"
            comp_nnetar<-ts(tail(actuals_unfil[,-1],test_n-n_step) -err,frequency = freq,end = c(yr,mth))
            
            ####forecast op  
            forecast_nn<-forecast_nnetar$mean
            showNotification("NNETAR is done",type="message")
            print("nnetar done")
            p<-p+1
            updateProgressBar(session = session, id = "pb4", value = (p/max_p)*100)
          },#try catch,
          warning=function(w){
            message("data warning! nnetar")
            print(w)
          },
          error= function(e){
            message("no nnetar model fits the data")
          }
        )
      }#close if
      #########33tslm###tslm is largely a wrapper for lm() except that it allows variables "trend" and "season" which are created on the fly from the time series 
      #characteristics of the data.
      if("TSLM" %in% modelname)
      {
        tryCatch(
          {
            # ########training and predicting for test period
            # fit_kq<-tslm(ts_kq_df ~ trend + season)
            # forecast_tsl_kq<-forecast(fit_kq,h=test_n)
            ######training on entire data and predicting for selected period
            fit<-tslm(actuals_unfil_ts ~ trend + season)
            forecast_tsl<-forecast(fit,h=n)
            
            #################cross validation
            f<-function(x,h){forecast(tslm(x ~ trend + season),h=h)}
            error_tslm<-tsCV(actuals_unfil_ts,f,h=n_step,initial = initial)
            err<-if(n_step>1){
              tail(error_tslm,test_n)[,n_step][1:(length(tail(error_tslm,test_n)[,n_step])-n_step)]
            } else {
              tail(error_tslm,test_n)[1:(length(tail(error_tslm,test_n))-n_step)]
            }
            
            acc_tslm_test<-accuracy(ts(tail(actuals_unfil[,-1],test_n-n_step) - err), tail(actuals_unfil[,-1],test_n-n_step))
            rownames(acc_tslm_test)<-"tslm"
            comp_tslm<-ts(tail(actuals_unfil[,-1],test_n-n_step) -err,frequency = freq,end = c(yr,mth))
            comp_tslm<-tail(comp_tslm,test_n)
            ####forecast op 
            forecast_tslm<-forecast_tsl$mean
            showNotification("TSLM is done",type="message")
            print("tslm done")
            p<-p+1
            updateProgressBar(session = session, id = "pb4", value = (p/max_p)*100)
          },#try catch,
          warning=function(w){
            message("data warning! tslm")
            print(w)
          },
          error= function(e){
            message("no tslm model fits the data")
          }
        )
      }##if close
      ########prophet
      if("PROPHET" %in% modelname)
      {
        tryCatch(
          {
            # ts_kq_prpt<-ts_kq
            # colnames(ts_kq_prpt)<-c("ds","y")
            
            actuals_unfil_prpt<-actuals_unfil
            colnames(actuals_unfil_prpt)<-c("ds","y")
            
            # #######################training and predicting for test period
            # pht_fcst_kq<-prophet(ts_kq_prpt)
            # future_kq = make_future_dataframe(pht_fcst_kq,periods=test_n,freq='m')
            # forecast_kq = predict(pht_fcst_kq, future_kq)
            
            ######training on entire data and predicting for selected period
            pht_fcst<-prophet(actuals_unfil_prpt)
            future = make_future_dataframe(pht_fcst,periods=n,freq='m',include_history = FALSE)
            forecast = predict(pht_fcst, future)
            
            cv<-cross_validation(pht_fcst,horizon = n_step*(365+365+365+366)/48, units = "days", period = n_step*(365+365+365+366)/48, initial = initial*(365+365+365+366)/48)
            acc_prpt_test<-accuracy(cv$yhat,cv$y)
            rownames(acc_prpt_test)<-"prophet"
            
            
            ##obtaining only the forecast values for forecast output
            fcst<-tail(forecast[,c("ds","yhat")],n)
            fct<-fcst
            rownames(fct)<-fct$ds
            fct$ds<-NULL
            fcst_prpt<-ts(fct,frequency = freq,start=c(lubridate::year(min(fcst$ds)),lubridate::month(min(fcst$ds)))) 
            
            # ###### prpht prediction for test period
            # fcst<-tail(forecast_kq[,c("ds","yhat")],test_n)
            # fct<-fcst
            # rownames(fct)<-fct$ds
            # fct$ds<-NULL
            # comp_prpt<-ts(fct,frequency = freq,start=c(lubridate::year(min(fcst$ds)),lubridate::month(min(fcst$ds)))) 
            # 
            fcst<-tail(cv[,c("ds","yhat")],test_n)
            fct<-fcst
            rownames(fct)<-fct$ds
            fct$ds<-NULL
            comp_prpt<-ts(fct,frequency = freq,start=c(lubridate::year(min(fcst$ds)),lubridate::month(min(fcst$ds))))
            
            p<-p+1
            updateProgressBar(session = session, id = "pb4", value = (p/max_p)*100)
            showNotification("PROPHET is done",type="message")
            
          },#try catch,
          warning=function(w){
            message("data warning! prophet")
            print(w)
          },
          error= function(e){
            message("no prophet model fits the data")
          }
        )#TRY CATCH 
      }#IF CLOSING
      
      if(exists("forecast_ar")==F){forecast_ar<-0}
      if(exists("forecast_stl")==F){forecast_stl<-0}
      if(exists("forecast_nn")==F){forecast_nn<-0}
      if(exists("forecast_tslm")==F){forecast_tslm <-0}
      if(exists("fcst_prpt")==F){fcst_prpt<-0}
      if(exists("forecast_ets")==F){forecast_ets<-0}
      if(exists("forecast_simpex")==F){forecast_simpex<-0}
      if(exists("forecast_spline")==F){forecast_spline<-0}
      if(exists("forecast_rwf")==F){forecast_rwf<-0}
      
      actuals<-actuals_unfil_ts
      combined<-cbind(actuals,forecast_ar,forecast_stl,forecast_nn,forecast_tslm,fcst_prpt,forecast_ets,forecast_simpex,forecast_spline,forecast_rwf)
      colnames(combined)<-c("actuals","ARIMA","STLM","NNETAR","TSLM","PROPHET","ETS","SES","SPLINE","RWF")
      a<-which(colSums(combined,na.rm=T)>0)
      combined<-combined[ ,which((colnames(combined) %in%  names(a))==TRUE)]
      cmb<-data.frame(combined)
      rownames(cmb)<-as.yearmon(time(combined))
      # cmb <- cmb[-1]
      cmb<-tail(cmb,n)
      print("table forecast op")
      print(tail(cmb))
      
      
      
      output$forecast_vals<-renderRHandsontable(
        rhandsontable(cmb, rowHeaderWidth = 100) %>% hot_cols(colWidths = 90)
      )
      if(exists("comp_ar")==F){comp_ar<-0}
      if(exists("comp_stlm")==F){comp_stlm<-0}
      if(exists("comp_nnetar")==F){comp_nnetar<-0}
      if(exists("comp_tslm")==F){comp_tslm <-0}
      if(exists("comp_prpt")==F){comp_prpt<-0}
      if(exists("comp_ets")==F){comp_ets<-0}
      if(exists("comp_ses")==F){comp_ses<-0}
      if(exists("comp_rwf")==F){comp_rwf<-0}
      if(exists("comp_spline")==F){comp_spline<-0}
      
      combined_1<-cbind(actuals,comp_ar,comp_stlm,comp_nnetar,comp_tslm,comp_prpt,comp_ets,comp_ses,comp_spline,comp_rwf)
      colnames(combined_1)<-c("actuals","ARIMA","STLM","NNETAR","TSLM","PROPHET","ETS","SES","SPLINE","RWF")
      a<-which(colSums(combined_1,na.rm=T)>0)
      combined_1<-combined_1[ ,which((colnames(combined_1) %in%  names(a))==TRUE)]
      cmb_1<-data.frame(combined_1)
      rownames(cmb_1)<-as.yearmon(time(combined_1))
      # cmb_1 <- cmb_1[-1]
      cmb_1<-tail(cmb_1,test_n-n_step)
      
      print("comparison op")
      print(tail(cmb_1,1))
      
      closest <- function(v) names(which.min(abs( v[-1]-v[1] )))
      l<-apply(cmb_1,1,closest)
      
      cmb_1$best<-l
      output$forecast_test<-renderRHandsontable(
        rhandsontable(cmb_1, rowHeaderWidth = 100) %>%
          hot_cols(colWidths = 90)
      )   
      
      combined_grph<-combined
      for(i in 2:ncol(combined_grph))
      {
        combined_grph[,i]<-coalesce(combined_grph[,i],combined_grph[,1])
      } 
      print("graph op table")
      print(tail(combined_grph,2))
      
      dyop<-dygraph(combined_grph,main="Trend Forecast") %>%
        dySeries("actuals", color = "black")%>% 
        dyRangeSelector() %>%
        dyHighlight(highlightSeriesOpts = list(strokeWidth = 1)) %>%
        dyCSS(textConnection("
                             .dygraph-legend > span { display: all; }
                             .dygraph-legend > span.highlight { display: inline;background-color: #cbfcc7;}
                             "))
      
      output$plot<-renderDygraph({
        dyop
      })
      #     dySeries(c("lower","forecast", "upper"),color = "blue")
      
      
      
      #############train and test accuracy tables ###################
      if(exists("acc_ar_test")==F){acc_ar_test<-0}
      if(exists("acc_stlm_test")==F){acc_stlm_test<-0}
      if(exists("acc_nnetar_test")==F){acc_nnetar_test<-0}
      if(exists("acc_tslm_test")==F){acc_tslm_test <-0}
      if(exists("acc_prpt_test")==F){acc_prpt_test <-0}
      if(exists("acc_ets_test")==F){acc_ets_test<-0}
      if(exists("acc_simpex_test")==F){acc_simpex_test<-0}
      if(exists("acc_rwf_test")==F){acc_rwf_test<-0}
      if(exists("acc_spline_test")==F){acc_spline_test<-0}
      
      # if(exists("acc_ar_train")==F){acc_ar_train<-0}
      # if(exists("acc_stlm_train")==F){acc_stlm_train<-0}
      # if(exists("acc_nnetar_train")==F){acc_nnetar_train<-0}
      # if(exists("acc_tslm_train")==F){acc_tslm_train <-0}
      # if(exists("acc_prpt_train")==F){acc_prpt_train <-0}
      # if(exists("acc_ets_train")==F){acc_ets_train<-0}
      # if(exists("acc_simpex_train")==F){acc_simpex_train<-0}
      
      # acc_cmb_train<-rbind(acc_ar_train,acc_stlm_train,acc_nnetar_train,acc_tslm_train,acc_prpt_train,acc_ets_train,acc_simpex_train)
      # b1<-which(rowSums(acc_cmb_train,na.rm=T)!=0)
      # acc_cmb_train<-acc_cmb_train[b1,]
      # print(acc_cmb_train)
      
      # print(b)
      
      acc_cmb_test<-rbind(acc_ar_test,acc_stlm_test,acc_nnetar_test,acc_tslm_test,acc_prpt_test,acc_ets_test,acc_simpex_test,acc_rwf_test,acc_spline_test)
      b<-which(rowSums(acc_cmb_test,na.rm=T)!=0)
      acc_cmb_test<-data.frame(acc_cmb_test[b,])
      # print(acc_cmb_test)
      
      
      
      # output$models_acc_train<-renderRHandsontable(
      #   rhandsontable(data.frame(acc_cmb_train)) %>% hot_cols(colWidths = 90)
      # )
      # print(acc_ar_cv)
      # print(acc_ar_cv)
      # 
      # print(acc_nn_cv)
      
      # best_model_fil<- acc_cmb_test %>% filter(MASE < 1)
      best_model<-acc_cmb_test %>% mutate(sum_err=RMSE+MAPE) 
      
      rownames(best_model)<-rownames(acc_cmb_test)
      best_model<-best_model[order(best_model$sum_err),]
      print(best_model)
      # print(rownames(best_model)[1])
      
      # bm<-rownames(best_model)[1]
      # output$best_model<-renderText({paste("Best model based on MAPE and RMSE is",bm)})
      render="function(instance, td, row, col, prop, value, cellProperties) {
      Handsontable.renderers.NumericRenderer.apply(this, arguments);
      
      if (instance.params) {
      hcols = instance.params.col_highlight;
      hcols = hcols instanceof Array ? hcols : [hcols];
      }
      
      if (instance.params && hcols.includes(col)) {
      td.style.background = '#e6e7e8';
      }
                             }"
      
      best_model$sum_err<-NULL
      output$models_acc<-renderRHandsontable(
        rhandsontable(data.frame(best_model)) %>% hot_cols(colWidths = 90) %>% hot_heatmap(c(2,5),color_scale = c("#aaf0b1", "#f2faf3"))
      )
      # saved_data$data[i]<-
    } #for loop
    
})#apply observe event 
  
  ##################33download button                 
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('data', '.xlsx', sep='')
      #paste0(paste0("test", Sys.Date()), ".png")
    },
    content = function(file) {
      
      table_acc<-
        tryCatch({
          data.frame(hot_to_r(input$models_acc))
        }, error=function(e) {
          data.frame(0,0)
        }
        )
      
      table_fcst<-
        tryCatch({
          data.frame(hot_to_r(input$forecast_vals))
        }, error=function(e) {
          data.frame(0,0)
        }
        )
      
      table_comp<-
        tryCatch({
          data.frame(hot_to_r(input$forecast_test))
        }, error=function(e) {
          data.frame(0,0)
        }
        )
      # filters<-paste("Process:",input$process,",Indication:",input$indication,
      #              ",Country:",input$country,",Segment:",
      #              input$segment,",Product",input$product,",LOT",input$lot)
      
      
      list_of_datasets <- list("Comparison" = table_comp, "Forecast" = table_fcst,"error"=table_acc)
      openxlsx::write.xlsx(list_of_datasets, file = file)
      
      while (!is.null(dev.list()))  dev.off()
    }) 
  }###server closing 
shinyApp(ui,server)
#***********unused models******************###

# tsclean(x)
# tsoutliers(x,
# Cubic Spline Forecast
###dshw double ets 
##hybridforecast
# ##########pr with kalams filter#######
# fpk<-fittestPolyRKF(ts_kq_df,h=12)
# pred<-data.frame(fpk$pred) 
# ##error metircs
# metric<-data.frame(fmas$rank.val)[1,]
# ##plot
# end<-end(ts_kq_df)+c(0,1)
# ts_obj<-ts(pred$mean,start=end,frequency = 12)
# ts.plot(ts_kq_df, ts_obj, gpars = list(col = c("black", "red")))
# ########3holt winters
# hw_kq<-HoltWinters(ts_kq_df)
# hw_fcst_kq<-forecast(hw_kq, h=12,level = c(90))
# 
# plot(hw_fcst_kq)
# #######ets

# #############bagged ets
# bget_kq<-baggedETS(ts_kq_df,bld.mbb.bootstrap(ts_kq_df,5))
# bget_kq_fcst<-forecast(bget_kq, h=12,level = c(90))
# acc<-data.frame(accuracy(bget_kq))
# autoplot(bget_kq_fcst)
# ##########tbats#########
# tb<-ts(ts_kq[,-1])
# tbat_kq<-tbats(ts_kq_df)
# tbatfcst<-forecast(tbat_kq,h=12)
# autoplot(tbatfcst)
#############thetaf model
# theta_kq<-thetaf(ts_kq_df)
# theta_kq_fcst<-forecast(theta_kq,h=12)
# autoplot(theta_kq_fcst)
# ##########empirical mode decomposition#######
# femd<-fittestEMD(ts_kq_df,h=12,rank.by = "errors")
# pred<-data.frame(femd$pred)
# ##error metircs
# metric<-data.frame(femd$rank.val)[1,]
# metric<-metric[,7:ncol(metric)-1]
# ##plot
# end<-end(ts_kq_df)+c(0,1)
# ts_obj<-ts(pred$mean,start=end,frequency = 12)
# ts.plot(ts_kq_df, ts_obj, gpars = list(col = c("black", "red")))
#######arima/ets prediction with wavelet transform as base############
# tryCatch(
#   {
#     fw<-fittestWavelet(ts_kq_df,h=12)
#     pred<-data.frame(fw$pred)
#     ##error metircs
#     metric<-data.frame(fw$rank.val)[1,]
#     ##plot
#     end<-end(ts_kq_df)+c(0,1)
#     ts_obj<-ts(pred$mean,start=end,frequency = 12)
#     ts.plot(ts_kq_df, ts_obj, gpars = list(col = c("black", "red")))
#   },#try catch,
#   warning=function(w){
#     message("data warning! wavelet")
#     print(w)
#   },
#   error= function(e){
#     message("no wavelet model fits the data")
#   }
# )
#############moving avergae smoothening as base for arima/ets/stlf as fitter above it#########
# tryCatch(
#   {
#     fmas<-fittestMAS(ts_kq_df,h=12)
#     pred<-data.frame(fmas$pred)
#     ##error metircs
#     metric<-data.frame(fmas$rank.val)[1,]
#     ##plot
#     end<-end(ts_kq_df)+c(0,1)
#     ts_obj<-ts(pred$mean,start=end,frequency = 12)
#     ts.plot(ts_kq_df, ts_obj, gpars = list(col = c("black", "red")))
#   },#try catch,
#   warning=function(w){
#     message("data warning! mas")
#     print(w)
#   },
#   error= function(e){
#     message("no mas model fits the data")
#   }
# )
#}#for loop
# # ##############polynomial regression ###########
#   tryCatch(
#     {
#       fPolyR <- TSPred::fittestPolyR(ts_kq_df,h=freq,maxorder = 4,minorder = 1)
#       fitted(fPolyR$model)#fitted values for testing
#       poly_op<-data.frame(fPolyR$pred)
#       ##error metrics
#       metric<-data.frame(fPolyR$rank.val)[1,]
#       metric<-metric[,7:ncol(metric)-1]
#       ##plot
#       ts_obj<-ts(poly_op$mean,start=end(ts_kq_df)+c(0,1),frequency = freq)
#       ts.plot(ts_kq_df, ts_obj, gpars = list(col = c("black", "red")))
#       print("polynomial reg done")
#     },#try catch,
#     warning=function(w){
#       message("data warning! polyr")
#       print(w)
#     },
#     error= function(e){
#       message("no polyr model fits the data")
#     }
#   )
# ts_kq_df1 <- ts(
#   data=ts_kq, 
#   start = c(2017,3), 
#   frequency = 12)
# mdl<- baggedModel(ts_kq_df1,frequency=12)
# fct<-forecast(mdl,h=6)
# autoplot(fct)
###########outlier detection and treatment
#####gives output of rows that have outliers
#ts_kq[which(ts_kq$nbrx %in% boxplot(ts_kq$nbrx)$out),]
#index<-which(ts_kq$nbrx %in% boxplot(ts_kq$nbrx)$out) ##positions of outliers index
#####replace them by next point
# ts_kq[index,]

