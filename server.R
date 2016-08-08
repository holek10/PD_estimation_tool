library(shiny)
library(rCharts)
options(shiny.maxRequestSize=30*1024^2)  # to handle 30 MB of upload

source("functions_PD.r", local=TRUE)


#--- Login page ----
ui_login_page <- function() {
  fluidPage(title= "PD estimation tool", theme= "bootstrap.css",            

            # Login window form            
            HTML("<form class='form-horizontal well' style='width:400px;height:300px; position:fixed;margin-left:-200px;margin-top:-150px;top:50%;left:50%;'>
                 <legend >PD estimation tool</legend>   
                 <div class='control-group'>
                 <label class='control-label' for='inputUsername'>Username</label>
                 <div class='controls'>
                 <input type='text' id='inputUsername' placeholder='Username' >
                 </div>
                 </div>
                 <div class='control-group'>
                 <label class='control-label' for='inputPassword'>Password</label>
                 <div class='controls'>
                 <input type='text' id='inputPassword' placeholder='Password' style='-webkit-text-security: disc;'>
                 </div>
                 </div>  
                 <div class='control-group'>
                 <div class='controls'> 
                 <button type='button' class='btn action-button btn-primary' id='button_login'>Log in</button>
                 </div> 
                 </div>
                 <hr>
                 <div  style='font-size:small; text-align:left;'>
                  Type <b> demo </b> as username and password
                 </div>
                 </form>
                 ")
  ) 
}         


shinyServer(function(input, output) {

# login details  
LoginData <- reactiveValues()
LoginData$LoggedIn <- FALSE
 
source("mainPage.R", local=T)
 
#-- Login ----  
doLogin <- reactive({
   
  input$button_login      # trigger
  
  isolate({
    if (!is.null(input$button_login)) {
      if (input$button_login > 0) {     
        user_input <- input$inputUsername
        user_password <- input$inputPassword
        if (user_input == "demo" & user_password == "demo") {    
            LoginData$LoggedIn <- TRUE
            LoginData$Account <- input$inputUsername        
        }
      }
    }
  })  
})


# -- Main page (launch app)----

output$mainPage <- renderUI({
  doLogin()
  #if (doLogin()$login_go == 1) {
  if (LoginData$LoggedIn) {      
    doLogout()
    ui_main_page()  
  } else { 
    ui_login_page()
  }
})


# -- Logout ----  
doLogout <- reactive({
  
  input$button_logout
  isolate({
    if (!is.null(input$button_logout)) {
      if (input$button_logout > 0) {  
        LoginData$LoggedIn <- FALSE
      }
    }
  })
})

# define all parameters that user can change
  
  user_dataset_load <- reactive( {
    
    userData <-NULL
#     if (input$dataset_load_choice == "1" & !is.null(input$file)) {                     # User has not uploaded a file yet
#       userData <- read.csv(input$file$datapath, as.is=T) 
#       #return(NULL)
#      }
    if (!is.null(input$file)) {                     # User has not uploaded a file yet
      userData <- read.csv(input$file$datapath, header=input$header, sep=input$sep,  dec=input$dec ) 
      #return(NULL)
    }
    #  } else {
    if( input$wd_file_load != 0) {
      
      input$wd_file_load
      isolate(
        if (input$dir_file != "none") {
          dataset_path <- isolate(input$dir_file)
          userData <- read.csv(dataset_path, as.is=T)} else {userData<-NULL}
      )
    }
    #userData <- read.csv2(input$file$datapath, as.is=T)      
    #read.csv2("C:\\Projects\\Testing Area\\Shiny - PD module\\Book3.csv")
    #userData <- read.csv2("C:\\Projects\\Testing Area\\Shiny - PD module\\test_dataset_VBCZK.csv", as.is=T)
    
#     if(input$dataset_load_choice == "2" & input$table != "" & input$dsn !="") {
#       library(RODBC)
#       #if( input$dsn =="dsn...") return(NULL)
#       #channel <- odbcConnect(input$dsn)
#       #if( input$table == "" | input$dsn =="" ) return(NULL)
#       #userData <- sqlQuery(channel, paste("select top 10 * from ",input$table,sep=""))
#       userData <- sqlQuery(odbcConnect(input$dsn), paste("select * from ",input$table,sep=""))
#       }
    
#     if(input$dataset_load_choice == "3" ) {
#       input$wd_file_load
#       isolate(
#       if (input$dir_file != "none") {
#       dataset_path <- isolate(input$dir_file)
#       userData <- read.csv(dataset_path, as.is=T)} else {userData<-NULL}
#       )
#     }
    
    return(userData)
  })
  
  user_dataset <- reactive( {        # return NULL if wrong dataset was uploaded
    
    userData <- NULL 
    columns <- c("id", "record_date", "DPD")
    if (FALSE %in% (columns %in% colnames( user_dataset_load() ) ) ) {
      userData <- NULL
    } else { 
      userData <- user_dataset_load()
    }
    return(userData)
  })
   
#  output$data_loaded <- reactive( {
    
#    if(is.null(user_dataset())) return(FALSE)
#    else return(TRUE)
    
#  })
  
   output$data_loaded <- renderUI( {
    
    if(is.null(user_dataset())) {  
      #paste("false")
      HTML("<input id='dataset_loaded' value='0'  type='text' style='display:none'> ") 
    } else { 
      #paste("true")
      HTML("<input id='dataset_loaded' value='1'  type='text' style='display:none'> ")
    }
    
  })
  
  output$user.dataset <- renderPrint( {
    
    columns <- c("id", "record_date", "DPD")
    # colnames(user_dataset() )
    if (!is.null(user_dataset_load())) 
    {
      if (FALSE %in% (columns %in% colnames(user_dataset_load()) ) )  
      {    
        error <- tags$div(class="alert alert-error",  
                          HTML("<button type='button' class='close' data-dismiss='alert'>&times;</button>   "),
                          p(strong('Oh no!'), "Non-compliant dataset. 
                            Please make sure data has required pre-defined structure and try again."))                    
        cat(as.character(error))   
      } else {
        success <- tags$div(class="alert alert-success",  
                            HTML("<button type='button' class='close' data-dismiss='alert'>&times;</button>   "),
                            p(strong("Well done!"), "You have successfully loaded the dataset. 
                              Go to 'Matrix calculation' to compute matrices and see graphical representation in 'Graphs'."))   
        cat(as.character(success))            
      }
    } else {
      info <- tags$div(class="alert alert-info",  
                       HTML("<button type='button' class='close' data-dismiss='alert'>&times;</button>   "),
                       p(strong("Let's start!"), "Upload a dataset using one of the methods below.
                         Once data is uploaded go to 'Matrix calculation' tab to adjust parameters and compute matrices. 
                         See 'Graphs' for visual representation and 'Data explorer' for source data overview."))   
      cat(as.character(info))   
    }  
    
  }) # end of filetable 
  
  
  # coloured tables
  tableColor <- function(data, bold.last = F, bold.first.col = F, font.col ="white", header.col=EYcolors[3], row.col=EYcolors[6], font="sans-serif", ...) {
    if (is.null(data))
      return("")
    add.to.row= NULL
    if( nrow(data) > 1 ) {
      temp = as.list(c(seq(1,nrow(data)-1,by=2), nrow(data)-1)) 
      add.to.row=list(temp, c(rep("XXX", length(temp)-1), "LAST"))
      if( !bold.last ) {
        temp = as.list(c(seq(1,nrow(data)-1,by=2)))             # last row is not bold
        add.to.row=list(temp, c(rep("XXX", length(temp))))
      }      
    } 
    temp = renderTable(data, add.to.row=add.to.row, ...)
    #temp = renderTable(data, add.to.row=add.to.row)
    temp = temp()  
    temp = gsub(" align=\"right\"","", temp)    # remove aligment which doesn't work with HTML 5    
    if( bold.last )   # condition for making the last row bold
      temp = gsub(" LAST<TR>", paste("<TR style=\"font-weight:bold\">")  , temp)  
      temp = gsub("LAST", "", temp)
    if(!is.na(font) && !is.null(font) )  
      temp = gsub("<TD", paste("<TD style=\"font-family:",font,"; text-align:center\"", sep=""), temp)
    if( bold.first.col )
       temp = gsub("<TR> <TD style=\"", paste("<TR> <TD bgcolor=", header.col, "style=\"color:", font.col,"; font-weight:bold; "), temp )       
    if(!is.na(row.col) && !is.null(row.col))
      temp = gsub("XXX<TR", paste("<TR bgcolor=", row.col, ""), temp)
      temp = gsub("XXX", "", temp)
      temp = gsub("LAST", "", temp)
    if(!is.na(header.col) && !is.null(header.col)) 
      temp = gsub("<TR>\\s*<TH>\\s*</TH>",paste("<TR style=\"color:", font.col, "\" bgcolor=", header.col, "><TH></TH>"), temp)
      temp = gsub("<TH>", paste("<TH style=\"text-align:center\"> "), temp)

    return(temp)
  }
  
  

  
  
  ###########################
  #
  #
  output$test  <-  renderPrint( {
    
    #paste(input$file, input$wd_file_load)
    paste(input$header,input$sth, input$foo, input$plot_PD)
 
   })
    
  
  output$choose_portfolio <- renderUI( {
    
  #  if(!is.null(user_dataset)) {
      if(TRUE %in% (c("portfolio") %in% colnames(user_dataset())) )
      {
      portfolios <- unique(user_dataset()$portfolio)
      selectInput(inputId = "choose_portfolio",label = "", 
                  choices = portfolios, selected = "" , multiple=TRUE)
      } else {
        tags$input(type="text", placeholder="no portfolio has been found...", disabled="disabled")
      } 
  #  } else { return(NULL)}
    
    
  })
   
  final_dataset <- reactive({
    
    if (!is.null(user_dataset())) {
      if(TRUE %in% (c("portfolio") %in% colnames(user_dataset()))) {
      dta <- user_dataset()[user_dataset()$portfolio %in% input$choose_portfolio,]
      } else {
      dta <- user_dataset()
      } 
    } else { dta <- NULL}
    
    return(dta)
  })
  
  #
  ###############################
    user_defined <- reactive( {           
       if (as.numeric(input$user_defined) == 1) {
         choice <- c(4, 30, 60, 90)
       } else {
         choice <- sort(as.numeric(unlist(regmatches(input$user_defined_value, gregexpr("[[:digit:]]+", input$user_defined_value)))))  # retreive numbers only from user input
       }       
       return(choice)   
    })
    
    OK_excl_period <- reactive( {      
      if (as.numeric(input$OK_excl_period) == 1) {
          choice <- NULL
        } else {
          choice <- as.numeric(input$OK_excl_period_value)
        }
      return(choice)
    })
     
    default_period <- reactive( { 
      if (input$OK_D_state == FALSE) { 
        choice <- NULL
      } else {
        choice <- as.numeric(input$default_period_value)
      }
      return(choice)
    })   
      
    default_threshold <- reactive( { 
      if (input$OK_D_state == FALSE)  { 
        choice <- NULL
      } else {
        choice <- as.numeric(input$default_threshold)
      }
      return(choice)
    }) 
    

    state_names <- reactive( {
      
      if( input$OK_excl_state ) { ok_ex = 1 } else { ok_ex = 0}  
      if( input$OK_D_state ) { ok_d = 1 } else { ok_d = 0}
      if( input$repay_state ) { rep = 1 } else { rep = 0}
      
      state_names <- "OK"      
      for (i in 1:(length(user_defined())-1) ) {
        state_names[i+1+ok_ex+ok_d] <- user_defined()[i+1]        
      }
      curr_len <- length(state_names)    
      state_names[curr_len+1] <- "D"      
      if( ok_ex == 1 ) state_names[2] <- "OK!"
      if( ok_ex == 1 & ok_d == 1)  state_names [3] <- "OK_D" 
      if( ok_ex == 0 & ok_d == 1) state_names[2] <- "OK_D"    
      curr_len <- length(state_names)
      if( rep == 1 ) state_names[curr_len+1] <- "Repaid"
       
      return(state_names)
    })
  
    # set states and expand dataset
    expand.dataset <- reactive( {
    
    #if ( input$show_matrices  & !is.null(final_dataset()) )  {
    input$run_calculations | input$run_calculations1
    
    isolate(
      if (!is.null(final_dataset()) ) {
      source("functions_PD.r", local=TRUE)
      data <- setStates(final_dataset(), OK_excl_state = input$OK_excl_state, OK_D_state = input$OK_D_state, repay_state=input$repay_state, 
                        notOK_DPD = 4, absorbing = input$absorbing, user_defined = user_defined(), 
                        external_default=input$external_default, OK_excl_period=OK_excl_period(), default_threshold=default_threshold(), default_period=default_period())      
      data <- expand_dataset(data)
      
      if(input$history_length != 0) {
        
        last_period <- max(data$record_date)
        min_period <- date_add(last_period, -1* input$history_length)
        data <- data[data$record_date > min_period, ]
        
      }
    
      return(data)
    }
    )
  })
      
    # migration matrix output
    matrix.calculation <- reactive( {
     
     # if ( input$show_matrices  & !is.null(final_dataset()) )  
     #   {
     input$run_calculations | input$run_calculations1
     
     isolate({ 
       if( !is.null(final_dataset()))  {
      source("functions_PD.r", local=TRUE)
      mig_matrix <- Mm(expand.dataset(), weighted=input$weighted )
    
      pd_matrix <- PDm(mig_matrix, horizon=input$horizon, weighted=input$weighted )
      
      return(pd_matrix)
       }
      })
      
   #   } else { return(NULL) } )
      
    })
  
    output$absolute.mig.matrix <- reactive( {
      
      
      if ( !is.null(matrix.calculation()))  
        {
  
      matrix <- matrix.calculation()$mig_mat$abs_mat
      isolate(dimnames(matrix) <- list(state_names(), state_names()))
      matrix <- format(matrix,big.mark=" ", nsmall=0)
      tableColor(as.matrix(matrix), bold.first.col = T)
      } else { return(NULL) }
      
    })
  
    output$absolute.weighted.mig.matrix <- reactive( {
      
     
      if ( !is.null(matrix.calculation())  & isolate(input$weighted) )  
      {
        options(scipen=999)
        matrix <- matrix.calculation()$mig_mat$abs_mat_w
        isolate(dimnames(matrix) <- list(state_names(), state_names()))
        matrix <- format(round(matrix,0),big.mark=" ", nsmall=0)
        tableColor(as.matrix(matrix), bold.first.col = T)
      } else { return(NULL) }
      
    })
    
    output$relative.mig.matrix <- reactive( {
      
    
      if ( !is.null(matrix.calculation()))  
           {
                
        matrix <- matrix.calculation()$mig_mat$rel_mat
       isolate( dimnames(matrix) <- list(state_names(), state_names()))
        matrix <- as.data.frame(matrix)
        for (i in 1:dim(matrix)[1]) {
          matrix[,i] <- as.factor(paste(round(matrix[,i]*100,1),"%",sep=""))         
         }
       tableColor(as.matrix(matrix), bold.first.col = T)
       
      } else { return(NULL) }
      
    })
    
    output$relative.weighted.mig.matrix <- reactive( {
      
      
      if (  !is.null(matrix.calculation())   & isolate(input$weighted)) #& !is.null(user_dataset())) 
      {
        
        matrix <- matrix.calculation()$mig_mat$rel_mat_w
      isolate(  dimnames(matrix) <- list(state_names(), state_names()))
        matrix <- as.data.frame(matrix)
        for (i in 1:dim(matrix)[1]) {
          matrix[,i] <- as.factor(paste(round(matrix[,i]*100,1),"%",sep=""))         
        }
        tableColor(as.matrix(matrix), bold.first.col = T)
        
      } else { return(NULL) }
       
    })
  
  
  
  
    output$PD.matrix <- reactive( {
      
      
      if ( !is.null(matrix.calculation()))  
        {
        
        matrix <- matrix.calculation()$pd_mat
        if( isolate(input$repay_state) ) 
          {
      isolate(    dimnames(matrix) <- list(state_names()[-length(state_names())], paste(1:isolate(input$horizon),"M",sep="")) )
        } else {
      isolate(    dimnames(matrix) <- list(state_names(), paste(1:isolate(input$horizon),"M",sep=""))  )
        }
        
        matrix <- as.data.frame(matrix)
        
        for (i in 1:length(colnames(matrix))) {
          matrix[,i] <- as.factor(paste(round(matrix[,i]*100,1),"%",sep=""))         
        }
        
       matrix <- matrix[ ,c(3*(1:(isolate(input$horizon)/3)))]
       tableColor(as.matrix(matrix), bold.first.col = T)
        
      } else { return(NULL) }
      
    })
    
    output$PD.weighted.matrix <- reactive( {
      
      
      if (  !is.null(matrix.calculation())   & isolate(input$weighted)) # & !is.null(user_dataset()) )
      {
        
        matrix <- matrix.calculation()$pd_mat_w
        if( isolate(input$repay_state) ) 
        {
      isolate(    dimnames(matrix) <- list(state_names()[-length(state_names())], paste(1:isolate(input$horizon),"M",sep="")) )
        } else {
      isolate(     dimnames(matrix) <- list(state_names(), paste(1:isolate(input$horizon),"M",sep=""))  )
        }
        
        matrix <- as.data.frame(matrix)
        
        for (i in 1:length(colnames(matrix))) {
          matrix[,i] <- as.factor(paste(round(matrix[,i]*100,1),"%",sep=""))         
        }
        
        matrix <- matrix[ ,c(3*(1:(isolate(input$horizon)/3)))]
        tableColor(as.matrix(matrix), bold.first.col = T)
      } else { return(NULL) }
       
    })
  
  
  
  output$return.from.default <- reactive( {
    
   
    if (   !is.null(matrix.calculation())  & isolate(input$OK_D_state) ) # & !is.null(user_dataset()) )
    {
      source("functions_PD.r", local=TRUE)
      temp <- return_from_default( expand.dataset() )
      tableColor(as.matrix(temp))
    } else { return(NULL) }
    
  })
  
#   output$dataset.overview <- renderTable( {
#     
#     
#     if ( !is.null(matrix.calculation()))  
#     {
#       source("functions_PD.r", local=TRUE)
#       overview <- overview( expand.dataset() )
#       overview[,c(1,2)] <- format(overview[,c(1,2)], nsmall=0)
#       if ("exposure" %in% colnames(overview)) {overview[,c(1,2,3)] <- format(overview[,c(1,2,3)], nsmall=0)}
#       
#       overview
#     } else { return(NULL) }
#     
#   })
  
#   plot_PD <-  {    
#         
#     source("functions_PD.r", local=TRUE)
#     data <- matrix.calculation()
#     if(input$repay_state) st_names <- state_names()[-length(state_names())] else st_names <- state_names()
#     plot(data, state_names = st_names )
#   
#   }
#     
#   plot_fPD <-  { 
#     
#     source("functions_PD.r", local=TRUE)
#     freq_pd <- fPD(expand.dataset(), default_horizon = input$default_horizon )
#     # if(input$repay_state) st_names <- state_names()[-length(state_names())] else st_names <- state_names()
#     plot(freq_pd, state_names = state_names() )
#     
#   }   
  
  output$plot.PD <- renderPlot( {
    
    #if ( input$plot_PD_value == "yes"  & !is.null(final_dataset()) )
    if ( !is.null(final_dataset()) )
    {
       input$plot_PD
       data <- matrix.calculation()
       if( isolate(input$repay_state) ) st_names <- state_names()[-length(state_names())] else st_names <- state_names()
       plot(data, state_names = st_names )
      #plot_PD()
      
    } else { return(plot.new()) }
    
  })
#outputOptions(output, "plot.PD", suspendWhenHidden = FALSE)
  
  
  output$plot.fPD <- renderPlot( {
    
    
    #if ( input$plot_freq_PD_value == "yes"  & !is.null(final_dataset()) )
    if (  !is.null(final_dataset()) )
    {
       input$plot_freq_PD
       freq_pd <- fPD(expand.dataset(), default_horizon = input$default_horizon )
      # if(input$repay_state) st_names <- state_names()[-length(state_names())] else st_names <- state_names()
       plot(freq_pd, state_names = state_names() )
    # plot_fPD()
    } else { return(plot.new()) }
    
  })
#outputOptions(output, "plot.fPD", suspendWhenHidden = FALSE)
  

  calculate.cPD <- reactive( {
    
  #   if ( (input$plot_cum_PD_value == "yes"  | input$plot_inc_PD_value == "yes" )  & !is.null(user_dataset()) )
  #   {
    if ( !is.null(user_dataset()) ) {
      
      cum_pd <- cPD(expand.dataset(), absorbing = input$absorbing )
      #save(cum_pd, file="temp.RData")
      return(cum_pd)
  #  } else { return(NULL) }
    }
  })

  
#   plot_cPD <-  {    
#     source("functions_PD.r", local=TRUE)
#     # cum_pd <- cPD(expand.dataset(), absorbing = input$absorbing )
#     if( input$cum_PD_by_cohort_value == "yes") by_cohort_ind <-T else by_cohort_ind <- F
#     plot(calculate.cPD(), weighted = input$weighted, by_cohort = by_cohort_ind )
#   }  

  output$plot.cPD <- renderPlot( {
    
    #if ( input$plot_cum_PD_value == "yes"  & !is.null(final_dataset()) )
    if (   !is.null(final_dataset()) )
    {
       
       source("functions_PD.r", local=TRUE)
       if( input$plot_cum_PD_by_cohort ) by_cohort_ind <-T else by_cohort_ind <- F
       plot(calculate.cPD(), weighted = input$weighted, by_cohort = by_cohort_ind )
     
    } else { return(plot.new()) }
    
  })
#outputOptions(output, "plot.cPD", suspendWhenHidden = FALSE)
  
#   plot_iPD <-  {    
#     source("functions_PD.r", local=TRUE)    
#     inc_pd <- iPD(calculate.cPD() )
#     if( input$inc_PD_by_cohort_value == "yes") by_cohort_ind <-T else by_cohort_ind <- F
#     if( input$inc_PD_by_quarter_value == "yes") by_quarter_ind <-T else by_quarter_ind <- F
#     plot(inc_pd,  by_cohort = by_cohort_ind, by_kvartal = by_quarter_ind )
#   } 


  output$plot.iPD <- renderPlot( {
    
    #if ( input$plot_inc_PD_value == "yes"  & !is.null(final_dataset()) )
    if (  !is.null(final_dataset()) )
    {
       source("functions_PD.r", local=TRUE)       
       inc_pd <- iPD(calculate.cPD() )
       if( input$plot_inc_PD_by_cohort ) by_cohort_ind <-T else by_cohort_ind <- F
       if( input$plot_inc_PD_by_quarter ) by_quarter_ind <-T else by_quarter_ind <- F
       plot(inc_pd,  by_cohort = by_cohort_ind, by_kvartal = by_quarter_ind )
    # plot_iPD() 
    } else { return(plot.new()) }
    
  })
#outputOptions(output, "plot.iPD", suspendWhenHidden = FALSE)
 

  output$missing_periods <- reactive( {
    
    if ( !is.null(matrix.calculation()))  
    {
      #source("functions_PD.r", local=TRUE)
      missing_periods <- matrix.calculation()$mig_mat$missing_periods
      if ( !is.null(missing_periods)  )
        {
        rownames(missing_periods) <- NULL
        tableColor(as.matrix(format(missing_periods, nsmall=0)))
        } else { return(NULL) } 
 
    } else { return(NULL) }
    
  })
  
  output$DPD_jumps <- reactive( {
    
    if ( !is.null(matrix.calculation()) )  
    {
      DPD_jumps <- matrix.calculation()$mig_mat$DPD_jumps
      if ( !is.null(DPD_jumps) )
      {  
      rownames(DPD_jumps) <- NULL
      tableColor(as.matrix(format(DPD_jumps, nsmall=0)))
      #DPD_jumps
      } else { return(NULL) }
      
    } else { return(NULL) }
    
  })
  
  
  output$DataTable = renderDataTable({
    
    if (!is.null(user_dataset())) {
      user_dataset()
    } else { return(NULL) }
    
  }, options = list(bSortClasses = TRUE , iDisplayLength = 10, 
                    #sScrollX = "100%", 
                    bScrollCollapse =TRUE))
  
  
#   output$dts <- reactive({
#     
#     pdf("fsafsa.pdf")
#     source("functions_PD.r", local=TRUE)
#     data <- matrix.calculation()
#     if(input$repay_state) st_names <- state_names()[-length(state_names())] else st_names <- state_names()
#     plot(data, state_names = st_names )
#     dev.off()
#     
#   })
  
#   output$save_pdf <- reactive( {
#    
#     if( input$save == TRUE) {
#       
#       pdf(file = "")
#       print(plot_PD())
#       dev.off()
#     }
#     }
#   )


#-- Graphs - PD curve chart ----  
  output$PD_curve <- renderChart({

    if (!is.null(matrix.calculation())) {
      
      # get values from calculation
      pd_mat <- matrix.calculation()$pd_mat
      
      # state names
      if( isolate(input$repay_state) ) st_names <- state_names()[-length(state_names())] else st_names <- state_names()
      
      # setting the parameters
      states <- dim(pd_mat)[1]
      horizon <-dim(pd_mat)[2]
      
      # create Highchart 
      h1 <- Highcharts$new()
      h1$chart(height = 400)
      h1$title(text = "Probability of default in time" ,style = list(color = "#646464"))
      h1$subtitle(text = isolate(paste("for credit states, for",input$choose_portfolio,"portfolio", sep=" ")), style = list(color = "#646464"))
      
      # set X and Y axis
      h1$yAxis( min = 0, max = 1, title = list(text = "Probability of default (%)", style= list(color = "#646464")), labels = list(format = "{value:.2f}", formatter =   "#! function() {return 100*this.value + '%' ;} !#"   )) 
      h1$xAxis(tickInterval = 2,  gridLineWidth = 1, min = 0, categories = c(0:(horizon)), labels = list(step = 3 ), title = list(enabled = TRUE, text = "Time horizon (in months)", style= list(color = "#646464")))
      h1$tooltip(formatter = "#!  function() {return '<span style=\"font-size:10px\">Period: '+ this.x +'</span><br>'+ this.series.name +'<br>Value: <b>'+ Highcharts.numberFormat(100*this.y, 2) +'%';}  !#")
      #h1$tooltip(hideDelay = 500, headerFormat = '<span style="font-size: 10px">Period: {point.key}</span><br/>',pointFormat = "Cohort: {series.name}; Value: <b>{point.y:.3f}</b><br/>")
      
      # set other options
      h1$exporting(enabled = TRUE)
      h1$plotOptions(series = list(stickyTracking = FALSE, marker = list(enabled = FALSE)))
      h1$legend(enabled = TRUE, layout = "horizontal", align="center", verticalAlign = "bottom")
      
      # add data points 
      for (i in 1:states){ 
        h1$data( x = pd_mat[i,],  lineWidth = 2,  type="line", name=paste("State ",st_names[i],sep=""), pointStart = 1)
      }

      # bind output to shiny-output (names must match!)
      h1$addParams(dom = 'PD_curve')
      #h1$print("chart9999")
      return(h1)  
    
  } else { return(NULL)}
  
  
})



})
