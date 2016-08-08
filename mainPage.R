

loadingBar <- tags$div(class="progress progress-striped active", tags$div(class="bar", style="width: 100%;"))

# Code for loading message
loadingMsg <- tags$div(class="modal", style="position:fixed; top:40% !important; left:50%;", tabindex="-1", role="dialog", "aria-labelledby"="myModalLabel", "aria-hidden"="true",  
                       tags$div(class="modal-header",tags$h3(id="myModalHeader", "In progress...")),
                       tags$div(class="modal-footer",loadingBar)
) 



ui_main_page <- function() {
  
  
  fluidPage( title = "PD estimation tool", theme = "bootstrap.css", 
             
   fluidRow(style="height:50px",
            column(2,offset = 5, style="position:relative;top:10px;text-align:center",
                   HTML("<font size=2>"),htmlOutput("message_user"),HTML("</font>")
            ),
            column(5, style="float:right;width:300px;position:relative;top:10px; text-align:right",
                   HTML("<font size=2>You are logged as: "),
                   #strong(doLogin()$username),
                   strong(LoginData$Account),HTML("</font>"),
                   tags$button(class="btn action-button btn-small", id="button_logout", type="button", as.character("Log out")), 
                   br()
            ) 
            #  )         
   ),                     
             tags$head(tags$script(src = "js/buttons.js", type="text/javascript")),
             
             # -- navbar          
             HTML('
                 
                  <div class="container">
                  <!-- main panel -->
                  <div class="navbar ">
                  <div class="navbar-inner">
                  <div class="container" style="width: auto;">
                  <span class="brand pull-left">PD estimation tool</span>
                  <ul class="nav" id="main_tabs">
                  <li class="active"><a data-toggle="tab" href="#data_upload_tab">Data upload</a></li>
                  <li><a data-toggle="tab" href="#matrix_tab">Matrix calculation</a></li>
                  <li><a data-toggle="tab" href="#graphs_tab">Graphs</a></li>
                  <li><a data-toggle="tab" href="#data_tab">Data explorer</a></li>
                  </ul>
                  </div>
                  </div>
                  </div>
                  </div>
                  <!-- end of main panel --> '),
             
             
             #verbatimTextOutput("test"),
             
             
             HTML('
                  <div class="row-fluid"><br></div>     <!-- extra space -->
                  
                  <!-- start of tab-content -->
                  <div class= "tab-content"> 
                    
                    <!-- start of Data upload panel -->
                    <div class="tab-pane active" data-value="" id="data_upload_tab">
                  
                  '),  
             
             
             
             #    tabPanel("Data loading", 
             fluidRow(br()),
             fluidRow(
               column(8, offset = 2, 
                      htmlOutput("user.dataset"),
                     # htmlOutput("dataset_check"), 
                      htmlOutput("data_loaded")    
                      #       htmlOutput("dataset_message"), 
                      #       htmlOutput("dataset_loaded") , 
                      #       htmlOutput("dataset_type")
               )
             ),
             fluidRow(
               column(4, offset = 2,
                      wellPanel(style="height:250px;",strong("Select CSV file to upload"),br(),
                                p("Use parameters to import dataset in proper structure."),
                                
                                HTML('<table border =0 width="100%"><tr><td valign=top>'),
                                checkboxInput('header', 'Header', TRUE),
                                HTML('</td><td>'),
                                radioButtons('sep', 'Separator',c(Comma=',',Semicolon=';',Tab='\t'),','),
                                HTML('</td><td valign=top>'),
                                radioButtons('dec', 'Decimal', c(Comma=',', Dot='.'),'.'),
                                HTML('</td></tr></table>'),                    
                                tags$input( id="file", type="file", accept=c('.csv'), style="display:none;"),
                                tags$button(id="file_upload_button", class="btn btn-block btn-large ", as.character("Upload a file"), 
                                            onclick="" ),          
                                br(),
                                div(id="file_progress", class="progress progress-striped shiny-file-input-progress",
                                    div(class="bar", width="100%", style="text-align:left" ))
                      )
               ),
               column(4, offset = 0,
                      wellPanel(style="height:250px;", strong("Select a file from working directory")  ,
                                p(""),
                                HTML("<style type='text/css'>#dir_file {width:100%; } </style>"),
                                selectInput(inputId = "dir_file", label="", choices=c("none", list.files(pattern="*.csv")), selected = "none",
                                            multiple = FALSE, selectize=FALSE),
                                tags$input(id="wd_file_load", type="text", value="0" ,style="display:none;"),
                                
                                tags$button(id="wd_file_load_button", type="button", class="btn btn-block btn-large ",
                                            as.character("Load a file"), onclick="")
                      )
               )
             ),
             
             HTML('</div>
                  <!-- end of Data upload panel -->
                  
                  <!-- start of Matrix calculation panel -->
                  <div class="tab-pane" data-value="" id="matrix_tab">
                  '),
             
             fluidRow(
               column(4,
                      conditionalPanel(condition = " input.dataset_loaded == 1",
                                       wellPanel(  
                                         # Run calculation button (up)
                                         div(style="position:relative;margin:auto;text-align:center;",
                                             tags$button(class="btn action-button btn-primary", id="run_calculations1",type="button", as.character("Calculate matrices") ),
                                             helpText("Allow some time for computation", style="font-size:small")),
                                         hr(),
                                         
                                         # Select portfolio
                                         strong("Select portfolio"), p("(click to add multiple items, backspace to remove)"),
                                         uiOutput("choose_portfolio"),
                                         hr(),
                                         
                                         # Define matrix parameters
                                         strong("Choose settings below:"),  
                                         # OK! state
                                         checkboxInput(inputId = "OK_excl_state", label = "create 'OK!' state", value = TRUE),   
                                         # OK_D state 
                                         checkboxInput(inputId = "OK_D_state", label = "create 'OK_D' state", value = FALSE),
                                         conditionalPanel(condition = "input.OK_D_state == true",
                                                          p("define curing period"),         
                                                          sliderInput(inputId = "default_period_value", label= "",format="# months", value = 6, min=1, max=20, step=1),
                                                          p("define DPD threshold for curing"),
                                                          sliderInput(inputId = "default_threshold", label= "",format="# days", value = 90, min=0, max=180, step=1)                  
                                         ),          
                                         #helpText("Note: OK_D state captures returns from default. Please specify 'curing' period below as well."),   
                                         # Repaid state
                                         checkboxInput(inputId = "repay_state", label = "create 'Repaid' state", value = FALSE),    
                                         # default state as absorbing
                                         checkboxInput(inputId = "absorbing", label = "default as absorbing state", value = TRUE),    
                                         # external default 
                                         checkboxInput(inputId = "external_default", label = "use bank's default date", value = FALSE),
                                         conditionalPanel(condition = "input.external_default == true",
                                                          helpText("Column 'default_date_bank' must be present in dataset")),
                                         # use exposure as weight       
                                         checkboxInput(inputId = "weighted", label = "calculate weighted matrices", value = FALSE),   
                                         conditionalPanel(condition = "input.weighted == true",
                                                          helpText("Column 'weight' must be present in dataset")),
                                         hr(),
                                         
                                         # user defined states
                                         strong("How many states do you want to create ?") ,      
                                         radioButtons(inputId = "user_defined", label="", choices = c("standard states (0-4, 5-30, 31-60, 61-90, 90+)" = 1, "define your own states" =2), selected ="standard states (0-4, 5-30, 31-60, 61-90, 90+)"),
                                         conditionalPanel( condition = "input.user_defined == 2",
                                                           textInput(inputId = "user_defined_value", label= "", value = ""),
                                                           helpText("Input upper bounds for each state except default separated by commas (e.g. 15,30,60,90,120,150 )")),
                                         hr(),
                                         
                                         # time span for calculating matrices (0 = max ) 
                                         strong("Choose history length for calculating migration matrices"),
                                         sliderInput(inputId = "history_length", label= "", format="# months",value = 0, min=0, max=60, step=1),
                                         helpText("Leave unchanged to take full dataset"),
                                         hr(),
                                         
                                         # how long in OK! state before going to OK
                                         strong("Apply 'curing' period from OK! state ?") ,
                                         radioButtons(inputId = "OK_excl_period", label="", choices = c("no curing period" = 1, "define your own curing period" =2), selected ="no curing period"),
                                         conditionalPanel(condition = "input.OK_excl_period == 2",
                                                          sliderInput(inputId = "OK_excl_period_value", label= "",format="# months", value = 4, min=1, max=20, step=1),
                                                          helpText("This option requires state OK!. Make sure 'create OK! state' is marked.")),
                                         hr(),
                                         
                                         # time horizon for PD matrix
                                         strong("Choose time horizon for PD matrix"),
                                         sliderInput(inputId = "horizon", label= "",format="# months", value = 36, min=1, max=60, step=1),
                                         hr(),           
                                         
                                         # default horizon for frequency PD                 
                                         strong("Choose default horizon for frequency PD"),
                                         sliderInput(inputId = "default_horizon", label= "",format="# months", value = 12, min=1, max=36, step=1),
                                         hr(),
                                         
                                         div(style="position:relative;margin:auto;text-align:center;",
                                             tags$button(class="btn action-button btn-primary", id="run_calculations",type="button", as.character("Calculate matrices") ),
                                             helpText("Allow some time for computation", style="font-size:small")),
                                         conditionalPanel("(input.run_calculations > 0 || input.run_calculations1 > 0)  & $('html').hasClass('shiny-busy')",loadingMsg)
                                         #actionButton("run_calculations", "Calculate matrices")
                                       )            
                      )                 
               ),
               column(8,
                      conditionalPanel(condition = "input.show_overview != 0  ",
                                       conditionalPanel("input.run_calculations != 0 || input.run_calculations1 != 0", 
                                                        fluidRow(                
                                                          column(6, HTML('<legend class="pagination-centered">Migration matrix - absolute</legend>'),
                                                                 div(align="center", htmlOutput("absolute.mig.matrix" ))
                                                          ), 
                                                          column(6, HTML('<legend class="pagination-centered">Migration matrix - relative</legend>'),
                                                                 div(align="center", htmlOutput("relative.mig.matrix" ))
                                                          )
                                                        ),
                                                        conditionalPanel("input.weighted==true",
                                                                         fluidRow(
                                                                           column(6, HTML('<legend class="pagination-centered">Migration matrix - absolute (wieghted)</legend>'),
                                                                                  div(align="center", htmlOutput("absolute.weighted.mig.matrix" ))
                                                                           ),
                                                                           column(6, HTML('<legend class="pagination-centered">Migration matrix - relative (weighted)</legend>'),
                                                                                  div(align="center", htmlOutput("relative.weighted.mig.matrix" ))
                                                                           )
                                                                         )                
                                                        ),            
                                                        fluidRow(
                                                          column(12, HTML('<legend class="pagination-centered">PD matrix</legend>'),
                                                                 div(align="center", htmlOutput("PD.matrix" )) 
                                                          )
                                                          
                                                        ),
                                                        conditionalPanel("input.weighted==true",
                                                                         fluidRow(
                                                                           column(12, HTML('<legend class="pagination-centered">PD matrix (weighted)</legend>'),
                                                                                  div(align="center", htmlOutput("PD.weighted.matrix" )) 
                                                                           )
                                                                         )
                                                        ),
                                                        #             fluidRow(
                                                        #               column(12,HTML('<legend class="pagination-centered">Dataset overview</legend>'),
                                                        #                      div(align="center", tableOutput("dataset.overview" )) 
                                                        #                      )
                                                        #             ), 
                                                        conditionalPanel("(input.run_calculations != 0 || input.run_calculations1 != 0) & input.OK_D_state == true",
                                                                         fluidRow(
                                                                           column(12, HTML('<legend class="pagination-centered">Returns from default - overview</legend>'),
                                                                                  div(align="center", htmlOutput("return.from.default" )) 
                                                                           )
                                                                         )
                                                        )
                                       )
                      )
               )
             ),
             
             HTML('</div>
                  <!-- end of Matrix calculation panel -->
                  
                  <!-- start of Graphs panel -->
                  <div class="tab-pane" data-value="" id="graphs_tab">
                  '),
             conditionalPanel(condition = " input.dataset_loaded == 1",
                              fluidRow(
                                column(6,
                                       fluidRow(
                                         HTML('<div class="well span8 offset2 pagination-centered">
                                              <label class="btn btn-primary">
                                              <span>Show PD graph</span>
                                              <input type="checkbox" name="plot_PD" id="plot_PD" class="hide" data-toggle="button" />
                                              </label>
                                              <span class="help-block" style="font-size:small">Allow some time for plotting</span>
                                              </div>
                                              '),
                                         conditionalPanel("input.plot_PD == true",style = "min-height: 550px;" ,
                                                          plotOutput("plot.PD")
                                                          #showOutput("PD_curve", "highcharts")
                                         )
                                         ),
                                       fluidRow(
                                         HTML('<div class="well span8 offset2 pagination-centered">
                                              <label class="btn btn-primary" style = "display:inline-block">
                                              <span>Show cumulative PD graph</span>
                                              <input type="checkbox"  id="plot_cum_PD" class="hide" data-toggle="button" />
                                              </label>
                                              <label id="cum_PD_by_cohort_dis" class="btn disabled" style = "display:inline-block">
                                              <span>by cohort</span>
                                              <input type="checkbox" id="plot_cum_PD_by_cohort" class="hide" data-toggle="button" />
                                              </label>
                                              <span class="help-block" style="font-size:small">Allow some time for plotting.<br> See "by cohort" view once graph is plotted</span>
                                              </div>
                                              '),
                                         conditionalPanel("input.plot_cum_PD == true",  style = "min-height: 550px;" ,
                                                          plotOutput("plot.cPD")
                                         )        
                                         )
                                ),
                                column(6,
                                       fluidRow(
                                         HTML('<div class="well span8 offset2 pagination-centered" >
                                              <label class="btn btn-primary">
                                              <span>Show frequency PD graph</span>
                                              <input type="checkbox" name="plot_freq_PD" id="plot_freq_PD" class="hide" data-toggle="button" />
                                              </label>
                                              <span class="help-block" style="font-size:small">Allow some time for plotting</span>
                                              </div>
                                              '),
                                         conditionalPanel("input.plot_freq_PD == true", style = "min-height: 550px;" ,
                                                          plotOutput("plot.fPD")
                                         )  
                                         ),
                                       fluidRow(
                                         HTML('<div class="well span8 offset2 pagination-centered">
                                              <label class="btn btn-primary" style = "display:inline-block">
                                              <span>Show incremental PD graph</span>
                                              <input type="checkbox" id="plot_inc_PD" class="hide" data-toggle="button" />
                                              </label>
                                              <label id="inc_PD_by_cohort_dis" class="btn disabled" style = "display:inline-block">
                                              <span>by cohort</span>
                                              <input type="checkbox"  id="plot_inc_PD_by_cohort" class="hide" data-toggle="button" />
                                              </label>
                                              <label id="inc_PD_by_quarter_dis" class="btn disabled" style = "display:inline-block">
                                              <span>by quarter</span>
                                              <input type="checkbox"  id="plot_inc_PD_by_quarter" class="hide" data-toggle="button" />
                                              </label>
                                              <span class="help-block" style="font-size:small">Allow some time for plotting.<br>
                                              See "by cohort" or "by quarter" view once graph is plotted</span>
                                              </div>
                                              '),
                                         conditionalPanel("input.plot_inc_PD == true", style = "min-height: 550px;" ,
                                                          plotOutput("plot.iPD")
                                         )     
                                         )
                                )
                              )
                                ),
             
             
             HTML('</div>
                  <!-- end of Graphs panel -->
                  
                  <!-- start of Data explorer panel -->
                  <div class="tab-pane" data-value="" id="data_tab">
                  '),
             fluidRow(
               column(10, offset = 1,
                      dataTableOutput('DataTable')  
               )
             ),
             
             HTML('</div>
                  <!-- end of Data explorer panel -->
                  
                  </div> <!-- end of tab-content -- >
                  ')
             
             )
}


