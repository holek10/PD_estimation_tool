#---------------------------- LIBRARY LOAD ------------------------------------#

#-------------------- load / install library ------------------------------#
#-- load required library 'xlsx' (download & install if required) -- #

initiate_library <- function() {
  
  if (!require("xlsx")) {
    setInternet2()
    #chooseCRANmirror()                          # let user choose location for download
    local({r <- getOption("repos")                                            
           r["CRAN"] <- "http://cran.at.r-project.org"                   # specify server for download (Austria)
           options(repos=r)})    
   install.packages("xlsx", lib =.libPaths(),  dependencies = TRUE)      # install "xlsx" package with 2 other needed ("rJava", "xlsxjars")
   library("xlsx")                  # load the package
  }   else {
    library(xlsx)
   }
   
}



####################################################################################################################################


#------------------------------- FUNCTIONS ------------------------------------#


#------------------------- store a data frame  -------------------------------------#
#-- default values:  write column names, start populating spreadsheet from row 1, column A --#

data2excel <- function(data, file, sheetname=NULL , colnames=T, rownames=F, startrow=1, startcol=1, append=F) {
  
  #-- check if library is loaded (or installed) --#
  if(!"xlsx" %in% search())  initiate_library()              
  
  #-- create a sheet name if not specified in parameters --#
  if (is.null(sheetname)) { sheetname <- gsub("-|_|\\."," ",deparse(substitute(data))) }          # if no sheet is specified, create from object's name
  
  #-- check if Excel file already exists and append it if necessary--#
  if(append) { 
    wb <- loadWorkbook(file)                # retreive a workbook from existing file
    existing_sheet <- getSheets(wb)         # retreive existing sheets 
      if( sheetname %in% names(existing_sheet) )  {                # check if sheet name already exists 
        warning(paste("WARNING: Sheet name already exists. Object was saved as '",sheetname,"_new'"))           
        sheetname <- paste(sheetname,"_new",sep="")          # if sheet exists, add suffix "_new" 
      }    
  } else { 
    ext <- gsub(".*\\.(.*)$", "\\1", basename(file))                # determine Excel file type (.xls or .xlsx) 
    wb <- createWorkbook(type = ext) }                              # create a new workbook 
  
  #-- create a new sheet --#
  mysheet <- createSheet(wb, sheetName=sheetname)               # initiate a sheet in workbook 
  
  #-- make changes to saving object if necessary --#
  if (!is.data.frame(data)) {  data <- as.data.frame(data) }    # convert object to data.frame 
  colnames(data) <- gsub("_"," ",colnames(data) )               # remove underscore from column names
  
  #-- define formatting for content, columns and rows --#
  cs1 <- CellStyle(wb) + Font(wb, isItalic=TRUE) # rowcolumns
  cs2 <- CellStyle(wb) + Font(wb, name="EYInterstate Light", heightInPoints=10) + Alignment(h="ALIGN_CENTER", v="VERTICAL_CENTER", wrapText=F) 
  cs3 <- CellStyle(wb) +  Border() + Alignment(h="ALIGN_CENTER", v="VERTICAL_CENTER", wrapText=F) +  
    Fill(foregroundColor="grey50", pattern="SOLID_FOREGROUND") +
    Font(wb, isBold=TRUE, name="EYInterstate Light", heightInPoints=12, color="grey99")       # header
  
  #-- auxilary variable to ensure proper column formatting in addDataFrame (colStyle="") --#
  col_format <- rep(c(list(cs2)),ncol(data))
  names(col_format) <- paste(seq(ncol(data)))  
  
  #-- initiate rows and columns in sheet --#
  rows <- createRow(mysheet, 1:nrow(data))
  cells <-createCell(rows, colIndex=1:(ncol(data)+2))
  
  #-- add object, with specified parameters and formatting --# 
  addDataFrame(data, mysheet, startRow=startrow, col.names=colnames, row.names=rownames, startColumn=startcol, colnamesStyle=cs3, colStyle=col_format ) 
  
  #-- additional option to manupilate cells --#
  autoSizeColumn(mysheet, seq(ncol(data)))                       # automatically adjust column size
  #createFreezePane(mysheet, 2, 1)                               # freeze row and/or column
  #createCellComment(cells[[4,3]], string="Testing comments... , Hello! ", author="Author", visible=TRUE)             # add comments
  #setColumnWidth(mysheet, 1:200, colWidth=17)                   # specify column width for particular column(s)
  #setZoom(mysheet, 200, 100)                                    # set zoom level 

  #-- save workbook to a file --#
  saveWorkbook(wb, file)
  
}


####################################################################################################################################
####################################################################################################################################
####################################################################################################################################'

####################################################################################################################################
#------------------------- store a picture -------------------------------------#
#--- default parameters: place picture starting from row 1 and column A ---#

picture2excel <- function (picture, file, sheetname=NULL, startrow=1 , startcol=1, append=F  ) {
  
  #-- check if library is loaded (or installed) --#
  if(!"xlsx" %in% search())  initiate_library()              
  
  #-- create a sheet name if not specified in parameters --#
  if (is.null(sheetname)) { sheetname <- gsub("[.][^.]*$|-|_|\\."," ",basename(picture)) }          # if no sheet is specified, create from file's name
  
  #-- check if Excel file already exists and append it if necessary--#
  if(append) { 
    wb <- loadWorkbook(file)                # retreive a workbook from existing file
    existing_sheet <- getSheets(wb)         # retreive existing sheets 
    if( sheetname %in% names(existing_sheet) )  {                # check if sheet name already exists 
      warning(paste("WARNING: Sheet name already exists. Object was saved as '",sheetname,"_new'"))           
      sheetname <- paste(sheetname,"_new",sep="")          # if sheet exists, add suffix "_new" 
    }    
  } else { 
    ext <- gsub(".*\\.(.*)$", "\\1", basename(file))                # determine Excel file type (.xls or .xlsx) 
    wb <- createWorkbook(type = ext) }                              # create a new workbook 
  
  #-- create a new sheet --#
  mysheet <- createSheet(wb, sheetName=sheetname)                        # initiate new sheet
  
  #-- add picture to specified sheet --#
  addPicture(file=picture , sheet=mysheet, startRow=startrow, startCol=startcol)  
  
  #-- save workbook to a file --#
  saveWorkbook(wb, file)
  
}

####################################################################################################################################
####################################################################################################################################
####################################################################################################################################'

####################################################################################################################################
#------------------------- store a matrix -------------------------------------#
#--- default parameters: place picture starting from row 1 and column A ---#

matrix2excel <- function(data, file, sheetname=NULL, colnames=NULL, is.percentage=F, rownames=NULL, startrow=1, startcol=1, append=F) {
    
  #-- check if library is loaded (or installed) --#
  if(!"xlsx" %in% search())  initiate_library()              
  
  #-- create a sheet name if not specified in parameters --#
  if (is.null(sheetname)) { sheetname <- gsub("-|_|\\."," ",deparse(substitute(data))) }          # if no sheet is specified, create from object's name
  
  #-- check if Excel file already exists and append it if necessary--#
  if(append) { 
    wb <- loadWorkbook(file)                # retreive a workbook from existing file
    existing_sheet <- getSheets(wb)         # retreive existing sheets 
    if( sheetname %in% names(existing_sheet) )  {                # check if sheet name already exists 
      warning(paste("WARNING: Sheet name already exists. Object was saved as '",sheetname,"_new'"))           
      sheetname <- paste(sheetname,"_new",sep="")          # if sheet exists, add suffix "_new" 
    }    
  } else { 
    ext <- gsub(".*\\.(.*)$", "\\1", basename(file))                # determine Excel file type (.xls or .xlsx) 
    wb <- createWorkbook(type = ext) }                              # create a new workbook 
  
  #-- create a new sheet --#
  mysheet <- createSheet(wb, sheetName=sheetname)    
  
  #-- convert to data frame --#
  if (!is.data.frame(data)) {  data <- as.data.frame(data) }    # convert object to data.frame 
  
  #-- set column and row names provided by user --#
  if(!is.null(rownames)) {
    row.names <-T 
    rownames(data) <- rownames            # assign row names specified by user
    } else { 
      row.names <- F }
  
  if(!is.null(colnames)) {
    col.names <-T 
    colnames(data) <- colnames            # assign column names specified by user
    } else  { 
      col.names <- F }      
  
  #-- define formatting for content, columns and rows --#
  font <- Font(wb, isBold=T, name="EYInterstate Light", heightInPoints=10)
  cs1 <- CellStyle(wb, font=font) + Alignment(h="ALIGN_CENTER", v="VERTICAL_CENTER", wrapText=F)                    # row
  cs2 <- CellStyle(wb) + Font(wb, name="EYInterstate Light", heightInPoints=10) + Alignment(h="ALIGN_CENTER", v="VERTICAL_CENTER", wrapText=F) 
  
  if(is.percentage) {
    cs2 <- cs2 + DataFormat("0.00%")            # change formatting to percentages if needed
  }
   
  #-- auxilary variable to ensure proper column formatting in addDataFrame (colStyle="") --#
  col_format <- rep(c(list(cs2)),ncol(data))
  names(col_format) <- paste(seq(ncol(data)))  
  
  #-- initiate rows and columns in sheet --#
  rows <- createRow(mysheet, 1:nrow(data))
  cells <-createCell(rows, colIndex=1:(ncol(data)+2))
  
  #-- add object, with specified parameters and formatting --# 
  addDataFrame(data, mysheet, startRow=startrow, col.names=col.names, row.names=row.names, startColumn=startcol, rownamesStyle=cs1, colnamesStyle=cs1, colStyle=col_format ) 
  
  #-- additional option to manupilate cells --#
  autoSizeColumn(mysheet, seq(ncol(data)))                       # automatically adjust column size
  
  #-- save workbook to a file --#
  saveWorkbook(wb, file)
  
}

####################################################################################################################################
####################################################################################################################################
####################################################################################################################################'









