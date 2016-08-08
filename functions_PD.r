  #------- CONSTANTS ARE DEFINED AND OTHER NECESSARY FUNCTIONS ARE CALLED -------#

source("functions_Colors.r")
no_state_dataset_column_names = c("id", "record_date", "DPD")
dataset_column_names = c("id", "record_date", "DPD", "state")
expanded_dataset_column_names = c("id", "record_date", "DPD", "state", "period", "first_date", "default_date")


####################################################################################################################################
####################################################################################################################################
####################################################################################################################################


#------------------------------- FUNCTIONS ------------------------------------#

#------------------------- create states -------------------------------------#
#-- default values are set to 90 days past due which gives 6 states, the last being default state --#
#-- state OK! is created for >= 5 days past due

setStates <- function(dta, OK_excl_state = T, OK_D_state = F, repay_state=F, notOK_DPD = 4, absorbing = T, user_defined = NULL, 
                      external_default=F, OK_excl_period=NULL, default_threshold=NULL, default_period=NULL)
{
 #-- check whether the names of columns of the dataset are correct --#
 if (FALSE %in% (no_state_dataset_column_names %in% colnames(dta))) { stop("Non-compliant dataset, please check the column names") }
  
 #-- order the data by id and record_date --#
 dta <- dta[order  (dta$id, dta$record_date),]                         # sorts the data

 #-- create periods for each id --#
 #max_period_by_id <- table(dta$id)                                   # identifies the maximum period for each id
 #dta$period <- sequence(max_period_by_id) - 1
 
 #-- choice of user defined DPD thresholds or standard settings --#
 if(!is.null(user_defined) )   { state_definition <- user_defined }                                # user settings
 if(is.null(user_defined)  )   { state_definition <- c(notOK_DPD, 30, 60, 90)  }                   # standard settings (0-4, 5-30, 31-60, 61-90, 90+ )

 #-- choice whether to create OK! state  --#
 if(OK_excl_state) { ok_ex = 1 } else { ok_ex = 0}                   # depends on setting the OK_excl_state parameter (True / False)

 #-- choice whether to create OK_D state --#                         # depends on setting the OK_D_state parameter (True / False)
 if(OK_D_state) { ok_d = 1 } else { ok_d = 0}
 
 #-- choice wheter to create repay_state --#
 #if(repay_state) { rep = 1 } else { rep = 0}
 
 #-- total number of states --#
 state_count <- length(state_definition) + 1 + ok_ex + ok_d   # + rep

 #-- create states , thresholds and state names --#
 dta$state <- 1                                                      # creates state 1
 for(i in 1:(length(state_definition) - 1) )
  {
   dta$state[dta$DPD > state_definition[i] & dta$DPD <= state_definition[i+1] ] <- i+1+ok_ex+ok_d       # create all other states (excl. default)
  }
 dta$state[dta$DPD > state_definition[length(state_definition)] ] <- state_count                   # assigs the last possible state to default

 #-- condition for OK! state (when migrate to 0 DPD)
 if(OK_excl_state)
 {
   notOK_DPD <- state_definition[1]                                  # take notOK_DPD from user defined thresholds
   pom_dta <- dta[dta$DPD > notOK_DPD,]                              # only records larger than notOK_DPD, set to 5 days by default
   pom_notOK <- pom_dta[!duplicated(pom_dta$id),]                    # only dates of default
   pom_match <- match(dta$id, pom_notOK$id)                          # matches original data with the default dates
   dta$notOK_date <- pom_notOK$record_date[pom_match]
   dta$state[dta$state == 1 & dta$record_date > dta$notOK_date] <- 2
   dta <- dta[,!names(dta) == 'notOK_date']                          # deletes no longer used column "notOK_date"
 }
 
 #-- create default date based on chosen default DPD threshold -- #
 default_DPD <- state_definition[length(state_definition)]         # default DPD (the last value in the vector)
 pom_dta <- dta[dta$DPD > default_DPD,]                            # only records larger than default_DPD ( initially set to 91 days )
 pom_default <- pom_dta[!duplicated(pom_dta$id),]                  # only dates of default
 pom_match <- match(dta$id, pom_default$id)                        # matches original data with the default dates
 dta$default_date <- pom_default$record_date[pom_match]
 
 #-- assign default state --#
 default_state <- max(dta$state)                                     # specifies the default state
 
 #-- setting default as absorbing state (i.e. no migration after default to lower states is possible) --#
 if (absorbing)
 {
   dta$state[dta$record_date > dta$default_date] <- default_state    # assigns default state created earlier   
 }
 
 #-- external default (based on default date provided by bank ) --#
 #-- if default occured earlier than stems from chosen DPD threshold --#
 if (external_default) 
 { 
   if (FALSE %in% (c("default_date_bank") %in% colnames(dta))) 
     { warning("Non-compliant dataset, please check if bank's default date column is present or labeled properly")  
   } else { 
     # dta[dta$id=="100189-4100000655",]
     dta$state[dta$record_date < dta$default_date & dta$record_date >= dta$default_date_bank & !is.na(dta$default_date) & !is.na(dta$default_date_bank)] <- default_state   
     }
 }
 
      
 #-- return from default (note: only 1st default is taken into account )  ----     
 #max_period_by_id <- table(dta$id)                                   # identifies the maximum period for each id
 #dta$period <- sequence(max_period_by_id) - 1      
 if(!is.null(default_threshold) & !is.null(default_period))    
 {
   if(!OK_D_state ) { warning("You need to specify OK_D state first (please check function parameters again)")
   } else {
   dta$state_old <- dta$state
   #dta[dta$id =="104622-3200507934",]   # example ID
   #dta[dta$id == "103675-180408",]
   #dta[dta$id == "103413-3200495347",] 
   #dta[dta$id=="33040-4030001053",]
   dta$default_threshold <- ifelse(dta$DPD > default_threshold , 0, 1 )     #  0 if above pre-defined threshold
   if (external_default)  dta$default_threshold[dta$record_date == dta$default_date_bank & !is.na(dta$default_date_bank)]  <- 0        # if default_date_bank column is chosen
   dta$counter <- 0
   #dta$counter <-  unlist(sapply(rle(dta$default_threshold)$lengths, function(x) { sequence(x) }  ) )   # sequence based on 0/1 from previous command
   dta$counter <- unlist(with(dta, by(default_threshold, id, function(x1)  sapply(rle(x1)$lengths , function(x) sequence(x)    ))))  # counter of 0/1 strings from previous command
   
   dta$counter[dta$state != default_state | dta$default_threshold == 0 ] <- 0        # assign zero to insignifficant records 
   dta$state[dta$counter > default_period] <- 1 + ok_ex + ok_d                         # for 'cured' records (after pre-definted period) change to state OK_D
   #dta$state[is.na(dta$state)] <- dta$state_old[is.na(dta$state)]     
   dta$counter <- NULL                                                      # delete redundant column
   dta$state_old <- NULL                                                    # delete redundant column     
   dta$default_threshold <- NULL                                            # delete redundant column     
    } 
 } 
 
 # -- return from OK! and/or OK_D state (based on pre-defined 'curing' period, specified as function parameter) -- #
 # dta[dta$id == "103967-1200121899",]        
 if(!is.null(OK_excl_period) )
 {
   if(!OK_excl_state ) { warning("You need to specify OK! state first (please check function parameters again)")
   } else {
     dta$state_old <- dta$state 
     dta$counter <- 0                                                         # create a counter
     #dta$counter <- unlist(sapply(rle(dta$state)$lengths, function(x) { sequence(x) }  ) )        # sequence based on state
     dta$counter <- unlist(with(dta, by(state, id, function(x1)  sapply(rle(x1)$lengths , function(x) sequence(x)   ))))  # counter of 0/1 strings based on states
     if( ok_ex == 1 && ok_d == 1) dta$counter[dta$state != 1 + ok_ex |  dta$state != 1 + ok_ex + ok_d ] <- 0       # leave sequence for state OK! and OK_D only 
     if( ok_ex == 1) dta$counter[dta$state != 1 + ok_ex ] <- 0                                      # leave sequence for state OK! only 
     if( ok_d == 1) dta$counter[dta$state != 1 + ok_d ] <- 0                                        # leave sequence for state OK_D only 
     dta$state[dta$counter > OK_excl_period] <- 1                            # assign state OK for records after pre-defined period (set as function parameter)
     #dta$state[is.na(dta$state)] <- dta$state_old[is.na(dta$state)]           # fill out the remaining from original state 
     # dta[dta$id == "103967-1200121899",]
     dta$counter <- NULL                                                      # delete redundant column
     dta$state_old <- NULL                                                    # delete redundant column
   }
 }
 
 #-- create repayment state, which is the last record for each id, except records at last possible date --# 
 if(repay_state) {
   last_date <- tapply(dta$record_date, dta$id, max )
   pom_match <- match(dta$id, rownames(last_date))
   dta$last_date <- as.numeric(last_date[pom_match])
   #length(last_date[last_date!=201209])
   #dta$state_old <- dta$state
   dta$state <- as.numeric(ifelse(dta$record_date==dta$last_date & dta$last_date != max(dta$record_date) & dta$state != default_state , state_count+1, dta$state ))
   dta$last_date <- NULL
 }
 
 # create state names 
# OK_excl_state = T, OK_D_state = F, repay_state=F
 
 if( OK_excl_state==T ) { ok_ex = 1 } else { ok_ex = 0}  
 if( OK_D_state==T ) { ok_d = 1 } else { ok_d = 0}
 if( repay_state==T ) { repay = 1 } else { repay = 0}
 
 state_names <- "OK"      
 for (i in 1:(length(state_definition)-1) ) {
   state_names[i+1+ok_ex+ok_d] <- state_definition[i+1]        
 }
 curr_len <- length(state_names)    
 state_names[curr_len+1] <- "D"      
 if( ok_ex == 1 ) state_names[2] <- "OK!"
 if( ok_ex == 1 & ok_d == 1)  state_names [3] <- "OK_D" 
 if( ok_ex == 0 & ok_d == 1) state_names[2] <- "OK_D"    
 curr_len <- length(state_names)
 if( repay == 1 ) state_names[curr_len+1] <- "Repaid"
 #state_names
 
 names(state_names) <- 1:length(state_names)
 dta$state_name <- factor(as.character(state_names), state_names)[match(dta$state, as.numeric(names(state_names)))]
 
 #-- delete default_date column --#
 dta$default_date <- NULL
 
 #-- returns expanded dataset --#
  return(dta)                                                        # returns expanded dataset

}


####################################################################################################################################
####################################################################################################################################
####################################################################################################################################'

####################################################################################################################################
#------------------------- expand_dataset -------------------------------------#
#--- expand the dataset by 3 columns - first_date, period and default_date ---#
expand_dataset <- function(dta)
{

 #-- check whether the names of columns of the dataset are correct --#
 if (FALSE %in% (dataset_column_names %in% colnames(dta))) { stop("Non-compliant dataset, please check the column names") }

 #-- order the data by id and record_date --#
 dta <- dta[order(dta$id, dta$record_date),]                         # sorts the data
 state_names <- names(table(dta$state_name))
 
 #-- create column period --#
 max_period_by_id <- table(dta$id)                                   # identifies the maximum period for each id
 dta$period <- sequence(max_period_by_id) - 1                        # adds column where for each id are values up to the max_period_by_id

 #-- create column first_date --#
 #pom_dta <- dta[!duplicated(dta$id),]
 pom_dta <- dta[dta$period == 0,]                                   # different way
 pom_match <- match(dta$id, pom_dta$id)
 dta$first_date <- pom_dta$record_date[pom_match]

 #-- adjust defaul when "repayment" state is present -- #
 if ("Repaid" %in%  state_names) {        
  default_state <- max(dta$state)-1   
 } else {
  default_state <- max(dta$state)
 }
 
 #-- create column default_date --# 
 pom_dta <- dta[dta$state == default_state,]                       # only records with default
 default_date <- tapply(pom_dta$record_date, pom_dta$id, min)      # the earliest possible date of default
 pom_match <- match(dta$id, rownames(default_date))                # matches original data with the default dates
 dta$default_date <- as.numeric(default_date[pom_match])
 rm(default_date)

 #-- returns expanded dataset --#
 return(dta)                                                         # returns expanded dataset
}


####################################################################################################################################
####################################################################################################################################
####################################################################################################################################

####################################################################################################################################
#------------------------- Mm - migration matrix  -------------------------------#
#--- creates migration matrix from the whole dataset ---#
Mm <- function(dta, weighted = F)
{

 #-- check whether the names of columns of the dataset are correct --#
 if (FALSE %in% (expanded_dataset_column_names %in% colnames(dta))) { stop("Non-compliant dataset, please check the column names!") }

 #-- check whether weight columne exist if weighted=T is called --#
 if(weighted & !("weight" %in% colnames(dta))) { stop("Non-compliant dataset. Column weight is missing.")}

 #-- order the data by id and record_date --#
 dta <- dta[order(dta$id, dta$record_date),]                                  # sorts the data
 states <- max(dta$state)                                                     # determines the number of states
 state_names <- names(table(dta$state_name))
 
 #-- creates column with next month --#
 dta$record_date_next_month <- dta$record_date + 1
 dta$record_date_next_month[((dta$record_date_next_month %% 100) == 13)] <- dta$record_date[((dta$record_date_next_month %% 100) == 13)] + 89    #condition for december

#-- creates column with next state (not state of next record, but state in next month as identified in previous two lines) --#
 pom_match <- match(paste(dta$id, dta$record_date_next_month), paste(dta$id,dta$record_date))
 dta$state_next_month <- dta$state[pom_match]
 if(weighted) {
   dta$weight_next_month <- dta$weight[pom_match]
 }
 
 #-- last record has always NULL state_next --#
 max_period_by_id <- tapply(dta$record_date, dta$id, max, na.rm=T)   # max period for each id
 pom_match <- match(dta$id, names(max_period_by_id))                 # match to dta
 pom_period <- max_period_by_id[pom_match]                           # column with last period for each id
 dta$state_next_month[dta$record_date == pom_period] <- "NULL"       # last record for each id is set as NULL
 if(weighted) {
   #   dta$weight_next_month[dta$record_date == pom_period] <- "NULL"       # last record for each id is set as NULL
   dta$weight_next_month[dta$record_date == pom_period] <- NA       # last record for each id is set as NULL
 }

 #-- warning for missing periods & short summary table --#
 na_count <- table(dta$id[is.na(dta$state_next_month)])
 missing_names <- names(na_count[na_count>1])
 if (length(missing_names) >0 ) {
   dta$id <- as.character(dta$id)
   max_period <- as.numeric(tapply(dta$record_date[dta$id %in% missing_names], dta$id[dta$id %in% missing_names], max, na.rm=T))
   min_period <- as.numeric(tapply(dta$record_date[dta$id %in% missing_names], dta$id[dta$id %in% missing_names], min, na.rm=T))
   period_diff <- as.numeric(substr(max_period,1,4))*12 + as.numeric(substr(max_period,5,6)) - ( as.numeric(substr(min_period,1,4))*12 + as.numeric(substr(min_period,5,6)) ) + 1
   missing_periods <- data.frame(as.character(missing_names), period_diff, period_diff - as.numeric( table(dta$id[dta$id %in% missing_names ]))  )
   colnames(missing_periods) <- c("loan ID","should be", "missing")
   missing_count <- length(missing_names)
   #if(missing_count > 0) {
      warning(paste("WARNING: there are", missing_count, "IDs with missing records! The short summary of the IDs is stored in the variable missing_periods."))
   #}
 } else { 
   missing_periods <- NULL
   warning("There are no missing periods in the dataset")}
 
 #-- DPD jumps in data --# 
 dta$DPD_next_month <- unlist(with(dta, by(DPD, id,  function(x) { c(x[-1],NA)  } ) ))            # creates next month DPD   
 dta$DPD_previous_month <- unlist(with(dta, by(DPD, id, function(x) { c(NA,x[-length(x)]) })))    # creates previous month DPD
 dta$DPD_jump_ind <- with(dta, ifelse( (DPD_previous_month>90 & DPD <50 & DPD_next_month >90) | (DPD_previous_month<50 & DPD>90 & DPD_next_month<50) , 1 , 0 ) )
 dta$DPD_jump_ind[is.na(dta$DPD_jump_ind)] <- 0
 DPD_jumps <- dta[dta$id %in% unique(dta$id[dta$DPD_jump_ind==1]),c("id", "record_date", "DPD", "DPD_jump_ind")]      # table with ID having a jump 
 colnames(DPD_jumps) <- c("loan ID", "record date", "DPD", "indicator")
 if( length(unique(dta$id[dta$DPD_jump_ind==1]))  > 0) {
   warning(paste("WARNING: there are", length(unique(dta$id[dta$DPD_jump_ind==1])), "IDs with DPD jumps! The short summary of the IDs is stored in the variable DPD_jumps."))
 } else {
   DPD_jumps <- NULL
   warning("There are no DPD jumps in the dataset")
 }
 dta$DPD_previous_month <- NULL
 dta$DPD_next_month <- NULL
 dta$DPD_jump_ind <- NULL
 
 
 #-- absolute not weighted migration matrix --#
 abs_mat <- with(dta, table(state, state_next_month))                # absolute transition matrix
 id <- which(! c(1:states) %in% rownames(abs_mat)  )                 # which rows are missing
 abs_mat <- rbind(abs_mat, matrix(0,length(id), dim(abs_mat)[2], dimnames=list(id, colnames(abs_mat) ) ) )   # add extra rows with zeros, if missing
 abs_mat <- abs_mat[order(rownames(abs_mat)),]                       # sort matrix ascending
 id2 <- which(! c(1:states) %in% colnames(abs_mat)  )                # which columns are missing
 abs_mat <- cbind(abs_mat,matrix(0,dim(abs_mat)[1],length(id2),dimnames=list(rownames(abs_mat),id2 )))       # add extra columns , if missing
 abs_mat <- abs_mat[,order(colnames(abs_mat))]                       # order matrix, ascending
 abs_mat_null <- abs_mat[,states+1]                                  # last column of the abs_mat is the state->NULL transitions
 abs_mat <- abs_mat[,1:states]                                       # we delete the last column, as it is already in the abs_mat_null variable
 abs_mat <- abs_mat[order(as.numeric(rownames(abs_mat))),order(as.numeric(colnames(abs_mat)))]        # order (if # of rows / columns >= 10 )
            
 # max(dta$state_next_month[!dta$state_next_month=="NULL"])
 
 #-- replace row with default state with zeroes (absorbing = no return from default is possible) --#
# abs_mat[states,1:(states-1)] <- 0
# if (!max(dta$state) %in% unique(dta$state[dta$record_date == max(dta$record_date)])) {
#   abs_mat[states-1, 1:(states-2) ] <-0                              # when last state is "repayment" , write zeroes to one but last
#  }
  
 
 #-- relative not weighted migration matrix --#
 state_sum <- apply(abs_mat,1,sum)                                   # the sum of transition FROM each state
 pom_mat <- matrix(state_sum, states, states, byrow=F)               # temporary matrix where in rows are the sums of the transitions from the state
 rel_mat <- abs_mat/pom_mat                                          # relative transition matrix
 rel_mat[is.na(rel_mat)] <- 0                                        # replace NaN with 0 (especially when an extra row with zeros is added earlier)

# if (!max(dta$state) %in% unique(dta$state[dta$record_date == max(dta$record_date)])) {
#   rel_mat[states-1, states-1 ] <- 1                              # when last state is "repayment" , assign 100% to default
#  }

 #-- weighted absolute and relative migration matrix --#
 if(weighted)
 {

   abs_mat_w_now <- tapply(dta$weight, list(dta$state, dta$state_next_month), sum, na.rm=T)  # absolute transition matrix
   abs_mat_w_next <- tapply(dta$weight_next_month, list(dta$state, dta$state_next_month), sum, na.rm=T)  # absolute transition matrix
   abs_mat_w_now[is.na(abs_mat_w_now)] <- 0                                             # replace NA with 0 (if any)
   abs_mat_w_next[is.na(abs_mat_w_next)] <- 0                                             # replace NA with 0 (if any)
#   abs_mat_w <- tapply(dta$weight, list(dta$state, dta$state_next_month), sum)  # absolute transition matrix

   id <- which(! c(1:states) %in% rownames(abs_mat_w_now)  )                        # which rows are missing
   abs_mat_w_now <- rbind(abs_mat_w_now, matrix(0,length(id), dim(abs_mat_w_now)[2], dimnames=list(id, colnames(abs_mat_w_now) ) ) )   # add extra rows with zeros, if missing
   abs_mat_w_now <- abs_mat_w_now[order(rownames(abs_mat_w_now)),]                          # sort matrix ascending
   id2 <- which(! c(1:states) %in% colnames(abs_mat_w_now)  )                       # which columns are missing
   abs_mat_w_now <- cbind(abs_mat_w_now,matrix(0,dim(abs_mat_w_now)[1],length(id2),dimnames=list(rownames(abs_mat_w_now),id2 )))       # add extra columns , if missing
   abs_mat_w_now <- abs_mat_w_now[,order(colnames(abs_mat_w_now))]                          # order matrix, ascending
   abs_mat_w_null <- abs_mat_w_now[,states+1]                                       # last column of the abs_mat is the state->NULL transitions
   abs_mat_w_now <- abs_mat_w_now[,1:states]                                            # we delete the last column, as it is already in the abs_mat_null variable
   abs_mat_w_now <- abs_mat_w_now[order(as.numeric(rownames(abs_mat_w_now))),order(as.numeric(colnames(abs_mat_w_now)))]

   id <- which(! c(1:states) %in% rownames(abs_mat_w_next)  )                        # which rows are missing
   abs_mat_w_next <- rbind(abs_mat_w_next, matrix(0,length(id), dim(abs_mat_w_next)[2], dimnames=list(id, colnames(abs_mat_w_next) ) ) )   # add extra rows with zeros, if missing
   abs_mat_w_next <- abs_mat_w_next[order(rownames(abs_mat_w_next)),]                          # sort matrix ascending
   id2 <- which(! c(1:states) %in% colnames(abs_mat_w_next)  )                       # which columns are missing
   abs_mat_w_next <- cbind(abs_mat_w_next,matrix(0,dim(abs_mat_w_next)[1],length(id2),dimnames=list(rownames(abs_mat_w_next),id2 )))       # add extra columns , if missing
   abs_mat_w_next <- abs_mat_w_next[,order(colnames(abs_mat_w_next))]                          # order matrix, ascending
   abs_mat_w_null <- abs_mat_w_next[,states+1]                                       # last column of the abs_mat is the state->NULL transitions
   abs_mat_w_next <- abs_mat_w_next[,1:states]                                            # we delete the last column, as it is already in the abs_mat_null variable
   abs_mat_w_next <- abs_mat_w_next[order(as.numeric(rownames(abs_mat_w_next))),order(as.numeric(colnames(abs_mat_w_next)))]

#   rel_mat_w <- abs_mat_w_next / abs_mat_w_now
#   rel_mat_w[is.na(rel_mat_w)] <- 0                                             # replace NA with 0 (if any)

   #-- replace row with default state with zeroes (absorbing = no return from default is possible) --#
#    abs_mat_w_now[states,1:(states-1)] <- 0
#    abs_mat_w_next[states,1:(states-1)] <- 0
#    if (!max(dta$state) %in% unique(dta$state[dta$record_date == max(dta$record_date)])) {
#      abs_mat_w_now[states-1, 1:(states-2) ] <-0                              # when last state is "repayment" , write zeroes to one but last row
#      abs_mat_w_next[states-1, 1:(states-2) ] <-0  
#    }

   state_sum_w <- apply(abs_mat_w_now,1,sum)                                        # the sum of transition FROM each state
   pom_mat_w <- matrix(state_sum_w, states, states, byrow=F)                    # temporary matrix where in rows are the sums of the transitions from the state
   rel_mat_w <- abs_mat_w_next/pom_mat_w                                             # relative transition matrix
   rel_mat_w[is.na(rel_mat_w)] <- 0                                             # replace NaN with 0 (especially when an extra row with zeros is added earlier)
 
   #-- assign 100% for default --#
#    if (!max(dta$state) %in% unique(dta$state[dta$record_date == max(dta$record_date)])) 
#     {
#      rel_mat_w[states-1,states-1] <- 1
#     } else {
#      rel_mat_w[states,states] <- 1
#     }
 }
# abs_mat   <- abs_mat[order(as.numeric(rownames(abs_mat))),order(as.numeric(colnames(abs_mat)))]
 #rel_mat   <- rel_mat[order(as.numeric(rownames(rel_mat))),order(as.numeric(colnames(rel_mat)))]

 
 
  if (weighted) {
   abs_mat_w <- abs_mat_w_now
   abs_mat_w_next <- abs_mat_w_next
  # abs_mat_w <- abs_mat_w[order(as.numeric(rownames(abs_mat_w))),order(as.numeric(colnames(abs_mat_w)))]
   rel_mat_w <- rel_mat_w
  } 

 if(weighted) { output <- list(abs_mat = abs_mat, abs_mat_w = abs_mat_w, abs_mat_w_next = abs_mat_w_next, rel_mat = rel_mat, rel_mat_w = rel_mat_w, data_exp = dta, missing_periods = missing_periods, DPD_jumps = DPD_jumps, abs_mat_null = abs_mat_null, abs_mat_w_null = abs_mat_w_null) }
 if(!weighted) { output <- list(abs_mat = abs_mat, rel_mat = rel_mat, data_exp = dta, missing_periods = missing_periods, DPD_jumps = DPD_jumps, abs_mat_null = abs_mat_null) }

 class(output) <- c("list", "Mm")
 return(output)
}


####################################################################################################################################
####################################################################################################################################
####################################################################################################################################

####################################################################################################################################
#------------------------- PDm - PD matrix ------------------------------------#
PDm <- function(Mm, horizon = 36, weighted = F)
{

 #-- check whether input is migration matrix class --#
 if(!("Mm" %in% class(Mm))) { stop("Input must be object of class Mm. Please run first Mm(data_exp) function.") }

  #-- check whether user wants to use weighted matrix or not (default) --#
  rel_mat <- Mm$rel_mat
  dta <- Mm$data_exp
  state_names <- names(table(dta$state_name))
 
 #-- get rid of "Repaid" state if exists --#
 if ("Repaid" %in%  state_names) {      
   rel_mat <- rel_mat[-dim(rel_mat)[1],-dim(rel_mat)[1]]    
 } 
 
 #-- check whether weighted matrix exist when weighted=T is set --#
  if(weighted)
  {
    if(is.null(Mm$rel_mat_w))  { stop("Input matrix does not contain weighted matrix! Please run Mm(data_exp, weighted = T) function first.") }
    if(!is.null(Mm$rel_mat_w)) 
      { 
        rel_mat_w <- Mm$rel_mat_w 
        if ("Repaid" %in%  state_names) {      
          rel_mat_w <- rel_mat_w[-dim(rel_mat_w)[1],-dim(rel_mat_w)[1]]    
        }         
      }
  }

  #-- setting number of credit states --#
  states <- dim(rel_mat)[1]
  if(weighted) 
   {
   states <- dim(rel_mat_w)[1]
   }
 
  #-- make sure default is set to 100% (even when is not absorbing state), otherwise PD matrix gives weird results  --#
  rel_mat[states,1:(states-1)] <- 0    
  rel_mat[states,states] <- 1
 
  #-- first column of PD matrix is set --#
  pd_mat <- matrix(0,states,horizon)
  pd_mat[,1] <- rel_mat[,states]

  #-- first matrix --#
  pow_mat <- list()
  pow_mat <- c(pow_mat, list(rel_mat))

  #-- temporary matrix which is used for computational purposes --#
  pom_mat <- rel_mat
  for (i in 2:horizon) {
       pom_mat <- pom_mat%*%rel_mat                                # rel_mat to the power of i
       pow_mat <- c(pow_mat, list(pom_mat))                        # store the powered matrix
       pd_mat[,i] <- pom_mat[,states]                              # store the last column (PD for each state)
       
  }

  #-- same computations for weighted matrix --#
  if(weighted)
  {
    rel_mat_w[states,1:(states-1)] <- 0    
    rel_mat_w[states,states] <- 1
    
    pd_mat_w <- matrix(0,states,horizon)
    pd_mat_w[,1] <- rel_mat_w[,states]
    pow_mat_w <- list()
    pow_mat_w <- rel_mat_w
    pom_mat_w <- rel_mat_w
    for (i in 2:horizon) {
         pom_mat_w <- pom_mat_w%*%rel_mat_w                                # rel_mat to the power of i
         pow_mat_w <- c(pow_mat_w, list(pom_mat_w))                        # store the powered matrix
         pd_mat_w[,i] <- pom_mat_w[,states]                                # store the last column (PD for each state)
    }
  }
  
  if(weighted) {  output <- list(mig_mat = Mm, pd_mat = pd_mat, pd_mat_w = pd_mat_w, pow_mat = pow_mat, pow_mat_w=pow_mat_w) }
  if(!weighted) { output <- list(mig_mat = Mm, pd_mat = pd_mat, pow_mat = pow_mat) }

  class(output) <- c("list", "PDm")
  return(output)
}

####################################################################################################################################
#------------------------- plot.PDm ----- -------------------------------------#
plot.PDm <- function(PDm, separate = F, weighted = F,
                     state_names = NULL, file_name = NULL,
                     main_title = "PD in time for credit state", y_lab = "Probability of default", x_lab = "Time horizon(months)",
                     pic_width = 800, pic_height = 500)
{

 #-- checking for the valid class of the object --#
 if(!("PDm" %in% class(PDm))) { stop("The input is not a PDm class object! Please run PDm(Mm) function first.") }

 #-- setting the input matrix --#
 if(!weighted) { pd_mat <- PDm$pd_mat }
 if(weighted)
 {
   if(is.null(PDm$pd_mat_w))  { stop("Input matrix does not contain weighted matrix! Please run PDm(Mm, weighted = T) function first.") }
   if(!is.null(PDm$pd_mat_w)) { pd_mat <- PDm$pd_mat_w }
 }

 #-- setting the parameters --#
 states <- dim(pd_mat)[1]
 horizon <-dim(pd_mat)[2]

 #-- checking the names of the states --#
 if(!is.null(state_names) & length(state_names) != states)
 {
     stop(paste("The number of specified names of the states is different than the number of states! \n",
               "  There are ", states, " states in the input file and ", length(state_names), " names specified.", sep=""))
 }

  #-- if we want to have all the states in one picture --#
  if(!separate)
  {
    #-- storing the picture, if user requires it --#
    if(!is.null(file_name))
    {
        png(filename=paste(file_name, ".png", sep=""), width=pic_width, height=pic_height)             # setting the filename for storing
        print(paste("The graph has been stored to the file ", file_name, ".png", sep=""))              # info for user
    }

    #-- plotting the graph, it will only show on the screen if the file_name is not set --#
    plot(pd_mat[1,], col=gray(1-(1/states)), xlim=c(1,horizon), ylim=c(0,1), lty=1, lwd=2,             # first line
         type="l", main=main_title, xlab=x_lab, ylab=y_lab)
    for (j in 2:states) {
        lines(pd_mat[j,], col=gray(1-(j/states)), lty=1, lwd=2, type="l")                              # lines for other states
    }
    grid()
    #-- specifying the legend of the plot --#
    if(is.null(state_names))  { legend("bottomright", col=c("lightgray", "black"), legend=c("OK", "default"), lwd=3, bg="white") }   # legend
    if(!is.null(state_names))
    {
      legend_titles = state_names[1]                                                       # setting first name
      for (k in 2:states)
      {
        legend_titles = c(legend_titles, state_names[k])                                   # adding names to the vector of names
      }
      legend_colors = gray( (1-(1:states)*(1/states)) )                                    # setting the colors of the legend
      legend("topleft", col=legend_colors, legend=legend_titles, lwd=3, bg="white")    # legend
    }

    #-- closing the store session, otherwise the picture would not be accesible--#
    if(!is.null(file_name)) { dev.off() }
  }
  
  #-- if we want to have all the states in separate pictures --#
  if(separate)
  {
    for (j in 1:states) {
      #-- storing the picture, if user requires it --#
      if(!is.null(file_name)) {
          png(filename=paste(file_name, "_", j, ".png", sep=""), width=pic_width, height=pic_height)       # setting the filename for storing
      }

      #-- plotting the graph, it will only show on the screen if the file_name is not set --#
      plot(pd_mat[j,], col="black", xlim=c(1,horizon), ylim=c(0,1), lty=1, lwd=2,                          # line
         type="l", main=main_title, xlab=x_lab, ylab=y_lab)
      if(is.null(state_names)) { plot_legend_s = paste("Credit state ", j, sep="") }                       # if state names are not set, use default name
      if(!is.null(state_names)) { plot_legend_s = state_names[j] }                                         # if state names are set use specified name
      grid()
      legend("bottomright", col="black", legend=plot_legend_s, lwd=3, bg="white")                          # legend

      #-- if we are not storing, the user is prompted to press any key to another graph --#
      if(is.null(file_name)) { readline("Press any key to view graph for another state!") }

      #-- if we are storing, this will close the storing session, otherwise the picture would not be accesible--#
      if(!is.null(file_name)) { dev.off() }
    }

    #-- user is informed about the name of the plot files --#
    if(!is.null(file_name)) {
      print(paste("The graphs were saved to files ", file_name, "_#.png files.", sep=""))                  # user is informed about the name of the plot files
    }
  }
}

####################################################################################################################################
####################################################################################################################################
####################################################################################################################################

####################################################################################################################################
#------------------------- fPD - frequential PD --------------------------------#
fPD <- function(dta, default_horizon = NULL, min_horizon = NULL)
{

 #-- check whether the names of columns of the dataset are correct --#
 if (FALSE %in% (expanded_dataset_column_names %in% colnames(dta))) { stop("non-compliant dataset, check the column names") }

 #-- sets the min_horizon to default_horizon, if it's not specified by user --#
 if(is.null(default_horizon))
 {
   default_horizon <- 12
   warning("The function has run, however you haven't specified the default_horizon parameter, so it was set to 12 months.")
 }
 if(is.null(min_horizon)) { min_horizon = default_horizon }
 if(!is.null(min_horizon)) { if(min_horizon > default_horizon) { min_horizon = default_horizon } }

 #-- order the data by id and record_date --#
 dta <- dta[order(dta$id, dta$record_date),]                                  # sorts the data
 states <- max(dta$state)                                                     # determines the number of states
 periods <- unique(dta$record_date)          # names of the period
 periods <- periods[order(periods)]           # names of the period
 loans <- unique(dta$id)                                                      # loans (unique)

 dta$state <- as.numeric(as.character(dta$state))

  #-- matrix of states by id in time, empty matrix so far --#
  fPD_by_id <- matrix(NA, nrow=length(loans), ncol=1+length(periods))

  #-- auxiliary columns for computational purposes --#
  dta$record_date_new <- match(dta$record_date, periods) + 1                  # identificator of the row of the next month
  dta$id_new <- match(dta$id, loans)                                          # identificator of the number of row of the id in the matrix

  #-- matrix filling --#
  for (i in 1:nrow(dta)) {
      #print(i)
      fPD_by_id[dta$id_new[i], dta$record_date_new[i]]  <- dta$state[i]       # algorithm goes through rows and assigns state to the loan by the identificators -> first is the row and second is the column of the matrix
      }
  fPD_by_id[,1] <- loans                                                      # first column is the loan id
  #fPD_by_id <- as.data.frame(fPD_by_id)                                       # transforms the matrix to data.frame
  fPD_by_id <- as.data.frame(fPD_by_id, stringsAsFactors = F)                                       # transforms the matrix to data.frame

  colnames(fPD_by_id) <- c("id",periods)                                      # assigns column names

  #-- algorithm for calculation of frequential PD
  #-- we go through dates and look at the window of next default_horizon months
  #-- if there is a default state present at least once, we have default indicator
  #-- NA is ommited, dealt with later

  freq_pd <- NULL

  for (i in 2:(ncol(fPD_by_id)-min_horizon)) {                                # cycle through periods
      states_actual <- fPD_by_id[,i]                                          # vector of states for the given date
      states_future <- fPD_by_id[,(i+1):min(ncol(fPD_by_id),i+default_horizon)]
      states_future[is.na(states_future)] <- 0
      states_future <- states_future==states
      pom_def <- states_future %*% rep(1,ncol(states_future))                 # sums the number of default states for each id
      pom_def <- pom_def > 0                                                  # if the sum is greater than zero, it means that there is at least one record with default state
      fPD_by_states <- table(states_actual, pom_def, exclude=NULL)            # number of defaults for each current state

      #-- auxiliary operations for computational purposes --#
      pom <- rbind(fPD_by_states)                                             # auxiliary variables and operations
      pom2 <- rep(colnames(fPD_by_id)[i], nrow(fPD_by_states))
      pom3 <- as.numeric(rownames(fPD_by_states))
      fPD_by_date <- cbind("state"=pom3, pom)
      #rownames(fPD_by_date) <- c(1:nrow(fPD_by_date))
      rownames(fPD_by_date) <- fPD_by_date[,"state"]
      rownames(fPD_by_date)[is.na(rownames(fPD_by_date))] <- "NA"
      fPD_by_date <- as.data.frame(fPD_by_date)
      fPD_by_date <- cbind("record_date"=as.numeric(pom2), fPD_by_date)

      freq_pd <- rbind(freq_pd, fPD_by_date)
  }

  #-- final table --#
  freq_pd[,5] <- freq_pd[,4] / (freq_pd[,4]+freq_pd[,3])                      # PD calculation
  colnames(freq_pd)[5] <- "pd"
  freq_pd$x <- match(freq_pd$record_date, periods)                            # x-axis
  freq_pd$state[is.na(freq_pd$state)] <- 0                                    # NA is set to zero, not used later
  freq_pd$pd[freq_pd$state==states] <- 1                                      # is defaulted and disappeared, PD = 1 forever

 output <- list(freq_pd = freq_pd, data_exp = dta)

 class(output) <- c("list", "fPD")
 return(output)
}

####################################################################################################################################
#------------------------- plot.fPD -------------------------------------------#
plot.fPD <- function(fPD, state_names = NULL, file_name = NULL, ticks_frequency = 1,
                     main_title="Yearly PD in time for credit states", y_lab="Probability of default", x_lab="Date",
                     pic_width = 800, pic_height = 500)
{

 #-- checking for the valid class of the object --#
 if(!("fPD" %in% class(fPD))) { stop("The input is not a fPD class object!") }

 #-- setting the parameters --#
 fpd <- fPD$freq_pd                                               # assigning the input variable to variable for ease of purpose
 states <- max(fpd$state)                                         # identifying the number of states
 periods <- unique(fpd$record_date)                               # getting the names of the periods
 periods <- periods[order(periods)]                               # sorting the names of the periods
 horizon <- length(periods)                                       # getting the length of the data sample

 #-- checking the names of the states --#
 if(!is.null(state_names) & length(state_names) != states)
 {
     stop(paste("The number of specified names of the states is different than the number of states! \n",
               "  There are ", states, " states in the input file and ", length(state_names), " names specified.", sep=""))
 }


    #-- storing the picture, if user requires it --#
    if(!is.null(file_name)) {
        png(filename=paste(file_name, ".png", sep=""), width=pic_width, height=pic_height)             # setting the filename for storing
        print(paste("The graph has been stored to the file ", file_name, ".png", sep=""))              # info for user
    }

    #-- margin of the graph --#
    par(mar=c(5,4,3,4))                                                                                # setting the magrins of the graph

    #-- plotting the graph, it will only show on the screen if the file_name is not set --#
    plot(fpd$x[fpd$state==1], fpd$pd[fpd$state==1], xlim=c(1,horizon), ylim=c(0,1), lty=1, lwd=3,      # initializng the plot, and draw first line
          type="l", col=gray(1-(1/states)), main=main_title, ylab=y_lab, xlab=x_lab, xaxt="n")
    last_pd <- fpd$pd[fpd$state==1][length(fpd$pd[fpd$state==1])]                                      # variable for right axis ticks (credit states)
    for (i in 2:states) {
      pom <- fpd[fpd$state==i,]
      lines(pom$x, pom$pd, ylim=c(0,1), lty=1, lwd=2, type="l", col=gray(1-(i/states)))                # drawing the rest of the lines
      last_pd <- c(last_pd, pom$pd[length(pom$pd)])                                                    # setting the position of the right axis tick for each line
    }

    #-- the ticks for identification of states on the right axis, if the names of the states set --#
    if(!is.null(state_names))
    {
      legend_titles = state_names[1]                                                                   # setting the first name
      for (k in 2:states)
      {
        legend_titles = c(legend_titles, state_names[k])                                               # adding names to the vector of names
      }
      mtext("Credit state", 4, line=2.5)                                                               # label of the right axis
      axis(4, at=last_pd, labels=legend_titles, cex=1.1)                                               # drawing the axis
    }

    # if incorrect ticks_frequency has been specified, we correct it
    if(ticks_frequency < 1) ticks_frequency <- 1
    if(ticks_frequency > 6) ticks_frequency <- 6

    #x_ticks <- c(horizon, horizon-(1:(floor(horizon/6))*6))              # multiples of half-year, from the last point
    x_ticks <- c(horizon, horizon-(1:(floor(horizon/ticks_frequency))*ticks_frequency))              # multiples of half-year, from the last point
    x_ticks <- x_ticks[length(x_ticks):1]
    x_ticks[x_ticks==0] <- 1

    axis(1, at=x_ticks, labels=substr(periods[1:horizon][x_ticks],3,6))                                # vertical and horizontal dotted lines
    abline(v = x_ticks, col = "lightgray", lty = "dotted")
    abline(h = c(0:5)/5, col = "lightgray", lty = "dotted")

    #-- closing the store session, otherwise the picture would not be accesible--#
    if(!is.null(file_name)) { dev.off() }
}
####################################################################################################################################
#------------------------- table.fPD ------------------------------------------#


####################################################################################################################################
####################################################################################################################################
####################################################################################################################################


####################################################################################################################################
#------------------------- cPD - cumulative PD -----------------------------#
cPD <- function(dta, absorbing = T)
{
 #-- check whether the names of columns of the dataset are correct --#
 if (FALSE %in% (expanded_dataset_column_names %in% colnames(dta))) { stop("non-compliant dataset, check the column names") }

 #-- order the data by id and record_date --#
 dta <- dta[order(dta$id, dta$record_date),]                                  # sorts the data
 states <- max(dta$state)                                                     # determines the number of states
 periods <- unique(dta$record_date)                                           # names of the periods
 horizon <- length(periods)                                                   # timespan of the dataset

  loans <- unique(dta$id)                                                     # all IDs
  first_dates <- dta$first_date[!duplicated(dta$id)]                          # cohorts
  pom_max_period  <- tapply(dta$period, dta$first_date, max)

  row_count <- pom_max_period[match(first_dates, as.numeric(names(pom_max_period)))] + 1 # number of rows in resulting matrix (nrow)

  # temporary supporting data.frame to be filled-in with specified data
  rec <- data.frame(id = rep(loans,row_count), first_date = rep(first_dates,row_count), period = sequence(row_count)-1)
  rec <- merge(dta, rec, all=T)

  # help function - moving last known forward
  fceH <- function(vect) {
     return(approx(vect, method="constant", xout=seq_along(vect), rule=2)$y)
  }

  if ("weight" %in% colnames(dta)) {
    weight_exists <- T
  } else {
    weight_exists <- F
  }

  if (absorbing) {
     # last known state moved forward
     rec$state  <- unlist(by(rec$state, rec$id, fceH))
     if(weight_exists) {
       rec$weight  <- unlist(by(rec$weight, rec$id, fceH))
     }
  }  else {
     rec$state[is.na(rec$state)]  <- 1
     if(weight_exists) {
       rec$weight[is.na(rec$weight)]  <- 0
     }
  }

  rec$first_date  <- unlist(by(rec$first_date, rec$id, fceH))

  #-- creates tables of pd by cohort (first_date) and by period --#
  pd_cohort_period <- with(rec[rec$state==states,], table(first_date, period))                      # only records in default, number of record for each cohort and period
  count_cohort_period <- with(rec, table(first_date, period))                                       # number of all loans (default and non-default) for each cohort and period
  #count_cohort_period_alt <- with(rec, table(first_date, record_date))                                       # number of all loans (default and non-default) for each cohort and period
  count_cohort <- with(rec[rec$period == 0,], table(first_date))                                       # number of all loans (default and non-default) for each cohort and period

  if(weight_exists) {
    pd_cohort_period_w <- with(rec[rec$state==states,], tapply(weight, list(first_date, period), sum, na.rm=T ) )                      # only records in default, number of record for each cohort and period
    count_cohort_period_w <- with(rec, tapply(weight, list(first_date, period), sum, na.rm=T ) )                                       # number of all loans (default and non-default) for each cohort and period
    #count_cohort_period_w_alt <- with(rec, tapply(weight, list(first_date, record_date), sum, na.rm=T ) )                                       # number of all loans (default and non-default) for each cohort and period
    count_cohort_w <- with(rec[rec$period == 0,], tapply(weight, first_date, sum, na.rm=T ) )                                       # number of all loans (default and non-default) for each cohort and period
  }

  #-- adds missing ROWS, for which there werent any default --#
  pom_periods <- rownames(count_cohort_period)[! rownames(count_cohort_period) %in% rownames(pd_cohort_period)]    #finds missing cohorts
 if(length(pom_periods)>0) {
      pd_cohort_period <- rbind(pd_cohort_period, matrix(0, nrow=length(pom_periods), ncol=ncol(pd_cohort_period)))    #adds rows with zeros
      rownames(pd_cohort_period)[nrow(pd_cohort_period):(nrow(pd_cohort_period)-length(pom_periods)+1)] <- pom_periods #sets the names of the new rows
      pd_cohort_period <- pd_cohort_period[order(rownames(pd_cohort_period)),]                                         #sorts data
      #rownames(pd_cohort_period) == rownames(count_cohort_period)                                                      #check whether pd and count have same names
    
      if (weight_exists) {
        pd_cohort_period_w <- rbind(pd_cohort_period_w, matrix(0, nrow=length(pom_periods), ncol=ncol(pd_cohort_period_w)))    #adds rows with zeros
        rownames(pd_cohort_period_w)[nrow(pd_cohort_period_w):(nrow(pd_cohort_period_w)-length(pom_periods)+1)] <- pom_periods #sets the names of the new rows
        pd_cohort_period_w <- pd_cohort_period_w[order(rownames(pd_cohort_period_w)),]                                         #sorts data
        #rownames(pd_cohort_period_w) == rownames(count_cohort_period)                                                      #check whether pd and count have same names
      }
  }
 
  #-- adds missing COLUMNS, for which there werent any default --#
  pom_periods_col <- colnames(count_cohort_period)[! colnames(count_cohort_period) %in% colnames(pd_cohort_period)]    #finds missing cohorts
  if(length(pom_periods_col) > 0) {
      pd_cohort_period <- cbind(pd_cohort_period, matrix(0, ncol=length(pom_periods_col), nrow=nrow(pd_cohort_period)))    #adds rows with zeros
      colnames(pd_cohort_period)[ncol(pd_cohort_period):(ncol(pd_cohort_period)-length(pom_periods_col)+1)] <- pom_periods_col #sets the names of the new rows
      pd_cohort_period <- pd_cohort_period[,order(as.integer(colnames(pd_cohort_period)))]                                         #sorts data
      #colnames(pd_cohort_period) == colnames(count_cohort_period)                                                      #check whether pd and count have same names

      if (weight_exists) {
        pd_cohort_period_w <- cbind(pd_cohort_period_w, matrix(0, ncol=length(pom_periods_col), nrow=nrow(pd_cohort_period_w)))    #adds rows with zeros
        if(length(pom_periods_col)>0)  colnames(pd_cohort_period_w)[ncol(pd_cohort_period_w):(ncol(pd_cohort_period_w)-length(pom_periods_col)+1)] <- pom_periods_col #sets the names of the new rows
        pd_cohort_period_w <- pd_cohort_period_w[,order(as.integer(colnames(pd_cohort_period_w)))]                                         #sorts data
        #rownames(pd_cohort_period_w) == rownames(count_cohort_period)                                                      #check whether pd and count have same names
      }
  }

  #count_cohort_m <- matrix(count_cohort, nrow(pd_cohort_period), ncol(pd_cohort_period), byrow=F)               # temporary matrix where in rows are the sums of the transitions from the state
  if (weight_exists)  {
  count_cohort_w_m <- matrix(count_cohort_w, nrow(pd_cohort_period_w), ncol(pd_cohort_period_w), byrow=F)               # temporary matrix where in rows are the sums of the transitions from the state
    }
    
  #-- final result - cumulative PD by cohort and period --#
  cum_pd_cohort_period <- pd_cohort_period / count_cohort_period
  #cum_pd_cohort_period_alt <- pd_cohort_period / count_cohort_m
  #cum_pd_cohort_period_alt_2 <- pd_cohort_period / count_cohort_period_alt
  if (weight_exists) {
    cum_pd_cohort_period_w <- pd_cohort_period_w / count_cohort_w_m
    #cum_pd_cohort_period_w_alt <- pd_cohort_period_w / count_cohort_period_w
    #cum_pd_cohort_period_w_alt_2 <- pd_cohort_period_w / count_cohort_period_w_alt
  }

  ##### getting rid of NA values for weighted matrix #####
  pd_cohort_period_bk <- pd_cohort_period
  pom <- as.matrix(pd_cohort_period)
  pom <- as.matrix(apply(t(pom),1,rev))
  pom[is.na(pom)] <- 0
  pom[upper.tri(pom, diag=F)] <- NA
  pd_cohort_period <- as.matrix(apply(t(pom),1,rev))

  if (weight_exists) {
    pd_cohort_period_w_bk <- pd_cohort_period_w
    pom <- as.matrix(pd_cohort_period_w)
    pom <- as.matrix(apply(t(pom),1,rev))
    pom[is.na(pom)] <- 0
    pom[upper.tri(pom, diag=F)] <- NA
    pd_cohort_period_w <- as.matrix(apply(t(pom),1,rev))
  
    cum_pd_cohort_period_w_bk <- cum_pd_cohort_period_w
    pom <- as.matrix(cum_pd_cohort_period_w)
    pom <- as.matrix(apply(t(pom),1,rev))
    pom[is.na(pom)] <- 0
    pom[upper.tri(pom, diag=F)] <- NA
    cum_pd_cohort_period_w <- as.matrix(apply(t(pom),1,rev))
  }

  #-- computing the sum of records (through cohorts) for each period  --#
  pom <- count_cohort_period
  pom[is.na(pom)] <- 0
  count_total <- rep(1, nrow(pom) ) %*% pom

  if (weight_exists) {
    pom_w <- count_cohort_period_w
    pom_w[is.na(pom_w)] <- 0
    count_total_w <- rep(1, nrow(pom_w) ) %*% pom_w
  }

  #-- computing the sum of defaults (through cohorts) for each period  --#
  pom2 <- pd_cohort_period
  pom2[is.na(pom2)] <- 0
  pd_total <- rep(1, nrow(pom2) ) %*% pom2

  if (weight_exists) {
    pom2_w <- pd_cohort_period_w
    pom2_w[is.na(pom2_w)] <- 0
    pd_total_w <- rep(1, nrow(pom2_w) ) %*% pom2_w
  }
  
  #-- final result - cumulative PD by period for all cohorts--#
  cum_pd_total <- pd_total / count_total

  if (weight_exists) {
    cum_pd_total_w <- pd_total_w / count_total_w
  }

  ########### Grouping by QUARTERS ###############
  #-- similar approach for quarters (based on Volksbank_SK - Backtesting_OP) --#
  kvartal <- floor(rec$period/3)+1                                                                                 # setting the quarter variable
  kvartaly <- unique(kvartal)                                                                                      # identifying the names of the quarters
  kvartaly <- kvartaly[order(kvartaly)]                                                                            # sorting the names
  kvartal_count <- length(kvartaly)                                                                                # getting the number of the quarters

  #-- initializing variables --#
  pom <- cbind(pd_cohort_period, matrix(NA, nrow(pd_cohort_period), 3-(ncol(pd_cohort_period)%%3)))                # auxiliary variable, extended pd_cohort_period matrix
  pd_cohort_kvartal <- matrix(0, nrow(pom), ncol(pom)/3)
  count_cohort_kvartal <- matrix(0, nrow(pom), ncol(pom)/3)
  if (weight_exists) {
    pom_w <- cbind(pd_cohort_period_w, matrix(NA, nrow(pd_cohort_period_w), 3-(ncol(pd_cohort_period_w)%%3)))                # auxiliary variable, extended pd_cohort_period matrix
    pd_cohort_kvartal_w <- matrix(0, nrow(pom_w), ncol(pom_w)/3)
    count_cohort_kvartal_w <- matrix(0, nrow(pom_w), ncol(pom_w)/3)
  }
  #-- computing quarter averages from period pd --#
  for (i in 1 : nrow(pom)) {
      for (j in 1 : (ncol(pom)/3)) {
          pd_cohort_kvartal[i,j] = round(mean(c(pom[i,3*j-2],pom[i,3*j-1],pom[i,3*j]),na.rm=T))
          count_cohort_kvartal[i,j] = count_cohort_period[i,1]
          if (weight_exists) {
            pd_cohort_kvartal_w[i,j] = round(mean(c(pom_w[i,3*j-2],pom_w[i,3*j-1],pom_w[i,3*j]),na.rm=T))          
            count_cohort_kvartal_w[i,j] = count_cohort_period_w[i,1]
         }   
      }
  }


  output <- list(cum_pd_cohort = cum_pd_cohort_period, cum_pd_total = cum_pd_total,
                 #cum_pd_cohort_alt = cum_pd_cohort_period_alt,
                 #cum_pd_cohort_w = cum_pd_cohort_period_w, cum_pd_total = cum_pd_total_w,
                 #cum_pd_cohort_w_alt = cum_pd_cohort_period_w,
                 pd_cohort = pd_cohort_period, pd_total = pd_total,
                 #pd_cohort_w = pd_cohort_period_w,  pd_total_w = pd_total_w,
                 count_cohort_period = count_cohort_period, count_cohort = count_cohort, count_total = count_total,
                 #count_cohort_period_w = count_cohort_period_w, count_cohort_w = count_cohort_w, count_total_w = count_total_w,
                 pd_cohort_kvartal = pd_cohort_kvartal,
                 #pd_cohort_kvartal_w = pd_cohort_kvartal_w,
                 count_cohort_kvartal = count_cohort_kvartal,
                 #count_cohort_kvartal_w = count_cohort_kvartal_w,
                 data_exp = dta)
  
   if (weight_exists) {
   output <-  list(cum_pd_cohort = cum_pd_cohort_period, cum_pd_total = cum_pd_total,
                 #cum_pd_cohort_alt = cum_pd_cohort_period_alt,
                 cum_pd_cohort_w = cum_pd_cohort_period_w, cum_pd_total = cum_pd_total_w,
                 #cum_pd_cohort_w_alt = cum_pd_cohort_period_w,
                 pd_cohort = pd_cohort_period, pd_total = pd_total,
                 pd_cohort_w = pd_cohort_period_w,  pd_total_w = pd_total_w,
                 count_cohort_period = count_cohort_period, count_cohort = count_cohort, count_total = count_total,
                 count_cohort_period_w = count_cohort_period_w, count_cohort_w = count_cohort_w, count_total_w = count_total_w,
                 pd_cohort_kvartal = pd_cohort_kvartal,
                 pd_cohort_kvartal_w = pd_cohort_kvartal_w,
                 count_cohort_kvartal = count_cohort_kvartal,
                 count_cohort_kvartal_w = count_cohort_kvartal_w,
                 data_exp = dta)
   }                         
                 

  class(output) <- c("list", "cPD")
  return(output)
}
####################################################################################################################################
#------------------------- plot.cPD -------------------------------------------#
plot.cPD <- function(cPD, file_name = NULL, by_cohort = F,
                     main_title="Cumulative PD in time", y_lab="% of defaulted loans", x_lab="Months from beginning",
                     pic_width = 800, pic_height = 500, weighted = F)
{

  if(!("cPD" %in% class(cPD))) { stop("The input is not a cPD class object!") }

   #-- assigment of variables --#
   cpd <- cPD

   #-- setting the variables --#
   dta <- cpd$data_exp
   pd_cohort <- cpd$pd_cohort
   pd_total <- cpd$pd_total
   count_pd_cohort <- cpd$count_pd_cohort
   count_pd_total <- cpd$count_pd_total

   dta <- dta[order(dta$id, dta$record_date),]                                  # sorts the data
   states <- max(dta$state)                                                     # determines the number of states
   periods <- unique(dta$record_date)                                           # names of the periods
   cohorts <- unique(dta$first_date)                                            # names of the periods

   if(!weighted) {
     cum_pd_cohort <- cpd$cum_pd_cohort
     cum_pd_total <- cpd$cum_pd_total
   } else {
     cum_pd_cohort <- cpd$cum_pd_cohort_w
     cum_pd_total <- cpd$cum_pd_total_w
   }
     cohort_count <- length(unique(dta$first_date))                               # names of the periods
     horizon <- length(periods)                                                   # timespan of the dataset

  #-- storing the picture, if user requires it --#
  if(!is.null(file_name)) {
      png(filename=paste(file_name, ".png", sep=""), width=pic_width, height=pic_height)             # setting the filename for storing
      print(paste("The graph has been stored to the file ", file_name, ".png", sep=""))              # info for user
  }

  #-- drawing separate lines for each cohorts --#
  if(by_cohort)
  {
    plot(as.vector(cum_pd_cohort[1,]),                                                               # initializing the plot and drawing first line
         type="l", lty=1, lwd=4, col=gray(1-(1/states)),
         ylim=c(0,max(cum_pd_cohort,na.rm=T)), xaxt="n", main=main_title, xlab=x_lab, ylab=y_lab)
    x_ticks <- c(horizon, horizon-(1:(floor(horizon/6))*6))                                          # x-axis ticks, multiples of half-year, from the last point
    x_ticks <- x_ticks[length(x_ticks):1]
    axis(1, x_ticks , x_ticks)
    abline(v=x_ticks, lty="dotted", col="lightgray")                                                 # vertical and horizontal grey dotted lines
    grid(nx=NA, ny=NULL)
    #abline(h=(0:5)/50, lty="dotted", col="lightgray")
    legend("topleft", bg="white", lwd=3, lty=1, col=c(gray(1-(10/cohort_count)), "black"), legend=c("old_cohorts", "young_cohorts")) # legend

    for (i in 2:cohort_count)
    {
      lines(as.vector(cum_pd_cohort[i,]), type="l", lty=1, lwd=4, col=gray(1-(i/cohort_count)))      # the rest of the lines
    }
  }

  #-- drawing the line for whole portfolio --#
  if(!by_cohort)
  {
    plot(as.vector(cum_pd_total),                                                                    # initializing the plot
         type="l", lty=1, lwd=4, col="gray80",
         ylim=c(0,max(cum_pd_total)), xaxt="n", main=main_title, xlab=x_lab, ylab=y_lab)
    x_ticks <- c(horizon, horizon-(1:(floor(horizon/6))*6))                                          # x-axis ticks, multiples of half-year, from the last point
    x_ticks <- x_ticks[length(x_ticks):1]
    axis(1, x_ticks, x_ticks)
    abline(v=x_ticks, lty="dotted", col="lightgray")                                                 # vertical and horizontal grey dotted lines
    grid(nx=NA, ny=NULL)
    #abline(h=(0:5)/50, lty="dotted", col="lightgray")
    legend("topleft", legend=c("Cumulative default rate"), bg="white", lwd=3, lty=1, col=c("gray80"))          # legend
   }

    #-- closing the store session, otherwise the picture would not be accesible--#
    if(!is.null(file_name)) { dev.off() }
}
####################################################################################################################################
#------------------------- table.cPD ------------------------------------------#


####################################################################################################################################
####################################################################################################################################
####################################################################################################################################

#------------------------- iPD - incremental PD -------------------------------#
iPD <- function(cPD)
{
 #-- check whether the names of columns of the dataset are correct --#
 if(!("cPD" %in% class(cPD))) { stop("The input is not a cPD class object!") }

 #-- setting the variables --#
 cpd <- cPD

 dta <- cpd$data_exp
 cum_pd_cohort <- cpd$cum_pd_cohort
 cum_pd_total <- cpd$cum_pd_total
 pd_cohort <- cpd$pd_cohort
 pd_cohort_kvartal <- cpd$pd_cohort_kvartal
 pd_total <- cpd$pd_total
 count_cohort <- cpd$count_cohort
 count_cohort_kvartal <- cpd$count_cohort_kvartal
 count_total <- cpd$count_total

 dta <- dta[order(dta$id, dta$record_date),]                                  # sorts the data
 states <- max(dta$state)                                                     # determines the number of states
 periods <- unique(dta$record_date)                                           # names of the periods
 cohorts <- unique(dta$first_date)                                            # names of the periods
 cohort_count <- length(unique(dta$first_date))                                # names of the periods
 horizon <- length(periods)                                                   # timespan of the dataset

 #-- computing incremental PDs - new defaults in each period for each cohort - absolute numbers --#
 incr_pd_cohort <- -1*(pd_cohort[,1:(ncol(pd_cohort)-1)] - pd_cohort[,2:(ncol(pd_cohort))])            # computing the increments
 incr_pd_cohort <- cbind(pd_cohort[,1],incr_pd_cohort)                                                 # adding the veru first column
 incr_pd_cohort[incr_pd_cohort < 0] <- 0                                                               # setting the last negative values to zero

 #-- computing incremental PDs - new defaults in each period for each cohort - relative numbers --#
 #incr_pd_cohort_rel <- incr_pd_cohort / count_cohort                                                   #computing relative numbers
 incr_pd_cohort_rel <- incr_pd_cohort / as.numeric(count_cohort)                                                   #computing relative numbers
 
 #-- sum of pd through cohorts for each period from beginning --#
 pom <- rep(1,nrow(incr_pd_cohort))
 incr_pd_total <- pom %*% incr_pd_cohort
 incr_pd_total_rel <- incr_pd_total / count_total


 #-- get rid of zeros --#
 pom <- incr_pd_cohort[,ncol(incr_pd_cohort):1]            #transpose the matrix
 pom[lower.tri(pom, diag=F)] <- NA                         #fill the lower triangle by NA
 incr_pd_cohort <- pom[,ncol(pom):1]                       #return the matrix to its original form


 #-- same procedure for quarters of the year --#
 incr_pd_cohort_kvartal <- -1*(pd_cohort_kvartal[,1:ncol(pd_cohort_kvartal)-1] - pd_cohort_kvartal[,2:ncol(pd_cohort_kvartal)])    # computing the increments
 incr_pd_cohort_kvartal <- cbind(pd_cohort_kvartal[,1],incr_pd_cohort_kvartal)                                                     # adding the veru first column
 incr_pd_cohort_kvartal[incr_pd_cohort_kvartal < 0] <- 0                                                                           # setting the last negative values to zero

 #-- relative incr pd for quarters --#
 incr_pd_cohort_kvartal_rel <- incr_pd_cohort_kvartal / count_cohort_kvartal             #computing relative numbers

 #-- aggregating the numbers through cohorts for each period --#s
 pom <- rep(1,nrow(incr_pd_cohort_kvartal))
 incr_pd_total_kvartal <- pom %*% incr_pd_cohort_kvartal
 count_total_kvartal <- pom %*% count_cohort_kvartal
 incr_pd_total_kvartal_rel <- incr_pd_total_kvartal / count_total_kvartal


 output <- list(cPD,
                incr_pd_total = incr_pd_total, incr_pd_total_rel = incr_pd_total_rel,
                incr_pd_cohort = incr_pd_cohort, incr_pd_cohort_rel = incr_pd_cohort_rel,
                incr_pd_total_kvartal = incr_pd_total_kvartal, incr_pd_total_kvartal_rel = incr_pd_total_kvartal_rel,
                incr_pd_cohort_kvartal = incr_pd_cohort_kvartal, incr_pd_cohort_kvartal_rel = incr_pd_cohort_kvartal_rel,
                count_cohort = count_cohort, count_total = count_total)

 class(output) <- c("list", "iPD")
 return(output)
}

####################################################################################################################################
#------------------------- plot.iPD -------------------------------------------#
plot.iPD <- function(iPD, file_name = NULL, by_cohort = F, by_kvartal = F, group_size = 1,
                     main_title="PD in time", y_lab="% of newly defaulted loans", x_lab="Months from beginning",
                     pic_width = 800, pic_height = 500)
{

 if(!("iPD" %in% class(iPD))) { stop("The input is not a cPD class object!") }

 #-- setting the variables --#
 ipd <- iPD

 incr_pd_cohort_rel = ipd$incr_pd_cohort_rel
 incr_pd_total_rel = ipd$incr_pd_total_rel

 incr_pd_cohort_kvartal_rel = ipd$incr_pd_cohort_kvartal_rel
 incr_pd_total_kvartal_rel = ipd$incr_pd_total_kvartal_rel


 #-- setting the main variable - if by_kvartal is not set to TRUE, it is incr_pd_cohort --#
 incr <- incr_pd_cohort_rel
 incr_total <- incr_pd_total_rel

 #-- setting the main variable - if by_kvartal is set to TRUE, it is incr_pd_cohort_kvartal --#
 if(by_kvartal)
 {
   incr <- incr_pd_cohort_kvartal_rel                            # % of defaults by cohort
   incr_total <- incr_pd_total_kvartal_rel                       # % of default for whole portfolio
   x_lab="Quarters from beginning"                               # changing the label of the x-axis
 }

 cohort_count = dim(incr)[1]                                     # the number of cohorts
 horizon = dim(incr)[2]                                          # the number of periods (quarters)


#-- grouping several cohorts into one for more readable pictures --#
if (group_size > 1)
{
  incr_pd_group_rel <- NULL                                   #initializing the variable
  for (i in 1:(ceiling(cohort_count/group_size)))
  {
     weight = group_size                                                                                    # setting the weight, most of the time set to group_size, except the last rows
     if(i*group_size - cohort_count > 0) { weight = (group_size - abs(i*group_size - cohort_count)) }       # for the last several rows weight is equal to the number of rows
     pom <- rep(1/weight, weight) %*% incr[(i*group_size-(group_size-1)):(min(i*group_size,cohort_count)),] # average of the rowss
     incr_pd_group_rel <- rbind(incr_pd_group_rel, pom)                                                     # adding the variable to the table
     #print(i)
  }
  group_count <- nrow(incr_pd_group_rel)                                                                    # the number of newly defined groups
}

    #-- storing the picture, if user requires it --#
    if(!is.null(file_name)) {
        png(filename=paste(file_name, ".png", sep=""), width=pic_width, height=pic_height)             # setting the filename for storing
        print(paste("The graph has been stored to the file ", file_name, ".png", sep=""))              # info for user
    }


  if(by_cohort)
  {
    #-- if group size is defined, it will set incr_pd_group_rel as data source --#
    incr_rel = incr                                                                                    # setting which variable to use
    if (group_size > 1) { incr_rel = incr_pd_group_rel}                                                # if group_size is specified, group data are used for plot

    count <- nrow(incr_rel)
                                                                                                       # getting the number of cohorts/groups
    plot(as.vector(incr_rel[1,]),
         type="l", lty=1, lwd=4, col=gray(1-(1/count)),                                                # initializing the plot, drawing the first line
         xlim=c(1, horizon), ylim=c(0,max(incr_rel, na.rm=T)), xaxt="n",
         main=main_title, xlab=x_lab, ylab=y_lab)
    x_ticks <- c(horizon, horizon-(1:(floor(horizon/6))*6))                                            # x-axis ticks, multiples of half-year, from the last point
    x_ticks <- x_ticks[length(x_ticks):1]
    axis(1, x_ticks , x_ticks)
    abline(v=x_ticks, lty="dotted", col="lightgray")                                                   # vertical and horizontal grey dotted lines
    grid(nx=NA, ny=NULL)
    #abline(h=(0:5)/50, lty="dotted", col="lightgray")
    legend("topleft", bg="white", lwd=3, lty=1, col=c(gray(1-(2/count)), "black"), legend=c("old_cohorts", "young_cohorts"))  # legend

    for (i in 2:count)
    {
      lines(as.vector(incr_rel[i,]), type="l", lty=1, lwd=4, col=gray(1-(i/count)))                    # drawing the rest of the lines
    }
  }

  if(!by_cohort)
  {
    plot(as.vector(incr_total),                                                                        # initializing the plot, drawing the first line
         type="l", lty=1, lwd=4, col="gray80",
         xlim=c(1,horizon), ylim=c(0,max(incr_total, na.rm=T)), xaxt="n",
         main=main_title, xlab=x_lab, ylab=y_lab)
    x_ticks <- c(horizon, horizon-(1:(floor(horizon/6))*6))                                            # x-axis ticks, multiples of half-year, from the last point
    x_ticks <- x_ticks[length(x_ticks):1]
    axis(1, x_ticks, x_ticks)
    abline(v=x_ticks, lty="dotted", col="lightgray")                                                   # vertical and horizontal grey dotted lines
    grid(nx=NA, ny=NULL)
    #abline(h=(0:5)/50, lty="dotted", col="lightgray")
    legend("topright", bg="white", lwd=3, lty=1, col=c("gray80"), legend=c("Newly defaulted loans"))   # legend
   }

    #-- closing the store session, otherwise the picture would not be accesible--#
    if(!is.null(file_name)) { dev.off() }
}
####################################################################################################################################
#------------------------- vintage_analysis ------------------------------------------#
vintage <- function(dta, absorbing = T)  
{
  #-- check whether the names of columns of the dataset are correct --#
  if (FALSE %in% (c("portfolio","weight") %in% colnames(dta))) { stop("non-compliant dataset, check the column names") }
  
  
  if ("weight" %in% colnames(dta)) {
    weight_exists <- T
  } else {
    weight_exists <- F
  }
  
  #-- exclude portfolio with less than 200 records --# FOR TESTING PURPOSE ONLY, VB_CZ dataset !!!
  dta <- dta[dta$portfolio %in% names(table(dta$portfolio)[table(dta$portfolio)>200]),]
  
  #-- create portfolio names --#
  portfolio_names <- as.character(unique(dta$portfolio))
  
  #-- create first default date only for defaulted records --#
  #dta[dta$id=="33040-4030001053",]
  default_state <- max(dta$state)
  dta$counter <- unlist(with(dta, by(state, id, function(x1) sapply(rle(x1)$lengths, function(x) sequence(x)  ))))    # create a counter by id, based on state
  dta$counter[dta$state != default_state] <- 0       # assign zero to non-defaulted records
  dta$default_date_vintage <- ifelse(dta$counter==1 | dta$period==0, dta$record_date, NA)     # assign record date only for counter=1 and 1st period , on id level
  dta$default_date_vintage <- unlist(by(dta$default_date_vintage, dta$id, function(x) approx(x, method="constant", xout=seq_along(x), rule=2)$y ))     # move last known value forward
  dta$default_date_vintage[dta$counter ==0 ] <- NA               # leave only default dates for relevant records 
  
  #-- create separate default period for defaulted records only 
  dta$period_vintage <- ifelse(dta$counter != 0, dta$counter - 1, 0)
  #dta$counter <- NULL         # remove redundant column
  
  
  #-- create 'triangle' matrices for each portfolio --# 
  #dta$first_date <- as.factor(dta$first_date)
  #dta$default_date_vintage <- as.factor(dta$default_date_vintage)
  
  # ---------------------------------------------------------- 
  # - Type I vintage analysis (loans as of origination date) 
  # ----------------------------------------------------------
  # calculate counts and exposure 
  count <- with(pta[dta$period==0,],  table(first_date))
  exposure <- with(dta[dta$period == 0,], tapply(weight, first_date, sum))
  # use function cPD, with standardized output 
  results <- cPD(dta, absorbing=absorbing)
  
  # -----------------------------------------------------
  # Type II vintage analysis (loans as of default date)  
  # -----------------------------------------------------
  dta_def <- dta[!is.na(dta$default_date_vintage),]
  # calculate counts and exposure
  count_def <- with(dta_def[dta_def$period_vintage == 0,], table(default_date_vintage))     
  exposure_def <- with(dta_def[dta_def$period_vintage==0,], tapply(weight, default_date_vintagentage, sum, na.rm=T))
  
  # -- final results - absolute default by cohort and period -- #
  pd_cohort_period <- with(dta_def, table(default_date_vintage, def_period))                      # only records in default, number of record for each cohort and period
  count_cohort_period <- with(dta, table(first_date, period))
  if(weight_exists) {
  pd_cohort_period_w <- with(dta_def, tapply(weight, list(default_date_vintage, def_period), sum, na.rm=T ) )                      # only records in default, number of record for each cohort and period                           
  }
  
  #-- adds missing ROWS, for which there werent any default --#
  pom_periods <- rownames(count_cohort_period)[! rownames(count_cohort_period) %in% rownames(pd_cohort_period)]    #finds missing cohorts
  if (length(pom_periods) >0 ) {
     pd_cohort_period <- rbind(pd_cohort_period, matrix(0, nrow=length(pom_periods), ncol=ncol(pd_cohort_period)))    #adds rows with zeros
     rownames(pd_cohort_period)[nrow(pd_cohort_period):(nrow(pd_cohort_period)-length(pom_periods)+1)] <- pom_periods #sets the names of the new rows
     pd_cohort_period <- pd_cohort_period[order(rownames(pd_cohort_period)),]                                         #sorts data
     #rownames(pd_cohort_period) == rownames(count_cohort_period)                                                      #check whether pd and count have same names
     
     if (weight_exists) {
       pd_cohort_period_w <- rbind(pd_cohort_period_w, matrix(0, nrow=length(pom_periods), ncol=ncol(pd_cohort_period_w)))    #adds rows with zeros
       rownames(pd_cohort_period_w)[nrow(pd_cohort_period_w):(nrow(pd_cohort_period_w)-length(pom_periods)+1)] <- pom_periods #sets the names of the new rows
       pd_cohort_period_w <- pd_cohort_period_w[order(rownames(pd_cohort_period_w)),]                                         #sorts data
       #rownames(pd_cohort_period_w) == rownames(count_cohort_period)                                                      #check whether pd and count have same names
     }
  }
  
  #-- adds missing COLUMNS, for which there werent any default --#
  pom_periods_col <- colnames(count_cohort_period)[! colnames(count_cohort_period) %in% colnames(pd_cohort_period)]    #finds missing cohorts
  if(length(pom_periods_col) > 0) {
  pd_cohort_period <- cbind(pd_cohort_period, matrix(0, ncol=length(pom_periods_col), nrow=nrow(pd_cohort_period)))    #adds rows with zeros
  colnames(pd_cohort_period)[ncol(pd_cohort_period):(ncol(pd_cohort_period)-length(pom_periods_col)+1)] <- pom_periods_col #sets the names of the new rows
  pd_cohort_period <- pd_cohort_period[,order(as.integer(colnames(pd_cohort_period)))]                                         #sorts data
  #colnames(pd_cohort_period) == colnames(count_cohort_period)                                                      #check whether pd and count have same names
  
  if (weight_exists) {
   pd_cohort_period_w <- cbind(pd_cohort_period_w, matrix(0, ncol=length(pom_periods_col), nrow=nrow(pd_cohort_period_w)))    #adds rows with zeros
   colnames(pd_cohort_period_w)[ncol(pd_cohort_period_w):(ncol(pd_cohort_period_w)-length(pom_periods_col)+1)] <- pom_periods_col #sets the names of the new rows
   pd_cohort_period_w <- pd_cohort_period_w[,order(as.integer(colnames(pd_cohort_period_w)))]                                         #sorts data
   #rownames(pd_cohort_period_w) == rownames(count_cohort_period)                                                      #check whether pd and count have same names
  }
  }
  
  #counts for division     
  temp <- matrix(0, nrow=1, ncol=nrow(count_cohort_period), dimnames=list("",rownames(count_cohort_period)))   # auxiliary matrix
  
  exposure_def <- merge(t(as.matrix(exposure_def)), temp, all=T)           # modify exposure_def table
  exposure_def <- exposure_def[-1,order(as.integer(colnames(exposure_def)))]
  exposure_def[is.na(exposure_def) ] <- 0
  
  count_def <- merge(t(as.matrix(count_def)), temp, all=T)           # modify exposure_def table
  count_def <- count_def[-1,order(as.integer(colnames(count_def)))]
  count_def[is.na(count_def) ] <- 0
  
  count_cohort_w <- matrix(as.numeric(exposure_def), nrow(pd_cohort_period_w), ncol(pd_cohort_period_w), byrow=F)               # temporary matrix where in rows are the sums of the transitions from the state
  count_cohort <- matrix(as.numeric(count_def), nrow(pd_cohort_period_w), ncol(pd_cohort_period_w), byrow=F)
  
  # remove NAa if any
  pd_cohort_period[is.na(pd_cohort_period)] <- 0
  pd_cohort_period_w[is.na(pd_cohort_period_w)] <- 0
  
  #-- final tables - relative defaults by cohort and period --#
  cum_pd_cohort_period <- pd_cohort_period / count_cohort
  if (weight_exists) { cum_pd_cohort_period_w <- pd_cohort_period_w / count_cohort_w }
  
  #generate NAs on diagonal matrix 
  generate_NA <- function(input) {
     pom <- as.matrix(input)
     pom <- as.matrix(apply(t(pom),1,rev))
     pom[is.na(pom)] <- 0
     pom[upper.tri(pom, diag=F)] <- NA
     input <- as.matrix(apply(t(pom),1,rev))
     return(input)
  }
  
  abs_cohort_period <- generate_NA(pd_cohort_period)
  rel_cohort_period <- generate_NA(cum_pd_cohort_period)
  abs_cohort_period_w <- generate_NA(pd_cohort_period_w)
  rel_cohort_period_w <- generate_NA(cum_pd_cohort_period_w)
  
  #-- put all results into a list 
  outlist <- list()
  outlist <- list( cbind(count, exposure, results$pd_cohort )  , 
                       cbind(count, exposure, results$pd_cohort_w),
                       cbind(count, exposure, results$cum_pd_cohort),
                       cbind(count, exposure, results$cum_pd_cohort_w), 
                       cbind(t(count_def), t(exposure_def), abs_cohort_period),
                       cbind(t(count_def), t(exposure_def), abs_cohort_period_w),
                       cbind(t(count_def), t(exposure_def), rel_cohort_period),
                       cbind(t(count_def), t(exposure_def), rel_cohort_period_w)
                       )
  
  #-- assign names to matrices --#
  names(outlist) <- c("absolute", "absolute_weighted", "relative", "relative_weighted", 
                          "absolute_default", "absolute_weighted_default", "relative_default","relative_weighted_default")    
   
  return(outlist)
} 
  

####################################################################################################################################
#------------------------- date difference ------------------------------------------#

date_diff <- function(start_date, end_date) {
  
  months_start <- as.numeric(substr(start_date,1,4))*12 + as.numeric(substr(start_date,5,6))
  months_end <-  as.numeric(substr(end_date,1,4))*12 + as.numeric(substr(end_date,5,6))
  period_diff <- months_end - months_start
  
  return(period_diff)
}

####################################################################################################################################
#------------------------- date addition ------------------------------------------#

date_add <- function(start_date, diff_month) {
  start_year <- as.numeric(substr(start_date,1,4))
  start_month <- as.numeric(substr(start_date,5,6))
  
  start_count_months <- start_year*12 + start_month
  end_count_months <- start_count_months + diff_month

  end_month <- ifelse(end_count_months%%12 == 0, 12, end_count_months%%12) 
  end_year <- (end_count_months - end_month) / 12
  
  
  if(end_month < 10) { end_month <- paste("0",end_month, sep="")
  } 
  
  end_date <- as.numeric(paste(end_year, end_month, sep=""))
      
  return(end_date)  
}



####################################################################################################################################
# ---------------------- return rom default -----------------------------------------#

return_from_default <- function(dta) {
  
  # -- check if columns are present in the dataset --#
  if(FALSE %in% (c("state","default_date") %in% colnames(dta))) { stop("Non-compliant dataset. Columns 'state' and 'default_date' are missing. Please run setStates and expand_dataset functions first.")}
  
  #-- retreive default state from dataset --#
  if (!max(dta$state) %in% unique(dta$state[dta$record_date == max(dta$record_date)])) {         # state "repayment" cannot appear in the last period
    default_state <- max(dta$state)-1   
  } else {
    default_state <- max(dta$state)
  }
    
  #-- number of defaults --#
  defaulted <- length(unique(dta$id[dta$state == default_state]))
  #defaulted <- length(unique(dta$id[!is.na(dta$default_date)]))
    
  #-- create cured date --#
  pom_dta <- dta[dta$record_date >= dta$default_date & !is.na(dta$default_date), ]                           # only records after default
  cure_date <- with(pom_dta[pom_dta$state < default_state, ], tapply(record_date ,id, min)  )                # date of first cure, after default
  pom_match <- match(dta$id, rownames(cure_date))
  dta$cure_date <- as.numeric(cure_date[pom_match])
  
  #dta$counter <- unlist(with(dta, by(state, id, function(x1)  sapply(rle(x1)$lengths , function(x) sequence(x)   )))) 
  #dta$counter[dta$record_date <  dta$cure_date ] <- 0
  
  #-- create 2nd default date --# 
  default_date_2 <- with(dta[dta$record_date>= dta$cure_date & !is.na(dta$cure_date) & dta$state == default_state,], tapply(record_date,id, min))
  pom_match <- match(dta$id, rownames(default_date_2))
  dta$default_date_2 <- as.numeric(default_date_2[pom_match])
    
  #-- number of "cured" loans --#
  cured <- length(unique(dta$id[!is.na(dta$cure_date)]))
  #cured <- length(unique(dta$id[dta$record_date > dta$default_date & dta$state < default_state]))
  #33040-4030001053
  
  #-- create 12 months since cure date --#
  dta$cure_date_12[!is.na(dta$cure_date)] <- paste(floor((as.numeric(substr(dta$cure_date[!is.na(dta$cure_date)],1,4))*12 + 
                                        as.numeric(substr(dta$cure_date[!is.na(dta$cure_date)],5,6)) + 12 ) / 12) ,
                                        sprintf( "%02d", (as.numeric(substr(dta$cure_date[!is.na(dta$cure_date)],1,4))*12 +  
                                           as.numeric(substr(dta$cure_date[!is.na(dta$cure_date)],5,6)) + 12 ) %% 12 ), sep="")  
  dta$cure_date_12 <- as.numeric(dta$cure_date_12)
    
  #-- number of re-defaulted in next 12 months --# 
  redefaulted_12 <- length(unique(dta$id[dta$default_date_2 > dta$cure_date & dta$default_date_2 <= dta$cure_date_12 & !is.na(dta$default_date_2) ]))
  
  #-- number of re-defaulted loans at all --#
  redefaulted <- length(unique(dta$id[!is.na(dta$default_date_2)]))
  
  #-- average length of default before return (before return to "cured" state) --#
  default_length <- date_diff(dta$default_date[!is.na(dta$cure_date) & !duplicated(dta$id)], dta$cure_date[!is.na(dta$cure_date) & !duplicated(dta$id)] )
  avg_length <- sum(default_length)/length(default_length)
  
  #-- statistics: create a table with results --#
  stat <- matrix(NA, nrow=5,ncol=2)                                                   # create empty matrix
  stat[1:4,1] <- c(defaulted, cured, redefaulted_12, redefaulted)                     # add items to 1st column
  stat[2:4,2] <- round(c(stat[2,1]/stat[1,1], stat[3,1]/stat[2,1], stat[4,1]/stat[2,1])*100,1)      # add ratios to 2nd column
  stat[5,1] <- round(avg_length,1)                                                    # add average length of default
  stat <- as.data.frame(stat)                                            
  stat$V1 <- as.factor(stat$V1)                                                       # convert data fields
  stat$V2 <- as.character(stat$V2)
  stat$V2 <- c("\x20" , paste(stat$V2[2:4],"%",sep=""), "\x20" )                      # change fields to blank space 
  rownames(stat) <- format(c("number of defaulted loans", "number of cured loans","number of re-defaulted loans within 12 months (out of cured)", 
                       "total number of re-defaulted loans (out of cured)", "average time (periods) in default before cure") , justify="right")           # format columns 
  colnames(stat) <- format(c("value","ratio"), width=8, justify="centre")              # format rowns
  stat <- format(stat, width=8, justify = "centre")                                   # adjust table view
  
  return(stat)
}

####################################################################################################################################
#------------------------- vintage_analysis ------------------------------------------#
overview <- function(dta)  {
  
  #-- check whether the names of columns of the dataset are correct --#
  if (FALSE %in% (expanded_dataset_column_names %in% colnames(dta))) { stop("Non-compliant dataset, please check the column names!") }
  
  record_dates <- unique(sort(dta$record_date)) 
  
  if ("weight" %in% colnames(dta)) {
    weight_exists <- T
  } else {
    weight_exists <- F
  }
  
  #-- overview --#
  pom_dta <- dta[dta$record_date == record_dates[length(record_dates)], ]

  overview <- cbind (   with(pom_dta, unique(sort(state))),
                        with(pom_dta, tapply(id, state, length  )),
                        with(pom_dta, tapply(id, state, length  ))/ length(pom_dta$id)  ) 
  overview <- as.data.frame(overview)
  colnames(overview) <- c("state","number of loans","delinquency rate")
 # rownames(overview) <- rep("", nrow(overview))
  overview[,length(colnames(overview))] <- paste(round(overview[,length(colnames(overview))]*100,1),"%",sep="")
  
  if(weight_exists) {
     overview <- cbind (  with(pom_dta, unique(sort(state))),
                          with(pom_dta, tapply(id, state, length  )),
                          with(pom_dta, tapply(weight, state, sum, na.rm=T)), 
                          with(pom_dta, tapply(id, state, length  ))/ length(pom_dta$id)  ) 
    overview <- as.data.frame(overview)
    colnames(overview) <- c("state","number of loans","exposure","delinquency rate")
   # rownames(overview) <- rep("", nrow(overview))
    overview[,length(colnames(overview))] <- paste(round(overview[,length(colnames(overview))]*100,1),"%",sep="")
   }
  
  return(overview)
  
}
  
# x <- scan(nmax=1)
# 
# # ---  historically , for each state ---
# # number of loans 
# x <- tapply(dta$id, list(dta$state, dta$record_date), length)
# #x <- as.data.frame(x)
# x <- as.data.frame(x, dimnames=list(NULL,c(colnames(x))))
# x$state <- rownames(x)
# x <- reshape(x, varying=colnames(x)[1:40], v.names="count", timevar="year", times=colnames(x)[1:40], direction="long")
# 
# y <- tapply(dta$weight, list(dta$state, dta$record_date), sum, na.rm=T)
# y <- as.data.frame(y, dimnames=list(NULL,c(colnames(y))))
# y$state <- rownames(y)
# y <- reshape(y, varying=colnames(y)[1:40], v.names="exposure", timevar="year", times=colnames(y)[1:40], direction="long")
# 
# z <- tapply(dta$id, list(dta$state, dta$record_date), length)
# z <- round(z/matrix(colSums(z, na.rm=T), nrow=nrow(z), ncol=ncol(z), byrow=T),2)
# z <- as.data.frame(z, dimnames=list(NULL,c(colnames(z))))
# z$state <- rownames(z)
# z <- reshape(z, varying=colnames(z)[1:40], v.names="delinquency", timevar="year", times=colnames(z)[1:40], direction="long")
# 
# 
# output <- cbind(x[,c("state","year","count")], y[,c("exposure")], z[,c("delinquency")])
# rownames(output) <- NULL
# colnames(output) <- c("state","year","count","exposure","delinquency")
# 
# output$state <- as.factor(output$state)
# output$year <- as.numeric(output$year)
# output$dates <- as.Date(paste(output$year,"01",sep=""), format="%Y%m%d", tz="")
# output$year2 <- as.numeric(trunc(output$year/100))
# levels(output$state) <- c("OK", "OK!", "OK_D", "30","60","90","D","Rep")
# 
# library(zoo)
# output$dates2 <- as.yearmon(output$dates)                        
# strftime(as.character(output$year), format="%Y-%m", tz="")
# as.Date(strptime("2001-10", format = "%Y-%m-%d"))
# 
# 
# Graph <- gvisMotionChart(output, idvar="state", timevar="dates", options=list(height=600, width=1200))
# plot(Graph)
# 
# library(googleVis)
# Motion=gvisMotionChart(Fruits, idvar="Fruit", timevar="Year", options=list(height=450, width=1200))
# # Display chart
# plot(Motion) 
# 
# 
# runApp("C:\\Projects\\Testing Area\\Shiny\\")

