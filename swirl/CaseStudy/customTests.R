# So swirl does not repeat execution of plot commands
AUTO_DETECT_NEWVAR <- FALSE

# Returns TRUE if e$expr matches any of the expressions given
# (as characters) in the argument.
ANY_of_exprs <- function(...){
  e <- get("e", parent.frame())
  any(sapply(c(...), function(expr)omnitest(expr)))
}

equiv_val <- function(correctVal){
  e <- get("e", parent.frame()) 
  #print(paste("User val is ",e$val,"Correct ans is ",correctVal))
  isTRUE(all.equal(correctVal,e$val))
  
}

obliterate <- function(vName){
  e <- get("e", parent.frame())
#     print(paste("Hello from obliterate",vName,sep=" "))
#     if(exists(vName,envir=globalenv()))print(paste(vName," exists"))
#     else print(paste(vName," doesn't exist"))
  suppressWarnings(try(rm(list=c(vName),envir=globalenv()),silent=TRUE))
#     if(exists(vName,envir=globalenv()))print(paste(vName," exists"))
#     else print(paste(vName," doesn't exist"))
  idx <- which(names(e$snapshot)==vName)
#      print(paste("Hello from obliterate idx of ",vName," is ",idx,sep=" "))
  suppressWarnings(try(e$snapshot <- e$snapshot[-idx],silent=TRUE))
  return(TRUE)
}

# Get the swirl state
getState <- function(){
  # Whenever swirl is running, its callback is at the top of its call stack.
  # Swirl's state, named e, is stored in the environment of the callback.
  environment(sys.function(1))$e
}

# Get the value which a user either entered directly or was computed
# by the command he or she entered.
getVal <- function(){
  getState()$val
}

# Get the last expression which the user entered at the R console.
getExpr <- function(){
  getState()$expr
}

coursera_on_demand <- function(){
  selection <- getState()$val
  if(selection == "Yes"){
    email <- readline("What is your email address? ")
    token <- readline("What is your assignment token? ")
    
    payload <- sprintf('{  
      "assignmentKey": "BEPieK8gEeWxaw7Jay15BQ",
      "submitterEmail": "%s",  
      "secret": "%s",  
      "parts": {  
        "G5iV8": {  
          "output": "correct"  
        }  
      }  
    }', email, token)
    url <- 'https://www.coursera.org/api/onDemandProgrammingScriptSubmissions.v1'
  
    respone <- httr::POST(url, body = payload)
    if(respone$status_code >= 200 && respone$status_code < 300){
      message("Grade submission succeeded!")
    } else {
      message("Grade submission failed.")
      message("Press ESC if you want to exit this lesson and you")
      message("want to try to submit your grade at a later time.")
      return(FALSE)
    }
  }
  TRUE
}