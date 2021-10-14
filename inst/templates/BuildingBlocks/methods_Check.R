
  # Allowed methods for this function
  allowedmethods <- c("XXXXX", "YYYYY", "ZZZZZ")

  if(!(method %in% allowedmethods)){
    stop("Method needs to be one of the following: 'XXXXX', 'YYYYY', 'ZZZZZ'.", call.=FALSE)
  }