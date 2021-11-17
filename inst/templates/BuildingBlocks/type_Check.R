
  # Check whether the input is of type {{{ object_type }}}
  if(!('{{{ object_type }}}' %in% type)){
    stop("Only objects of type '{{{ object_type }}}' are allowed in this function.", call.=FALSE)
  }
