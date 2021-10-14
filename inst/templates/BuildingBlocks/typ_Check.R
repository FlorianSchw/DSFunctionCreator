
  # Check whether the input is of type {{{ object_typ }}}
  if(!('{{{ object_typ }}}' %in% typ)){
    stop("Only objects of type '{{{ object_typ }}}' are allowed in this function.", call.=FALSE)
  }