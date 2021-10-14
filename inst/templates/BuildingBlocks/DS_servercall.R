
  # call the server side function that does the operation
  cally <- call("{{{ function_name }}}DS", {{{ input_object }}})
  DSI::datashield.{{{ datashield_typ }}}(datasources, newobj, cally)


}