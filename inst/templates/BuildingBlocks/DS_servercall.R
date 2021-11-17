
  # call the server side function that does the operation
  cally <- call("{{{ function_name }}}DS", {{{ input_object_assign }}})
  DSI::datashield.{{{ datashield_type }}}(datasources, newobj, cally)


}
