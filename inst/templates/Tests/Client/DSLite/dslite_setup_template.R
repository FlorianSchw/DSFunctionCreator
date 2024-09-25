
library(dplyr)


#### Reading in datasets
Data1 <- as.data.frame(read.csv("testdata/{{{ input_dataset }}}1.csv", sep = ",", dec = "."))
Data2 <- as.data.frame(read.csv("testdata/{{{ input_dataset }}}2.csv", sep = ",", dec = "."))
Data3 <- as.data.frame(read.csv("testdata/{{{ input_dataset }}}3.csv", sep = ",", dec = "."))

#### Defining factor variables for transformation from numeric
factor_variables <- unlist(strsplit(c("{{{ factor_variables_transform }}}"), split = ","))

Data1 <- Data1 |>
  mutate(across(all_of(factor_variables), ~factor(.x)))

Data2 <- Data2 |>
  mutate(across(all_of(factor_variables), ~factor(.x)))

Data3 <- Data3 |>
  mutate(across(all_of(factor_variables), ~factor(.x)))


#### Defining the server-side data
dslite.server <<- DSLite::newDSLiteServer(tables=list(Data1=Data1,
                                                      Data2=Data2,
                                                      Data3=Data3))

#### Defining the server-side packages
dslite.server$config(DSLite::defaultDSConfiguration(include=c("dsBase", "{{{ dsPackage }}}")))
dslite.server$profile()

#### Building the 3 different DSLite Servers with the different datasets
logindata.dslite.data <- data.frame(server = c("Server1", "Server2", "Server3"),
                                    url = c("dslite.server", "dslite.server", "dslite.server"),
                                    user = "",
                                    password = "",
                                    table = c("Data1", "Data2", "Data3"),
                                    options = "",
                                    driver = c("DSLiteDriver", "DSLiteDriver", "DSLiteDriver"))


#### Login to the 3 different DSLite Servers
conns <<- DSI::datashield.login(logindata.dslite.data, assign=TRUE, symbol = "D")

