#-------------------------------------------------------------------------------
# Copyright (c) 2019-2022 University of Newcastle upon Tyne. All rights reserved.
#
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------
#
# Datashield test suite set up
#

# This is the setup.R file which loads necessary DataSHIELD packages and connects to the Opal Server on the VM
# Be aware that in order to conduct testing the VM needs to be started prior

library(DSOpal)
library(dsBaseClient)
library(DSI)


source("connection_to_datasets/login_details.R")
source("connection_to_datasets/init_testing_datasets.R")
source("connection_to_datasets/init_studies_datasets.R")


