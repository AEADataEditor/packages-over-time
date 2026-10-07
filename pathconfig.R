# ###########################
# CONFIG: define paths and filenames for later reference
# ###########################

# Change the basepath depending on your system

basepath <- rprojroot::find_rstudio_root_file()

# Main directories
datapath     <- file.path(basepath, "data")
interwrk     <- file.path(datapath,"interwrk")
crossrefloc  <- file.path(datapath,"crossref")
migrationloc <- file.path(datapath,"migration")
dataciteloc  <- file.path(datapath,"datacite")
metadataloc  <- file.path(datapath,"metadata")
Outputs	     <- file.path(basepath,"outputs")
Figures      <- file.path(Outputs,"figures")

programs <- file.path(basepath,"programs")

for ( dir in list(datapath,interwrk,crossrefloc,migrationloc,dataciteloc,metadataloc,Outputs,Figures)){
	if (file.exists(dir)){
	} else {
	dir.create(file.path(dir))
	}
}
