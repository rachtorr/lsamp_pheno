##############################################################################################
#' @title Workflow for downloading dp0p data from S3

#' @author
#' David Durden \email{eddy4R.info@gmail.com}

#' @description
#' Workflow. Downloading dp0p data from S3.

#' @param Currently none

#' @return Currently none

#' @references

#' @keywords eddy-covariance, NEON

#' @examples Currently none

#' @seealso Currently none

# changelog and author contributions / copyrights
#   David (2020-01-25)
#     original creation
##############################################################################################

#site to download data for
site <- "WREF"
#domain - see map of domains here https://www.neonscience.org/field-sites/about-field-sites
dom <- "D16"

#SAE system (ecte vs. ecse)
#---ecte = turbulent exchange 
#---ecse = storage exchange 
# see website to 
sys <- "ecte"

#Create download folder, create if it doesn't exist
# can change to a place you recognize 
DirDnld <- paste0("~/Documents/Github/lsamp_pheno/data/eddy/data/pfit/inp/",site)
if(!dir.exists(DirDnld)) dir.create(DirDnld, recursive = TRUE)

#Create data download string
DateBgn <- as.Date("2022-07-18")
DateEnd <- as.Date("2022-08-01")
DateSeq <- seq.Date(from = DateBgn,to = DateEnd, by = "day")
PrdWndwDnld <- base::as.character(DateSeq)



#Filename base
fileInBase <- paste0("NEON.",dom,".",site,".IP0.00200.001.",sys,".")

#Create URL for data files
urlDnld <- paste0("https://storage.googleapis.com/neon-sae-files/ods/dataproducts/IP0/",PrdWndwDnld,"/",site,"/",fileInBase,PrdWndwDnld,".l0p.h5.gz")

#Download filename (full path)
fileDnld <-  paste0(DirDnld,"/",base::toupper(sys),"_dp0p_",site,"_",PrdWndwDnld,".h5.gz")

#Download files
sapply(seq_along(urlDnld), function(x){
  download.file(url = urlDnld[x], destfile = fileDnld[x])
})

#ungzip file
gzFile <- list.files(DirDnld, pattern = ".gz", full.names = TRUE)
lapply(gzFile, R.utils::gunzip)

