MapAndChloroData <- function() {
  moz.p <- readOGR(dsn="data/MOZ_adm_shp", "MOZ_adm3", verbose=F)
  moz.p$rn <- row.names(moz.p)
  
  moz.data <- as.data.table(moz.p@data)
  csv.data <- as.data.table(read.csv("data/Maps_9-1.csv", header=T))
  
  csv.data <- csv.data[, .(gadmPA, zScore, zpor, zelo, zemk, zech)]
  setnames(csv.data, "gadmPA", "NAME_3")
  
  setkey(csv.data, NAME_3)
  setkey(moz.data, NAME_3)
  
  moz.data <- csv.data[moz.data]
  
  setkey(moz.data, rn)
  moz.p@data <- moz.data[row.names(moz.p)]
  
  return(moz.p)
  
}

chloro.raw <- list(
  list(label='Bilingualism',
       colname='zScore'),
  list(label='Elomwe',
       colname='zelo'),
  list(label='Emakhua',
       colname='zemk'),
  list(label='Echuwabo',
       colname='zech'),
  list(label='Portuguese',
       colname='zpor')
)
