require(plyr)

##########################################################
# Enter the year for which you wish to run the analysis. 
# At the time of writing, valid dates: 2004-2013
# Previous years used diferent MRFSS Datasets, 
# which will not work here.
year <- "2013"
##########################################################

# This is used as a marker for the date the data were downloaded
today = as.Date(Sys.time())
#today = gsub('-', '', today)

# Form the url from the year variabl and the url template
url <- paste0("http://www.st.nmfs.noaa.gov/st1/recreational/MRIP_Survey_Data/CSV/PS_", year, "_CSV.zip")

# create a temporary directory
td = tempdir()
# create the placeholder file
tf = tempfile(tmpdir=td, fileext=".zip")
# download into the placeholder file
download.file(url, tf)
# Unzip the file into the temp directory
unzip(tf, exdir = td)

# Create a list of the filenames that were unzipped
files <- list.files(td, pattern = ".csv")
# Create a list of the names without .csv. This will correspond to our df names
names <- gsub(pattern='\\.csv$', '', files, ignore.case=T)

setwd(td)
# Load the files we just unziped into dataframes in R,
for( i in 1:length(names)){
        a <- paste(names[i], ' <- read.csv( file=\'', files[i], '\', header=TRUE, sep=\',\')', sep='')
        # print( a )
        eval(parse( text=a ))   
}

# Subset the df's where ST=='37' for fips code of State of North Carolina
for (i in 1:length(names)){
        b <- paste0(names[i], ' <-', names[i], '[', names[i], '$ST==\'37\',]')
        eval(parse( text=b ))
}


# This function merges the df's passed to it based on the ID_CODE variable
mergeDf <- function(catch,trip, i){
        b <- paste0('merged.w',i, ' <- merge(', paste0(catch),',', paste0(trip), ', by=\'ID_CODE\')') 
        print(b)
        return(eval(parse( text=b )))
}

# create dataframes and populate them by passing to mergedf. Then arrange them by id_code. For each wave.
for (i in 1:6){
        d <- paste0("merged.w",i," <- data.frame()")
        a <- paste0("merged.w",i, " <- mergeDf(paste0('catch_', year, '",i,"'), paste0('trip_', year, '",i,"'), ",i,")")
        b <- paste0("merged.w",i,"$common <- factor(merged.w",i,"$common)")
        c <- paste0("merged.w",i, " <- arrange(merged.w",i,", desc(ID_CODE))")
        eval(parse( text=d ))
        eval(parse( text=a ))
        eval(parse( text=b ))
        eval(parse( text=c ))        
}



# create vectors of county codes to subset by. These are the arbitrary divisions of N,S,Central in nc.
south <- c(19, 95, 129, 133, 141, 147)
north <- c(15, 29, 41, 53, 55, 139, 143, 177, 187)
central <- c(13, 31, 49, 137)


for (i in 1:6){
        #subset by county code into north and south NC
        a <- paste0("merged.w",i,".south <- merged.w",i,"[merged.w",i,"$CNTY %in% south,]")
        b <- paste0("merged.w",i,".north <- merged.w",i,"[merged.w",i,"$CNTY %in% north,]")
        c <- paste0("merged.w",i,".central <- merged.w",i,"[merged.w",i,"$CNTY %in% central,]")
        # Parse and eval all the strings
        eval(parse( text=a ))
        eval(parse( text=b ))
        eval(parse( text=c ))
        
        # Redefine the common names as factors. This will drop unused levels.
        d <- paste0("merged.w",i,".south$common <- factor(merged.w",i,".south$common)")
        e <- paste0("merged.w",i,".north$common <- factor(merged.w",i,".north$common)")
        f <- paste0("merged.w",i,".central$common <- factor(merged.w",i,".central$common)")
        eval(parse( text=d ))
        eval(parse( text=e ))
        eval(parse( text=f ))
        
        # create a vector of fish names caught from each area
        # I used m here since I couldnt reuse i
        g <- paste0("unique.w",i,".south <- levels(merged.w",i,".south$common)")
        h <- paste0("unique.w",i,".north <- levels(merged.w",i,".north$common)")
        m <- paste0("unique.w",i,".central <- levels(merged.w",i,".central$common)")
        eval(parse( text=g ))
        eval(parse( text=h ))
        eval(parse( text=m ))
        
        # Create a vector of the number of unique fish species caught
        j <- paste0("num.caught.south.w",i," <- length(unique.w",i,".south)")
        k <- paste0("num.caught.north.w",i," <- length(unique.w",i,".north)")
        l <- paste0("num.caught.central.w",i," <- length(unique.w",i,".central)")
        eval(parse( text=j ))
        eval(parse( text=k ))
        eval(parse( text=l ))

}






