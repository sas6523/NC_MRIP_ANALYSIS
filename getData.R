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
# Load the files we just unziped into dataframes in R, then subset the df by state=37
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

# Create empty df's to later populate
merged.w1 <- data.frame()
merged.w2 <- data.frame()
merged.w3 <- data.frame()
merged.w4 <- data.frame()
merged.w5 <- data.frame()
merged.w6 <- data.frame()

# This function merges the df's passed to it based on the ID_CODE variable
mergeDf <- function(catch,trip, i){
        b <- paste0('merged.w',i, ' <- merge(', paste0(catch),',', paste0(trip), ', by=\'ID_CODE\')') 
        print(b)
        return(eval(parse( text=b )))
}


# TODO: Make th next 3 blocks into one for loop!
# Merge catch and trip datasets by ID_CODE
merged.w1 <- mergeDf(paste0('catch_', year, '1'), paste0('trip_', year, '1'), 1)
merged.w2 <- mergeDf(paste0('catch_', year, '2'), paste0('trip_', year, '2'), 2)
merged.w3 <- mergeDf(paste0('catch_', year, '3'), paste0('trip_', year, '3'), 3)
merged.w4 <- mergeDf(paste0('catch_', year, '4'), paste0('trip_', year, '4'), 4)
merged.w5 <- mergeDf(paste0('catch_', year, '5'), paste0('trip_', year, '5'), 5)
merged.w6 <- mergeDf(paste0('catch_', year, '6'), paste0('trip_', year, '6'), 6)

# Redefine the common names as factors. This will drop unused levels.
merged.w1$common <- factor(merged.w1$common)
merged.w2$common <- factor(merged.w2$common)
merged.w3$common <- factor(merged.w3$common)
merged.w4$common <- factor(merged.w4$common)
merged.w5$common <- factor(merged.w5$common)
merged.w6$common <- factor(merged.w6$common)

# arrange by ID_CODE
merged.w1 <- arrange(merged.w1, desc(ID_CODE))
merged.w2 <- arrange(merged.w2, desc(ID_CODE))
merged.w3 <- arrange(merged.w3, desc(ID_CODE))
merged.w4 <- arrange(merged.w4, desc(ID_CODE))
merged.w5 <- arrange(merged.w5, desc(ID_CODE))
merged.w6 <- arrange(merged.w6, desc(ID_CODE))

# create vectors of county codes to subset by
south <- c(19, 95, 129, 133, 141, 147)
north <- c(15, 29, 41, 53, 55, 139, 143, 177, 187)
central <- c(13, 31, 49, 137)

###############################################
# The Following needs to be rewritten in functions
###############################################


#subset by county code into north and south NC
merged.w1.south <- merged.w1[merged.w1$CNTY %in% south,]
merged.w1.north <- merged.w1[merged.w1$CNTY %in% north,]
merged.w1.central <- merged.w1[merged.w1$CNTY %in% central,]

# Redefine the common names as factors. This will drop unused levels.
merged.w1.south$common <- factor(merged.w1.south$common)
merged.w1.north$common <- factor(merged.w1.north$common)
merged.w1.central$common <- factor(merged.w1.central$common)

# create a vector of fish names caught from each area
unique.w1.south <- levels(merged.w1.south$common)
unique.w1.north <- levels(merged.w1.north$common)
unique.w1.central <- levels(merged.w1.central$common)

# Create a vector of the number of unique fish species caught
num.caught.south.w1 <- length(unique.w1.south)
num.caught.north.w1 <- length(unique.w1.north)
num.caught.central.w1 <- length(unique.w1.central)

#subset by county code into north and south NC
merged.w2.south <- merged.w2[merged.w2$CNTY %in% south,]
merged.w2.north <- merged.w2[merged.w2$CNTY %in% north,]
merged.w2.central <- merged.w2[merged.w2$CNTY %in% central,]

# Redefine the common names as factors. This will drop unused levels.
merged.w2.south$common <- factor(merged.w2.south$common)
merged.w2.north$common <- factor(merged.w2.north$common)
merged.w2.central$common <- factor(merged.w2.central$common)

# create a vector of fish names caught from each area
unique.w2.south <- levels(merged.w2.south$common)
unique.w2.north <- levels(merged.w2.north$common)
unique.w2.central <- levels(merged.w2.central$common)

# Create a vector of the number of unique fish species caught
num.caught.south.w2 <- length(unique.w2.south)
num.caught.north.w2 <- length(unique.w2.north)
num.caught.central.w2 <- length(unique.w2.central)

#subset by county code into north and south NC
merged.w3.south <- merged.w3[merged.w3$CNTY %in% south,]
merged.w3.north <- merged.w3[merged.w3$CNTY %in% north,]
merged.w3.central <- merged.w3[merged.w3$CNTY %in% central,]

# Redefine the common names as factors. This will drop unused levels.
merged.w3.south$common <- factor(merged.w3.south$common)
merged.w3.north$common <- factor(merged.w3.north$common)
merged.w3.central$common <- factor(merged.w3.central$common)

# create a vector of fish names caught from each area
unique.w3.south <- levels(merged.w3.south$common)
unique.w3.north <- levels(merged.w3.north$common)
unique.w3.central <- levels(merged.w3.central$common)

# Create a vector of the number of unique fish species caught
num.caught.south.w3 <- length(unique.w3.south)
num.caught.north.w3 <- length(unique.w3.north)
num.caught.central.w3 <- length(unique.w3.central)

#subset by county code into north and south NC
merged.w4.south <- merged.w4[merged.w4$CNTY %in% south,]
merged.w4.north <- merged.w4[merged.w4$CNTY %in% north,]
merged.w4.central <- merged.w4[merged.w4$CNTY %in% central,]

# Redefine the common names as factors. This will drop unused levels.
merged.w4.south$common <- factor(merged.w4.south$common)
merged.w4.north$common <- factor(merged.w4.north$common)
merged.w4.central$common <- factor(merged.w4.central$common)

# create a vector of fish names caught from each area
unique.w4.south <- levels(merged.w4.south$common)
unique.w4.north <- levels(merged.w4.north$common)
unique.w4.central <- levels(merged.w4.central$common)

# Create a vector of the number of unique fish species caught
num.caught.south.w4 <- length(unique.w4.south)
num.caught.north.w4 <- length(unique.w4.north)
num.caught.central.w4 <- length(unique.w4.central)

#subset by county code into north and south NC
merged.w5.south <- merged.w5[merged.w5$CNTY %in% south,]
merged.w5.north <- merged.w5[merged.w5$CNTY %in% north,]
merged.w5.central <- merged.w5[merged.w5$CNTY %in% central,]

# Redefine the common names as factors. This will drop unused levels.
merged.w5.south$common <- factor(merged.w5.south$common)
merged.w5.north$common <- factor(merged.w5.north$common)
merged.w5.central$common <- factor(merged.w5.central$common)

# create a vector of fish names caught from each area
unique.w5.south <- levels(merged.w5.south$common)
unique.w5.north <- levels(merged.w5.north$common)
unique.w5.central <- levels(merged.w5.central$common)

# Create a vector of the number of unique fish species caught
num.caught.south.w5 <- length(unique.w5.south)
num.caught.north.w5 <- length(unique.w5.north)
num.caught.central.w5 <- length(unique.w5.central)

#subset by county code into north and south NC
merged.w6.south <- merged.w6[merged.w6$CNTY %in% south,]
merged.w6.north <- merged.w6[merged.w6$CNTY %in% north,]
merged.w6.central <- merged.w6[merged.w6$CNTY %in% central,]

# Redefine the common names as factors. This will drop unused levels.
merged.w6.south$common <- factor(merged.w6.south$common)
merged.w6.north$common <- factor(merged.w6.north$common)
merged.w6.central$common <- factor(merged.w6.central$common)

# create a vector of fish names caught from each area
unique.w6.south <- levels(merged.w6.south$common)
unique.w6.north <- levels(merged.w6.north$common)
unique.w6.central <- levels(merged.w6.central$common)

# Create a vector of the number of unique fish species caught
num.caught.south.w6 <- length(unique.w6.south)
num.caught.north.w6 <- length(unique.w6.north)
num.caught.central.w6 <- length(unique.w6.central)




