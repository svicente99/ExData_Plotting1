## 	----------------------------------------------------------------------------------------------------------
##  Coursera.org
## 
##	Exploratory Data Analysis - Course Project 1: solution 4
##
##  Student...:	Sergio Vicente (Niteroi, Brazil)
##  Twitter...: svicente99 (svicente99@yahoo.com)
##  Date......: Jan.09th 2015
## 	----------------------------------------------------------

## 	Refs.: https://class.coursera.org/exdata-010/human_grading/view/courses/973504/assessments/3/submissions

## 	----------------------------------------------------------------------------------------------------------

##  Dataset: Electric power consumption [20Mb]
##			 https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip

## 	----------------------------------------------------------------------------------------------------------

#	Step 1) Loading the data

#   Only using data from the dates 2007-02-01 and 2007-02-02. 

#	Step 2) Ploting data

# 	Construct the plot and save it to a PNG file with a width of 480 pixels and a height of 480 pixels.

## 	----------------------------------------------------------------------------------------------------------

# 	MAIN PARAMETERS

DATA_FOLDER <- "."									# same directory
DATA_FILE <- "household_power_consumption.txt"
DATE_INI  <- "2007-02-01"
DATE_END  <- "2007-02-02"

PNG_FILE  <- "plot4.png"
WIDTH 	  <- 480
HEIGHT 	  <- 480
BG_COLOR  <- "white"
FG_COLOR  <- "black"
X_LABEL	  <- "datetime"
Y_LABELS  <- c("Global Active Power", "Voltage", "Energy sub metering", "Global_reactive_power")


nmFile    <- paste(DATA_FOLDER, DATA_FILE, sep='/')
mLabels   <- matrix(Y_LABELS, nrow=2, ncol=2, byrow=TRUE)

Sys.setenv("LANGUAGE"="En") 

#	function: check if data file was found as expected
check_data_file <- function(f) {
	if(!file.exists(f)) 	
		return(FALSE)
	else		
		return(TRUE)		
}

#	function: read data only contained between period above specified 

#   It wasn't chosen the quicker alternative as reading file with data directly contained between dates 
#   (using "grep command") rather than reading the entire dataset and subsetting to those dates
#   because my "R 3.1.1" installation is under Windows operational system.

read_data <- function(fData, dIni, dEnd) {
	# dt <- fread( paste("grep ^[12]/2/2007", fData), na.strings = c("?", "") )
    # "grep" lost the headers, so get them
    # setnames(dt, colnames(fread(fData, nrows=0)))

	dt <- read.table(fData, sep=";", header=TRUE, na.strings="?")
	
	# it's necessary to create another column in "d/m/Y H:M:S" format mixing values from Date and Time coluns 
	# it was named "date_time". It uses "strptime" to convert string to time values 
	# ref.: http://www.mkssoftware.com/docs/man3/strptime.3.asp
	dt$date_time <- strptime(paste(dt$Date, dt$Time), "%d/%m/%Y %H:%M:%S")

	# Subseting of dates we are passed by parameters
	vPeriod <- c(as.Date(dIni), as.Date(dEnd))
	dt <- dt[as.Date(dt$date_time) %in% vPeriod,]

	return( dt )
}

#	function: draw line graph of Global Active Power, with these features: color, y-label (same of used in "plot2.R")
plot_graph_1_1 <- function(dt, color, yLabel, xLabel="") {
	with(dt, {    
		plot(date_time, Global_active_power, type="l", xlab=xLabel, ylab=yLabel)
	}) 
}

#	function: draw line graph of Voltage, with these features: color, y-label, x-label
plot_graph_1_2 <- function(dt, color, yLabel, xLabel="") {
	with(dt, {    
		plot(date_time, Voltage, type="l", xlab=xLabel, ylab=yLabel)
	}) 
}

#	function: draw LINE GRAPH of each Sub_metering variable with legends and colors (same of used in "plot3.R")
plot_graph_2_1 <- function(dt, color, yLabel, xLabel="") {
	with(dt, {
		plot(date_time, Sub_metering_1, type="n", xlab=xLabel, ylab=yLabel)	## create canvas
		vColors <- c("black", "red", "blue")								## create colors vector
		vSubMeter <- paste0("Sub_metering_", 1:3)							## create set of 'sub_metering*' variables
		# do loop from these 3 variables, drawing its graphs
		for (i in seq_along(vSubMeter)) {
			# the 'i-vSubMeter' stores the name of 'Sub_metering_*' column
			nm_col <- vSubMeter[i] 				
			# using double bracket extracts all values of data.frame according to 'nm_col' name of column
			values <- dt[[nm_col]]
			# draw line graph of values, being each variable associated to its 'i' color
			lines(date_time, values, col=vColors[i])			 
		}
		# put legend and its names, colors and solid lines relative to each case
		legend("topright", legend=vSubMeter, col=vColors, lty="solid")
	})
}

plot_graph_2_2 <- function(dt, color, yLabel, xLabel="") {
	with(dt, {    
		plot(date_time, Global_reactive_power, type="l", xlab=xLabel, ylab=yLabel)
	}) 
}

#	function: draw 4 graphs, each one associated with a panel position
plot_graph4 <- function(dt, color, mLabels) 
{
	par(mfrow=c(2, 2))														## create a panel layout 2x2
	plot_graph_1_1(dt, color, mLabels[1,1])									## draw the graph to (1,1) position
	plot_graph_1_2(dt, color, mLabels[1,2], X_LABEL)						## draw the graph to (1,2) position
	plot_graph_2_1(dt, color, mLabels[2,1])									## draw the graph to (2,1) position
	plot_graph_2_2(dt, color, mLabels[2,2], X_LABEL)						## draw the graph to (2,2) position
}


##	Main code  ##

if(!check_data_file(nmFile)) {
	print("UCI Electric power consumption: FILE NOT FOUND!")
	print(nmFile) 
} else {
	print("UCI Electric power consumption is being read...")
	x<-read_data(nmFile, DATE_INI, DATE_END)

	# effectivelly plots graph in a png file 
	png(PNG_FILE, width=WIDTH, height=HEIGHT, bg=BG_COLOR)
	plot_graph4(x, FG_COLOR, mLabels)
	dev.off()		# important!
	
	print( paste(paste(paste("Histogram of",TITLE,seṕ=" "),"was saved in file named",sep=" "),PNG_FILE,sep=" ") )
}

