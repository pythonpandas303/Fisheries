###Size Structure Snippet for R###

###Required Libraries (FSA, magrittr, dplyr, plotrix, Matching)###

### Set WD###

###Load Libraries###

###Data set input###

name <- read.csv("File Path Here")

###Seperate by species or other variable###

name1 <- filter(name,variable==x)

###Length Frequency, 10mm inc, lcat10=name of data, tl=variable indicating total length

###This essentially cuts total length into 10mm increments (w=10), 0-9,10-19,20-29,etc. According to Neumann et al. (2012), suggested interval widths are 10mm for species that sample 300mm in tl, 20mm for species that sample 600mm in tl, and 50mm for species that sample over 600 in tl### 

name1 %<>% mutate(lcat10=lencat(tl,w=10))

###Display###

headtail (name1)

###Length Frequency Table###

( nameFreq <- xtabs(~lcat10,data=name1) )

###Convert LF Table into percentages###

round(prop.table(nameFreq)*100,1)

###Display###

name1