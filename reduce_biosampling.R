# reduce_biosampling.R
# takes a set of BioStat input files and reduces sampling

# set working directory to source file location to begin

library(tidyverse)

mylenfile <- "codlen_2016w.dat"
myagefile <- "codage_2016w.dat"

origlen <- read.table(mylenfile, header = FALSE, sep = "", colClasses = c(rep(NA, 10), "character"), stringsAsFactors = FALSE, col.names = c("Year", "Month", "Quarter", "Area", "Gear","Permit", "Market", "Sex", "Length", "NumLen", "Link1"))

origage <- read.table(myagefile, header = FALSE, sep = "", colClasses = c(rep(NA, 11), "character"), stringsAsFactors = FALSE, col.names = c("Year", "Month", "Quarter", "Area", "Gear","Permit", "Market", "Sex", "Length", "Age", "NumAge", "Link1"))

# function to collect randomly sampled trips from original length and age files and write out these reduced sample files
# set myseed to positive integer for repeatable output files
get_reduced_samples <- function(origlen, origage, nsamples, myseed=NULL, outlenfile, outagefile){

  uniquelenlink1 <- unique(origlen$Link1)

  mysamp <- sample(uniquelenlink1, nsamples)

  nlenrows <- length(origlen[,1])
  myfirstline <- TRUE
  xx <- readLines(mylenfile)  

  for (i in 1:nlenrows){
    thislink <- substr(xx[i], 41, 60)
    if (thislink %in% mysamp){
      if (myfirstline == TRUE){
        myfirstline <- FALSE
        cat(xx[i], "\n", file = outlenfile, append = FALSE)
      }else{
        cat(xx[i], "\n", file = outlenfile, append = TRUE)
      }
    }
  }

  nagerows <- length(origage[,1])
  myfirstline <- TRUE
  xx <- readLines(myagefile)  
  
  for (i in 1:nagerows){
    thislink <- substr(xx[i], 44, 63)
    if (thislink %in% mysamp){
      if (myfirstline == TRUE){
        myfirstline <- FALSE
        cat(xx[i], "\n", file = outagefile, append = FALSE)
      }else{
        cat(xx[i], "\n", file = outagefile, append = TRUE)
      }
    }
  }
  return(NULL)
}

# get five sets of reduced sample files
# can run these in BioStat by hand
for (i in 1:5){
  myseed <- ifelse(i == 1, 14, NA) # makes all 5 sets repeatable
  get_reduced_samples(origlen, origage, nsamples=16, myseed, outlenfile=paste0("samplen", i, ".dat"), outagefile=paste0("sampage", i, ".dat"))
}

# compare the length distributions by market category in original and five samples
lentable <- origlen %>%
  group_by(Market, Length) %>%
  summarize(n = sum(NumLen)) %>%
  mutate(freq = n / sum(n)) %>%
  mutate(Source = "original")

for (i in 1:5){
  rs <- read.table(paste0("samplen", i, ".dat"), header = FALSE, sep = "", colClasses = c(rep(NA, 10), "character"), stringsAsFactors = FALSE, col.names = c("Year", "Month", "Quarter", "Area", "Gear","Permit", "Market", "Sex", "Length", "NumLen", "Link1"))
  
  rstable <- rs %>%
    group_by(Market, Length) %>%
    summarize(n = sum(NumLen)) %>%
    mutate(freq = n / sum(n)) %>%
    mutate(Source = paste0("sample", i))
  
  lentable <- rbind(lentable, rstable)
}
lentable

markets <- unique(origlen$Market)
nmarkets <- length(markets)
p1 <- list()

for (i in 1:nmarkets){
  thistable <- filter(lentable, Market == markets[i])
  p1[[i]] <- ggplot(thistable, aes(x=Length, y=freq)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~Source, ncol = 2, scales = "fixed") +
    labs(title = markets[i]) +
    theme_bw()
}

pdf("compare_length_freqs.pdf")

walk(p1, print)

dev.off()

