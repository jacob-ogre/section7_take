# Initial read, examination of section 7 take data.
# Copyright (c) 2016 Defenders of Wildlife, jmalcom@defenders.org

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, see <http://www.gnu.org/licenses/>.
# 

library(readxl)

# First, get the original data...
base <- "~/Google Drive/Defenders/data/ESA_consultations/Excels/"
infile <- paste0(base, "Formal consultations 2008-2014.xlsx")
col_types <- readxl:::xlsx_col_types(path = infile, nskip = 698, n = 1)
dat <- read_excel(path = infile, col_types = col_types)

names(dat)

takes <- unique(dat$Take)
length(takes)
head(takes, 10)

dat$combo <- paste(dat$`Activity Code`, dat$`Biological Opinion Species`)
dat$dups <- duplicated(dat$combo)

dim(dat)
d2 <- dat[dat$dups == FALSE, ]
dim(d2)
names(d2)
d2 <- d2[, c(1:5, 9, 32:37)]
names(d2)
names(d2) <- c("region", "ESOffice", "FY", "activity_code", "title",
               "lead_agency", "BO_species", "BO_determ", "CH", "CH_flag",
               "take", "work_type")

# Convenience function to convert html codes
html2txt <- function(str) {
    xpathApply(htmlParse(str, asText=TRUE),
               "//body//text()", 
               xmlValue)[[1]] 
}

d2$take <- sapply(d2$take, FUN = html2txt)

outbase <- "~/Repos/Defenders/analyses/section7_take/"
outfile <- paste0(outbase, "take_2008-2014.RData")
take_1 <- d2
save(take_1, file = outfile)


# Next, get the mid-term update (late 2014-early 2015)...well no, I can't find
# the data...
# Instead, we'll get the most recent update (early 2015 -  early 2016).
base <- "~/Dropbox/TAILS data/extracted/"
infile <- paste0(base, "new_data_raw.tsv")
dat <- read.csv(infile, sep="\t", header=TRUE, stringsAsFactors=FALSE)

names(dat)
dim(dat)
table(dat$Formal)
dat <- dat[dat$Formal == "Yes", ]

dat$combo <- paste(dat$`Activity.Code`, dat$`Biological.Opinion.Species`)
dat$dups <- duplicated(dat$combo)

dim(dat)
d2 <- dat[dat$dups == FALSE, ]
dim(d2)
names(d2)
d2 <- d2[, c(1:5, 9, 43:48)]
names(d2)
names(d2) <- c("region", "ESOffice", "FY", "activity_code", "title",
               "lead_agency", "BO_species", "BO_determ", "CH", "CH_flag",
               "take", "work_type")

d2$take <- sapply(d2$take, FUN = html2txt)

outbase <- "~/Repos/Defenders/analyses/section7_take/"
outfile <- paste0(outbase, "take_2015-2016.RData")
take_2 <- d2
save(take_2, file = outfile)

full <- rbind(take_1, take_2)
length(unique(full$activity_code))

full$with_take <- ifelse(full$take == "NA" | full$take == "" | full$take == "N/A" | is.na(full$take),
                         0,
                         1)
sum(full$with_take)
length(full$with_take)

outbase <- "~/Repos/Defenders/analyses/section7_take/"
outfile <- paste0(outbase, "take_data.RData")
save(full, file = outfile)



