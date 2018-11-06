library(data.table)

# load medications data
load("/R project/3 medication_DT.Rdata") #dataset is loaded as "medication"
medication <- as.data.table(medication)

# extract required information
med <- as.data.table(medication[, c("﻿serial_no", "item_cd", "disp_date", "issuer")])
names(med) <- c("serial_no", "item_cd", "disp_date", "issuer")

# tidy up dataframe
med$serial_no <- as.character(med$serial_no)
med$issuer <- as.factor(med$issuer)
med$disp_date <- gsub("([[:alpha:]])([12])", "\\1-\\2", med$disp_date)
med$disp_date <- paste0("15-", med$disp_date)
med$disp_date <- as.Date(med$disp_date, "%d-%b-%Y")

# combine with clinical values dataframe to know first Dx date
load("/R project/pt_clinicalvalues.rds")