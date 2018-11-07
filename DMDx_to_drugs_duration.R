library(data.table)

# file locations
dataset_folder <- "C:/Users/janet/Documents/Codes/DM Project/"

# load medications data
load(paste0(dataset_folder,"3 medication_DT.Rdata")) #dataset is loaded as "medication"
medication <- as.data.table(medication)

saveRDS(medication, paste0(dataset_folder,"medication_DT.rds"))
write.csv(medication, paste0(dataset_folder,"medication_DT.csv"))

# extract required information, remove large data
med <- as.data.table(medication[, c("﻿serial_no", "item_cd", "disp_date", "issuer")])
rm(medication)
gc()

# tidy up dataframe
names(med)[1] <- "serial_no"

med$serial_no <- as.character(med$serial_no)
med$issuer <- as.factor(med$issuer)
med$disp_date <- gsub("([[:alpha:]])([12])", "\\1-\\2", med$disp_date)
med$disp_date <- paste0("15-", med$disp_date) #assume all medications dispensed on 15th of each month
med$disp_date <- as.Date(med$disp_date, "%d-%b-%Y")

saveRDS(med, paste0(dataset_folder,"med_disp_date.rds"))
write.csv(med, paste0(dataset_folder,"med_disp_date.csv"))

# select only DM-related drugs
insulin.code <- c(paste0("DILU0", 1:4), paste0("INSU0", 1:9), paste0("INSU", 20:51),"NOVO01", "S00054", "S00094", "S00134", "S00195", "S00210", "S00211", "S00215", "S00306", "S00477", "S00562", "S00644", "S00710", "S00795", "S00816", "S00817", "S00868", "S00906", "SYRII1", "SYRII2", "SYRII3")
non.insulin.code <- c("ACAR01", "ACAR02", "EXEN01", "EXEN02", "EXEN03", "EXEN04", "S00623", "S00624", "GLIB01", "GLIB02", "GLIC01", "GLIC02", "GLIC03", "S00089", "GLIM01", "GLIM02", "GLIM03", "S00248", "S00249", "GLIP01", "GLIP02", "GLUC01", "GLUC37", "LINA01", "S00893", "METF01", "METF02", "METF03", "METF04", "S00466", "PIOG01", "PIOG02", "S00016", "S00599", "S00790", "SAXA01", "S00600", "S00602", "S00675", "SITA02", "SITA03", "SITA06", "TOLB01", "S00731", "VILD01")

med$insulin <- med$item_cd %in% insulin.code
med$non.insulin <- med$item_cd %in% non.insulin.code
dm <- med[non.insulin==TRUE | insulin==TRUE, ]

rm(med)
gc()

# combine with clinical values dataframe to know first Dx date
d <- readRDS(paste0(dataset_folder, "pt_clinicalvalues.rds"))
pt_med <- merge(dm, d, by = c("serial_no"), all.y = TRUE) 
     