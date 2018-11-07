library(data.table)
setwd("C:/Users/janet/Documents/Codes")
load("medication_DT.Rdata") #"medication"

str(medication)
#--------------------------------------
# only diabetes drugs
insulin.code <- c(paste0("DILU0", 1:4), paste0("INSU0", 1:9), paste0("INSU", 20:51),"NOVO01", "S00054", "S00094", "S00134", "S00195", "S00210", "S00211", "S00215", "S00306", "S00477", "S00562", "S00644", "S00710", "S00795", "S00816", "S00817", "S00868", "S00906", "SYRII1", "SYRII2", "SYRII3")
non.insulin.code <- c("ACAR01", "ACAR02", "EXEN01", "EXEN02", "EXEN03", "EXEN04", "S00623", "S00624", "GLIB01", "GLIB02", "GLIC01", "GLIC02", "GLIC03", "S00089", "GLIM01", "GLIM02", "GLIM03", "S00248", "S00249", "GLIP01", "GLIP02", "GLUC01", "GLUC37", "LINA01", "S00893", "METF01", "METF02", "METF03", "METF04", "S00466", "PIOG01", "PIOG02", "S00016", "S00599", "S00790", "SAXA01", "S00600", "S00602", "S00675", "SITA02", "SITA03", "SITA06", "TOLB01", "S00731", "VILD01")

# data size is too big, cut into 5 sections for extracting DM drugs
# m1
m1<-medication[1:62638498,]
rm(medication)
m1$insulin <- m1$item_cd %in% insulin.code
m1$non.insulin <- m1$item_cd %in% non.insulin.code

dm1 <- m1[non.insulin==TRUE | insulin==TRUE,list(n=sum(disp_qty)), by=c("drug_displayname","new_mod_strength", "item_cd")]

# m2
rm(m1)
load("Codes/medication_DT.Rdata")
m2<-medication[62638499:125276996,]
rm(medication)

m2$insulin <- m2$item_cd %in% insulin.code
m2$non.insulin <- m2$item_cd %in% non.insulin.code

dm2 <- m2[non.insulin==TRUE | insulin==TRUE,list(n=sum(disp_qty)), by=c("drug_displayname","new_mod_strength", "item_cd")]

# m3
rm(m2)
load("medication_DT.Rdata")
m3<-medication[125276997:187915494,]
rm(medication)

m3$insulin <- m3$item_cd %in% insulin.code
m3$non.insulin <- m3$item_cd %in% non.insulin.code

dm3 <- m3[non.insulin==TRUE | insulin==TRUE,list(n=sum(disp_qty)), by=c("drug_displayname","new_mod_strength", "item_cd")]

# m4
rm(m3)
load("medication_DT.Rdata")
m4<-medication[187915495:250553992,]
rm(medication)

m4$insulin <- m4$item_cd %in% insulin.code
m4$non.insulin <- m4$item_cd %in% non.insulin.code

dm4 <- m4[non.insulin==TRUE | insulin==TRUE,list(n=sum(disp_qty)), by=c("drug_displayname","new_mod_strength", "item_cd")]

# m5
rm(m4)
load("medication_DT.Rdata")
m5<-medication[250553993:313192492,]
rm(medication)

m5$insulin <- m5$item_cd %in% insulin.code
m5$non.insulin <- m5$item_cd %in% non.insulin.code

dm5 <- m5[non.insulin==TRUE | insulin==TRUE,list(n=sum(disp_qty)), by=c("drug_displayname","new_mod_strength", "item_cd")]
rm(m5)

# combine
dm<-rbind(dm1, dm2, dm3, dm4, dm5)
dm_sum<-dm[,list(n = sum(n)),c("drug_displayname", "new_mod_strength", "item_cd")][order(item_cd)]
str(dm_sum)

# some drug display names are missing, add them back and correct discrepancies in display names
dm_sum$drug_displayname<-as.factor(dm_sum$drug_displayname)
dm_sum$new_mod_strength<-as.factor(dm_sum$new_mod_strength)
dm_sum[] <- lapply(dm_sum, function(x) {
  is.na(levels(x)) <- levels(x) == ""
  x
})
dm_sum$new_mod_strength<-as.character(dm_sum$new_mod_strength)
str(dm_sum)

table(is.na(dm_sum$drug_displayname))
dm_sum[(item_cd=="ACAR01" | item_cd =="ACAR02"), 1] <- "ACARBOSE"
dm_sum[item_cd=="DILU01", 1] <- "DILUTING MEDIUM FOR ISOPHANE INSULIN (FREE GOODS)"
dm_sum[item_cd=="DILU02", 1] <- "DILUTING MEDIUM FOR SOLUBLE INSULIN (FREE GOODS)"
dm_sum[item_cd=="DILU03", 1] <- "DILUTING MEDIUM FOR NOVORAPID + LEVEMIR (FREE GOODS)"
dm_sum[(item_cd=="EXEN01" | item_cd =="EXEN02"), 1] <- "EXENATIDE (FREE GOODS) PREFILLED"
dm_sum[item_cd =="EXEN03", 1] <- "EXENATIDE PREFILLED"
dm_sum[item_cd=="GLIB02", 1] <- "GLIBENCLAMIDE"
dm_sum[item_cd=="GLIC02", 1] <- "GLICLAZIDE MODIFIED RELEASE"
dm_sum[(item_cd=="GLIC03" | item_cd == "GLIC01"), 1] <- "GLICLAZIDE"
dm_sum[(item_cd=="GLIM01" | item_cd =="GLIM02" | item_cd == "GLIM03"), 1] <- "GLIMEPIRIDE"
dm_sum[item_cd=="GLIP01", 1] <- "GLIPIZIDE"
dm_sum[item_cd=="GLUC37", 1] <- "GLUCAGON (HCL)"
dm_sum[(item_cd=="INSU02" | item_cd=="INSU27" | item_cd=="INSU35" | item_cd=="INSU40"), 1] <- "INSULIN ISOPHANE 70% +NEUTRAL30%"
dm_sum[(item_cd=="INSU03" | item_cd=="INSU06" | item_cd=="INSU25" | item_cd=="INSU37"), 1] <- "INSULIN NEUTRAL HUMAN"
dm_sum[(item_cd=="INSU09"), 1] <- "INSULIN ZINC SUSPENSION HUMAN"
dm_sum[(item_cd=="INSU26" | item_cd=="INSU36" | item_cd=="INSU25"), 1] <- "INSULIN ISOPHANE"
dm_sum[item_cd=="INSU31", 1] <- "INSULIN HUMAN MIXTARD 20HM"
dm_sum[(item_cd=="INSU38" | item_cd=="INSU39" | item_cd=="INSU48"), 1] <- "INSULIN LISPRO HUMAN"
dm_sum[(item_cd=="INSU41" | item_cd=="INSU47"), 1] <- "INSULIN ASPART HUMAN"
dm_sum[(item_cd=="INSU42" | item_cd=="INSU43" | item_cd=="INSU46"), 1] <- "INSULIN GLARGINE"
dm_sum[item_cd=="INSU44", 1] <- "INSULIN ASPART + ASPART PROTAMINE HUMAN"
dm_sum[item_cd=="INSU45", 1] <- "INSULIN DETERMIR"
dm_sum[item_cd=="INSU41", 1] <- "INSULIN ASPART HUMAN"
dm_sum[item_cd=="INSU49", 1] <- "INSULIN LISPRO(HUMALOGMIX25 KWIKPEN"
dm_sum[item_cd=="INSU50", 1] <- "INSULIN LISPRO(HUMALOGMIX50 KWIKPEN"
dm_sum[(item_cd=="INSU51"), 1] <- "INSULIN GLULISINE"
dm_sum[(item_cd=="LINA01"), 1] <- "LINAGLIPTIN"
dm_sum[(item_cd=="METF01" | item_cd=="METF02" | item_cd=="METF03") & is.na(drug_displayname), 1] <- "METFORMIN"
dm_sum[(item_cd=="PIOG01" | item_cd=="PIOG02"), 1] <- "PIOGLITAZONE"
dm_sum[item_cd=="SAXA01", 1] <- "SAXAGLIPTIN (HCL)"
dm_sum[(item_cd=="SITA02" | item_cd=="SITA03" | item_cd=="SITA06"), 1] <- "SITAGLIPTIN (PHOSPHATE)"
dm_sum[item_cd=="TOLB01", 1] <- "TOLBUTAMIDE"
dm_sum[item_cd=="VILD01", 1] <- "VILDAGLIPTIN"
dm_sum[item_cd=="S00016", 1] <- "NON FORMULARY ITEM PIOGLITAZONE"
dm_sum[item_cd=="S00054", 1] <- "NON FORMULARY ITEM INSULIN GLARGINE (LANTUS)"
dm_sum[item_cd=="S00089", 1] <- "NON FORMULARY ITEM GLICLAZIDE"
dm_sum[item_cd=="S00094", 1] <- "NON FORMULARY ITEM INSULIN ASPART HUMAN (NOVORAPID)"
dm_sum[item_cd=="S00134", 1] <- "NON FORMULARY ITEM DILUTING MEDIUM FOR SOLUBLE INSULIN"
dm_sum[item_cd=="S00195", 1] <- "NON FORMULARY ITEM INSULIN HUMAN (HUMULIN 70/30)"
dm_sum[item_cd=="S00215", 1] <- "NON FORMULARY ITEM DILUTING MEDIUM FOR ISOPHANE INSULIN"
dm_sum[(item_cd=="S00248" | item_cd=="S00249"), 1] <- "NON FORMULARY ITEM GLIMEPIRIDE"
dm_sum[item_cd=="S00466", 1] <- "NON FORMULARY ITEM METFORMIN HCL EXTENDED RELEASE"
dm_sum[item_cd=="S00477", 1] <- "NON FORMULARY ITEM INSULIN ASPART + ASPART PROTAMINE HUMAN (NOVOMIX 30)"
dm_sum[item_cd=="S00562", 1] <- "NON FORMULARY ITEM INSULIN DETEMIR (LEVEMIR)"
dm_sum[(item_cd=="S00600" | item_cd=="S00602" | item_cd=="S00675"), 1] <- "NON FORMULARY ITEM SITAGLIPTIN (PHOSPHATE)"
dm_sum[item_cd=="S00644", 1] <- "NON FORMULARY ITEM INSULIN GLARGINE(LANTUS)OPTISET P/F"
dm_sum[item_cd=="S00710", 1] <- "NON FORMULARY ITEM INSULIN LISPRO HUMAN (HUMALOG MIX25"
dm_sum[item_cd=="S00731", 1] <- "NON FORMULARY ITEM VILDAGLIPTIN"
dm_sum[item_cd=="S00790", 1] <- "NON FORMULARY ITEM SAXAGLIPTIN"
dm_sum[item_cd=="S00795", 1] <- "NON FORMULARY ITEM INSULIN GLARGINE(LANTUS SOLOSTAR PF"
dm_sum[(item_cd=="S00816" | item_cd=="S00817"), 1] <- "NON FORMULARY ITEM INSULIN LISPRO(HUMALOGMIX25 KWIKPEN"
dm_sum[item_cd=="S00868", 1] <- "NON FORMULARY ITEM INSULIN LISPRO (HUMALOG KWIKPEN) PF"
dm_sum[item_cd=="S00893", 1] <- "NON FORMULARY ITEM LINAGLIPTIN"
dm_sum[item_cd=="S00906", 1] <- "NON FORMULARY ITEM INSULIN GLULISINE (APIDRA SOLOSTAR)"
dm_sum[item_cd=="S00623" | item_cd=="S00624", 1] <- "NON FORMULARY ITEM EXENATIDE"
table(is.na(dm_sum$drug_displayname))
dm_sum<-dm_sum[,list(n = sum(n)),c("drug_displayname", "new_mod_strength", "item_cd")][order(item_cd)]

# correct discrepancies in dosage
dm_sum[grepl("100U/ML", x = dm_sum$new_mod_strength), 2] <- "100U/ML"
dm_sum[grepl("20U/ML", x = dm_sum$new_mod_strength), 2] <- "20U/ML"
dm_sum[grepl("50U/ML", x = dm_sum$new_mod_strength), 2] <- "50U/ML"
dm_sum[item_cd=="DILU01", 2] <- "10ML"
dm_sum[item_cd=="DILU02", 2] <- "10ML"
dm_sum[item_cd=="DILU03", 2] <- "10U/ML 10ML"
dm_sum[item_cd=="GLIC01", 2] <- "80MG"
dm_sum[item_cd=="GLIC02", 2] <- "30MG"
dm_sum[item_cd=="GLIC03", 2] <- "60MG"
dm_sum[item_cd=="INSU47", 2] <- "20U/ML"
dm_sum[item_cd=="INSU25", 2] <- "100U/ML"
dm_sum[item_cd=="METF01" | item_cd=="METF02", 2] <- "250MG"
dm_sum[item_cd=="PIOG01", 2] <- "30MG"
dm_sum[item_cd=="S00134" | item_cd=="S00215", 2] <- "10ML"
dm_sum[item_cd=="S00054" | item_cd=="S00094" | item_cd=="S00477" | item_cd=="S00562" | item_cd=="S00644" | item_cd=="S00816" | item_cd=="S00817" | item_cd=="S00868" | item_cd=="S00906", 2] <- "100U/ML"
dm_sum[item_cd=="INSU02", 2] <- "100U/ML"
dm_sum[new_mod_strength == "1000U/10ML", 2] <- "100U/ML"
dm_sum[item_cd=="TOLB01", 2] <- "500MG"
dm_sum[(item_cd=="GLIB02" | item_cd =="GLIP01"), 2] <- "5MG"
dm_sum[grepl("E-RM", x = dm_sum$new_mod_strength), 2] <- "100U/ML"
dm_sum[grepl("E-ROOM", x = dm_sum$new_mod_strength), 2] <- "100U/ML"
dm_sum[new_mod_strength == "20IU/ML" | new_mod_strength == "1.2U/0.06ML", 2] <- "20U/ML"
dm_sum[(new_mod_strength == "15/5" | new_mod_strength == "12/5" | new_mod_strength == "1" | new_mod_strength == "(16/7)10ML"| new_mod_strength == "10ML") & item_cd=="INSU06", 2] <- NA
dm_sum<-dm_sum[,list(quantity = sum(n)),c("drug_displayname", "new_mod_strength", "item_cd")][order(item_cd)]

# add years for inputing unit price
dm_sum$yr2006<-""
dm_sum$yr2007<-""
dm_sum$yr2008<-""
dm_sum$yr2009<-""
dm_sum$yr2010<-""
dm_sum$yr2011<-""
dm_sum$yr2012<-""
dm_sum$yr2013<-""
dm_sum$yr2014<-""

save(dm1, dm2, dm3, dm4, dm5, dm, dm_sum, file=("dm_medications.Rdata"))
write.csv(dm_sum, file="datashell_dm.csv")
gc()

#------------------------------------------------
#cvs drugs
load("medication_DT.Rdata") #"medication"
str(medication)

# drugcodes from HA datascheme table (sheet 2)
statin.code <- c(paste0("ATOR0", 1:4), "S00080", "S00876", "S00887", "S00917", "FLUV02", "FLUV03", "S00240", "FLUV05", "S00082", "LOVA01", "PRAV01", "PRAV02", paste0("ROSU0", 1:3), "S00033", "S00183", "S00007", "S00293", "SIMV01", "SIMV02", "SIMV04", "SIMV05")

# blood pressure treatment
bprx.code <- c("ACEB01", "ACEB02", "AMIL01", paste0("AMLO0", 1:3), paste0("ATEN0", 1:3), "BISO01","BISO02", "S00256", "S00274", "BOSE01", "BOSE02", "S00276", "S00277", "BUME01", "BUME02", "CAND01", "CAND02", "S00063", "S00434", "S00870", paste0("CAPT0", 1:7), paste0("CARV0", 1:4), "S00371", "S00372", "CELI01", "CILA01", "CILA02", "CILA03", "DIHY01", "DILT01", "DILT02", "DILT05", "DILT06", "DILT07", "DILT08", "S00354", "S00355", "DILT03", "DILT04", "DOXA05", "DOXA06", "S00330", "DOXA02", "DOXA03", "DOXA04", "DUXA01", "S00356", "DYAZ01", "ENAL01", "ENAL02", "ENAL03", "ENAL04", "FELO01", "FELO02", "FELO03", "S00362", "FOSI01", "FRUS09", "FRUS01", "FRUS02", "FRUS03", "FRUS04", "FRUS06", "FRUS07", "FRUS10", "FRUS05", "FRUS08", "HYDR01", "HYDR02", "HYDR03", "HYDR46", "HYDR65", "HYDR05", "HYDR30", "HYDR38", "HYDR49", "HYDR50", "ILOP02", "S00471", "ILOP01", "S00259", "INDA02", "S00085", "INDA01", "IRBE03", "IRBE04", "S00531", "IRBE01", "IRBE02", "S00230", "S00250", "LABE01", "LABE02", "LABE03", "LABE04", "LABE05", "LABE06", "LACI01", "LACI02", "LERC01", "S00226", "LISI01", "LISI02", "LISI03", "LOSA02", "LOSA04", "S00162", "LOSA01", "LOSA03", "S00076", "S00396", "MANN02", "MANN03", "MANN07", "MANN08", "METH21", "METH22", "METH23", "METH78", "METO05", "METO10", "METO11", "METO13", "METO16", "S00077", "S00188", "S00380", "S00410", "METO08", "METO06", "METO07", "METO09", "METO15", "METO17", "MODU01", "NADO01", "S00943", "NIFE04", "NIFE05", "NIFE07", "S00067", "S00071", "NIFE03", "NIFE08", "NIFE06", "NIFE01", "NIFE02", "NIMO02", "NIMO01", "NITR06", "S00453", "S00454", "S00455", "OXPR02", "PERI29", "PERI17", "PERI28", "S00265", "PIND01", "PRAZ03", "PRAZ04", "PRAZ05", "PRED25", "PROP07", "PROP13", "PROP04", "PROP05", "PROP06", "PROP08", "PROP14", "PROP16", "PROP22", "PROP23", "PROP25", "PROP29", "QUIN08", "QUIN09", "QUIN10", "RAMI01", "RAMI02", "RAMI03", "S00029", "S00030", "RESE01", "SILD04", "SOTA01", "SOTA02", "SOTA03", "SOTA04", "SOTA05", "SPIR01", "SPIR02", "SPIR04", "SPIR05", "SPIR06", "S00031", "S00032", "TELM01", "TELM02", "TERA01", "TERA02", "TERA03", "S00069", "VALS01", "VALS02", "VALS03", "S00209", "VERA04", "VERA01", "VERA02", "VERA03")

# anticoagulant treatment (other than aspirin)
anticoag.code <- c("ABCI01", "S00002", "AGGR01", "S00539", "CLOP05", "DABI01", "DABI02", "DABI03", "S00694", "S00695", "S00920", "DALT04", "DALT03", "DALT01", "DALT02", "DIPY01", "DIPY02", "DIPY03", "DIPY04", "ENOX01", "ENOX02", "ENOX03", "ENOX04", "S00495", "EPTI02", "EPTI01", "S00364", "HEPA16", "HEPA21", "HEPA22", "HEPA29", "HEPA01", "HEPA27", "HEPA11", "HEPA18", "HEPA19", "HEPA23", "HEPA05", "HEPA15", "HEPA41", "HEPA42", "HEPA17", "HEPA03", "HEPA04", "NADR04", "NADR03", "NADR02", "NADR01", "PRAS01", "PRAS02", "S00760", "RIVA07", "RIVA08", "RIVA09", "S00708", "S00900", "S00901", "S00862", "TICA01", "S00735", "TINZ03", "S00005", "TINZ02", "S00736", "TINZ04", "TINZ01", "S00187", "TINZ05", "TINZ06", "WARF01", "WARF02", "WARF03")

medication$statin <- medication$item_cd %in% statin.code
medication$bprx <- medication$item_cd %in% bprx.code
medication$anticoag <- medication$item_cd %in% anticoag.code
medication$statin <- medication[item_cd %in% statin.code]


# data size is too big, cut into 5 sections for extracting DM drugs
# m1
m1<-medication[1:60000000,]
rm(medication)

m1$statin <- m1$item_cd %in% statin.code
m1$bprx <- m1$item_cd %in% bprx.code
m1$anticoag <- m1$item_cd %in% anticoag.code

cvs1 <- m1[statin==TRUE | bprx==TRUE | anticoag==TRUE, list(n=sum(disp_qty)), by=c("drug_displayname", "new_mod_strength", "item_cd")]
View(cvs1)

# m2
rm(m1)
load("medication_DT.Rdata")
m2<-medication[60000001:125000000,]
rm(medication)

m2$statin <- m2$item_cd %in% statin.code
m2$bprx <- m2$item_cd %in% bprx.code
m2$anticoag <- m2$item_cd %in% anticoag.code

cvs2 <- m2[statin==TRUE | bprx==TRUE | anticoag==TRUE, list(n=sum(disp_qty)), by=c("drug_displayname", "new_mod_strength", "item_cd")]

# m3
rm(m2)
load("medication_DT.Rdata")
m3<-medication[125000001:190000000,]
rm(medication)

m3$statin <- m3$item_cd %in% statin.code
m3$bprx <- m3$item_cd %in% bprx.code
m3$anticoag <- m3$item_cd %in% anticoag.code

cvs3 <- m3[statin==TRUE | bprx==TRUE | anticoag==TRUE, list(n=sum(disp_qty)), by=c("drug_displayname", "new_mod_strength", "item_cd")]

# m4
rm(m3)
load("medication_DT.Rdata")
m4<-medication[190000001:250000000,]
rm(medication)

m4$statin <- m4$item_cd %in% statin.code
m4$bprx <- m4$item_cd %in% bprx.code
m4$anticoag <- m4$item_cd %in% anticoag.code

cvs4 <- m4[statin==TRUE | bprx==TRUE | anticoag==TRUE, list(n=sum(disp_qty)), by=c("drug_displayname", "new_mod_strength", "item_cd")]

# m5
rm(m4)
load("medication_DT.Rdata")
m5<-medication[250000001:313192492,]
rm(medication)

m5$statin <- m5$item_cd %in% statin.code
m5$bprx <- m5$item_cd %in% bprx.code
m5$anticoag <- m5$item_cd %in% anticoag.code

cvs5 <- m5[statin==TRUE | bprx==TRUE | anticoag==TRUE, list(n=sum(disp_qty)), by=c("drug_displayname", "new_mod_strength", "item_cd")]
rm(m5)

# combine
cvs<-rbind(cvs1, cvs2, cvs3, cvs4, cvs5)
cvs<-cvs[,list(n = sum(n)),c("drug_displayname", "new_mod_strength", "item_cd")][order(item_cd)]
str(cvs)

cvs$drug_displayname<-as.factor(cvs$drug_displayname)
cvs$new_mod_strength<-as.factor(cvs$new_mod_strength)
cvs[] <- lapply(cvs, function(x) {
  is.na(levels(x)) <- levels(x) == ""
  x
})
cvs$new_mod_strength<-as.character(cvs$new_mod_strength)
cvs$n<-as.numeric(cvs$n)
str(cvs)

sum(is.na(cvs))
cvs[(item_cd=="ABCI01"), 1] <- "ABCIXIMAB"
cvs[(item_cd=="ABCI01"), 2] <- "2MG/ML"
cvs[(item_cd=="AGGR01"), 2] <- "(FREE GOODS)"
cvs[(item_cd=="AMIL01"), 1] <- "AMILORIDE"
cvs[(item_cd=="AMIL01"), 2] <- "5MG"
cvs[(item_cd=="AMLO01"|item_cd=="AMLO02"|item_cd=="AMLO03"), 1] <- "AMLODIPINE BESYLATE"
cvs[(item_cd=="AMLO01"), 2] <- "5MG"
cvs[(item_cd=="AMLO02"), 2] <- "10MG"
cvs[(item_cd=="AMLO03"), 2] <- "1MG/ML"
cvs[(item_cd=="ATEN01"|item_cd=="ATEN02"|item_cd=="ATEN03"), 1] <- "ATENOLOL"
cvs[(item_cd=="ATEN01"), 2] <- "50MG"
cvs[(item_cd=="ATEN02"), 2] <- "100MG"
cvs[(item_cd=="ATEN03"), 2] <- "2MG/ML"
cvs[(item_cd=="ATOR01"|item_cd=="ATOR02"|item_cd=="ATOR03"|item_cd=="ATOR04"), 1] <- "ATORVASTATIN"
cvs[(item_cd=="ATOR01"), 2] <- "10MG"
cvs[(item_cd=="ATOR02"), 2] <- "20MG"
cvs[(item_cd=="ATOR03"), 2] <- "40MG"
cvs[(item_cd=="ATOR04"), 2] <- "80MG"
cvs[grepl("BISO", x = cvs$item_cd), 1] <- "BISOPROLOL HEMIFUMARATE"
cvs[(item_cd=="BISO02"), 2] <- "2.5MG"
cvs[(item_cd=="BOSE02"), 1] <- "BOSENTAN"
cvs[(item_cd=="BOSE02"), 2] <- "125MG"
cvs[(item_cd=="BUME01"), 1] <- "BUMETANIDE"
cvs[(item_cd=="BUME01"), 2] <- "1MG"
cvs[grepl("CAND", x = cvs$item_cd), 1] <- "CANDESARTAN CILEXETIL"
cvs[(item_cd=="CAND01"), 2] <- "8MG"
cvs[grepl("CAPT", x = cvs$item_cd), 1] <- "CAPTOPRIL"
cvs[(item_cd=="CAPT01"), 2] <- "25MG"
cvs[(item_cd=="CAPT03"), 2] <- "12.5MG"
cvs[(item_cd=="CAPT04"), 2] <- "5MG/ML"
cvs[(item_cd=="CAPT06"), 2] <- "6.25MG"
cvs[(item_cd=="CAPT07"), 2] <- "5MG/ML"
cvs[grepl("CARV", x = cvs$item_cd), 1] <- "CARVEDILOL"
cvs[(item_cd=="CARV02"), 2] <- "6.25MG"
cvs[(item_cd=="CARV03"), 2] <- "12.5MG"
cvs[(item_cd=="CARV04"), 2] <- "3.125MG"
cvs[(item_cd=="CELI01"), 1] <- "CELIPROLOL HYDROCHLORIDE"
cvs[(item_cd=="CLOP05"), 1] <- "CLOPIDOGREL"
cvs[(item_cd=="CLOP05"), 2] <- "75MG"
cvs[grepl("DABI", x = cvs$item_cd), 1] <- "DABIGATRAN ETEXILATE"
cvs[(item_cd=="DABI02"), 2] <- "110MG"
cvs[(item_cd=="DIHY01"), 1] <- "DIHYDRALAZINE MESYLATE"
cvs[grepl("DILT", x = cvs$item_cd), 1] <- "DILTIAZEN HCL"
cvs[(item_cd=="DILT01"), 2] <- "30MG"
cvs[(item_cd=="DILT02"), 2] <- "60MG"
cvs[(item_cd=="DILT05"|item_cd=="DILT06"|item_cd=="DILT07"|item_cd=="DILT08"), 1] <- "DILTIAZEM HCL S.R."
cvs[(item_cd=="DILT05"), 2] <- "90MG"
cvs[(item_cd=="DILT06"), 2] <- "180MG"
cvs[(item_cd=="DILT07"), 2] <- "100MG"
cvs[(item_cd=="DILT08"), 2] <- "200MG"
cvs[grepl("DIPY", x = cvs$item_cd), 1] <- "DIPYRIDAMOLE"
cvs[(item_cd=="DIPY01"), 2] <- "25MG"
cvs[(item_cd=="DIPY02"), 2] <- "75MG"
cvs[grepl("DOXA", x = cvs$item_cd), 1] <- "DOXAZOSIN MESYLATE"
cvs[(item_cd=="DOXA03"), 2] <- "2MG"
cvs[(item_cd=="DOXA05"), 1] <- "DOXAZOSIN MESYLATE GITS"
cvs[(item_cd=="DOXA05"), 2] <- "4MG"
cvs[(item_cd=="DOXA06"), 1] <- "DOXAZOSIN MESYLATE GITS"
cvs[(item_cd=="DYAZ01"), 1] <- "DYAZIDE"
cvs[grepl("ENAL", x = cvs$item_cd), 1] <- "ENALAPRIL MALEATE"
cvs[(item_cd=="ENAL01"), 2] <- "20MG"
cvs[(item_cd=="ENAL02"), 2] <- "5MG"
cvs[(item_cd=="ENAL03"), 2] <- "10MG"
cvs[grepl("ENOX", x = cvs$item_cd), 1] <- "ENOXAPARIN SODIUM PREFILLED"
cvs[(item_cd=="ENOX02"), 2] <- "40MG/0.4ML"
cvs[(item_cd=="ENOX03"), 2] <- "60MG/0.6ML"
cvs[(item_cd=="EPTI01"), 1] <- "EPTIFIBATIDE"
cvs[(item_cd=="EPTI01"), 2] <- "2MG/ML"
cvs[grepl("FELO", x = cvs$item_cd), 1] <- "FELODIPINE"
cvs[(item_cd=="FELO01"), 2] <- "5MG"
cvs[(item_cd=="FELO02"), 2] <- "10MG"
cvs[(item_cd=="FELO03"), 2] <- "2.5MG"
cvs[grepl("FLUV", x = cvs$item_cd), 1] <- "FLUVASTATIN SODIUM"
cvs[(item_cd=="FLUV03"), 2] <- "40MG"
cvs[(item_cd=="FLUV05"), 1] <- "FLUVASTATIN SODIUM EXTENDED RELEASE"
cvs[(item_cd=="FLUV05"), 2] <- "80MG"
cvs[(item_cd=="FOSI01"), 1] <- "FOSINOPRIL"
cvs[grepl("FRUS", x = cvs$item_cd), 1] <- "FRUSEMIDE"
cvs[(item_cd=="FRUS01"), 2] <- "40MG"
cvs[(item_cd=="FRUS02"), 2] <- "500MG"
cvs[(item_cd=="FRUS03"|item_cd=="FRUS04"|item_cd=="FRUS10"), 2] <- "10MG/ML"
cvs[(item_cd=="FRUS06"), 2] <- "20MG/5ML"
cvs[(item_cd=="FRUS09"), 1] <- "FRUSEMIDE PAEDIATRIC"
cvs[grepl("HEPA", x = cvs$item_cd), 1] <- "HEPARIN SODIUM"
cvs[(item_cd=="HEPA03" | item_cd=="HEPA05"), 2] <- "1000U/ML"
cvs[(item_cd=="HEPA04"), 2] <- "5000U/ML"
cvs[(item_cd=="HEPA11"), 2] <- "10U/ML"
cvs[(item_cd=="HEPA15"), 2] <- "1000U/ML"
cvs[(item_cd=="HEPA11"), 1] <- "HEPARIN SALINE"
cvs[(item_cd=="HEPA42"), 2] <- "5000U/ML"
cvs[grepl("HYDR", x = cvs$item_cd), 1] <- "HYDRALAZINE"
cvs[(item_cd=="HYDR01"), 2] <- "10MG"
cvs[(item_cd=="HYDR02"), 2] <- "25MG"
cvs[(item_cd=="HYDR05" | item_cd=="HYDR30"| item_cd=="HYDR38"), 1] <- "HYDROCHLOROTHIAZIDE"
cvs[(item_cd=="HYDR05"), 2] <- "50MG"
cvs[(item_cd=="HYDR30"), 2] <- "25MG"
cvs[(item_cd=="HYDR38"), 2] <- "2MG/ML"
cvs[(item_cd=="ILOP02"), 2] <- "20MCG/ML"
cvs[(item_cd=="INDA01"), 1] <- "INDAPAMIDE"
cvs[(item_cd=="INDA01"), 2] <- "2.5MG"
cvs[(item_cd=="INDA02"), 1] <- "INDAPAMIDE S.R."
cvs[(item_cd=="INDA02"), 2] <- "1.5MG"
cvs[(item_cd=="IRBE01" | item_cd=="IRBE02"), 1] <- "IRBESARTAN"
cvs[(item_cd=="IRBE01"), 2] <- "150MG"
cvs[(item_cd=="IRBE02"), 2] <- "300MG"
cvs[(item_cd=="IRBE03"| item_cd=="IRBE04"), 1] <- "IRBESARTAN + HYDROCHLOROTHIAZIDE"
cvs[(item_cd=="IRBE03"), 2] <- "150MG+12.5MG"
cvs[(item_cd=="IRBE04"), 2] <- "300MG+12.5MG"
cvs[(item_cd=="LABE01"), 2] <- "100MG"
cvs[(item_cd=="LABE02"), 2] <- "200MG"
cvs[(item_cd=="LABE03"), 2] <- "5MG/ML 20ML"
cvs[(item_cd=="LABE04"), 2] <- "5MG/ML 5ML"
cvs[(item_cd=="LABE05"), 2] <- "5MG/ML"
cvs[grepl("LABE", x = cvs$item_cd), 1] <- "LABETALOL HCL"
cvs[grepl("LACI", x = cvs$item_cd), 1] <- "LACIDIPINE"
cvs[grepl("LERC", x = cvs$item_cd), 1] <- "LERCANIDIPINE HCL"
cvs[grepl("LISI", x = cvs$item_cd), 1] <- "LISINOPRIL"
cvs[(item_cd=="LISI01"), 2] <- "5MG"
cvs[(item_cd=="LISI02"), 2] <- "10MG"
cvs[(item_cd=="LISI03"), 2] <- "20MG"
cvs[(item_cd=="LOSA01"|item_cd=="LOSA03"), 1] <- "LOSARTAN POTASSIUM"
cvs[(item_cd=="LOSA01"), 2] <- "50MG"
cvs[(item_cd=="LOSA02" | item_cd=="LOSA04"), 1] <- "LOSARTAN K + HYDROCHLOROTHIAZIDE"
cvs[(item_cd=="LOSA02"), 2] <- "50MG+12.5MG"
cvs[(item_cd=="LOSA03"), 2] <- "100MG"
cvs[(item_cd=="LOSA04"), 2] <- "100MG+12.5MG"
cvs[(item_cd=="MANN03"), 2] <- "20%"
cvs[grepl("METH", x = cvs$item_cd), 1] <- "METHYLDOPA"
cvs[(item_cd=="METH22"), 2] <- "125MG"
cvs[(item_cd=="METH23"), 2] <- "250MG"
cvs[grepl("METO", x = cvs$item_cd), 1] <- "METOPROLOL"
cvs[(item_cd=="METO05"), 1] <- "METOLAZONE"
cvs[(item_cd=="METO05"), 2] <- "5MG"
cvs[(item_cd=="METO06"), 2] <- "50MG"
cvs[(item_cd=="METO07"), 2] <- "100MG"
cvs[(item_cd=="METO08"), 1] <- "METOPROLOL S.R."
cvs[(item_cd=="METO09"), 2] <- "1MG/ML"
cvs[(item_cd=="METO10"|item_cd=="METO11"|item_cd=="METO13"|item_cd=="METO16"), 1] <- "METOPROLOL C.R."
cvs[(item_cd=="METO15"), 2] <- "1.25MG/ML"
cvs[(item_cd=="MODU01"), 1] <- "MODURETIC"
cvs[(item_cd=="MODU01"), 2] <- "NA"
cvs[(item_cd=="NADO01"), 1] <- "NADOLOL"
cvs[grepl("NADR", x = cvs$item_cd), 1] <- "NADROPARIN CALCIUM"
cvs[(item_cd=="NADR01"), 2] <- "5700AXA IU"
cvs[(item_cd=="NADR02"), 2] <- "3800AXA IU"
cvs[(item_cd=="NADR03"), 2] <- "2850AXA IU"
cvs[grepl("NIFE", x = cvs$item_cd), 1] <- "NIFEDIPINE"
cvs[(item_cd=="NIFE01"), 2] <- "5MG"
cvs[(item_cd=="NIFE02"), 2] <- "10MG"
cvs[(item_cd=="NIFE03"), 2] <- "20MG"
cvs[(item_cd=="NIFE03"), 1] <- "NIFEDIPINE S.R."
cvs[(item_cd=="NIFE04"), 2] <- "30MG"
cvs[(item_cd=="NIFE04"), 1] <- "NIFEDIPINE EXTENDED RELEASE"
cvs[grepl("NIMO", x = cvs$item_cd), 1] <- "NIMODIPINE"
cvs[(item_cd=="NITR06"), 1] <- "NITROPRUSSIDE DIHYDRATE SODIUM"
cvs[(item_cd=="NITR06"), 2] <- "50MG"
cvs[(item_cd=="PERI17"), 1] <- "PERINDOPRIL"
cvs[(item_cd=="PERI17"), 2] <- "4MG"
cvs[(item_cd=="PERI28"), 2] <- "2MG"
cvs[(item_cd=="PIND01"), 1] <- "PINDOLOL"
cvs[(item_cd=="PIND01"), 2] <- "5MG"
cvs[grepl("PRAS", x = cvs$item_cd), 1] <- "PRASUGREL (HCL)"
cvs[grepl("PRAV", x = cvs$item_cd), 1] <- "PRAVASTATIN SODIUM"
cvs[grepl("PRAZ", x = cvs$item_cd), 1] <- "PRAZOSIN HCL"
cvs[(item_cd=="PRAZ03"), 2] <- "1MG"
cvs[(item_cd=="PRAZ04"), 2] <- "2MG"
cvs[(item_cd=="PRAZ05"), 2] <- "5MG"
cvs[grepl("PROP", x = cvs$item_cd), 1] <- "PROPANOLOL HCL"
cvs[(item_cd=="PROP04"), 2] <- "10MG"
cvs[(item_cd=="PROP05"), 2] <- "40MG"
cvs[(item_cd=="PROP07"), 1] <- "PROPANOLOL HCL S.R."
cvs[grepl("RAMI", x = cvs$item_cd), 1] <- "RAMIPRIL"
cvs[(item_cd=="RAMI01"), 2] <- "2.5MG"
cvs[(item_cd=="RAMI02"), 2] <- "5MG"
cvs[grepl("RIVA", x = cvs$item_cd), 1] <- "RIVAROXABAN"
cvs[grepl("ROSU", x = cvs$item_cd), 1] <- "ROSUVASTATIN CALCIUM"
cvs[(item_cd=="ROSU01"), 2] <- "10MG"
cvs[(item_cd=="ROSU02"), 2] <- "20MG"
cvs[(item_cd=="S00002"), 1] <- "NON FORMULARY ITEM AGGRENOX"
cvs[(item_cd=="S00029" | item_cd=="S00030"), 1] <- "NON FORMULARY ITEM RAMIPRIL"
cvs[(item_cd=="S00031" | item_cd=="S00032"), 1] <- "NON FORMULARY ITEM TELMISARTAN"
cvs[(item_cd=="S00033" | item_cd=="S00183"), 1] <- "NON FORMULARY ITEM ROSUVASTATIN"
cvs[(item_cd=="S00063" | item_cd=="S00434" | item_cd=="S00870"), 1] <- "NON FORMULARY ITEM CANDESARTAN"
cvs[(item_cd=="S00067" | item_cd=="S00071"), 1] <- "NON FORMULARY ITEM NIFEDIPINE"
cvs[(item_cd=="S00069"), 1] <- "NON FORMULARY ITEM VALSARTAN"
cvs[(item_cd=="S00076" | item_cd=="S00396"), 1] <- "NON FORMULARY ITEM LOSARTAN"
cvs[(item_cd=="S00077" | item_cd=="S00380"), 1] <- "NON FORMULARY ITEM METOPROLOL"
cvs[(item_cd=="S00080" | item_cd=="S00876" | item_cd=="S00887" | item_cd=="S00917"), 1] <- "NON FORMULARY ITEM ATORVASTATIN"
cvs[(item_cd=="S00082"), 1] <- "NON FORMULARY ITEM FLUVASTATIN"
cvs[(item_cd=="S00085"), 1] <- "NON FORMULARY ITEM INDAPAMIDE"
cvs[(item_cd=="S00162"), 1] <- "NON FORMULARY ITEM LOSARTAN K 50MG + HYDROCHLOROTHIAZIDE 12.5MG"
cvs[(item_cd=="S00209"), 1] <- "NON FORMULARY ITEM VERAPAMIL"
cvs[(item_cd=="S00226"), 1] <- "NON FORMULARY ITEM LERCANIDIPINE"
cvs[(item_cd=="S00230"), 1] <- "NON FORMULARY ITEM IRBESARTAN"
cvs[(item_cd=="S00240"), 1] <- "NON FORMULARY ITEM FLUVASTATIN"
cvs[(item_cd=="S00256" | item_cd=="S00274"), 1] <- "NON FORMULARY ITEM BISOPROLOL"
cvs[(item_cd=="S00277"), 1] <- "NON FORMULARY ITEM BOSENTAN"
cvs[(item_cd=="S00362"), 1] <- "NON FORMULARY ITEM FELODIPINE"
cvs[(item_cd=="S00453" | item_cd=="S00454" | item_cd=="S00455"), 1] <- "NON FORMULARY ITEM OLMESARTAN"
cvs[(item_cd=="S00495"), 1] <- "NON FORMULARY ITEM"
cvs[(item_cd=="S00531"), 1] <- "NON FORMULARY ITEM IRBESARTAN 150MG + HYDROCHLOROTHIAZIDE 12.5MG"
cvs[(item_cd=="S00694" | item_cd=="S00695"), 1] <- "NON FORMULARY ITEM DABIGATRAN"
cvs[(item_cd=="S00708" | item_cd=="S00900" | item_cd=="S00901"), 1] <- "NON FORMULARY ITEM RIVAROXABAN"
cvs[(item_cd=="S00539"), 2] <- "NA"
cvs[(item_cd=="S00736" | item_cd=="S00735"), 1] <- "NON FORMULARY ITEM TINZAPARIN"
cvs[(item_cd=="S00735"), 2] <- "10000AXA IU/0.5ML"
cvs[(item_cd=="S00736"), 2] <- "14000AXA IU/0.7ML"
cvs[(item_cd=="S00760"), 1] <- "NON FORMULARY ITEM PRASUGREL"
cvs[(item_cd=="S00862"), 1] <- "NON FORMULARY ITEM TICAGRELOR"
cvs[(item_cd=="S00920"), 1] <- "NON FORMULARY ITEM DABIGATRAN"
cvs[(item_cd=="S00943"), 1] <- "NON FORMULARY ITEM NEBIVOLOL"
cvs[(item_cd=="SILD04"), 1] <- "SILDENAFIL CITRATE (REVATIO)"
cvs[(item_cd=="SILD04"), 2] <- "20MG"
cvs[grepl("SIMV", x = cvs$item_cd), 1] <- "SIMVASTATIN"
cvs[(item_cd=="SIMV01"), 2] <- "10MG"
cvs[(item_cd=="SIMV02"), 2] <- "20MG"
cvs[(item_cd=="SIMV04"), 2] <- "40MG"
cvs[grepl("SOTA", x = cvs$item_cd), 1] <- "SOTALOL HYDROCHLORIDE"
cvs[(item_cd=="SOTA01"), 2] <- "80MG"
cvs[grepl("SPIR", x = cvs$item_cd), 1] <- "SPIRONOLACTONE"
cvs[(item_cd=="SPIR01"), 2] <- "25MG"
cvs[(item_cd=="SPIR04"), 2] <- "2MG/ML"
cvs[(item_cd=="SPIR06"), 2] <- "4MG/ML"
cvs[grepl("TELM", x = cvs$item_cd), 1] <- "TELMISARTAN"
cvs[(item_cd=="TELM01"), 2] <- "40MG"
cvs[(item_cd=="TELM02"), 2] <- "80MG"
cvs[grepl("TERA", x = cvs$item_cd), 1] <- "TERAZOSIN HCL"
cvs[(item_cd=="TERA01"), 2] <- "2MG"
cvs[(item_cd=="TERA02"), 2] <- "1MG"
cvs[(item_cd=="TICA01"), 2] <- "90MG"
cvs[(item_cd=="TICA01"), 1] <- "TICAGRELOR"
cvs[grepl("TINZ", x = cvs$item_cd), 1] <- "TINZAPARIN SODIUM"
cvs[(item_cd=="TINZ01"), 2] <- "20000AXA IU/ML"
cvs[(item_cd=="TINZ02"), 2] <- "10000AXA IU/ML"
cvs[(item_cd=="TINZ03"), 2] <- "10000AXA IU/0.5ML"
cvs[(item_cd=="TINZ04"), 2] <- "14000AXA IU/0.7ML"
cvs[(item_cd=="TINZ05"), 2] <- "3500AXA IU/0.35ML"
cvs[(item_cd=="TINZ06"), 2] <- "4500AXA IU/0.45ML"
cvs[grepl("VALS", x = cvs$item_cd), 1] <- "VALSARTAN"
cvs[(item_cd=="VALS02"), 2] <- "80MG"
cvs[(item_cd=="VALS03"), 2] <- "160MG"
cvs[grepl("VERA", x = cvs$item_cd), 1] <- "VERAPAMIL"
cvs[(item_cd=="VERA03"), 2] <- "2.5MG/ML 2ML"
cvs[(item_cd=="VERA04"), 1] <- "VERAPAMIL S.R."
cvs[grepl("WARF", x = cvs$item_cd), 1] <- "WARFARIN SODIUM"
cvs[(item_cd=="WARF01"), 2] <- "1MG"
cvs[(item_cd=="WARF02"), 2] <- "3MG"
cvs[(item_cd=="WARF03"), 2] <- "5MG"

sum(is.na(cvs))
cvs<-cvs[,list(n = sum(n)),c("drug_displayname", "new_mod_strength", "item_cd")][order(drug_displayname)]

cvs$yr2006<-""
cvs$yr2007<-""
cvs$yr2008<-""
cvs$yr2009<-""
cvs$yr2010<-""
cvs$yr2011<-""
cvs$yr2012<-""
cvs$yr2013<-""
cvs$yr2014<-""

save(cvs, cvs1, cvs2, cvs3, cvs4, cvs5, file="cvs_medications.Rdata")
write.csv(cvs, file="datashell_cvs.csv")