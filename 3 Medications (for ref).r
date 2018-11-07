# Medications data for RECODe risk model
###################################################################
rm(list = ls())

# Extract DM medications
######################################
load("Rdata/medication.Rdata")
d <- medication[, c("serial_no", "disp_date", "item_cd", "duration")]
head(d)
dim(d)
rm(medication); gc()

# drugcodes from HA datascheme table (sheet 2)
statin.code <- c(paste0("ATOR0", 1:4), 
"S00080","S00876","S00887","S00917",
"FLUV02","FLUV03","S00240","FLUV05","S00082",
"LOVA01",
"PRAV01","PRAV02",
paste0("ROSU0", 1:3),"S00033", "S00183", "S00007", 
"S00293", "SIMV01", "SIMV02", "SIMV04", "SIMV05")

# blood pressure treatment
bprx.code <- c("ACEB01", "ACEB02", "AMIL01",
	paste0("AMLO0", 1:3),
	paste0("ATEN0", 1:3),
	"BISO01","BISO02",
	"S00256",
	"S00274",
	"BOSE01",
	"BOSE02",
	"S00276",
	"S00277",
	"BUME01",
	"BUME02",
	"CAND01",
	"CAND02",
	"S00063",
	"S00434",
	"S00870",
	paste0("CAPT0", 1:7),
	paste0("CARV0", 1:4),
	"S00371",
	"S00372",
	"CELI01",
	"CILA01",
	"CILA02",
	"CILA03",
	"DIHY01",
	"DILT01",
	"DILT02",
	"DILT05",
	"DILT06",
	"DILT07",
	"DILT08",
	"S00354",
	"S00355",
	"DILT03",
	"DILT04",
	"DOXA05",
	"DOXA06",
	"S00330",
	"DOXA02",
	"DOXA03",
	"DOXA04",
	"DUXA01",
	"S00356",
	"DYAZ01",
	"ENAL01",
	"ENAL02",
	"ENAL03",
	"ENAL04",
	"FELO01",
	"FELO02",
	"FELO03",
	"S00362",
	"FOSI01",
	"FRUS09",
	"FRUS01",
	"FRUS02",
	"FRUS03",
	"FRUS04",
	"FRUS06",
	"FRUS07",
	"FRUS10",
	"FRUS05",
	"FRUS08",
	"HYDR01",
	"HYDR02",
	"HYDR03",
	"HYDR46",
	"HYDR65",
	"HYDR05",
	"HYDR30",
	"HYDR38",
	"HYDR49",
	"HYDR50",
	"ILOP02",
	"S00471",
	"ILOP01",
	"S00259",
	"INDA02",
	"S00085",
	"INDA01",
	"IRBE03",
	"IRBE04",
	"S00531",
	"IRBE01",
	"IRBE02",
	"S00230",
	"S00250",
	"LABE01",
	"LABE02",
	"LABE03",
	"LABE04",
	"LABE05",
	"LABE06",
	"LACI01",
	"LACI02",
	"LERC01",
	"S00226",
	"LISI01",
	"LISI02",
	"LISI03",
	"LOSA02",
	"LOSA04",
	"S00162",
	"LOSA01",
	"LOSA03",
	"S00076",
	"S00396",
	"MANN02",
	"MANN03",
	"MANN07",
	"MANN08",
	"METH21",
	"METH22",
	"METH23",
	"METH78",
	"METO05",
	"METO10",
	"METO11",
	"METO13",
	"METO16",
	"S00077",
	"S00188",
	"S00380",
	"S00410",
	"METO08",
	"METO06",
	"METO07",
	"METO09",
	"METO15",
	"METO17",
	"MODU01",
	"NADO01",
	"S00943",
	"NIFE04",
	"NIFE05",
	"NIFE07",
	"S00067",
	"S00071",
	"NIFE03",
	"NIFE08",
	"NIFE06",
	"NIFE01",
	"NIFE02",
	"NIMO02",
	"NIMO01",
	"NITR06",
	"S00453",
	"S00454",
	"S00455",
	"OXPR02",
	"PERI29",
	"PERI17",
	"PERI28",
	"S00265",
	"PIND01",
	"PRAZ03",
	"PRAZ04",
	"PRAZ05",
	"PRED25",
	"PROP07",
	"PROP13",
	"PROP04",
	"PROP05",
	"PROP06",
	"PROP08",
	"PROP14",
	"PROP16",
	"PROP22",
	"PROP23",
	"PROP25",
	"PROP29",
	"QUIN08",
	"QUIN09",
	"QUIN10",
	"RAMI01",
	"RAMI02",
	"RAMI03",
	"S00029",
	"S00030",
	"RESE01",
	"SILD04",
	"SOTA01",
	"SOTA02",
	"SOTA03",
	"SOTA04",
	"SOTA05",
	"SPIR01",
	"SPIR02",
	"SPIR04",
	"SPIR05",
	"SPIR06",
	"S00031",
	"S00032",
	"TELM01",
	"TELM02",
	"TERA01",
	"TERA02",
	"TERA03",
	"S00069",
	"VALS01",
	"VALS02",
	"VALS03",
	"S00209",
	"VERA04",
	"VERA01",
	"VERA02",
	"VERA03")

# anticoagulant treatment (other than aspirin)
anticoag.code <- c("ABCI01",
	"S00002",
	"AGGR01",
	"S00539",
	"CLOP05",
	"DABI01",
	"DABI02",
	"DABI03",
	"S00694",
	"S00695",
	"S00920",
	"DALT04",
	"DALT03",
	"DALT01",
	"DALT02",
	"DIPY01",
	"DIPY02",
	"DIPY03",
	"DIPY04",
	"ENOX01",
	"ENOX02",
	"ENOX03",
	"ENOX04",
	"S00495",
	"EPTI02",
	"EPTI01",
	"S00364",
	"HEPA16",
	"HEPA21",
	"HEPA22",
	"HEPA29",
	"HEPA01",
	"HEPA27",
	"HEPA11",
	"HEPA18",
	"HEPA19",
	"HEPA23",
	"HEPA05",
	"HEPA15",
	"HEPA41",
	"HEPA42",
	"HEPA17",
	"HEPA03",
	"HEPA04",
	"NADR04",
	"NADR03",
	"NADR02",
	"NADR01",
	"PRAS01",
	"PRAS02",
	"S00760",
	"RIVA07",
	"RIVA08",
	"RIVA09",
	"S00708",
	"S00900",
	"S00901",
	"S00862",
	"TICA01",
	"S00735",
	"TINZ03",
	"S00005",
	"TINZ02",
	"S00736",
	"TINZ04",
	"TINZ01",
	"S00187",
	"TINZ05",
	"TINZ06",
	"WARF01",
	"WARF02",
	"WARF03")
	
	
d$statin <- d$item_cd %in% statin.code
d$bprx <- d$item_cd %in% bprx.code
d$anticoag <- d$item_cd %in% anticoag.code

d <- d[d$statin == T | d$bprx == T | d$anticoag == T, ]
format(object.size(d), "auto")
length(unique(d$serial_no))

d$disp_date <- as.Date(paste("15", d$disp_date, sep = ""), "%d%b%Y")
d <- d[order(d$serial_no, d$disp_date),]
head(d)

library(data.table)
DT <- data.table(d)
DT <- DT[, list(statin=max(statin), bprx=max(bprx), anticoag=max(anticoag)), by=c("serial_no", "disp_date")]
DT
format(object.size(DT), "auto")

RECODe.meds <- DT
# save(RECODe.meds, file = "Rdata/RECODe_meds.Rdata")


# Medications data for Taiwan risk model
###################################################################
# rm(list = ls())

# Extract DM medications
######################################
load("Rdata/medication.Rdata")
d <- medication[, c("serial_no", "disp_date", "item_cd", "duration")]
head(d)
dim(d)
rm(medication); gc()

# Fibrate derivatives (antihyperlipidaemic drugs other than statins)
fibrate.code <- c("CHOL10", 
                  "CHOL04", 
                  "EZET01",
                  "S00125",
                  "FENO05",
                  "FENO06",
                  "FENO03",
                  "FENO04",
                  "GEMF01",
                  "GEMF02",
                  "GEMF03")

d$fibrate <- d$item_cd %in% fibrate.code

d <- d[d$fibrate == T, ]
format(object.size(d), "auto")
length(unique(d$serial_no))

d$disp_date <- as.Date(paste("15", d$disp_date, sep = ""), "%d%b%Y")
d <- d[order(d$serial_no, d$disp_date),]
head(d)

library(data.table)
DT <- data.table(d)
DT <- DT[, list(fibrate=max(fibrate)), by=c("serial_no", "disp_date")]
DT
format(object.size(DT), "auto")

meds.TW <- DT
# save(meds.TW, file = "Rdata/meds_TW.Rdata")

