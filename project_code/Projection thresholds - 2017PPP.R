setwd("G:/My Drive/Work/GitHub/poverty_predictions/")
required.packages <- c("WDI","data.table", "readxl")
lapply(required.packages, require, character.only=T)

projections <- function(PLs=c(1.9), Year="all"){
  wb_un.regions <- fread("project_data/WB_UN regions.csv")
  names(wb_un.regions)[names(wb_un.regions) == "Povcal_Region"] <- "region"
  
  wb_un.regions[ISO3 == "KSV"]$ISO3 <- "XKX"
  wb_un.regions[ISO3 == "WBG"]$ISO3 <- "PSE"
  
  tmp <- tempfile(fileext = ".xlsx")
  download.file("https://www.imf.org/external/pubs/ft/weo/data/WEOhistorical.xlsx", tmp, mode = "wb")
  WEOraw <- as.data.table(read_excel(tmp, sheet = "pcpi_pch"))
  
  WEO.inflation <- WEOraw
  WEO.inflation <- WEO.inflation[, .(value = apply(.SD, 1, function(x) unlist(x)[x != "."][ifelse(length(unlist(x)[x != "."]) == 0, 1, length(unlist(x)[x != "."]))][[1]])), .SDcols = names(WEO.inflation)[-c(1:4)], by = .(country, ISOAlpha_3Code, year)]
  
  #WEO.inflation <- dcast(WEO.inflation, country + ISOAlpha_3Code ~ year, value.var = "latest")
  #WEO.inflation <- melt(WEO.inflation[, c("ISOAlpha_3Code", as.character(1980:2020))], id.vars = "ISOAlpha_3Code")
  
  WEO.inflation <- WEO.inflation[, .(year = as.numeric(as.character(year)), WEO.cpi = as.numeric(value)), by=ISOAlpha_3Code]
  
  ppps <- fread("project_data/ppps.csv")
  ppps[iso2c == "KP"]$iso3c <- "PRK"
  ppps[iso2c == "MK"]$iso3c <- "MKD"
  
  ppps <- ppps[iso3c != ""]
  
  ppps <- merge(ppps, WEO.inflation, by.x=c("iso3c", "year"), by.y=c("ISOAlpha_3Code", "year"), all.x=T)
  ppps[, WEO.cpi.ind := (cumprod(1+(ifelse(is.na(WEO.cpi), 0, WEO.cpi))/100)), by=iso3c]
  
  ppps[, WEO.cpi.ind := ifelse(is.na(WEO.cpi), as.numeric(NA), WEO.cpi.ind/WEO.cpi.ind[year == 2010]*100), by=.(iso3c)]
  
  ppps[, wb.incomplete := any(is.na(FP.CPI.TOTL)), by=iso3c][, `:=` (cpi = ifelse(wb.incomplete, WEO.cpi.ind, FP.CPI.TOTL), wb.incomplete = NULL)]
  
  ppps <- ppps[, `:=` (cpi = cpi/cpi[year==2011], PPP2011 = PA.NUS.PRVT.PP[year == 2011]), by=.(iso3c)][year == 2017]
  ppps[, LCU2011.PPP2017 := PA.NUS.PRVT.PP/cpi, by=.(iso3c)]
  
  #Manual PPP fixes
  ppps[iso3c == "BLR"]$LCU2011.PPP2017 <- ppps[iso3c == "BLR"]$LCU2011.PPP2017*10000
  ppps[iso3c == "LTU"]$LCU2011.PPP2017 <- ppps[iso3c == "LTU"]$LCU2011.PPP2017*3.4528
  ppps[iso3c == "LVA"]$LCU2011.PPP2017 <- ppps[iso3c == "LVA"]$LCU2011.PPP2017*0.702804
  ppps[iso3c == "ZMB" | iso3c == "STP"]$LCU2011.PPP2017 <- ppps[iso3c == "ZMB" | iso3c == "STP"]$LCU2011.PPP2017*1000
  ppps[iso3c == "MRT"]$LCU2011.PPP2017 <- ppps[iso3c == "MRT"]$LCU2011.PPP2017*10
  
  ppps[iso3c == "BLR"]$PPP2011 <- ppps[iso3c == "BLR"]$PPP2011*10000
  ppps[iso3c == "LTU"]$PPP2011 <- ppps[iso3c == "LTU"]$PPP2011*3.4528
  ppps[iso3c == "LVA"]$PPP2011 <- ppps[iso3c == "LVA"]$PPP2011*0.702804
  ppps[iso3c == "ZMB" | iso3c == "STP"]$PPP2011 <- ppps[iso3c == "ZMB" | iso3c == "STP"]$PPP2011*1000
  ppps[iso3c == "MRT"]$PPP2011 <- ppps[iso3c == "MRT"]$PPP2011*10
  
  ################
  #HHFCEG v GPDG
  # fce.gdp.est <- as.data.table(WDI("all", c("NE.CON.PRVT.PC.KD.ZG","NY.GDP.PCAP.KD.ZG"), start=1981, end=2018, extra=T))
  # fce.gdp.est[, nas:=!all(is.na(NE.CON.PRVT.PC.KD.ZG))&!all(is.na(NY.GDP.PCAP.KD.ZG)), by=iso2c]
  # fce.gdp.est <- fce.gdp.est[fce.gdp.est$nas]
  # fce.gdp.est <- fce.gdp.est[, .(gdp=NY.GDP.PCAP.KD.ZG/100, fce=NE.CON.PRVT.PC.KD.ZG/100),by=.(iso3c, iso2c, country, year)]
  # fce.gdp.est <- fce.gdp.est[, .(beta=glm(fce ~ gdp + shift(gdp) + 0)$coefficients, lag=c(0,1)), by=.(iso3c, iso2c, country)]
  # 
  # fce.gdp.est[iso2c == "MK"]$iso3c <- "MKD"
  # 
  # fce.gdp.est <- dcast(fce.gdp.est[!is.na(iso3c)], iso3c ~ lag, value.var = "beta")
  
  povcal.ind.out <- function(RefYears=T, countries, coverage, years="all", PLs=1.9, PPPs=NULL, display="c"){
    covtypes <- data.table(code=c("R", "U", "N", "A"), num=c(1, 2, 3, 5))
    coverage <- with(covtypes, num[match(coverage, code)])
    countries <- paste0(countries, "_", coverage)
    if(length(PLs) == 1){
      p <- paste0("PovertyLine=", PLs)
    } else {
      p <- paste0("PL", seq(0, length(PLs)-1), "=", PLs, collapse = "&")
    }
    d <- paste0("display=", display)
    c <- paste0("C", seq(0, length(countries)-1), "=", countries, collapse = "&")
    if(RefYears){
      y <- paste0("RefYears=", paste0(years, collapse=","))
    } else {
      if(length(years) == 1){
        years <- rep(years, length(countries))
      }
      y <- paste0("Y", seq(0, length(years)-1), "=", years, collapse = "&")
    }
    if(!(is.null(PPPs))){
      pp <- paste0("PPP", seq(0, length(PPPs)-1), "=", PPPs, collapse = "&")
    } else {
      pp <- NULL
    }
    param <- paste0(c(y, c, p, pp, d), collapse="&")
    url <- paste0("http://iresearch.worldbank.org/PovcalNet/PovcalNetAPI.ashx?",param)
    while(T){
      out <- read.csv(url,header=T)
      if(ncol(out) > 2){
        break
      }
      message("PovcalNet error. Retrying.")
    }
    return(out)
  }
  
  povcal.tot.out <- function(country="all",year="all",PL=1.9,display="c"){
    param <- paste0("RefYears=",year,"&PovertyLine=",PL,"&Countries=",country,"&display=",display)
    url <- paste0("http://iresearch.worldbank.org/PovcalNet/PovcalNetAPI.ashx?",param)
    return(read.csv(url,header=T))
  }
  
  #pov.lines <- c(seq(0.01, 25, 0.01), seq(26, 1000, 1))
  pov.lines <- PLs
  
  povlist <- list()
  for(i in 1:length(pov.lines)){
    povlist[[i]] <- as.data.table(povcal.tot.out(PL=pov.lines[i]))
  }
  pov <- rbindlist(povlist)
  
  pov <- pov[!is.na(HeadCount)]
  
  pov.rec <- pov[pov[PovertyLine == unique(pov$PovertyLine)[1], .I[which.max(RequestYear)], by=.(CountryCode, CoverageType)]$V1]
  
  countries <- pov.rec[, c("CountryCode", "CoverageType", "RequestYear", "PPP", "Mean")]
  
  countries <- rbind(countries[CoverageType %in% c("N", "A")], countries[, .SD[!any(CoverageType %in% c("N", "A"))],by=CountryCode])
  
  #GDP per capita growth
  tmp <- tempfile(fileext = ".xlsx")
  download.file("https://www.imf.org/external/pubs/ft/weo/data/WEOhistorical.xlsx", tmp, mode = "wb")
  WEOraw <- as.data.table(read_excel(tmp, sheet = "ngdp_rpch"))
  
  WEO <- WEOraw
  WEO <- WEO[, .(latest = apply(.SD, 1, function(x) unlist(x)[x != "."][ifelse(length(unlist(x)[x != "."]) == 0, 1, length(unlist(x)[x != "."]))][[1]])), .SDcols = names(WEO)[-c(1:4)], by = .(country, ISOAlpha_3Code, year)]
  
  WEO <- dcast(WEO, country + ISOAlpha_3Code ~ year, value.var = "latest")
  
  year.cols <- as.character(seq(min(countries$RequestYear), max(as.numeric(names(WEO)), na.rm=T)))
  WEO <- WEO[, lapply(.SD, as.numeric), .SDcols=(year.cols), by=.(ISOAlpha_3Code)]
  
  WEO <- WEO[, lapply(.SD, function(x) x[!is.na(x)][1]), .SDcols=(year.cols), by=ISOAlpha_3Code]
  
  WEO[WEO=="--"] <- 0
  
  WEO[ISOAlpha_3Code == "KOS"]$ISOAlpha_3Code <- "XKX"
  
  WEO <- merge(WEO, countries, by.x="ISOAlpha_3Code", by.y="CountryCode", all.y=T)
  WEO <- merge(WEO, wb_un.regions, by.x="ISOAlpha_3Code", by.y="ISO3")
  
  #Calculate new effective PPPs for each year of growth data
  WEO[, (year.cols) := lapply(1:ncol(.SD), function(i) ifelse(names(.SD)[i] <= RequestYear, 0, .SD[[i]])), .SDcols=(year.cols)]
  # WEO <- merge(WEO, fce.gdp.est, by.x="WB_Region", by.y="iso3c", all.x=T)
  
  WEO[ISOAlpha_3Code=="CHN", (year.cols) := as.data.table(t(apply(.SD, 1, function(x) cumprod(1+((x*0.72)/100))))), .SDcols=(year.cols), by=ISOAlpha_3Code]
  WEO[ISOAlpha_3Code=="IND", (year.cols) := as.data.table(t(apply(.SD, 1, function(x) cumprod(1+((x*0.51)/100))))), .SDcols=(year.cols), by=ISOAlpha_3Code]
  WEO[!(ISOAlpha_3Code %in% c("CHN", "IND")), (year.cols) := as.data.table(t(apply(.SD, 1, function(x) cumprod(1+((x*0.87)/100))))), .SDcols=(year.cols), by=ISOAlpha_3Code]
  
  # WEO[, (year.cols) := as.data.table(t(apply(.SD, 1, function(x, beta0, beta1) (1+(x*beta0+shift(x)*beta1)/100), beta0=`0`, beta1=`1`))), .SDcols=(year.cols), by=ISOAlpha_3Code]
  # WEO[, (year.cols) := lapply(1:ncol(.SD), function(i) ifelse(names(.SD)[i] <= RequestYear, 1, .SD[[i]])), .SDcols=(year.cols)]
  # WEO[, (year.cols) := as.data.table(t(apply(.SD, 1, function(x) cumprod(x)))), .SDcols=(year.cols), by=ISOAlpha_3Code]
  
  WEO <- merge(WEO, ppps, by.x="ISOAlpha_3Code", by.y="iso3c", all.x=T)
  
  WEO[, (year.cols) := LCU2011.PPP2017/.SD, .SDcols=(year.cols)]

  proj.years <- as.character(Year)
  
  year.lines <- expand.grid(ProjYears=proj.years, PovertyLines=pov.lines)
  #year.lines <- rbind(year.lines, setNames(p20thresholds, c("ProjYears", "PovertyLines")))
  
  WEO.split <- split(WEO, seq(1:4))
  
  projpov.list <- list()
  for(i in 1:nrow(year.lines)){
    proj.year <- as.character(year.lines$ProjYears[i])
    pl <- year.lines$PovertyLines[i]
    message(paste("Year:",proj.year,"; Poverty Line:",round(pl,2)))
    year.data <- list()
    for(j in 1:length(WEO.split)){
      year.data[[j]] <- povcal.ind.out(RefYears = T, countries = WEO.split[[j]]$ISOAlpha_3Code, coverage=WEO.split[[j]]$CoverageType, years = unique(WEO.split[[j]]$RequestYear), PLs = pl, PPPs = unlist(WEO.split[[j]][,proj.year, with=F]))
    }
    proj <- rbindlist(year.data)
    proj$ProjYear <- proj.year
    projpov.list[[i]] <- proj
  }
  
  projpov <- rbindlist(projpov.list)
  projpov <- projpov[projpov[!is.na(HeadCount), .I[which.max(RequestYear)], by=.(CountryCode, CoverageType, PovertyLine, ProjYear)]$V1]
  #fwrite(projpov, "project_data/scrape.csv")
  
  keep <- c("CountryCode","CountryName","CoverageType","PovertyLine","HeadCount")
  projpov <- projpov[,c(..keep, "ProjYear")]
  
  old.pov <- WEO[!is.na(LCU2011.PPP2017), c("ISOAlpha_3Code", "CoverageType", "LCU2011.PPP2017")]
  old.pov.split <- split(old.pov, seq(1:4))
  old.pov.list <- list()
  for(i in 1:length(pov.lines)){
    pl <- pov.lines[i]
    year.data <- list()
    for(j in 1:length(old.pov.split)){
      year.data[[j]] <- povcal.ind.out(RefYears = T, countries = old.pov.split[[j]]$ISOAlpha_3Code, coverage=old.pov.split[[j]]$CoverageType, PLs = pl, PPPs=old.pov.split[[j]]$LCU2011.PPP2017)
    }
    old.pov.list[[i]] <- rbindlist(year.data)
  }
  
  pov <- rbindlist(old.pov.list)
  pov <- pov[!is.na(HeadCount)]
  
  pov <- pov[,c(..keep, "RequestYear")]
  names(pov)[names(pov)=="RequestYear"] <- "ProjYear"
  
  #modelled <- fread("project_data/modelled_countries.csv")
  
  projpov <- projpov[!(paste0(CountryName, ProjYear) %in% paste0(pov$CountryName, pov$ProjYear))]
  
  projpov <- rbind(projpov, pov)#, modelled)
  
  if(!("WUP_urban.xls" %in% list.files("project_data") & "WUP_rural.xls" %in% list.files("project_data"))){
    download.file("https://population.un.org/wup/Download/Files/WUP2018-F19-Urban_Population_Annual.xls", "project_data/WUP_urban.xls", mode="wb")
    download.file("https://population.un.org/wup/Download/Files/WUP2018-F20-Rural_Population_Annual.xls", "project_data/WUP_rural.xls", mode="wb")
  }
  
  all.years <- c(min(as.character(projpov$ProjYear)):max(as.character(projpov$ProjYear)))
  
  wup.urb <- read_xls("project_data/WUP_urban.xls", skip=16)
  wup.rur <- read_xls("project_data/WUP_rural.xls", skip=16)
  wup.urb <- data.table(CoverageType="U",wup.urb[,c("Region, subregion, country or area", as.character(all.years))])
  wup.rur <- data.table(CoverageType="R",wup.rur[,c("Region, subregion, country or area", as.character(all.years))])
  wup.tot <- data.table(CoverageType="N",wup.urb[,"Region, subregion, country or area"],wup.urb[, as.character(all.years), with=F]+wup.rur[, as.character(all.years), with=F])
  wup.all <- rbind(wup.tot,wup.urb,wup.rur)
  names(wup.all)[names(wup.all) == "Region, subregion, country or area"] <- "CountryName"
  
  {
    wup.all$CountryName[which(wup.all$CountryName=="Bolivia (Plurinational State of)")]="Bolivia"
    wup.all$CountryName[which(wup.all$CountryName=="Democratic Republic of the Congo")]="Congo, Democratic Republic of"
    wup.all$CountryName[which(wup.all$CountryName=="Congo")]="Congo, Republic of"
    wup.all$CountryName[which(wup.all$CountryName=="Côte d'Ivoire")]="Cote d'Ivoire"
    wup.all$CountryName[which(wup.all$CountryName=="Czechia")]="Czech Republic"
    wup.all$CountryName[which(wup.all$CountryName=="Egypt")]="Egypt, Arab Republic of"
    wup.all$CountryName[which(wup.all$CountryName=="Gambia")]="Gambia, The"
    wup.all$CountryName[which(wup.all$CountryName=="Iran (Islamic Republic of)")]="Iran, Islamic Republic of"
    wup.all$CountryName[which(wup.all$CountryName=="Republic of Korea")]="Korea, Republic of"
    wup.all$CountryName[which(wup.all$CountryName=="Kyrgyzstan")]="Kyrgyz Republic"
    wup.all$CountryName[which(wup.all$CountryName=="TFYR Macedonia")]="North Macedonia"
    wup.all$CountryName[which(wup.all$CountryName=="Micronesia (Fed. States of)")]="Micronesia, Federated States of"
    wup.all$CountryName[which(wup.all$CountryName=="Republic of Moldova")]="Moldova"
    wup.all$CountryName[which(wup.all$CountryName=="Slovakia")]="Slovak Republic"
    wup.all$CountryName[which(wup.all$CountryName=="Saint Lucia")]="St. Lucia"
    wup.all$CountryName[which(wup.all$CountryName=="Swaziland")]="Eswatini"
    wup.all$CountryName[which(wup.all$CountryName=="United Republic of Tanzania")]="Tanzania"
    wup.all$CountryName[which(wup.all$CountryName=="United States of America")]="United States"
    wup.all$CountryName[which(wup.all$CountryName=="Venezuela (Bolivarian Republic of)")]="Venezuela, Republica Bolivariana de"
    wup.all$CountryName[which(wup.all$CountryName=="Viet Nam")]="Vietnam"
    wup.all$CountryName[which(wup.all$CountryName=="Yemen")]="Yemen, Republic of"
    wup.all$CountryName[which(wup.all$CountryName=="State of Palestine")]="West Bank and Gaza"
    wup.all$CountryName[which(wup.all$CountryName=="China, Taiwan Province of China")]="Taiwan, China"
    wup.all[CountryName == "Bahamas"]$CountryName <- "Bahamas, The"
    wup.all[CountryName == "Curaçao"]$CountryName <- "Curacao"
    wup.all[CountryName == "Faeroe Islands"]$CountryName <- "Faroe Islands"
    wup.all[CountryName == "China, Hong Kong SAR"]$CountryName <- "Hong Kong SAR, China"
    wup.all[CountryName == "Dem. People's Republic of Korea"]$CountryName <- "Korea, Democratic People's Republic of"
    wup.all[CountryName == "China, Macao SAR"]$CountryName <- "Macao SAR, China"
    wup.all[CountryName == "Caribbean Netherlands"]$CountryName <- "Netherlands Antilles"
    wup.all[CountryName == "Saint Kitts and Nevis"]$CountryName <- "St. Kitts and Nevis"
    wup.all[CountryName == "Martinique"]$CountryName <- "St. Martin (French part)"
    wup.all[CountryName == "Saint Vincent and the Grenadines"]$CountryName <- "St. Vincent and the Grenadines"
    wup.all[CountryName == "United States Virgin Islands"]$CountryName <- "Virgin Islands, US"
  }
  
  wup.wb <- merge(wup.all, wb_un.regions)
  wup.wb <- dcast.data.table(wup.wb[CoverageType == "N"], region ~ ., value.var = as.character(all.years), fun.aggregate = sum)
  wup.wb <- melt(wup.wb, id.vars = "region")
  names(wup.wb) <- c("RegionCode", "ProjYear", "ReqYearPopulation")
  wup.wb$ReqYearPopulation <- wup.wb$ReqYearPopulation*1000
  
  wup.all <- melt(wup.all, id.vars = c("CountryName", "CoverageType"))
  names(wup.all) <- c("CountryName", "CoverageType", "ProjYear", "ReqYearPopulation")
  wup.all$ReqYearPopulation <- wup.all$ReqYearPopulation*1000
  
  projpov[CoverageType == "A"]$CoverageType <- "N"
  projpov[CountryName=="Argentina"]$CoverageType <- "N"
  
  projpov <- merge(projpov, wup.all, by=c("CountryName", "CoverageType", "ProjYear"), all.x=T)
  
  projpov <- projpov[complete.cases(projpov)]
  
  projpov$Level <- "National"
  projpov[CoverageType == "U" | CoverageType == "R"]$Level <- "Subnational"
  projpov$DisplayName <- projpov$CountryName
  projpov[CoverageType=="U"]$DisplayName <- paste0(projpov[CoverageType=="U"]$CountryName,"-Urban")
  projpov[CoverageType=="R"]$DisplayName <- paste0(projpov[CoverageType=="R"]$CountryName,"-Rural")
  projpov$NumPoor <- projpov$HeadCount*projpov$ReqYearPopulation
  projpov <- merge(projpov,wb_un.regions, by.x="CountryCode",by.y="ISO3",all.x=T)
  projpov.melt <- melt(projpov, id.vars=c("CountryCode","DisplayName","region","Level","ProjYear","PovertyLine"), measure.vars=c("HeadCount","NumPoor", "ReqYearPopulation"))
  
  regions <- data.table(regionCode=c("EAP","ECA","LAC","MNA","NAC","SAS","SSA","OHI"),regionName=c("East Asia & Pacific","Europe & Central Asia","Latin America & Caribbean","Middle East & North Africa","North America","South Asia","Sub-Saharan Africa","Other High Income"))
  
  regionsprojpov <- projpov[Level=="National", .(HeadCount=sum(NumPoor, na.rm=T)/sum(ReqYearPopulation, na.rm=T)), by=.(region, ProjYear, PovertyLine)]
  names(regionsprojpov) <- c("regionCode", "requestYear", "PovertyLine", "hc")
  regionsprojpov <- merge(regionsprojpov, regions)
  
  regionsprojpovpop <- merge(regionsprojpov, wup.wb, by.x=c("requestYear","regionCode"), by.y=c("ProjYear","RegionCode"),all.x=T)
  regionsprojpovpop$NumPoor <- regionsprojpovpop$hc*regionsprojpovpop$ReqYearPopulation
  regionsprojpovpop$HeadCount <- regionsprojpovpop$hc
  regionsprojpovpop$DisplayName <- regionsprojpovpop$regionName
  regionsprojpovpop$Level <- "Regional"
  regionsprojpovpop$region <- "Aggregates"
  
  regionsprojpov.melt <- melt(regionsprojpovpop,id.vars=c("regionCode","DisplayName","region","Level","requestYear","PovertyLine"), measure.vars=c("HeadCount","NumPoor", "ReqYearPopulation"))
  names(regionsprojpov.melt) <- c("CountryCode","DisplayName","region","Level","ProjYear","PovertyLine","variable","value")
  
  projpov.melt <- rbind(projpov.melt,regionsprojpov.melt)
  
  globalprojpov <- regionsprojpovpop[, .(NumPoor=sum(NumPoor)), by=.(requestYear,PovertyLine)]
  globalprojpov <- merge(globalprojpov, wup.all[CountryName=="WORLD" & CoverageType == "N"],by.x="requestYear",by.y="ProjYear",all.x=T)
  globalprojpov$HeadCount <- globalprojpov$NumPoor/globalprojpov$ReqYearPopulation
  globalprojpov$CountryCode <- "WLD"
  globalprojpov$DisplayName <- "World"
  globalprojpov$Level <- "Global"
  globalprojpov$region <- "Aggregates"
  globalprojpov.melt <- melt(globalprojpov,id.vars=c("CountryCode","DisplayName","region","Level","requestYear","PovertyLine"), measure.vars=c("HeadCount","NumPoor","ReqYearPopulation"))
  names(globalprojpov.melt) <- c("CountryCode","DisplayName","region","Level","ProjYear","PovertyLine","variable","value")
  
  projpov.melt <- rbind(projpov.melt,globalprojpov.melt)
  
  if(Year == "all"){return(projpov.melt)}else{return(projpov.melt[ProjYear==Year])}
}

find.threshold <-function(threshold, year = seq(2018,2021), lower = 0.01, upper = 25, tol = 0.001) {
  pvalue <- paste0("P", as.character(threshold*100))
  p <- rbindlist(lapply(year, function(setyear){data.table(requestYear = setyear, PL = optimise(function(x){abs(projections(x, setyear)[CountryCode == "WLD" & variable == "HeadCount"]$value - threshold)}, lower = lower, upper = upper, tol = tol)$minimum)}))
  setNames(p, c("year", pvalue))
}

old.pov <- fread("output/globalproj_long_Apr20.csv")
old.pov <- old.pov[DisplayName == "World" & variable == "HeadCount"]

old.pov <- old.pov[ProjYear == 2018]

out <- list()
for(i in 1:nrow(old.pov)){
  out[[i]] <- find.threshold(threshold=round(old.pov$value[i], 4), year=old.pov$ProjYear[i], lower=old.pov$PovertyLine[i], upper=old.pov$PovertyLine[i]*1.5, tol=0.01)
}

out <- rbindlist(out, fill=T)