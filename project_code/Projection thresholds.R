required.packages <- c("WDI","data.table", "readxl")
lapply(required.packages, require, character.only=T)

projections <- function(PLs=c(1.9), Year="all"){
  
  setwd("G:/My Drive/Work/GitHub/poverty_predictions/")
  
  pov.lines <- PLs
  
  wb_un.regions <- fread("project_data/WB_UN regions.csv")
  names(wb_un.regions)[names(wb_un.regions) == "Povcal_Region"] <- "region"
  
  povcal.ind.out <- function(RefYears=T, countries, years="all", PLs=1.9, PPPs=NULL, display="c"){
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
    return(read.csv(url,header=T))
  }
  
  povcal.tot.out <- function(country="all",year="all",PL=1.9,display="c"){
    param <- paste0("RefYears=",year,"&PovertyLine=",PL,"&Countries=",country,"&display=",display)
    url <- paste0("http://iresearch.worldbank.org/PovcalNet/PovcalNetAPI.ashx?",param)
    return(read.csv(url,header=T))
  }
  
  pov <- as.data.table(povcal.tot.out())
  pov <- pov[pov[!is.na(HeadCount), .I[which.max(RequestYear)], by=.(CountryCode, CoverageType)]$V1]
  
  countries <- pov[, c("CountryCode", "CoverageType", "RequestYear", "PPP")]
  countries <- countries[CoverageType %in% c("N", "A")]
  
  #GDP per capita growth
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
  
  #Calculate new effective PPPs for each year of growth data
  WEO[, (year.cols) := lapply(1:ncol(.SD), function(i) ifelse(names(.SD)[i] <= RequestYear, 0, .SD[[i]])), .SDcols=(year.cols)]
  WEO[ISOAlpha_3Code=="CHN", (year.cols) := as.data.table(t(apply(.SD, 1, function(x) cumprod(1+((x*0.72)/100))))), .SDcols=(year.cols), by=ISOAlpha_3Code]
  WEO[ISOAlpha_3Code=="IND", (year.cols) := as.data.table(t(apply(.SD, 1, function(x) cumprod(1+((x*0.51)/100))))), .SDcols=(year.cols), by=ISOAlpha_3Code]
  WEO[!(ISOAlpha_3Code %in% c("CHN", "IND")), (year.cols) := as.data.table(t(apply(.SD, 1, function(x) cumprod(1+((x*0.87)/100))))), .SDcols=(year.cols), by=ISOAlpha_3Code]
  WEO[, (year.cols) := PPP/.SD, .SDcols=(year.cols)]
  
  proj.years <- seq(max(WEO$RequestYear), max(as.numeric(names(WEO)), na.rm=T))
  
  WEO.split <- split(WEO, seq(1:3))
  
  projpov.list <- list()
  for(i in 1:length(proj.years)){
    proj.year <- as.character(proj.years[i])
    year.data <- list()
    for(j in 1:length(WEO.split)){
      pl.data <- list()
      for(k in 1:length(pov.lines)){
        pl <- pov.lines[k]
        message(paste("Year:",proj.year,"; Poverty Line:",round(pl,2)))
        pl.data[[k]] <- povcal.ind.out(RefYears = T, countries = WEO.split[[j]]$ISOAlpha_3Code, years = unique(WEO.split[[j]]$RequestYear), PLs = pl, PPPs = unlist(WEO.split[[j]][,proj.year, with=F]))
      }
      year.data[[j]] <- rbindlist(pl.data)
    }
    proj <- rbindlist(year.data)
    proj$ProjYear <- proj.year
    projpov.list[[i]] <- proj
  }
  
  projpov <- rbindlist(projpov.list)
  projpov <- projpov[projpov[!is.na(HeadCount), .I[which.max(RequestYear)], by=.(CountryCode, CoverageType, PovertyLine, ProjYear)]$V1]
  
  keep <- c("CountryCode","CountryName","CoverageType","PovertyLine","HeadCount", "ProjYear")
  projpov <- projpov[,..keep]
  
  if(!("WUP_urban.xls" %in% list.files("project_data") & "WUP_rural.xls" %in% list.files("project_data"))){
    download.file("https://population.un.org/wup/Download/Files/WUP2018-F19-Urban_Population_Annual.xls", "project_data/WUP_urban.xls", mode="wb")
    download.file("https://population.un.org/wup/Download/Files/WUP2018-F20-Rural_Population_Annual.xls", "project_data/WUP_rural.xls", mode="wb")
  }
  
  wup.urb <- read_xls("project_data/WUP_urban.xls", skip=16)
  wup.rur <- read_xls("project_data/WUP_rural.xls", skip=16)
  wup.urb <- data.table(CoverageType="U",wup.urb[,c("Region, subregion, country or area", as.character(proj.years))])
  wup.rur <- data.table(CoverageType="R",wup.rur[,c("Region, subregion, country or area", as.character(proj.years))])
  wup.tot <- data.table(CoverageType="N",wup.urb[,"Region, subregion, country or area"],wup.urb[, as.character(proj.years), with=F]+wup.rur[, as.character(proj.years), with=F])
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
  wup.wb <- dcast(wup.wb[CoverageType == "N"], region ~ ., value.var = as.character(proj.years), fun.aggregate = sum)
  wup.wb <- melt(wup.wb, id.vars = "region")
  names(wup.wb) <- c("RegionCode", "ProjYear", "ReqYearPopulation")
  wup.wb$ReqYearPopulation <- wup.wb$ReqYearPopulation*1000
  
  wup.all <- melt(wup.all, id.vars = c("CountryName", "CoverageType"))
  names(wup.all) <- c("CountryName", "CoverageType", "ProjYear", "ReqYearPopulation")
  wup.all$ReqYearPopulation <- wup.all$ReqYearPopulation*1000
  
  projpov[CoverageType == "A"]$CoverageType <- "N"
  projpov[CountryName=="Argentina"]$CoverageType <- "N"
  
  projpov <- merge(projpov, wup.all, by=c("CountryName", "CoverageType", "ProjYear"), all.x=T)
  
  projpov$Level <- "National"
  projpov[CoverageType == "U" | CoverageType == "R"]$Level <- "Subnational"
  projpov$DisplayName <- projpov$CountryName
  projpov[CoverageType=="U"]$DisplayName <- paste0(projpov[CoverageType=="U"]$CountryName,"-Urban")
  projpov[CoverageType=="R"]$DisplayName <- paste0(projpov[CoverageType=="R"]$CountryName,"-Rural")
  projpov$NumPoor <- projpov$HeadCount*projpov$ReqYearPopulation
  projpov <- merge(projpov,wb_un.regions, by.x="CountryCode",by.y="ISO3",all.x=T)
  projpov.melt <- melt(projpov, id.vars=c("CountryCode","DisplayName","region","Level","ProjYear","PovertyLine"), measure.vars=c("HeadCount","NumPoor"))
  
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
  
  regionsprojpov.melt <- melt(regionsprojpovpop,id.vars=c("regionCode","DisplayName","region","Level","requestYear","PovertyLine"), measure.vars=c("HeadCount","NumPoor"))
  names(regionsprojpov.melt) <- c("CountryCode","DisplayName","region","Level","ProjYear","PovertyLine","variable","value")
  
  projpov.melt <- rbind(projpov.melt,regionsprojpov.melt)
  
  globalprojpov <- regionsprojpovpop[, .(NumPoor=sum(NumPoor)), by=.(requestYear,PovertyLine)]
  globalprojpov <- merge(globalprojpov, wup.all[CountryName=="WORLD" & CoverageType == "N"],by.x="requestYear",by.y="ProjYear",all.x=T)
  globalprojpov$HeadCount <- globalprojpov$NumPoor/globalprojpov$ReqYearPopulation
  globalprojpov$CountryCode <- "WLD"
  globalprojpov$DisplayName <- "World"
  globalprojpov$Level <- "Global"
  globalprojpov$region <- "Aggregates"
  globalprojpov.melt <- melt(globalprojpov,id.vars=c("CountryCode","DisplayName","region","Level","requestYear","PovertyLine"), measure.vars=c("HeadCount","NumPoor"))
  names(globalprojpov.melt) <- c("CountryCode","DisplayName","region","Level","ProjYear","PovertyLine","variable","value")
  
  projpov.melt <- rbind(projpov.melt,globalprojpov.melt)
  
  if(Year == "all"){return(projpov.melt)}else{return(projpov.melt[ProjYear==Year])}
}

tmp <- tempfile(fileext = ".xlsx")
download.file("https://www.imf.org/external/pubs/ft/weo/data/WEOhistorical.xlsx", tmp, mode = "wb")

find.threshold <-function(threshold, year = seq(2018,2025), lower = 0.01, upper = 25, tol = 0.001) {
  pvalue <- paste0("P", as.character(threshold*100))
  p <- rbindlist(lapply(year, function(setyear){data.table(requestYear = setyear, PL = optimise(function(x){abs(projections(x, setyear)[CountryCode == "WLD" & variable == "HeadCount"]$value - threshold)}, lower = lower, upper = upper, tol = tol)$minimum)}))
  setNames(p, c("year", pvalue))
}

out <- find.threshold(0.2, seq(2018,2025), lower=2.7, upper=3.5, tol=0.01)
