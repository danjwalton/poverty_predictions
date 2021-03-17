required.packages <- c("data.table","WDI","XML","readxl")
lapply(required.packages, require, character.only=T)

setwd("G:/My Drive/Work/GitHub/poverty_predictions")

i <- 0
while(i < 10){
  try({
    rm(ppps)
    ppps <- as.data.table(WDI(indicator = c("PA.NUS.PRVT.PP", "FP.CPI.TOTL"), extra=T))
  }, silent=T)
  if(exists("ppps")){
    if(all(c("PA.NUS.PRVT.PP", "FP.CPI.TOTL") %in% names(ppps))){
      fwrite(ppps, "project_data/ppps.csv")
      break
    }
  }
  i <- i + 1
  print(paste0("Error. Retrying... ",i,"/10"))
}
ppps <- ppps[, FP.CPI.TOTL := FP.CPI.TOTL/FP.CPI.TOTL[year==2011], by=.(iso3c)][year == 2017]
ppps <- ppps[, c("iso3c", "PA.NUS.PRVT.PP", "FP.CPI.TOTL")]

###FUNCTIONS

#Remap survey types
remap_cov <- function(x){
  cov_dict <- c(
    "R"=1,
    "U"=2,
    "N"=3,
    "A"=NA
  )
  return(cov_dict[as.character(x)])
}
remap_cov <- Vectorize(remap_cov)

#Solve distributions for headcounts
GQsolve <- function(a,b,c,PL,mu){
  e <- -(a+b+c+1)
  m <- b^2-4*a
  n <- 2*b*e-4*c
  z <- PL/mu+b/2
  y <- 16*z^2*m-4*m^2
  x <- 16*z^2*n-4*n*m
  w <- 16*z^2*e^2-n^2
  p1 <- (-x-(sqrt(x^2-4*y*w)))/(2*y)
  p2 <- (-x+(sqrt(x^2-4*y*w)))/(2*y)
  if(p1>1 | p1<0){
    p1 <- NaN
  }
  if(p2>1 | p2<0){
    p2 <- NaN
  }
  return(data.table(GQP1=p1,GQP2=p2))
}

Betasolve <- function(a,b,c,PL,mu,init){
  p0 <- init
  N <- 10000
  tol <- 1E-15
  i <- 1
  p1 <- p0
  s <- numeric(N)
  while (i<=N) {
    f <- a*(p0^b)*((1-p0)^c)*((b/p0)-c/(1-p0))-(1-PL/mu)
    df.dx <- a*(p0^(b-2))*((1-p0)^(c-2))*((b^2)*((p0-1)^2)+b*(p0-1)*((2*c-1)*p0+1)+(c-1)*c*(p0^2))
    p1 <- (p0 - 0.01*(f/df.dx))
    s[i] <- p1
    i <- i + 1
    if (abs(p1-p0) < tol) {break}
    p0 <- p1
  }
  return(data.table(BetaP1=s[i-1]))
}

#Manual addition

afg <- data.table(A= c(0.935463769, 0.5760727),
                  B= c(-1.476794803, 0.856948964),
                  C= c(0.083512829, 0.49178234),
                  ISOAlpha_3Code= "AFG",
                  CountryName = "Afghanistan",
                  year= 2016.5,
                  mean= 21973.19478/365.2424,
                  ppp=  21.5432,
                  type=c("GQ","Beta")
                  )

eri <- data.table(A= c(1.374421323, 1.261376489),
                  B= c(-1.165524803, 1.152561641),
                  C= c(-0.156452176, 0.800896719),
                  ISOAlpha_3Code= "ERI",
                  CountryName = "Eritrea",
                  year= 2011,
                  mean= 6758.835465/365.2424,
                  ppp=  6.5235252 ,
                  type=c("GQ","Beta")
)

lby <- data.table(A= c(1.002559972, 0.613658334),
                  B= c(-1.341360921, 0.934277217),
                  C= c(0.241518659, 0.607012527),
                  ISOAlpha_3Code= "LBY",
                  CountryName = "Libya",
                  year= 2008,
                  mean= 1918.570249/365.2424,
                  ppp= 0.6577641,
                  type=c("GQ","Beta")
)

gnq <- data.table(A= c(0.770509436, 0.799965732),
                  B= c(-0.622602953, 0.91474227),
                  C= c(0.046025736, 0.324983405),
                  ISOAlpha_3Code= "GNQ",
                  CountryName = "Equatorial Guinea",
                  year= 2006,
                  mean= 485978.87/365.2424,
                  ppp= 321.354314316056,
                  type=c("GQ","Beta")
)

# som <- data.table(A= c(0.88737242, 0.666110634),
#                   B= c(-1.254176897, 0.95205282),
#                   C= c(0.196100839, 0.562072863),
#                   ISOAlpha_3Code= "SOM",
#                   CountryName = "Somalia",
#                   year= 2017,
#                   mean= 421.8353938/365.2424,
#                   ppp= 0.674879848994,
#                   type=c("GQ","Beta")
# )
# 
# nru <- data.table(A= c(0.984898208, 0.88028882),
#                   B= c(-1.794465503, 0.998651171),
#                   C= c(-0.439162335, 0.446140553),
#                   ISOAlpha_3Code= "NRU",
#                   CountryName = "Nauru",
#                   year= 2012.5,
#                   mean= 125.38/7,
#                   ppp= 1.20879771681625,
#                   type=c("GQ","Beta")
# )

khm <- data.table(A= c(0.683555591,0.580351138),
                  B= c(-1.00090442, 0.952211708),
                  C= c(0.439753837, 0.507103243),
                  ISOAlpha_3Code= "KHM",
                  CountryName = "Cambodia",
                  year= 2011,
                  mean= ,
                  ppp= ,
                  type=c("GQ","Beta")
)

countries <- rbind(afg, eri, lby, gnq)
countries <- merge(countries, ppps, by.x="ISOAlpha_3Code", by.y="iso3c")

#GDP per capita growth
tmp <- tempfile(fileext = ".xlsx")
download.file("https://www.imf.org/external/pubs/ft/weo/data/WEOhistorical.xlsx", tmp, mode = "wb")
WEOraw <- as.data.table(read_excel(tmp, sheet = "ngdp_rpch"))

WEO <- WEOraw
WEO <- WEO[, .(latest = apply(.SD, 1, function(x) unlist(x)[x != "."][ifelse(length(unlist(x)[x != "."]) == 0, 1, length(unlist(x)[x != "."]))][[1]])), .SDcols = names(WEO)[-c(1:4)], by = .(country, ISOAlpha_3Code, year)]

WEO <- dcast(WEO, country + ISOAlpha_3Code ~ year, value.var = "latest")

year.cols <- as.character(seq(min(countries$year), max(as.numeric(names(WEO)), na.rm=T)))

WEO <- WEO[, lapply(.SD, as.numeric), .SDcols=(year.cols), by=.(ISOAlpha_3Code)]

WEO <- WEO[, lapply(.SD, function(x) x[!is.na(x)][1]), .SDcols=(year.cols), by=ISOAlpha_3Code]

WEO[WEO=="--"] <- 0

WEO[ISOAlpha_3Code == "KOS"]$ISOAlpha_3Code <- "XKX"

WEO <- merge(WEO, countries, by="ISOAlpha_3Code", all.y=T)
WEO[is.na(WEO$year)]$year <- 2018

#Calculate new effective means for each year of growth data
#WEO[, (year.cols) := lapply(1:ncol(.SD), function(i) ifelse(names(.SD)[i] < year, 0, .SD[[i]])), .SDcols=(year.cols)]

WEO[is.na(WEO)] <- 0

WEO[ISOAlpha_3Code=="CHN", (year.cols) := as.data.table(t(apply(.SD, 1, function(x) cumprod(1+((x*0.72)/100))*ifelse(x==0, NA, 1)))), .SDcols=(year.cols), by=ISOAlpha_3Code]
WEO[ISOAlpha_3Code=="IND", (year.cols) := as.data.table(t(apply(.SD, 1, function(x) cumprod(1+((x*0.51)/100))*ifelse(x==0, NA, 1)))), .SDcols=(year.cols), by=ISOAlpha_3Code]
WEO[!(ISOAlpha_3Code %in% c("CHN", "IND")), (year.cols) := as.data.table(t(apply(.SD, 1, function(x) cumprod(1+((x*0.87)/100))*ifelse(x==0, NA, 1)))), .SDcols=(year.cols), by=ISOAlpha_3Code]

WEO[, (year.cols) := .SD/(approx(x=names(.SD), y=.SD[1], xout=year)$y), .SDcols=(year.cols), by=.(ISOAlpha_3Code)]

WEO[, (year.cols) := mean*.SD, .SDcols=(year.cols)]

pov.lines <- fread("project_data/Poverty thresholds.csv")
pov.lines <- melt(pov.lines, id.vars="year")
names(pov.lines)[names(pov.lines) == "value"] <- "PL"
names(pov.lines)[names(pov.lines) == "variable"] <- "line"

#proj.years <- seq(min(WEO$year), max(as.numeric(names(WEO)), na.rm=T))
#pov.lines <- c(1.9, 3.2, 5.5)

WEO.melt <- melt(WEO, id.vars=c("ISOAlpha_3Code", "CountryName", "year", "mean", "ppp", "type", "A", "B", "C"))
WEO.melt$variable <- as.numeric(levels(WEO.melt$variable)[WEO.melt$variable])
WEO.melt <- merge(WEO.melt, pov.lines, by.x="variable", by.y="year", allow.cartesian = T)

WEO.melt$Projmean <- WEO.melt$value/WEO.melt$ppp

#Split GQ and Beta
countries_GQ <- WEO.melt[type=="GQ"]
countries_Beta <- WEO.melt[type=="Beta"]

#Run distribution solutions for effective projected poverty lines
data.list <- list()
for(i in 1:nrow(countries_GQ)){
  dat <- countries_GQ[i]
  data.list[[i]] <- data.table(tryCatch({GQsolve(dat$A,dat$B,dat$C,dat$PL,dat$Projmean)},error=function(e){return(data.table(GQP1=NaN,GQP2=NaN))}))
}

GQhc <- rbindlist(data.list, fill=T)
countries_GQ <- cbind(countries_GQ, GQhc)

data.list <- list()
for(i in 1:nrow(countries_Beta)){
  dat <- countries_Beta[i]
  data.list[[i]] <- data.table(tryCatch({Betasolve(dat$A,dat$B,dat$C,dat$PL,dat$Projmean,0.1)},error=function(e){return(data.table(BetaP1=NaN))}))
}

Betahc <- rbindlist(data.list)
countries_Beta <- cbind(countries_Beta,Betahc)

#Output raw solutions; check GQ results against Beta and select nearest root
hc <- merge(countries_GQ[,c("ISOAlpha_3Code", "CountryName", "variable", "PL", "GQP1", "GQP2")], countries_Beta[,c("ISOAlpha_3Code", "CountryName", "PL", "variable", "BetaP1")], by=c("ISOAlpha_3Code", "CountryName", "variable", "PL"))
hc[is.na(hc)] <- -1

hc <- hc[, .(Beta = BetaP1, GQ = c(GQP1, GQP2)[which.min(c(abs(GQP1-BetaP1), abs(GQP2-BetaP1)))]), by=.(ISOAlpha_3Code, CountryName, variable, PL)]

hc[(hc)==-1] <- NA

hc <- hc[, .(HeadCount = mean(c(GQ, Beta), na.rm=T), CoverageType="N"), by=.(ISOAlpha_3Code, CountryName, variable, PL)]
names(hc) <- c("CountryCode", "CountryName", "ProjYear", "PovertyLine", "HeadCount", "CoverageType")

fwrite(hc, "project_data/modelled_countries_2017PPP.csv")
