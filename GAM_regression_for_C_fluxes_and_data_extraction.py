library(gamair)
library(tidyverse)
library(mgcv)

# GAM regression for NEP and data extraction
NEPmodel<-gam(NEP ~ s(S_dep, k = -1, bs = "cs")+s(N_dep, k = -1, bs = "cs")+s(Precip, k = -1, bs = "cs")+s(Temp, k = -1, bs = "cs")+s(SW_radiation, k = -1, bs = "cs")+s(Forest_age, k = -1, bs = "cs")+Soil, select = TRUE, data = data, method = "REML")
extract_NEP <- plot(NEPmodel, all.terms = TRUE, pages = 1)
table_NEP = matrix(0, nrow=length(extract_NEP[[1]]$x), ncol=18)
table_NEP[,1] = extract_NEP[[1]]$x
table_NEP[,2] = extract_NEP[[1]]$fit
table_NEP[,3] = extract_NEP[[1]]$se
table_NEP[,4] = extract_NEP[[2]]$x
table_NEP[,5] = extract_NEP[[2]]$fit
table_NEP[,6] = extract_NEP[[2]]$se
table_NEP[,7] = extract_NEP[[3]]$x
table_NEP[,8] = extract_NEP[[3]]$fit
table_NEP[,9] = extract_NEP[[3]]$se
table_NEP[,10] = extract_NEP[[4]]$x
table_NEP[,11] = extract_NEP[[4]]$fit
table_NEP[,12] = extract_NEP[[4]]$se
table_NEP[,13] = extract_NEP[[5]]$x
table_NEP[,14] = extract_NEP[[5]]$fit
table_NEP[,15] = extract_NEP[[5]]$se
table_NEP[,16] = extract_NEP[[6]]$x
table_NEP[,17] = extract_NEP[[6]]$fit
table_NEP[,18] = extract_NEP[[6]]$se
write.table(table_NEP, file=filename, row.names=FALSE, col.names=FALSE)
NEPloam = NEPmodel[[1]][[2]]
NEPsand = NEPmodel[[1]][[3]]
NEPsilt = NEPmodel[[1]][[4]]
NEPbaseline = NEPmodel[[1]][[1]]


# GAM regression for GPP and data extraction
GPPmodel<-gam(GPP ~ s(S_dep, k = -1, bs = "cs")+s(N_dep, k = -1, bs = "cs")+s(Precip, k = -1, bs = "cs")+s(Temp, k = -1, bs = "cs")+s(SW_radiation, k = -1, bs = "cs")+s(Forest_age, k = -1, bs = "cs")+Soil, select = TRUE, data = data, method = "REML")
extract_GPP <- plot(GPPmodel, all.terms = TRUE, pages = 1)
table_GPP = matrix(0, nrow=length(extract_GPP[[1]]$x), ncol=18)
table_GPP[,1] = extract_GPP[[1]]$x
table_GPP[,2] = extract_GPP[[1]]$fit
table_GPP[,3] = extract_GPP[[1]]$se
table_GPP[,4] = extract_GPP[[2]]$x
table_GPP[,5] = extract_GPP[[2]]$fit
table_GPP[,6] = extract_GPP[[2]]$se
table_GPP[,7] = extract_GPP[[3]]$x
table_GPP[,8] = extract_GPP[[3]]$fit
table_GPP[,9] = extract_GPP[[3]]$se
table_GPP[,10] = extract_GPP[[4]]$x
table_GPP[,11] = extract_GPP[[4]]$fit
table_GPP[,12] = extract_GPP[[4]]$se
table_GPP[,13] = extract_GPP[[5]]$x
table_GPP[,14] = extract_GPP[[5]]$fit
table_GPP[,15] = extract_GPP[[5]]$se
table_GPP[,16] = extract_GPP[[6]]$x
table_GPP[,17] = extract_GPP[[6]]$fit
table_GPP[,18] = extract_GPP[[6]]$se
write.table(table_GPP, file=filename, row.names=FALSE, col.names=FALSE)
GPPloam = GPPmodel[[1]][[2]]
GPPsand = GPPmodel[[1]][[3]]
GPPsilt = GPPmodel[[1]][[4]]
GPPbaseline = GPPmodel[[1]][[1]]


# GAM regression for RECO and data extraction
RECOmodel<-gam(RECO ~ s(S_dep, k = -1, bs = "cs")+s(N_dep, k = -1, bs = "cs")+s(Precip, k = -1, bs = "cs")+s(Temp, k = -1, bs = "cs")+s(SW_radiation, k = -1, bs = "cs")+s(Forest_age, k = -1, bs = "cs")+Soil, select = TRUE, data = data, method = "REML")
extract_RECO <- plot(RECOmodel, all.terms = TRUE, pages = 1)
table_RECO = matrix(0, nrow=length(extract_RECO[[1]]$x), ncol=18)
table_RECO[,1] = extract_RECO[[1]]$x
table_RECO[,2] = extract_RECO[[1]]$fit
table_RECO[,3] = extract_RECO[[1]]$se
table_RECO[,4] = extract_RECO[[2]]$x
table_RECO[,5] = extract_RECO[[2]]$fit
table_RECO[,6] = extract_RECO[[2]]$se
table_RECO[,7] = extract_RECO[[3]]$x
table_RECO[,8] = extract_RECO[[3]]$fit
table_RECO[,9] = extract_RECO[[3]]$se
table_RECO[,10] = extract_RECO[[4]]$x
table_RECO[,11] = extract_RECO[[4]]$fit
table_RECO[,12] = extract_RECO[[4]]$se
table_RECO[,13] = extract_RECO[[5]]$x
table_RECO[,14] = extract_RECO[[5]]$fit
table_RECO[,15] = extract_RECO[[5]]$se
table_RECO[,16] = extract_RECO[[6]]$x
table_RECO[,17] = extract_RECO[[6]]$fit
table_RECO[,18] = extract_RECO[[6]]$se
write.table(table_RECO, file=filename, row.names=FALSE, col.names=FALSE)
RECOloam = RECOmodel[[1]][[2]]
RECOsand = RECOmodel[[1]][[3]]
RECOsilt = RECOmodel[[1]][[4]]
RECObaseline = RECOmodel[[1]][[1]]