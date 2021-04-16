#Get map of United States
usa_map<-get_googlemap(center=c(lon=-92.4247,lat=38.51073),
                       zoom=4,color="color",
                       maptype = "roadmap")
ggmap(usa_map) +
  geom_point(data = AI_par.survey_age5_num_dur136_spv,
             aes(x = Longitude, y = Latitude))
#capitalize states
#CapStr <- function(y) {
#  c <- strsplit(y, " ")[[1]]
#  paste(toupper(substring(c, 1,1)), substring(c, 2),
#        sep="", collapse=" ")
#}
#AI_par.survey_age5_num_dur136_spv$State<-sapply(
#  AI_par.survey_age5_num_dur136_spv$State,CapStr
#)

#create different data frames for this map by census region
AI_par.survey_age5_num_dur136_spv.west<-subset(
  AI_par.survey_age5_num_dur136_spv,cens.reg=="West"
)
AI_par.survey_age5_num_dur136_spv.midwest<-subset(
  AI_par.survey_age5_num_dur136_spv,cens.reg=="Midwest"
)
AI_par.survey_age5_num_dur136_spv.south<-subset(
  AI_par.survey_age5_num_dur136_spv,cens.reg=="South"
)
AI_par.survey_age5_num_dur136_spv.northeast<-subset(
  AI_par.survey_age5_num_dur136_spv,cens.reg=="Northeast"
)
#combine the plots for each cenus region
#west
mean.long_West<-mean(AI_par.survey_age5_num_dur136_spv.west$Longitude)
mean.lat_West<-mean(AI_par.survey_age5_num_dur136_spv.west$Latitude)
mean.ppt_West<-mean(AI_par.survey_age5_num_dur136_spv.west$ParentProbTech)
mean.tfs_West<-mean(AI_par.survey_age5_num_dur136_spv.west$NumDevInt)
mean.tam_West<-mean(AI_par.survey_age5_num_dur136_spv.west$TAM)
#midwest
mean.long_Midwest<-mean(AI_par.survey_age5_num_dur136_spv.midwest$Longitude)
mean.lat_Midwest<-mean(AI_par.survey_age5_num_dur136_spv.midwest$Latitude)
mean.ppt_Midwest<-mean(AI_par.survey_age5_num_dur136_spv.midwest$ParentProbTech)
mean.tfs_Midwest<-mean(AI_par.survey_age5_num_dur136_spv.midwest$NumDevInt)
mean.tam_Midwest<-mean(AI_par.survey_age5_num_dur136_spv.midwest$TAM)
#south
mean.long_South<-mean(AI_par.survey_age5_num_dur136_spv.south$Longitude)
mean.lat_South<-mean(AI_par.survey_age5_num_dur136_spv.south$Latitude)
mean.ppt_South<-mean(AI_par.survey_age5_num_dur136_spv.south$ParentProbTech)
mean.tfs_South<-mean(AI_par.survey_age5_num_dur136_spv.south$NumDevInt)
mean.tam_South<-mean(AI_par.survey_age5_num_dur136_spv.south$TAM)
#northeast
mean.long_Northeast<-mean(AI_par.survey_age5_num_dur136_spv.northeast$Longitude)
mean.lat_Northeast<-mean(AI_par.survey_age5_num_dur136_spv.northeast$Latitude)
mean.ppt_Northeast<-mean(AI_par.survey_age5_num_dur136_spv.northeast$ParentProbTech)
mean.tfs_Northeast<-mean(AI_par.survey_age5_num_dur136_spv.northeast$NumDevInt)
mean.tam_Northeast<-mean(AI_par.survey_age5_num_dur136_spv.northeast$TAM)
#make data frames
mean.long<-c(mean.long_West,mean.long_Midwest,
             mean.long_South,mean.long_Northeast)
mean.lat<-c(mean.lat_West,mean.lat_Midwest,
            mean.lat_South,mean.lat_Northeast)
mean.ppt<-c(mean.ppt_West,mean.ppt_Midwest,
            mean.ppt_South,mean.ppt_Northeast)
mean.tfs<-c(mean.tfs_West,mean.tfs_Midwest,
            mean.tfs_South,mean.tfs_Northeast)
mean.tam<-c(mean.tam_West,mean.tam_Midwest,
            mean.tam_South,mean.tam_Northeast)
region<-c("West","Midwest","South","Northeast")
AI.Par.Surv.Map_cens.reg<-data.frame(
  Longitude=mean.long,
  Latitude=mean.lat,
  Parent_Prob_Tech=mean.ppt,
  Technoference_Scale=mean.tfs,
  TAMPU=mean.tam,
  Region=region
)
#map the census region
#assign them census regions
#break data points into four US Census regions
us.states[us.states=="connecticut" | 
            us.states=="maine" | 
            us.states=="massachusetts" | 
            us.states=="new hampshire" | 
            us.states=="rhode island" | 
            us.states=="vermont" | 
            us.states=="new jersey" | 
            us.states=="new york" | 
            us.states=="pennsylvania"]<-"Northeast"
us.states[us.states=="illinois" | 
            us.states=="indiana" | 
            us.states=="michigan" | 
            us.states=="ohio" | 
            us.states=="wisconsin" | 
            us.states=="iowa" | 
            us.states=="kansas" | 
            us.states=="minnesota" | 
            us.states=="missouri" | 
            us.states=="nebraska" | 
            us.states=="north dakota" | 
            us.states=="south dakota"]<-"Midwest"
us.states[us.states=="delaware" | 
            us.states=="florida" | 
            us.states=="georgia" | 
            us.states=="maryland" | 
            us.states=="north carolina" | 
            us.states=="south carolina" | 
            us.states=="virginia" | 
            us.states=="district of columbia" | 
            us.states=="west virginia" | 
            us.states=="alabama" |
            us.states=="kentucky" | 
            us.states=="mississippi" | 
            us.states=="tennessee" | 
            us.states=="arkansas" | 
            us.states=="louisiana" | 
            us.states=="oklahoma" | 
            us.states=="texas"]<-"South"
us.states[us.states=="arizona" | 
            us.states=="colorado" |
            us.states=="idaho" | 
            us.states=="montana" | 
            us.states=="nevada" | 
            us.states=="new mexico" | 
            us.states=="alaska" | 
            us.states=="california" | 
            us.states=="hawaii" | 
            us.states=="oregon" | 
            us.states=="washington" | 
            us.states=="utah" | 
            us.states=="wyoming"]<-"West"
#USA map
usa_map<-get_googlemap(center=c(lon=-96.5,lat=38.5),
                       zoom=1,color="color",
                       maptype = "terrain")
#ggmap(usa_map) + 
#  geom_point(data = AI_par.survey_age5_num_dur136_spv,
#           aes(x = Longitude, y = Latitude), 
#           alpha = 1) +
#  scale_size_continuous(range = c(1, 4)) +
#  labs(title = "AI Parenting Survey Map",
#       size = "Technoference Scale",
#       color = "Technoacceptance Scale") +
#  scale_colour_gradient2(low = "red", mid = "yellow",
#                         high = "green", midpoint = 3.54, 
#                         space = "Lab",
#                         na.value = "grey50", 
#                         guide = "colourbar", 
#                         aesthetics = "colour") + 
#  geom_text_repel(data = AI.Par.Surv.Map_cens.reg, 
#                  aes(x = Longitude, y = Latitude, 
#                     label = Region, size=3)) 
#ggmap(usa_map) + 
#  geom_point(data = AI.Par.Surv.Map_cens.reg,
#             aes(x = Longitude, y = Latitude, 
#                 size = Parent_Prob_Tech, 
#                 color = Technoacceptance_Scale), 
#             alpha = 1) +
#  scale_size_continuous(range = c(1, 5)) +
#  labs(title = "AI Parenting Survey Map",
#       size = "Parent Prob Tech",
#       color = "Technoacceptance Scale") +
#  scale_colour_gradient2(low = "red", mid = "yellow",
#                         high = "green", midpoint = 3.54, 
#                         space = "Lab",
#                         na.value = "grey50", 
#                         guide = "colourbar", 
#                        aesthetics = "colour") + 
#  geom_text_repel(data = AI.Par.Surv.Map_cens.reg, 
#                  aes(x = Longitude, y = Latitude, 
#                      label = Region, size=3)) 

#make census divisions map
us_states <- map_data("state")
#break data points in the census regions
us_states$census.region<-NA
us_states$census.region[us_states$region=="connecticut" | 
                          us_states$region=="maine" | 
                          us_states$region=="massachusetts" | 
                          us_states$region=="new hampshire" | 
                          us_states$region=="rhode island" | 
                          us_states$region=="vermont" | 
                          us_states$region=="new jersey" | 
                          us_states$region=="new york" | 
                          us_states$region=="pennsylvania"]<-"Northeast"
us_states$census.region[us_states$region=="illinois" | 
                          us_states$region=="indiana" | 
                          us_states$region=="michigan" | 
                          us_states$region=="ohio" | 
                          us_states$region=="wisconsin" | 
                          us_states$region=="iowa" | 
                          us_states$region=="kansas" | 
                          us_states$region=="minnesota" | 
                          us_states$region=="missouri" | 
                          us_states$region=="nebraska" | 
                          us_states$region=="north dakota" | 
                          us_states$region=="south dakota"]<-"Midwest"
us_states$census.region[us_states$region=="delaware" | 
                          us_states$region=="florida" | 
                          us_states$region=="georgia" | 
                          us_states$region=="maryland" | 
                          us_states$region=="north carolina" | 
                          us_states$region=="south carolina" | 
                          us_states$region=="virginia" | 
                          us_states$region=="district of columbia" | 
                          us_states$region=="west virginia" | 
                          us_states$region=="alabama" |
                          us_states$region=="kentucky" | 
                          us_states$region=="mississippi" | 
                          us_states$region=="tennessee" | 
                          us_states$region=="arkansas" | 
                          us_states$region=="louisiana" | 
                          us_states$region=="oklahoma" | 
                          us_states$region=="texas"]<-"South"
us_states$census.region[us_states$region=="arizona" | 
                          us_states$region=="colorado" |
                          us_states$region=="idaho" | 
                          us_states$region=="montana" | 
                          us_states$region=="nevada" | 
                          us_states$region=="new mexico" | 
                          us_states$region=="alaska" | 
                          us_states$region=="california" | 
                          us_states$region=="hawaii" | 
                          us_states$region=="oregon" | 
                          us_states$region=="washington" | 
                          us_states$region=="utah" | 
                          us_states$region=="wyoming"]<-"West"
#break data points into the census divisions
us_states$subregion[us_states$region=="connecticut" | 
                      us_states$region=="maine" | 
                      us_states$region=="massachusetts" | 
                      us_states$region=="new hampshire" |
                      us_states$region=="rhode island" | 
                      us_states$region=="vermont"]<-"New England"
us_states$subregion[us_states$region=="new jersey" | 
                      us_states$region=="new york" | 
                      us_states$region=="pennsylvania"]<-"Mid-Atlantic"
us_states$subregion[us_states$region=="illinois" | 
                      us_states$region=="indiana" | 
                      us_states$region=="michigan" | 
                      us_states$region=="ohio" | 
                      us_states$region=="wisconsin"]<-"E. North Central"
us_states$subregion[us_states$region=="iowa" | 
                      us_states$region=="kansas" | 
                      us_states$region=="minnesota" | 
                      us_states$region=="missouri" | 
                      us_states$region=="nebraska" | 
                      us_states$region=="north dakota" | 
                      us_states$region=="south dakota"]<-"W. North Central"
us_states$subregion[us_states$region=="delaware" | 
                      us_states$region=="florida" | 
                      us_states$region=="georgia" | 
                      us_states$region=="maryland" | 
                      us_states$region=="north carolina" | 
                      us_states$region=="south carolina" | 
                      us_states$region=="virginia" | 
                      us_states$region=="district of columbia" | 
                      us_states$region=="west virginia"]<-"South Atlantic"
us_states$subregion[us_states$region=="alabama" | 
                      us_states$region=="kentucky" | 
                      us_states$region=="mississippi" | 
                      us_states$region=="tennessee"]<-"E. South Central"
us_states$subregion[us_states$region=="arkansas" | 
                      us_states$region=="louisiana" | 
                      us_states$region=="oklahoma" | 
                      us_states$region=="texas"]<-"W. South Central"
us_states$subregion[us_states$region=="arizona" | 
                      us_states$region=="colorado" | 
                      us_states$region=="idaho" | 
                      us_states$region=="montana" | 
                      us_states$region=="nevada" | 
                      us_states$region=="new mexico" | 
                      us_states$region=="utah" | 
                      us_states$region=="wyoming"]<-"Mountain"
us_states$subregion[us_states$region=="alaska" | 
                      us_states$region=="california" | 
                      us_states$region=="hawaii" | 
                      us_states$region=="oregon" | 
                      us_states$region=="washington"]<-"Pacific"
#create different data frames for this map by census region
AI_par.survey_age5_num_dur136_spv.pacific<-subset(
  AI_par.survey_age5_num_dur136_spv,cens.div=="Pacific"
)
AI_par.survey_age5_num_dur136_spv.mountain<-subset(
  AI_par.survey_age5_num_dur136_spv,cens.div=="Mountain"
)
AI_par.survey_age5_num_dur136_spv.wsouthcentral<-subset(
  AI_par.survey_age5_num_dur136_spv,cens.div=="W. South Central"
)
AI_par.survey_age5_num_dur136_spv.esouthcentral<-subset(
  AI_par.survey_age5_num_dur136_spv,cens.div=="E. South Central"
)
AI_par.survey_age5_num_dur136_spv.wnorthcentral<-subset(
  AI_par.survey_age5_num_dur136_spv,cens.div=="W. North Central"
)
AI_par.survey_age5_num_dur136_spv.enorthcentral<-subset(
  AI_par.survey_age5_num_dur136_spv,cens.div=="E. North Central"
)
AI_par.survey_age5_num_dur136_spv.southatlantic<-subset(
  AI_par.survey_age5_num_dur136_spv,cens.div=="South Atlantic"
)
AI_par.survey_age5_num_dur136_spv.middleatlantic<-subset(
  AI_par.survey_age5_num_dur136_spv,cens.div=="Mid-Atlantic"
)
AI_par.survey_age5_num_dur136_spv.newengland<-subset(
  AI_par.survey_age5_num_dur136_spv,cens.div=="New England"
)
#combine the plots for each cenus region
#pacific
mean.long_pacific<-mean(AI_par.survey_age5_num_dur136_spv.west$Longitude)
mean.lat_pacific<-mean(AI_par.survey_age5_num_dur136_spv.west$Latitude)
mean.ppt_pacific<-mean(AI_par.survey_age5_num_dur136_spv.west$ParentProbTech)
mean.tfs_pacific<-mean(AI_par.survey_age5_num_dur136_spv.west$NumDevInt)
mean.tam_pacific<-mean(AI_par.survey_age5_num_dur136_spv.west$TAM)
#mountain
mean.long_mountain<-mean(AI_par.survey_age5_num_dur136_spv.mountain$Longitude)
mean.lat_mountain<-mean(AI_par.survey_age5_num_dur136_spv.mountain$Latitude)
mean.ppt_mountain<-mean(AI_par.survey_age5_num_dur136_spv.mountain$ParentProbTech)
mean.tfs_mountain<-mean(AI_par.survey_age5_num_dur136_spv.mountain$NumDevInt)
mean.tam_mountain<-mean(AI_par.survey_age5_num_dur136_spv.mountain$TAM)
#wnorthcentral
mean.long_wnorthcentral<-mean(AI_par.survey_age5_num_dur136_spv.wnorthcentral$Longitude)
mean.lat_wnorthcentral<-mean(AI_par.survey_age5_num_dur136_spv.wnorthcentral$Latitude)
mean.ppt_wnorthcentral<-mean(AI_par.survey_age5_num_dur136_spv.wnorthcentral$ParentProbTech)
mean.tfs_wnorthcentral<-mean(AI_par.survey_age5_num_dur136_spv.wnorthcentral$NumDevInt)
mean.tam_wnorthcentral<-mean(AI_par.survey_age5_num_dur136_spv.wnorthcentral$TAM)
#enorthcentral
mean.long_enorthcentral<-mean(AI_par.survey_age5_num_dur136_spv.enorthcentral$Longitude)
mean.lat_enorthcentral<-mean(AI_par.survey_age5_num_dur136_spv.enorthcentral$Latitude)
mean.ppt_enorthcentral<-mean(AI_par.survey_age5_num_dur136_spv.enorthcentral$ParentProbTech)
mean.tfs_enorthcentral<-mean(AI_par.survey_age5_num_dur136_spv.enorthcentral$NumDevInt)
mean.tam_enorthcentral<-mean(AI_par.survey_age5_num_dur136_spv.enorthcentral$TAM)
#southatlantic
mean.long_southatlantic<-mean(AI_par.survey_age5_num_dur136_spv.southatlantic$Longitude)
mean.lat_southatlantic<-mean(AI_par.survey_age5_num_dur136_spv.southatlantic$Latitude)
mean.ppt_southatlantic<-mean(AI_par.survey_age5_num_dur136_spv.southatlantic$ParentProbTech)
mean.tfs_southatlantic<-mean(AI_par.survey_age5_num_dur136_spv.southatlantic$NumDevInt)
mean.tam_southatlantic<-mean(AI_par.survey_age5_num_dur136_spv.southatlantic$TAM)
#wsouthcentral
mean.long_wsouthcentral<-mean(AI_par.survey_age5_num_dur136_spv.wsouthcentral$Longitude)
mean.lat_wsouthcentral<-mean(AI_par.survey_age5_num_dur136_spv.wsouthcentral$Latitude)
mean.ppt_wsouthcentral<-mean(AI_par.survey_age5_num_dur136_spv.wsouthcentral$ParentProbTech)
mean.tfs_wsouthcentral<-mean(AI_par.survey_age5_num_dur136_spv.wsouthcentral$NumDevInt)
mean.tam_wsouthcentral<-mean(AI_par.survey_age5_num_dur136_spv.wsouthcentral$TAM)
#esouthcentral
mean.long_esouthcentral<-mean(AI_par.survey_age5_num_dur136_spv.esouthcentral$Longitude)
mean.lat_esouthcentral<-mean(AI_par.survey_age5_num_dur136_spv.esouthcentral$Latitude)
mean.ppt_esouthcentral<-mean(AI_par.survey_age5_num_dur136_spv.esouthcentral$ParentProbTech)
mean.tfs_esouthcentral<-mean(AI_par.survey_age5_num_dur136_spv.esouthcentral$NumDevInt)
mean.tam_esouthcentral<-mean(AI_par.survey_age5_num_dur136_spv.esouthcentral$TAM)
#middleatlantic
mean.long_middleatlantic<-mean(AI_par.survey_age5_num_dur136_spv.middleatlantic$Longitude)
mean.lat_middleatlantic<-mean(AI_par.survey_age5_num_dur136_spv.middleatlantic$Latitude)
mean.ppt_middleatlantic<-mean(AI_par.survey_age5_num_dur136_spv.middleatlantic$ParentProbTech)
mean.tfs_middleatlantic<-mean(AI_par.survey_age5_num_dur136_spv.middleatlantic$NumDevInt)
mean.tam_middleatlantic<-mean(AI_par.survey_age5_num_dur136_spv.middleatlantic$TAM)
#newengland
mean.long_newengland<-mean(AI_par.survey_age5_num_dur136_spv.newengland$Longitude)
mean.lat_newengland<-mean(AI_par.survey_age5_num_dur136_spv.newengland$Latitude)
mean.ppt_newengland<-mean(AI_par.survey_age5_num_dur136_spv.newengland$ParentProbTech)
mean.tfs_newengland<-mean(AI_par.survey_age5_num_dur136_spv.newengland$NumDevInt)
mean.tam_newengland<-mean(AI_par.survey_age5_num_dur136_spv.newengland$TAM)

#make data frames
mean.long<-c(mean.long_pacific,mean.long_mountain,
             mean.long_wnorthcentral,mean.long_wsouthcentral,
             mean.long_enorthcentral,mean.long_esouthcentral,
             mean.long_southatlantic,mean.long_middleatlantic,
             mean.long_newengland)
mean.lat<-c(mean.lat_pacific,mean.lat_mountain,
            mean.lat_wnorthcentral,mean.lat_wsouthcentral,
            mean.lat_enorthcentral,mean.lat_esouthcentral,
            mean.lat_southatlantic,mean.lat_middleatlantic,
            mean.lat_newengland)
mean.ppt<-c(mean.ppt_pacific,mean.ppt_mountain,
            mean.ppt_wnorthcentral,mean.ppt_wsouthcentral,
            mean.ppt_enorthcentral,mean.ppt_esouthcentral,
            mean.ppt_southatlantic,mean.ppt_middleatlantic,
            mean.ppt_newengland)
mean.tfs<-c(mean.tfs_pacific,mean.tfs_mountain,
            mean.tfs_wnorthcentral,mean.tfs_wsouthcentral,
            mean.tfs_enorthcentral,mean.tfs_esouthcentral,
            mean.tfs_southatlantic,mean.tfs_middleatlantic,
            mean.tfs_newengland)
mean.tam<-c(mean.tam_pacific,mean.tam_mountain,
            mean.tam_wnorthcentral,mean.tam_wsouthcentral,
            mean.tam_enorthcentral,mean.tam_esouthcentral,
            mean.tam_southatlantic,mean.tam_middleatlantic,
            mean.tam_newengland)
division<-c("Pacific","Mountain",
            "W. North Central","W. South Central",
            "E. North Central","E. South Central",
            "South Atlantic","Mid-Atlantic",
            "New England")
AI.Par.Surv.Map_cens.div<-data.frame(
  Longitude=mean.long,
  Latitude=mean.lat,
  Parent_Prob_Tech=mean.ppt,
  Technoference_Scale=mean.tfs,
  TAMPU=mean.tam,
  Division=division
)
###
#by state
ParProbTech_state<-NA
TAM_state<-NA
Technoference_state<-NA
#ParProbTech by State
for(i in 1:NROW(AI_par.survey_age5_num_dur136_spv)){
  if(AI_par.survey_age5_num_dur136_spv$State[i]=="alabama"){
    ParProbTech_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("ParentProbTech")],
      list(AI_par.survey_age5_num_dur136_spv$State=="alabama"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="alaska"){
    ParProbTech_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("ParentProbTech")],
      list(AI_par.survey_age5_num_dur136_spv$State=="alaska"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="arizona"){
    ParProbTech_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("ParentProbTech")],
      list(AI_par.survey_age5_num_dur136_spv$State=="arizona"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="california"){
    ParProbTech_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("ParentProbTech")],
      list(AI_par.survey_age5_num_dur136_spv$State=="california"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="colorado"){
    ParProbTech_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("ParentProbTech")],
      list(AI_par.survey_age5_num_dur136_spv$State=="colorado"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="connecticut"){
    ParProbTech_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("ParentProbTech")],
      list(AI_par.survey_age5_num_dur136_spv$State=="connecticut"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="district of columbia"){
    ParProbTech_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("ParentProbTech")],
      list(AI_par.survey_age5_num_dur136_spv$State=="district of columbia"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="florida"){
    ParProbTech_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("ParentProbTech")],
      list(AI_par.survey_age5_num_dur136_spv$State=="florida"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="georgia"){
    ParProbTech_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("ParentProbTech")],
      list(AI_par.survey_age5_num_dur136_spv$State=="georgia"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="hawaii"){
    ParProbTech_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("ParentProbTech")],
      list(AI_par.survey_age5_num_dur136_spv$State=="hawaii"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="idaho"){
    ParProbTech_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("ParentProbTech")],
      list(AI_par.survey_age5_num_dur136_spv$State=="idaho"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="illinois"){
    ParProbTech_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("ParentProbTech")],
      list(AI_par.survey_age5_num_dur136_spv$State=="illinois"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="indiana"){
    ParProbTech_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("ParentProbTech")],
      list(AI_par.survey_age5_num_dur136_spv$State=="indiana"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="iowa"){
    ParProbTech_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("ParentProbTech")],
      list(AI_par.survey_age5_num_dur136_spv$State=="iowa"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="kansas"){
    ParProbTech_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("ParentProbTech")],
      list(AI_par.survey_age5_num_dur136_spv$State=="kansas"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="kentucky"){
    ParProbTech_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("ParentProbTech")],
      list(AI_par.survey_age5_num_dur136_spv$State=="kentucky"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="louisiana"){
    ParProbTech_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("ParentProbTech")],
      list(AI_par.survey_age5_num_dur136_spv$State=="louisiana"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="maine"){
    ParProbTech_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("ParentProbTech")],
      list(AI_par.survey_age5_num_dur136_spv$State=="maine"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="maryland"){
    ParProbTech_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("ParentProbTech")],
      list(AI_par.survey_age5_num_dur136_spv$State=="maryland"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="massachusetts"){
    ParProbTech_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("ParentProbTech")],
      list(AI_par.survey_age5_num_dur136_spv$State=="massachusetts"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="michigan"){
    ParProbTech_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("ParentProbTech")],
      list(AI_par.survey_age5_num_dur136_spv$State=="michigan"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="missouri"){
    ParProbTech_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("ParentProbTech")],
      list(AI_par.survey_age5_num_dur136_spv$State=="missouri"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="nebraska"){
    ParProbTech_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("ParentProbTech")],
      list(AI_par.survey_age5_num_dur136_spv$State=="nebraska"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="nevada"){
    ParProbTech_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("ParentProbTech")],
      list(AI_par.survey_age5_num_dur136_spv$State=="nevada"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="new hampshire"){
    ParProbTech_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("ParentProbTech")],
      list(AI_par.survey_age5_num_dur136_spv$State=="new hampshire"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="new jersey"){
    ParProbTech_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("ParentProbTech")],
      list(AI_par.survey_age5_num_dur136_spv$State=="new jersey"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="new york"){
    ParProbTech_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("ParentProbTech")],
      list(AI_par.survey_age5_num_dur136_spv$State=="new york"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="north carolina"){
    ParProbTech_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("ParentProbTech")],
      list(AI_par.survey_age5_num_dur136_spv$State=="north carolina"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="north dakota"){
    ParProbTech_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("ParentProbTech")],
      list(AI_par.survey_age5_num_dur136_spv$State=="north dakota"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="ohio"){
    ParProbTech_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("ParentProbTech")],
      list(AI_par.survey_age5_num_dur136_spv$State=="ohio"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="oklahoma"){
    ParProbTech_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("ParentProbTech")],
      list(AI_par.survey_age5_num_dur136_spv$State=="oklahoma"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="oregon"){
    ParProbTech_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("ParentProbTech")],
      list(AI_par.survey_age5_num_dur136_spv$State=="oregon"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="pennsylvania"){
    ParProbTech_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("ParentProbTech")],
      list(AI_par.survey_age5_num_dur136_spv$State=="pennsylvania"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="rhode island"){
    ParProbTech_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("ParentProbTech")],
      list(AI_par.survey_age5_num_dur136_spv$State=="rhode island"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="south carolina"){
    ParProbTech_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("ParentProbTech")],
      list(AI_par.survey_age5_num_dur136_spv$State=="south carolina"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="tennessee"){
    ParProbTech_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("ParentProbTech")],
      list(AI_par.survey_age5_num_dur136_spv$State=="tennessee"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="texas"){
    ParProbTech_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("ParentProbTech")],
      list(AI_par.survey_age5_num_dur136_spv$State=="texas"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="utah"){
    ParProbTech_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("ParentProbTech")],
      list(AI_par.survey_age5_num_dur136_spv$State=="utah"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="virginia"){
    ParProbTech_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("ParentProbTech")],
      list(AI_par.survey_age5_num_dur136_spv$State=="virginia"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="washington"){
    ParProbTech_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("ParentProbTech")],
      list(AI_par.survey_age5_num_dur136_spv$State=="washington"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="west virginia"){
    ParProbTech_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("ParentProbTech")],
      list(AI_par.survey_age5_num_dur136_spv$State=="west virginia"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="wisconsin"){
    ParProbTech_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("ParentProbTech")],
      list(AI_par.survey_age5_num_dur136_spv$State=="wisconsin"),
      mean
    )[2,2]
  }
}
#TAMPU by state
for(i in 1:NROW(AI_par.survey_age5_num_dur136_spv)){
  if(AI_par.survey_age5_num_dur136_spv$State[i]=="alabama"){
    TAM_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("TAM")],
      list(AI_par.survey_age5_num_dur136_spv$State=="alabama"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="alaska"){
    TAM_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("TAM")],
      list(AI_par.survey_age5_num_dur136_spv$State=="alaska"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="arizona"){
    TAM_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("TAM")],
      list(AI_par.survey_age5_num_dur136_spv$State=="arizona"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="california"){
    TAM_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("TAM")],
      list(AI_par.survey_age5_num_dur136_spv$State=="california"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="colorado"){
    TAM_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("TAM")],
      list(AI_par.survey_age5_num_dur136_spv$State=="colorado"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="connecticut"){
    TAM_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("TAM")],
      list(AI_par.survey_age5_num_dur136_spv$State=="connecticut"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="district of columbia"){
    TAM_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("TAM")],
      list(AI_par.survey_age5_num_dur136_spv$State=="district of columbia"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="florida"){
    TAM_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("TAM")],
      list(AI_par.survey_age5_num_dur136_spv$State=="florida"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="georgia"){
    TAM_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("TAM")],
      list(AI_par.survey_age5_num_dur136_spv$State=="georgia"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="hawaii"){
    TAM_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("TAM")],
      list(AI_par.survey_age5_num_dur136_spv$State=="hawaii"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="idaho"){
    TAM_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("TAM")],
      list(AI_par.survey_age5_num_dur136_spv$State=="idaho"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="illinois"){
    TAM_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("TAM")],
      list(AI_par.survey_age5_num_dur136_spv$State=="illinois"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="indiana"){
    TAM_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("TAM")],
      list(AI_par.survey_age5_num_dur136_spv$State=="indiana"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="iowa"){
    TAM_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("TAM")],
      list(AI_par.survey_age5_num_dur136_spv$State=="iowa"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="kansas"){
    TAM_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("TAM")],
      list(AI_par.survey_age5_num_dur136_spv$State=="kansas"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="kentucky"){
    TAM_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("TAM")],
      list(AI_par.survey_age5_num_dur136_spv$State=="kentucky"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="louisiana"){
    TAM_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("TAM")],
      list(AI_par.survey_age5_num_dur136_spv$State=="louisiana"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="maine"){
    TAM_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("TAM")],
      list(AI_par.survey_age5_num_dur136_spv$State=="maine"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="maryland"){
    TAM_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("TAM")],
      list(AI_par.survey_age5_num_dur136_spv$State=="maryland"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="massachusetts"){
    TAM_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("TAM")],
      list(AI_par.survey_age5_num_dur136_spv$State=="massachusetts"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="michigan"){
    TAM_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("TAM")],
      list(AI_par.survey_age5_num_dur136_spv$State=="michigan"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="missouri"){
    TAM_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("TAM")],
      list(AI_par.survey_age5_num_dur136_spv$State=="missouri"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="nebraska"){
    TAM_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("TAM")],
      list(AI_par.survey_age5_num_dur136_spv$State=="nebraska"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="nevada"){
    TAM_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("TAM")],
      list(AI_par.survey_age5_num_dur136_spv$State=="nevada"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="new hampshire"){
    TAM_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("TAM")],
      list(AI_par.survey_age5_num_dur136_spv$State=="new hampshire"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="new jersey"){
    TAM_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("TAM")],
      list(AI_par.survey_age5_num_dur136_spv$State=="new jersey"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="new york"){
    TAM_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("TAM")],
      list(AI_par.survey_age5_num_dur136_spv$State=="new york"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="north carolina"){
    TAM_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("TAM")],
      list(AI_par.survey_age5_num_dur136_spv$State=="north carolina"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="north dakota"){
    TAM_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("TAM")],
      list(AI_par.survey_age5_num_dur136_spv$State=="north dakota"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="ohio"){
    TAM_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("TAM")],
      list(AI_par.survey_age5_num_dur136_spv$State=="ohio"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="oklahoma"){
    TAM_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("TAM")],
      list(AI_par.survey_age5_num_dur136_spv$State=="oklahoma"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="oregon"){
    TAM_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("TAM")],
      list(AI_par.survey_age5_num_dur136_spv$State=="oregon"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="pennsylvania"){
    TAM_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("TAM")],
      list(AI_par.survey_age5_num_dur136_spv$State=="pennsylvania"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="rhode island"){
    TAM_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("TAM")],
      list(AI_par.survey_age5_num_dur136_spv$State=="rhode island"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="south carolina"){
    TAM_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("TAM")],
      list(AI_par.survey_age5_num_dur136_spv$State=="south carolina"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="tennessee"){
    TAM_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("TAM")],
      list(AI_par.survey_age5_num_dur136_spv$State=="tennessee"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="texas"){
    TAM_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("TAM")],
      list(AI_par.survey_age5_num_dur136_spv$State=="texas"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="utah"){
    TAM_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("TAM")],
      list(AI_par.survey_age5_num_dur136_spv$State=="utah"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="virginia"){
    TAM_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("TAM")],
      list(AI_par.survey_age5_num_dur136_spv$State=="virginia"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="washington"){
    TAM_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("TAM")],
      list(AI_par.survey_age5_num_dur136_spv$State=="washington"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="west virginia"){
    TAM_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("TAM")],
      list(AI_par.survey_age5_num_dur136_spv$State=="west virginia"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="wisconsin"){
    TAM_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("TAM")],
      list(AI_par.survey_age5_num_dur136_spv$State=="wisconsin"),
      mean
    )[2,2]
  }
}
#Technoference State
for(i in 1:NROW(AI_par.survey_age5_num_dur136_spv)){
  if(AI_par.survey_age5_num_dur136_spv$State[i]=="alabama"){
    Technoference_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("NumDevInt")],
      list(AI_par.survey_age5_num_dur136_spv$State=="alabama"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="alaska"){
    Technoference_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("NumDevInt")],
      list(AI_par.survey_age5_num_dur136_spv$State=="alaska"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="arizona"){
    Technoference_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("NumDevInt")],
      list(AI_par.survey_age5_num_dur136_spv$State=="arizona"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="california"){
    Technoference_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("NumDevInt")],
      list(AI_par.survey_age5_num_dur136_spv$State=="california"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="colorado"){
    Technoference_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("NumDevInt")],
      list(AI_par.survey_age5_num_dur136_spv$State=="colorado"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="connecticut"){
    Technoference_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("NumDevInt")],
      list(AI_par.survey_age5_num_dur136_spv$State=="connecticut"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="district of columbia"){
    Technoference_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("NumDevInt")],
      list(AI_par.survey_age5_num_dur136_spv$State=="district of columbia"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="florida"){
    Technoference_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("NumDevInt")],
      list(AI_par.survey_age5_num_dur136_spv$State=="florida"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="georgia"){
    Technoference_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("NumDevInt")],
      list(AI_par.survey_age5_num_dur136_spv$State=="georgia"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="hawaii"){
    Technoference_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("NumDevInt")],
      list(AI_par.survey_age5_num_dur136_spv$State=="hawaii"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="idaho"){
    Technoference_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("NumDevInt")],
      list(AI_par.survey_age5_num_dur136_spv$State=="idaho"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="illinois"){
    Technoference_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("NumDevInt")],
      list(AI_par.survey_age5_num_dur136_spv$State=="illinois"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="indiana"){
    Technoference_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("NumDevInt")],
      list(AI_par.survey_age5_num_dur136_spv$State=="indiana"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="iowa"){
    Technoference_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("NumDevInt")],
      list(AI_par.survey_age5_num_dur136_spv$State=="iowa"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="kansas"){
    Technoference_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("NumDevInt")],
      list(AI_par.survey_age5_num_dur136_spv$State=="kansas"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="kentucky"){
    Technoference_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("NumDevInt")],
      list(AI_par.survey_age5_num_dur136_spv$State=="kentucky"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="louisiana"){
    Technoference_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("NumDevInt")],
      list(AI_par.survey_age5_num_dur136_spv$State=="louisiana"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="maine"){
    Technoference_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("NumDevInt")],
      list(AI_par.survey_age5_num_dur136_spv$State=="maine"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="maryland"){
    Technoference_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("NumDevInt")],
      list(AI_par.survey_age5_num_dur136_spv$State=="maryland"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="massachusetts"){
    Technoference_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("NumDevInt")],
      list(AI_par.survey_age5_num_dur136_spv$State=="massachusetts"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="michigan"){
    Technoference_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("NumDevInt")],
      list(AI_par.survey_age5_num_dur136_spv$State=="michigan"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="missouri"){
    Technoference_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("NumDevInt")],
      list(AI_par.survey_age5_num_dur136_spv$State=="missouri"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="nebraska"){
    Technoference_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("NumDevInt")],
      list(AI_par.survey_age5_num_dur136_spv$State=="nebraska"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="nevada"){
    Technoference_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("NumDevInt")],
      list(AI_par.survey_age5_num_dur136_spv$State=="nevada"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="new hampshire"){
    Technoference_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("NumDevInt")],
      list(AI_par.survey_age5_num_dur136_spv$State=="new hampshire"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="new jersey"){
    Technoference_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("NumDevInt")],
      list(AI_par.survey_age5_num_dur136_spv$State=="new jersey"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="new york"){
    Technoference_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("NumDevInt")],
      list(AI_par.survey_age5_num_dur136_spv$State=="new york"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="north carolina"){
    Technoference_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("NumDevInt")],
      list(AI_par.survey_age5_num_dur136_spv$State=="north carolina"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="north dakota"){
    Technoference_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("NumDevInt")],
      list(AI_par.survey_age5_num_dur136_spv$State=="north dakota"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="ohio"){
    Technoference_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("NumDevInt")],
      list(AI_par.survey_age5_num_dur136_spv$State=="ohio"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="oklahoma"){
    Technoference_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("NumDevInt")],
      list(AI_par.survey_age5_num_dur136_spv$State=="oklahoma"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="oregon"){
    Technoference_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("NumDevInt")],
      list(AI_par.survey_age5_num_dur136_spv$State=="oregon"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="pennsylvania"){
    Technoference_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("NumDevInt")],
      list(AI_par.survey_age5_num_dur136_spv$State=="pennsylvania"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="rhode island"){
    Technoference_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("NumDevInt")],
      list(AI_par.survey_age5_num_dur136_spv$State=="rhode island"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="south carolina"){
    Technoference_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("NumDevInt")],
      list(AI_par.survey_age5_num_dur136_spv$State=="south carolina"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="tennessee"){
    Technoference_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("NumDevInt")],
      list(AI_par.survey_age5_num_dur136_spv$State=="tennessee"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="texas"){
    Technoference_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("NumDevInt")],
      list(AI_par.survey_age5_num_dur136_spv$State=="texas"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="utah"){
    Technoference_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("NumDevInt")],
      list(AI_par.survey_age5_num_dur136_spv$State=="utah"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="virginia"){
    Technoference_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("NumDevInt")],
      list(AI_par.survey_age5_num_dur136_spv$State=="virginia"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="washington"){
    Technoference_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("NumDevInt")],
      list(AI_par.survey_age5_num_dur136_spv$State=="washington"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="west virginia"){
    Technoference_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("NumDevInt")],
      list(AI_par.survey_age5_num_dur136_spv$State=="west virginia"),
      mean
    )[2,2]
  }
  else if(AI_par.survey_age5_num_dur136_spv$State[i]=="wisconsin"){
    Technoference_state[i]<-aggregate(
      AI_par.survey_age5_num_dur136_spv[,c("NumDevInt")],
      list(AI_par.survey_age5_num_dur136_spv$State=="wisconsin"),
      mean
    )[2,2]
  }
}
#add them to the data frame
AI_par.survey_age5_num_dur136_spv$ParProbTech_state<-ParProbTech_state
AI_par.survey_age5_num_dur136_spv$TAMPU_state<-TAM_state
AI_par.survey_age5_num_dur136_spv$Technofere_state<-Technoference_state
##Create map of states
us.states_AIparsurv<-left_join(us_states,
                               AI_par.survey_age5_num_dur136_spv,
                               by=c("region"="State"))
tam.map<-ggplot(data = us.states_AIparsurv,
                mapping = aes(x = long, y = lat,
                              group = group,fill=TAMPU_state)) + 
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
  #  geom_point(data = AI.Par.Surv.Map_cens.div,
  #        aes(x = Longitude, y = Latitude, 
  #             size = Technoference_Scale, 
  #              color = TAMPU), 
  #           alpha = 1) +
  # scale_size_continuous(range = c(1, 4)) +
  labs(#title = "AI Parenting Survey Results",
    fill = "TAM-PU") + 
  theme(      axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank()) +
  scale_fill_gradient2(low = "red", mid = "yellow",
                       high = "green", midpoint = 3.54, 
                       space = "Lab",
                       na.value = "grey50", 
                       breaks=c(1,2,3,4,5,6))
#for parent prob tech
ppt.map<-ggplot(data = us.states_AIparsurv,
                mapping = aes(x = long, y = lat,
                              group = group,fill=ParProbTech_state)) + 
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
  #  geom_point(data = AI.Par.Surv.Map_cens.div,
  #        aes(x = Longitude, y = Latitude, 
  #             size = Technoference_Scale, 
  #              color = TAMPU), 
  #           alpha = 1) +
  # scale_size_continuous(range = c(1, 4)) +
  labs(#title = "AI Parenting Survey Map",
    fill = "PPT Scale") + 
  theme(      axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank()) +
  scale_fill_gradient2(low = "red", mid = "yellow",
                       high = "green", midpoint = 3.54, 
                       space = "Lab",
                       na.value = "grey50", 
                       breaks=c(1,2,3,4,5,6))
#for techno
tfs.map<-ggplot(data = us.states_AIparsurv,
                mapping = aes(x = long, y = lat,
                              group = group,fill=Technofere_state)) + 
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
  #  geom_point(data = AI.Par.Surv.Map_cens.div,
  #        aes(x = Longitude, y = Latitude, 
  #             size = Technoference_Scale, 
  #              color = TAMPU), 
  #           alpha = 1) +
  # scale_size_continuous(range = c(1, 4)) +
  labs(#title = "AI Parenting Survey Map",
    fill = "TFS Scale") + 
  theme(      axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank()) +
  scale_fill_gradient2(low = "red", mid = "yellow",
                       high = "green", midpoint = 3.54, 
                       space = "Lab",
                       na.value = "grey50", 
                       breaks=c(1,2,3,4,5,6))
#put them together
tam.map
ppt.map
tfs.map
ggarrange(tam.map,
          ppt.map,
          tfs.map,ncol=1,nrow=3)

##Create for census regions
##Create map of census regions
us_cens.reg_AIparsurv<-left_join(us_states,
                                 AI.Par.Surv.Map_cens.reg,
                                 by=c("census.region"="Region"))
tam.map<-ggplot(data = us_cens.reg_AIparsurv,
                mapping = aes(x = long, y = lat,
                              group=group,fill=TAMPU)) + 
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
  #  geom_point(data = AI.Par.Surv.Map_cens.div,
  #        aes(x = Longitude, y = Latitude, 
  #             size = Technoference_Scale, 
  #              color = TAMPU), 
  #           alpha = 1) +
  # scale_size_continuous(range = c(1, 4)) +
  labs(#title = "AI Parenting Survey Results",
    fill = "TAM-PU") + 
  theme(      axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank()) +
  scale_fill_gradient2(low = "red", mid = "yellow",
                       high = "green", midpoint = 3.5, 
                       space = "Lab",
                       na.value = "grey50", 
                       limits=c(1,6))
tam.map
#for parent prob tech
ppt.map<-ggplot(data = us_cens.reg_AIparsurv,
                mapping = aes(x = long, y = lat,
                              group = group,fill=Parent_Prob_Tech)) + 
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
  #  geom_point(data = AI.Par.Surv.Map_cens.div,
  #        aes(x = Longitude, y = Latitude, 
  #             size = Technoference_Scale, 
  #              color = TAMPU), 
  #           alpha = 1) +
  # scale_size_continuous(range = c(1, 4)) +
  labs(#title = "AI Parenting Survey Map",
    fill = "PPT Scale") + 
  theme(      axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank()) +
  scale_fill_gradient2(low = "red", mid = "yellow",
                       high = "green", midpoint = 3.54, 
                       space = "Lab",
                       na.value = "grey50", 
                       limits=c(1,6))
#for techno
tfs.map<-ggplot(data = us_cens.reg_AIparsurv,
                mapping = aes(x = long, y = lat,
                              group = group,fill=Technoference_Scale)) + 
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
  #  geom_point(data = AI.Par.Surv.Map_cens.div,
  #        aes(x = Longitude, y = Latitude, 
  #             size = Technoference_Scale, 
  #              color = TAMPU), 
  #           alpha = 1) +
  # scale_size_continuous(range = c(1, 4)) +
  labs(#title = "AI Parenting Survey Map",
    fill = "TFS Scale") + 
  theme(      axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank()) +
  scale_fill_gradient2(low = "red", mid = "yellow",
                       high = "green", midpoint = 3.54, 
                       space = "Lab",
                       na.value = "grey50", 
                       limits=c(1,6))
#put them together
tam.map
ppt.map
tfs.map
ggarrange(tam.map,
          ppt.map,
          tfs.map,ncol=1,nrow=3)

##Create Map for Census Divisions
us_cens.div_AIparsurv<-left_join(us_states,
                                 AI.Par.Surv.Map_cens.div,
                                 by=c("subregion"="Division"))
tam.map<-ggplot(data = us_cens.div_AIparsurv,
                mapping = aes(x = long, y = lat,
                              group=group,fill=TAMPU)) + 
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
  #  geom_point(data = AI.Par.Surv.Map_cens.div,
  #        aes(x = Longitude, y = Latitude, 
  #             size = Technoference_Scale, 
  #              color = TAMPU), 
  #           alpha = 1) +
  # scale_size_continuous(range = c(1, 4)) +
  labs(#title = "AI Parenting Survey Results",
    fill = "TAM-PU") + 
  theme(      axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank()) +
  scale_fill_gradient2(low = "red", mid = "yellow",
                       high = "green", midpoint = 3.5, 
                       space = "Lab",
                       na.value = "grey50", 
                       limits=c(1,6))
tam.map
#for parent prob tech
ppt.map<-ggplot(data = us_cens.div_AIparsurv,
                mapping = aes(x = long, y = lat,
                              group = group,fill=Parent_Prob_Tech)) + 
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
  #  geom_point(data = AI.Par.Surv.Map_cens.div,
  #        aes(x = Longitude, y = Latitude, 
  #             size = Technoference_Scale, 
  #              color = TAMPU), 
  #           alpha = 1) +
  # scale_size_continuous(range = c(1, 4)) +
  labs(#title = "AI Parenting Survey Map",
    fill = "PPT Scale") + 
  theme(      axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank()) +
  scale_fill_gradient2(low = "red", mid = "yellow",
                       high = "green", midpoint = 3.54, 
                       space = "Lab",
                       na.value = "grey50", 
                       limits=c(1,6))
#for techno
tfs.map<-ggplot(data = us_cens.div_AIparsurv,
                mapping = aes(x = long, y = lat,
                              group = group,fill=Technoference_Scale)) + 
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
  #  geom_point(data = AI.Par.Surv.Map_cens.div,
  #        aes(x = Longitude, y = Latitude, 
  #             size = Technoference_Scale, 
  #              color = TAMPU), 
  #           alpha = 1) +
  # scale_size_continuous(range = c(1, 4)) +
  labs(#title = "AI Parenting Survey Map",
    fill = "TFS Scale") + 
  theme(      axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank()) +
  scale_fill_gradient2(low = "red", mid = "yellow",
                       high = "green", midpoint = 3.54, 
                       space = "Lab",
                       na.value = "grey50", 
                       limits=c(1,6))
#put them together
tam.map
ppt.map
tfs.map
ggarrange(tam.map,
          ppt.map,
          tfs.map,ncol=1,nrow=3)





#by county instead of state
us_county <- map_data("county")
latlong2county <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per county
  counties <- map('county', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
  counties_sp <- map2SpatialPolygons(counties, IDs=IDs,
                                     proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, counties_sp)
  
  # Return the county names of the Polygons object containing each point
  countyNames <- sapply(counties_sp@polygons, function(x) x@ID)
  countyNames[indices]
}

#ggmap(usa_map) +
#  geom_point(data = AI_par.survey_age5_num_dur136_spv.west,
#             aes(x = Longitude, y = Latitude, 
#                 size = NumDevInt, color = ParentProbTech), 
#             alpha = 0.5) +
#  geom_point(data = AI_par.survey_age5_num_dur136_spv.midwest,
#             aes(x = Longitude, y = Latitude, 
#                 size = NumDevInt, color = ParentProbTech), 
#             alpha = 0.5) +
#  geom_point(data = AI_par.survey_age5_num_dur136_spv.south,
#             aes(x = Longitude, y = Latitude, 
#                 size = NumDevInt, color = ParentProbTech), 
#            alpha = 0.5) +
#  geom_point(data = AI_par.survey_age5_num_dur136_spv.northeast,
#             aes(x = Longitude, y = Latitude, 
#                 size = NumDevInt, color = ParentProbTech), 
#             alpha = 0.5) +
#  scale_size_continuous(range = c(1, 4)) +
#  labs(title = "AI Parenting Survey Map",
#       size = "Technoference Scale",
#       color = "Parent Prob Tech") +
#  scale_colour_gradient2(low = "red", mid = "yellow",
#                         high = "green", midpoint = 3.5, 
#                         space = "Lab",
#                         na.value = "grey50", 
#                         guide = "colourbar", 
#                         aesthetics = "colour") +

#geom_text_repel(data = AI_par.survey_age5_num_dur136_spv, 
#                aes(x = Longitude, y = Latitude, label = State)) +
#  guides(color = guide_legend(override.aes = list(size = 2)))

#write csv to Jill with geographic variables
