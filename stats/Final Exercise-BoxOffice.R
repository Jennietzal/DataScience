library(dplyr)
#library(DBI)
#con <- dbConnect(odbc::odbc(), "BoxOffice", timeout = 10)
#FF<-dbGetQuery(con,'SELECT * FROM "BoxOffice"."dbo"."movies_ff_v"')
load(file = "C:/Users/Xnes/DataScience/stats/FF.RData" )

View(FF)

#factorize 
for(v in names(FF)) {
  if(is.character(FF[[v]])==TRUE) {
    FF[[v]] <- factor(FF[[v]])
  }
}
cols<-c("sw_lang_en","sw_web_presence","sw_has_poster","sw_tagline","keyword_cnt",
        "high_release_month","sw_collection","lang_US","lang_FR","lang_RU","lang_ES",
        "lang_JA","sw_female_actor0","sw_female_actor1",
        "sw_female_actor2","sw_male_actor0","sw_male_actor1","sw_male_actor2",
        "genre_adventure","genre_fantasy","genre_animation","genre_drama","genre_horror",
        "genre_action","genre_comedy","genre_history","genre_western","genre_thriller",
        "genre_crime","genre_documentary","genre_science_fiction","genre_mystery",
        "genre_music","genre_romance","genre_family","genre_war","genre_foreign",
        "depart_Art_female","depart_Camera_female","depart_Crew_female","depart_Custom_Mkup_female",
        "depart_Directing_female","depart_Editing_female","depart_Lighting_female",
        "depart_Production_female","depart_Sound_female","depart_Visual_Effects_female",
        "depart_Writing_female")
FF[cols] <- lapply(FF[cols], factor) 
summary(FF)
library(mechkar)
tab1 <- Table1(data=FF)
View(tab1)

exploreData(data=FF,dir = "C:/Users/Xnes/DataScience/stats/report-Boxoffice")

## Create a correlation matrix 

## to make the correlation matrix we need to select the numeric variables. 
## We use a loop for this
numvar <- NULL

for(v in names(FF)) {
  if(is.numeric(FF[[v]])==TRUE) {
    numvar <- c(numvar, v)
  }  
}

library(Hmisc)
cormat <- rcorr(as.matrix(FF[,numvar]))

#lowcorp1<-apply(cormat$P, 2, function(x) which(x < 0.05) )
#lowcorr1<-apply(cormat$r, 2, function(x) which(x > 0.7) )

#lowcorp<-c("budget","popularity","runtime","sw_web_presence","sw_tagline","keyword_cnt",
           "release_day","seasonality","countries_cnt","keywords_cnt","actor0_movies_cnt",
           "actor0_movies_5y_cnt","actor1_movies_cnt","actor1_movies_5y_cnt","actor2_movies_cnt",
           "actor2_movies_5y_cnt","sw_male_actor1","sw_male_actor2","actor0_prev_revenue",
           "actor1_prev_revenue","actor2_prev_revenue","director_movies_cnt","director_movies_5y_cnt",
           "genre_adventure","genre_fantasy","genre_drama","genre_action","genre_history","genre_romance",
           "genre_family","genre_foreign","depart_Art","depart_Camera","depart_Crew","depart_Custom_Mkup",
           "depart_Directing","depart_Editing","depart_Lighting","depart_Production","depart_Sound",
           "depart_Visual_Effects","depart_Writing","depart_Art_female","depart_Custom_Mkup_female",
           "depart_Lighting_female","depart_Production_female","depart_Visual_Effects_female")


heatmap(cormat$r[,numvar])


####################
###   Functions  ###
####################
outlierMatrix <- function(data,threshold=1.5) {
  vn <- names(data)
  outdata <- data.frame(row1=1:nrow(data))
  for(v in vn) {
    if(is.numeric(data[[v]])) {
      outlow <- quantile(data[[v]],probs = 0.25,na.rm = T) 
      outhigh <- quantile(data[[v]],probs = 0.75, na.rm = T)
      iqr_level <- (outhigh - outlow) * threshold
      outlow <- outlow - iqr_level
      outhigh <- outhigh +  iqr_level
      mv <- ifelse(data[[v]] < outlow | data[[v]] > outhigh, 1, 0)
      outdata[v] <- mv
    } else {
      mv <- rep(0,nrow(data))
    }
  }
  outdata$row1 <- NULL
  return(outdata)
}


missingMatrix <- function(data) {
  vn <- names(data)
  missdata <- data.frame(row1=1:nrow(data))
  for(v in vn) {
    mv <- ifelse(is.na(data[[v]]),1,0)
    missdata[v] <- mv
  }
  missdata$row1 <- NULL
  return(missdata)
}

minmax <- function(x) {
  return(((x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T))))
}

getMissingness <- function (data, getRows = FALSE) {
  require(dplyr)
  l <- nrow(data)
  vn <- names(data)
  nadf <- data
  cnt <- NULL
  miss <- function(x) return(sum(is.na(x)))
  for (n in vn) {
    nadf[[n]] <- ifelse(is.na(nadf[[n]]) == T, 1, 0)
    cnt <- rbind(cnt, data.frame(n, sum(nadf[[n]])))
  }
  names(cnt) <- c("var", "na.count")
  cnt$rate <- round((cnt$na.count/nrow(nadf)) * 100, 1)
  nadf$na.cnt <- 0
  nadf$na.cnt <- rowSums(nadf)
  cnt <- cnt %>% dplyr::arrange(desc(na.count)) %>% dplyr::filter(na.count > 
                                                                    0)
  totmiss <- nadf %>% dplyr::filter(na.cnt == 0) %>% dplyr::tally()
  idx <- NULL
  msg <- (paste("This dataset has ", as.character(totmiss), 
                " (", as.character(round(totmiss/nrow(data) * 100, 1)), 
                "%)", " complete rows. Original data has ", nrow(data), 
                " rows.", sep = ""))
  if (getRows == TRUE & totmiss != 0) {
    nadf$rn <- seq_len(nrow(data))
    idx <- nadf %>% dplyr::filter(na.cnt == 0) %>% dplyr::select(rn)
  }
  print(list(head(cnt, n = 10), msg))
  return(list(missingness = cnt, message = msg, rows = idx$rn))
}

#Boxplot for Outlier

 boxplot(FF[,numvar])

for(v in names(FF[,numvar])) {
  scatter.smooth(FF[[v]] ~ FF$release_year, main=v, xlab="movies",ylab=v, family="symmetric")
}

Om<-(outlierMatrix(FF[,numvar]))
View(Om)
heatmap(as.matrix(Om))
#MissingMatrix

Mm<-missingMatrix(FF[,numvar])


heatmap(as.matrix(Mm))

colnames(Mm)<- c("movie_id","budget","popularity","runtime","revenue","sw_lang_en",
                  "sw_web_presence","sw_has_poster","sw_tagline","keyword_cnt",
                  "release_year","release_month","high_release_month","release_day",
                  "seasonality","sw_collection","producers_cnt","countries_cnt","lang_US",
                  "lang_FR","lang_RU","lang_ES","lang_JA","keywords_cnt","actor0_movies_cnt",
                  "actor0_movies_5y_cnt","actor1_movies_cnt","actor1_movies_5y_cnt",
                  "actor2_movies_cnt","actor2_movies_5y_cnt","sw_female_actor0","sw_female_actor1",
                  "sw_female_actor2","sw_male_actor0","sw_male_actor1","sw_male_actor2",
                  "actor0_prev_revenue","actor1_prev_revenue","actor2_prev_revenue",
                  "director_movies_cnt","director_movies_5y_cnt","genre_adventure",
                  "genre_fantasy","genre_animation","genre_drama","genre_horror",
                  "genre_action","genre_comedy","genre_history","genre_western","genre_thriller",
                  "genre_crime","genre_documentary","genre_science_fiction","genre_mystery",
                  "genre_music","genre_romance","genre_family","genre_war","genre_foreign",
                  "depart_Art","depart_Camera","depart_Crew","depart_Custom_Mkup",
                  "depart_Directing","depart_Editing","depart_Lighting","depart_Production",
                  "depart_Sound","depart_Visual_Effects","depart_Writing","depart_Art_female",
                  "depart_Camera_female","depart_Crew_female","depart_Custom_Mkup_female",
                  "depart_Directing_female","depart_Editing_female","depart_Lighting_female",
                  "depart_Production_female","depart_Sound_female","depart_Visual_Effects_female",
                  "depart_Writing_female")


om<-outlierMatrix(FF[,numvar],threshold = 1.5)
FF2<-FF
outliers<-boxplot(FF2[,numvar],plot=FALSE)$out

library(naniar)
FF2$revenue %>% replace_with_na(replace = list(x = ))


boxplot.stats(FF2$producers_cnt)$out


#plot of all Matrices


#Boxplot for Outlayers and DBSCAn cant do with NA
#library(dbscan)
#dbsmod<- dbscan(FF[numvar] ,minPts = 5)



