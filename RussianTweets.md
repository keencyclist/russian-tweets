# Russian Tweets - January 2019 Data

Load libraries


```r
library(rmarkdown)
```

```
## Warning: package 'rmarkdown' was built under R version 3.4.2
```

```r
library(knitr)
```

```
## Warning: package 'knitr' was built under R version 3.4.2
```

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(tidyverse)
```

```
## Warning: package 'tidyverse' was built under R version 3.4.4
```

```
## -- Attaching packages --------------------------------------- tidyverse 1.2.1 --
```

```
## v ggplot2 3.1.0     v readr   1.1.1
## v tibble  2.0.1     v purrr   0.2.4
## v tidyr   0.8.0     v stringr 1.2.0
## v ggplot2 3.1.0     v forcats 0.2.0
```

```
## Warning: package 'ggplot2' was built under R version 3.4.4
```

```
## Warning: package 'tibble' was built under R version 3.4.4
```

```
## Warning: package 'tidyr' was built under R version 3.4.4
```

```
## Warning: package 'readr' was built under R version 3.4.4
```

```
## Warning: package 'purrr' was built under R version 3.4.4
```

```
## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(lubridate)
```

```
## Warning: package 'lubridate' was built under R version 3.4.3
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
library(stringr)
library(tidytext)
```

```
## Warning: package 'tidytext' was built under R version 3.4.4
```

```r
library(ggthemes)#Themes for formating
```

```
## Warning: package 'ggthemes' was built under R version 3.4.4
```

```r
library(grid) #Add grid line
library(wordcloud2)
```

```
## Warning: package 'wordcloud2' was built under R version 3.4.4
```

Read data from tweets from 418 Russian accounts released by Twitter in January 2019.


```r
# read data
russia_users <- read.csv("russia_201901_1_users_csv_hashed.csv", encoding = "UTF-8")
russia_tweets <- read_csv("russian_201901_1_tweets_csv_hashed.csv")
```

```
## Parsed with column specification:
## cols(
##   .default = col_character(),
##   tweetid = col_double(),
##   userid = col_double(),
##   follower_count = col_integer(),
##   following_count = col_integer(),
##   account_creation_date = col_date(format = ""),
##   tweet_time = col_datetime(format = ""),
##   in_reply_to_tweetid = col_double(),
##   in_reply_to_userid = col_double(),
##   quoted_tweet_tweetid = col_double(),
##   retweet_userid = col_double(),
##   retweet_tweetid = col_double(),
##   quote_count = col_double(),
##   reply_count = col_double(),
##   like_count = col_double(),
##   retweet_count = col_double()
## )
```

```
## See spec(...) for full column specifications.
```

```
## Warning in rbind(names(probs), probs_f): number of columns of result is not
## a multiple of vector length (arg 1)
```

```
## Warning: 536234 parsing failures.
## row # A tibble: 5 x 5 col     row col    expected actual                     file                     expected   <int> <chr>  <chr>    <chr>                      <chr>                    actual 1  1869 userid a double l+5xaB0DjZSlsfrmaT91U2DAI~ 'russian_201901_1_tweet~ file 2  1870 userid a double l+5xaB0DjZSlsfrmaT91U2DAI~ 'russian_201901_1_tweet~ row 3  1871 userid a double l+5xaB0DjZSlsfrmaT91U2DAI~ 'russian_201901_1_tweet~ col 4  1872 userid a double l+5xaB0DjZSlsfrmaT91U2DAI~ 'russian_201901_1_tweet~ expected 5  1873 userid a double l+5xaB0DjZSlsfrmaT91U2DAI~ 'russian_201901_1_tweet~
## ... ................. ... ........................................................................... ........ ........................................................................... ...... ........................................................................... .... ........................................................................... ... ........................................................................... ... ........................................................................... ........ ...........................................................................
## See problems(...) for more details.
```

Trend over time - filtering out the few tweets from before 2015

```r
# trend over time
tweets <- russia_tweets %>%
  mutate(tweet_date=date(tweet_time)) %>%
  filter(tweet_date > ymd("2014-12-31"))

ggplot(data = tweets, mapping = aes(tweet_date)) + geom_area(stat = "bin",  fill = "#00AFBB", color = "black", binwidth=7) + 
  labs(title="Russian Tweets per Week", subtitle = "Data Released January 2019", x = "Tweet Date", y= "tweets per week")
```

![](RussianTweets_files/figure-html/unnamed-chunk-3-1.png)<!-- -->


```r
# likes and retweets
tweets %>%
  group_by(is_retweet) %>%
  summarize(n=n()) %>%
  mutate(prop = n/sum(n))
```

```
## # A tibble: 2 x 3
##   is_retweet      n  prop
##   <chr>       <int> <dbl>
## 1 False      192872 0.212
## 2 True       716485 0.788
```




```r
# most common hashtags
hashtag_summary <- russia_tweets %>%
  select(hashtags, userid, user_display_name, account_language) %>%
  filter(hashtags != "[]", account_language == "en")  %>% # get rid of tweets with no hashtags
  mutate(hashtags = str_sub(hashtags,2,str_length(hashtags)-1)) %>% # remove first and last characters (brackets)
  mutate(hashtags = str_split(hashtags,",")) # separate multiple hashtags into a list

hashtag_list <- unnest(hashtag_summary, hashtags) # restructure so that there is 1 hashtag per record

hashtag_freq <- hashtag_list %>%
  group_by(hashtags) %>%
  summarize(n=n()) %>%
  filter(n >= 200) %>%
  rename(word = hashtags, freq = n)

# word clouds

wordcloud2(data=hashtag_freq)
```

<!--html_preserve--><div id="htmlwidget-7bdbddb07c44783f84df" style="width:672px;height:480px;" class="wordcloud2 html-widget"></div>
<script type="application/json" data-for="htmlwidget-7bdbddb07c44783f84df">{"x":{"word":[" سورية"," 1A"," 2A"," 2ADefenders"," 2ndAmendment"," AdamSchiff"," Afghanistan"," AgendaOfEvil"," Aleppo"," AllahuAkbar"," AllInWithBernie"," AllLivesMatter"," AlwaysTrump"," America"," AmericaFirst"," AMJoy"," Antifa"," ANTIFA"," ArrestLindaSarsour"," ArrestObama"," ARRESTObama"," ArrestObamaForTreason"," AwanBrothers"," Baltimore"," BaltimoreRiots"," BanALLMosques"," BanCAIR"," BanCAIRNation"," banCAIRNational"," BanCAIRNational"," BanFGM"," banislam"," BanIslam"," BANISLAM"," BanIslaminAmerica"," BanIslamInAmerica"," BanIslamSupremacyHateCult"," BanMuslimbrotherhood"," BanMuslimBrotherhood"," BanMuslims"," BanNationOfIslam"," BanSharia"," BanShariaCourts"," BanShariaLaw"," BanShariaLawInAmerica"," bantheburka"," BanTheBurka"," BanTheBurqa"," BanTheMoslemBrotherhood"," BanTheMuslimbrotherhood"," BanTheMuslimBrotherhood"," BanTheMuslimBrotherHood"," BanTheQuran"," BanTheQuranasaHATEBOOK"," BanTheQuranAsAHATEBook"," BanTheQuranAsAHATEBOOK"," BarackObama"," BasketOfDeplorables"," BenGarrison"," Benghazi"," Berkeley"," BerkeleyRiots"," bestUSAtoday"," BigLeagueTruth"," BillClinton"," BillWarnerPhD"," BlackLivesMatter"," blackviolence"," BLM"," BlueLivesMatter"," Boomers"," BoycottCNN"," BOYCOTTExpedia"," BOYCOTTHollywood"," BOYCOTTMcDonalds"," BoycottOscars2018"," BoycottStarbucks"," BOYCOTTStarbucks"," BoycottUnitedAirlines"," breaking"," BREAKING"," Brexit"," BuildThatWall"," BuildTheWall"," butthis"," CAIR"," CampaignZero"," CashInIn"," CBTS"," ccot"," CCOT"," ChildAbuse"," CivilWarII"," Cleveland"," Clinton"," ClintonBodyCount"," ClintonCash"," ClintonCorruption"," ClintonCrimeFamily"," ClintonEmails"," ClintonFoundation"," ClintonScandals"," CloseALLMosques"," CloseAllMosquesNOW"," CloseALLMosquesNOW"," CNN"," CNNFakeNews"," CNNScript"," Coachella"," College"," Comey"," ComeyBook"," ComeyDisgrace"," ComeyMemos"," ConfirmGorsuch"," Conservatives"," Corruption"," COSProject"," CounterJihad"," CPAC2018"," CreepingSharia"," Crooked"," CrookedHillary"," CruzCrew"," CruzSexScandal"," CSPI"," CureForIslam"," Daesh"," Damascus"," DC"," Deash"," DeathByDemocrat"," DeathCult"," debatenight"," Debates2016"," DeepState"," DeleteIslam"," Democraps"," DemocratLiesMatter"," Democrats"," DemocRATS"," DeportALLMoslems"," DeportALLMoslemsNOW"," DeportALLMuslims"," DeportALLMuslimsNOW"," DeportIllegals"," DeportLindaSarsour"," DeportThemAll"," DirtyRice"," DNC"," DNCLeaks"," Dobbs"," DonaldTrump"," DonaldTrumpforPresident"," DontGetFooledAgain"," DrainTheDeepState"," DrainTheDeepStateSwamp"," DrainTheSwamp"," DropOutHillary"," dumbassmoslemloverobama"," Easter"," EducateYourselfOnIslam"," EducateYourSelfOnIslam"," Election2016"," EndFGM"," EndIslam"," EndSanctuaryCities"," EnoughIsEnough"," Europe"," evil"," Evil"," EvilDeathCult"," EvilLosers"," ExposeIslam"," ExtremeVetting"," fakenews"," FakeNews"," FAKENEWS"," FBI"," feedly"," FeelTheBern"," Ferguson"," FGM"," FightBack"," FindOurGirls"," FireColbert"," FireKushner"," FireMueller"," FireMuellerNow"," FireRyan"," FollowTheMoney"," FollowTheWhiteRabbit"," FoundInArea51"," FoxNews"," FreedomOfSpeech"," FreeSpeech"," FreeTommy"," FreeTommyRobinson"," FridayFeeling"," FuckIslam"," FUCKIslam"," FuckIslamicExtremism"," GenX"," GeorgeSoros"," GetWoke"," GodBlessAmerica"," GoodFriday"," gop"," GOP"," GOPDebate"," GorsuchHearing"," GovernmentShutdown"," GreatAwakening"," Hannity"," Hawaii"," HighSchool"," Hillary"," Hillary2016"," Hillary4Prison"," HillaryClinton"," HillaryClinton4prison"," HillaryClintonliesmatter"," HillaryClintonLiesMatter"," HillaryEmail"," HillaryEmails"," HillaryForPrison"," HillaryForPrison2016"," HillaryHealth"," HillaryLiesMatter"," HillaryRottenClinton"," HillarysEmails"," HillarysHealth"," IllegalAliens"," immigration"," impeachobama"," ImpeachObama"," ImWithHer"," ImWithYou"," infidel"," infidelforlife"," Inners"," InsideSyriaMC"," InternetBillOfRights"," Iran"," IranDeal"," IranNuclearDeal"," Iraq"," ISIL"," isis"," ISIS"," ISL"," islam"," Islam"," IslamExposed"," IslamicState"," IslamicTerrorism"," IslamIsCancer"," IslamIsCANCER"," IslamIsDangerous"," islamisevil"," Islamisevil"," IslamIsEvil"," IslamIsNotPeaceful"," IslamIsPureEvil"," islamistheproblem"," Islamistheproblem"," IslamIsTheProblem"," IslamNotCompatibleWithDemocracy"," Israel"," JailObamaSpyRing"," Jihad"," JihadistLinda"," JihadistNOTWelcome"," JohnMcCuck"," KAG"," KateSteinle"," KatesWall"," KeepBannon"," KillTheBill"," Liberal"," LiberalIdiots"," LiberalismIsAMentalDisorder"," LiberalLogic"," Liberals"," LiberalsAreIdiots"," LiberalsSUCK"," lnyhbt"," LNYHBT"," LockHerUp"," London"," LondonAttacks"," LyingCrookedHillary"," LyingCrookedHillaryClinton"," LyingCrookedHillaryUnFitBitch"," LyingTed"," maga"," Maga"," MAGA"," MAGA3X"," MakeAmerciaGreatAgain"," MakeAmericaGreatAgain"," MakeAmericaSafeAgain"," MakeDCListen"," MarchAgainstSharia"," MarchForScience"," MiddleEast"," military"," Military"," Millennials"," MissingDCGirls"," MolonLabe"," MondayMotivation"," MoslemsArentPeaceful"," MoslemsAreTerrorist"," moslemsaretheproblem"," Moslemscumbags"," MSM"," Mueller"," MulticulturalismIsntWorking"," muslim"," Muslim"," muslimaretheproblem"," MuslimBan"," MuslimBanForever"," MuslimBrotherhood"," Muslims"," muslimsaretheproblem"," muslimsaretheproblems"," Muslimscumbags"," MuslimsDontAnnihilate"," MyJihad"," NationOfIslam"," NeverForget"," NeverHillary"," NeverSubmitToIslam"," NeverTrustAMoslem"," news"," News"," NewUnitedAirlinesMottos"," NoAmnesty"," NoDACA"," NoDACADeal"," NoGunFreeZones"," NoHillary2016"," NoHillaryClinton"," NoIslam"," NOIslam"," NoMoreMoslemRefugees"," NoMoreMoslemsRefugees"," NoMoreMosques"," NoMoreMuslimRefugees"," NoMoreMuslimsRefugees"," NoMoreRefugees"," NoRapefugees"," NoRefugees"," NoRINOs"," NoSharia"," NOSharia"," NoShariaLaw"," NothingGoodComesFromIslam"," NRA"," NuclearOption"," NYC"," NYPrimary"," obama"," Obama"," Obamacare"," ObamaDestroyingAmerica"," ObamaFail"," ObamaGate"," ObamaHatesAmerica"," obamaisalier"," obamaisamuslimlover"," Obamaisamuslimlover"," obamaisapussy"," obamaistheworstpresidentever"," Obamaistheworstpresidentever"," Obamalegacy"," ObamaLegacy"," ObamaLegacyOfFailures"," obamalies"," ObamaLies"," ObamasAmerica"," ObamaScandals"," ObamasLegacy"," ObamaSpyRing"," obamassandals"," obamasucks"," ObamaUSUCK"," ObamaWiretap"," OhHillNo"," OneNationUnderGOD"," OnlyTrump"," Oscars90"," Outnumbered"," p2"," Paris"," Patriot"," PayToPlay"," pedogate"," PedoGate"," pizzagate"," PizzaGate"," pjnet"," PJNET"," PodestaEmails"," PoliceLivesMatter"," PoliticalIslam"," POTUS"," Pray4DJT"," PresidentialDebate"," PresidentTrump"," ProudInfidel"," Q"," qanon"," Qanon"," QAnon"," QANON"," Qanon8chan"," Quran"," QuranIsTheProblem"," RadicalIslam"," RadicalIslamicTerror"," RadicalIslamicTerrorism"," RadicalIslamicTerrorist"," ReadTheDamnQuran"," RealWarOnWomen"," RedNationRising"," RedWaveRising"," RedWaveRising2018"," ReleaseTheMemo"," ReligionOfHate"," ReligionOfPeace"," ReligionOfPeaceMyAss"," RepealAndReplaceObamaCare"," RETWEET"," RiggedSystem"," RNCinCLE"," RockvilleRape"," Rotterdam"," RT"," Russia"," SanBernadino"," satchat"," SayNoToIslam"," Scandal"," Schumer"," SchumerDACAShutdown"," SchumerSellout"," SchumerShutdown"," SchumerSurrender"," SecureTheBorder"," SethRich"," ShadowBan"," ShadowGovernment"," Shameful"," ShariaLaw"," SheLies"," SickHillary"," SnowFlakes"," Soros"," SorosRiots"," SpeakUp"," StandUpAgainstIslam"," StopBuildingMosques"," StopCuttingOurGirls"," StopFGM"," StopFundingHate"," StopHillary"," StopImportingEvilLosers"," StopImportingIslam"," StopImportingIslamicTerror"," StopImportingIslamicTerrorism"," StopImportingJihad"," StopImportingJihadists"," StopImportingMoslems"," StopImportingMoslemscum"," StopImportingMoslemscumbags"," StopIslam"," STOPIslam"," StopIslamicTerror"," StopIslamization"," StopIslamizationOfAmerica"," StopMakingExcusesForIslam"," StopTheHateIslam"," StopTheHATEIslam"," StopTheIslamicInvasion"," SundayMorning"," SuperTuesday"," SupportNRA"," SupportOurTroops"," SupportOurVeterans"," SusanRice"," Syria"," SyriaStrikes"," tcot"," TCOT"," TeamTrump"," teaparty"," TeaParty"," Teen"," Terror"," terrorism"," Terrorism"," tgdn"," ThankYouMaddow"," TheFive"," TheGreatAwakening"," TheHammer"," TheMessyTruth"," TheQuranIsTheProblem"," TheRainMakers"," TheStorm"," TheStormIsHere"," ThingsMoreTrustedThanTrump"," ThingsToBeAshamedOfIslam"," ThinkBIGSundayWithMarsha"," ThisIsIslam"," ThursdayThoughts"," tlot"," traitor"," TravelBan"," treason"," Treason"," TriggerALiberalIn4Words"," TriggerALiberalInFourWords"," trump"," Trump"," TRUMP"," trump2016"," Trump2016"," Trump2020"," Trump45"," TrumpForPresident"," TrumpPence16"," TrumpPence2016"," TrumpRally"," TrumpsArmy"," TrumpTaxReturns"," trumptrain"," TrumpTrain"," TrumpWasRight"," Tucker"," TuesdayMotivation"," Turkey"," TwitterHack"," UK"," UnFitBitch"," UnfitToLead"," UnHolyQuran"," UniteBlue"," Unmasked"," US"," USA"," USA4DJT"," Vault7"," veterans"," Veterans"," VoteGOP"," VoteTrump"," VoteTrump2016"," WakeUp"," WakeUpAmerica"," WakeUpAmericans"," WakeUpUSA"," WakeUpWorld"," WarOnWomen"," Watergate"," WednesdayWisdom"," WeThePeople"," WhiteGenocide"," wikileaks"," Wikileaks"," WikiLeaks"," WIPrimary"," Wiretap"," Wisconsin"," WWG1WGA"," Zuckerberg","سورية_النظر_من_الداخل","2A","2ndAmendment","AbusiveAlec","AllahuAkbar","AlwaysTrump","America","AmericaFirst","Antifa","Aries","ArrestLindaSarsour","Baltimore","BaltimoreRiots","BanCAIR","BanCAIRNational","banislam","BanIslam","BanIslamInAmerica","BanShariaLaw","BanTheBurka","BanTheMuslimBrotherhood","BanTheQuran","BatonRouge","BB4SP","Benghazi","BillWarnerPhD","BlackLivesMatter","BLM","BlueLivesMatter","BotoxBill","BoycottCNN","BOYCOTTHawaii","breaking","Breaking","BREAKING","Brexit","Brussels","BuildTheWall","BUT","CAIR","ccot","CCOT","Clinton","ClintonFoundation","CloseALLMosques","CloseALLMosquesNOW","CNN","Comey","ComeyInterview","COSProject","CPAC2018","CrookedHillary","CruzCrew","CruzSexScandal","Daesh","Dallas","DeathCult","debate","debatenight","DeepState","DemDebate","DemocratLiesMatter","Democrats","DemsInPhilly","Dobbs","DonaldTrump","DrainTheSwamp","dtmag","ebola","Ebola","EducateYourselfOnIslam","Election2016","ElectionNight","ExposeIslam","fakenews","FakeNews","FAKENEWS","FBI","FeelTheBern","Ferguson","FGM","FinalFour","FireColbert","FireMueller","FireRosenstein","FlashbackFriday","FollowTheWhiteRabbit","FoxNews","FreeTommy","FreeTommyRobinson","FridayFeeling","GOP","GOPDebate","GOPTownHall","GreatAwakening","Hannity","Hillary","Hillary2016","HillaryClinton","HillaryLiesMatter","HillarysEmails","HillarysHealth","HillarysHuma","ImpeachObama","ImWithHer","Inauguration","infidel","InsideSyriaMC","Iran","IranDeal","ISIS","islam","Islam","IslamExposed","IslamicState","IslamIsCANCER","IslamIsDangerous","IslamIsEvil","islamistheproblem","IslamIsTheProblem","Israel","IStandWithHannity","JointAddress","KeepBannon","KellyFile","LDTPoll","LiberalCrybabies","LiberalismIsAMentalDisorder","LiberalLogic","Liberals","local","LockHerUp","LyinCrookedHillary","LyingCrookedHillaryClinton","LyingTed","maga","MAGA","MakeAmericaGreatAgain","MarchAgainstSharia","MemorialDay","MerryChristmas","MichelleLegacy","MilitaryMonday","Mizzou","MSM","Muslim","MuslimBan","MuslimBrotherhood","Muslims","MuslimsAreBeautiful","MuslimWomensDay","NeverForget","NeverHillary","NeverSubmitToIslam","NeverTrump","NeverTrustAMoslem","news","NewYork","NoAmnesty","NoDACA","NRA","obama","Obama","Obamacare","ObamaDestroyingAmerica","ObamaGate","ObamaHatesAmerica","obamaisamuslimlover","obamaistheworstpresidentever","Obamaistheworstpresidentever","ObamaLegacy","ObamasLegacy","OhHillNo","Orlando","Oscars","Paris","PeoplePower","Periscope","PizzaGate","pjnet","PJNET","PoliceLivesMatter","politics","POTUS","PresidentTrump","Q","qanon","Qanon","QAnon","QANON","RadicalIslam","RadicalIslamicTerrorism","realdonaldtrump","RealWarOnWomen","RedEye","RedFriday","RedNationRising","ReleaseTheMemo","ReligionOfPeaceMyAss","ResignSheriffIsrael","RHONJ","RHONY","RNCinCLE","RT","Russia","SAA","SanBernadino","SanctuarySewer","SaturdayMorning","SaturdayNightMassacre","SayNoToIslam","Scandal","SchumerShutdown","SCOTUS","SethRich","SheLies","SOTU","sports","StopImportingIslam","StopImportingJihad","StopIslam","STOPIslam","StopMakingExcusesForIslam","SundayMorning","SuperTuesday","SupportNRA","SupportOurTroops","SusanRice","Syria","SyrianArmy","tcot","TCOT","TeamTrump","TedCruz","Texas","TheFive","TheHammer","ThePlan","ThingsToBeAshamedOfIslam","ThisIsIslam","ThursdayThoughts","Today","TravelBan","TriggerALiberalIn4Words","trump","Trump","TRUMP","Trump2016","Trump2020","TrumpPence16","TrumpRally","TrumpTrain","Tucker","Turkey","TwitterLockOut","US","USA","Vault7","VoteTrump","WakeUpAmerica","WakeUpAmericans","WakeUpWorld","WAR","WednesdayWisdom","WeThePeople","WomensMarch"],"freq":[220,417,2404,240,1114,218,296,311,240,616,435,208,974,571,8781,420,210,1638,314,427,4299,298,292,203,254,451,2266,223,232,4762,532,979,8814,210,336,1971,718,276,796,307,636,995,501,13395,365,522,3488,341,247,543,1711,310,6721,265,1001,1987,538,248,207,930,715,465,365,214,203,637,600,648,479,606,278,245,478,472,521,201,384,221,307,704,547,262,762,5002,361,1013,527,233,420,2159,764,240,335,335,461,202,521,212,482,464,935,265,2086,209,3753,490,245,218,348,253,251,511,504,229,1099,236,216,301,414,692,281,358,2200,381,266,235,230,750,202,212,220,208,2292,270,319,2952,444,311,616,634,440,656,439,384,490,237,201,616,1064,252,252,218,942,355,1630,991,806,3871,399,417,610,5698,382,409,773,1140,278,1600,204,252,363,205,663,2178,1101,205,1192,3663,558,206,378,479,208,335,352,263,562,440,417,1568,573,1528,367,234,271,274,607,692,333,802,393,214,275,356,210,421,215,762,535,460,301,335,1727,588,393,202,930,778,233,1738,271,511,222,415,340,629,467,201,929,304,669,694,218,274,216,1745,429,231,270,399,253,2750,349,629,502,238,332,321,488,3889,350,701,5145,1558,1571,504,777,281,1046,1600,294,2365,439,2668,2360,334,14537,211,330,1152,368,214,367,278,2758,519,476,1095,357,263,361,792,982,741,203,320,292,391,1479,321,265,317,548,433,203,1237,201,28557,545,223,5650,280,1101,924,368,246,353,405,355,498,217,202,372,325,1317,556,287,599,274,355,853,238,1362,340,627,1874,829,213,522,231,346,286,381,3017,928,865,697,443,204,1859,2069,470,503,254,277,255,731,1592,702,267,547,534,619,258,398,234,826,714,269,561,883,388,247,204,241,1628,204,1013,344,3811,1030,279,1086,344,208,3390,862,668,1580,237,308,394,301,351,533,745,238,365,340,463,891,206,212,227,307,566,212,317,369,205,335,211,994,1882,3958,496,313,430,632,313,210,784,224,449,528,1502,4996,376,335,453,435,637,308,275,289,206,1703,1718,640,480,34480,448,209,795,226,353,369,276,702,265,492,457,203,225,2106,219,1517,1502,1498,2287,1499,290,359,354,514,214,379,389,401,213,231,392,251,201,528,216,729,257,337,660,14423,219,417,1081,221,1051,766,596,3633,4186,253,446,226,6043,274,346,286,347,225,370,664,515,1352,1851,501,6621,1829,343,384,439,252,302,236,292,282,331,346,803,375,428,493,628,887,614,206,451,226,786,481,497,285,1055,342,206,416,412,539,4152,1519,600,5268,677,210,257,929,225,418,305,386,772,5315,218,407,289,478,211,449,315,273,264,253,294,509,2051,439,1722,232,319,1937,777,848,403,6636,507,496,1039,500,262,981,1467,231,223,284,461,215,1670,225,732,396,382,1103,273,309,369,492,345,2850,270,901,320,432,409,442,814,218,1665,683,1635,698,397,240,204,494,938,241,2317,294,488,347,223,901,581,266,1505,276,270,782,484,593,235,204,344,395,294,748,462,451,681,224,248,1865,229,404,222,302,235,308,285,480,587,530,304,210,207,570,1163,260,277,468,2104,230,226,240,305,1368,711,496,255,969,291,229,316,521,579,279,1548,431,361,240,262,265,1846,313,248,695,1032,470,1246,262,214,577,213,434,220,210,213,281,687,623,2837,314,3667,554,291,237,247,216,223,3703,262,501,219,356,236,208,270,278,321,223,276,406,274,251,222,1140,9147,2272,425,228,288,227,203,220,343,473,476,666,463,228,266,303,1127,385,301,315,873,262,551,845,206,239,2373,274,303,960,247,212,668,234,858,237,210,306,536,265,361,351,230,255,711,526,423,429,620,328,1096,1912,6673,384,237,209,327,621,745,243,526,3103,274,298,237,211,567,1547,281,225,218,222,202,250,227,456,2688,229,844,245,446,350,740,298,535,791,807,326,316,493,206,696,1173,308,4894,511,224,251,202,280,832,332,225,825,268,290,280,529,267,4159,633,3753,200,411,338,1678,436,535,446,458,715,346,417,3492,735,1131,341,323,463,232],"fontFamily":"Segoe UI","fontWeight":"bold","color":"random-dark","minSize":0,"weightFactor":0.00522041763341067,"backgroundColor":"white","gridSize":0,"minRotation":-0.785398163397448,"maxRotation":0.785398163397448,"shuffle":true,"rotateRatio":0.4,"shape":"circle","ellipticity":0.65,"figBase64":null,"hover":null},"evals":[],"jsHooks":{"render":[{"code":"function(el,x){\n                        console.log(123);\n                        if(!iii){\n                          window.location.reload();\n                          iii = False;\n\n                        }\n  }","data":null}]}}</script><!--/html_preserve-->



