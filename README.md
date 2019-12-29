# CECscraper
CECscraper is an open source tool for web scraping the data from the website of the Central Election Commission (CEC, CIK).  The package's functions help to implement a wide range of web-scrapping tasks related to different types of electoral data. The package's webscraping flexibility is based on the "constructor" principle, according to which any webscraping task can be divided into the sequence of basic webscrapping steps realized through the package's functions.  While using this package the user is expected  to follow the
[guidance for the ethic webscraping](https://towardsdatascience.com/ethics-in-web-scraping-b96b18136f01) and fully comply with Central Election Commission's webscrapping guidelines.  

## I Extracting electoral data
The webscraping of electoral data implies that the user constructs the webscraping algorithm using package's functions as building blocs.  The scraping procedure consists of two stages. On the first stage the user creates the data frame via the set of *URL extraction functions* that contains collection of target URLs and links (attributes directly linked to URLs), as well as attributes or information inherited from higher levels of data (called "level" or "info"). On the second stage the user is able to scrape and build the dataframe containing electoral data via the Data building function. 

To give you a taste of how easy scraping would be, let's imagine that we are interested in webscraping the precinct-level data for Moscow City Council election held on September 2019.  The sequence of possible scraping steps would be as follows:

First, load three R packages. Find the  *root URL* using election search of [http://www.izbirkom.ru](http://www.izbirkom.ru).

    library(CECscraper)
    library(dplyr)
    library(rvest)

Second, click on the election link and copy-paste election's URL "http://www.vybory.izbirkom.ru/region/izbirkom?action=show&vrn=27720002327736&region=77&prver=0&pronetvd=null" into a variable *root_url*
    
    root_url <- "http://www.vybory.izbirkom.ru/region/izbirkom?action=show&vrn=27720002327736&region=77&prver=0&pronetvd=null".
    
Third, by keeping in mind that the hierarchy of URLs is **general election results-> election results for OIKs/SMDs -> election results for rayons -> election results for precincts**,
let's derive URLs for OIKs. There are different ways to do this. For instance, we can obtain a set of URLs for OIKs simply by clicking on the drop-down menu of "Нижестоящие избирательные комиссии".  Let's extract this URL data via listURLextractor() function.

    oiks_urls <- listURLextractor(root_url)
    
    oiks_urls[1,]

Notice that in our data the *link* variable directly refers to URLs. Check if we've collected the right set of URLs by copy-pasting one of the scraped URLs into the web browser.  Then, in the browser by clicking on the drop-down menu of "Нижестоящие избирательные комиссии" for OIKs, we can find URLs for rayons (i.e. rayons are nested within OIKs).  Let's scrape the URLs for rayons by using listURLextractor() once again.

    rayons_urls <- listURLextractor(oiks_urls)
    
    rayons_urls[1,]
    
Check if we've collected the right set of URLs by copy-pasting one of the scraped URLs into the web browser.  By the way, notice that the function "memorizes" OIKs by pushing them in the *level* variable, and each time adds new *link* variable directly linked to current scraping level, i.e. rayons.  This is an important "cumulative" feature of *URL extraction functions*  enabling us to store information from previous levels. 

By the way, since we are dealing with nested URLs, we can simplify our previous tasks by resorting to nested functions:

    rayons_urls <- listURLextractor(
                        listURLextractor(root_url)
                            )


Again, to check if we've collected correct set of URLs simply copy-paste one of the scraped URLs into the web browser. Now it appears that we can't access the precinct-level data without clicking on
"сайт избирательной комиссии субъекта Российской Федерации".

Fourth, let's scrape the URLs linked to the "сайт избирательной комиссии субъекта Российской Федерации" link for all rayons_urls (scraping these URLs would be equivalent to actual link clicking).  
Note that all we need is just to copy-paste the link from the website "сайт избирательной комиссии субъекта Российской Федерации", and then properly transliterate it via package's internal transliterator, tranliterate() function.

    rayons_urls2 <- rowURLextractor(rayons_urls, transliterate("сайт избирательной комиссии субъекта Российской Федерации"))
    
    rayons_urls2[1,]

As always, check if extracted URLs are OK by copy-pasting one of the URLs in a web browser.

Fifth, now if we click the drop-down menu of Нижестоящие избирательные комиссии  we access the precinct-level data.  So, let's use the listURLextractor() again:

    uiks_urls <- listURLextractor(rayons_urls2)

Check if URLs for precincts are OK by copy-pasting one of the URLs to your web browser.

Sixth, to access the precinct-level data we need to click on the link "Результаты выборов по одномандатному (многомандатному) округу".  
In other words, we would need to use rowURLextractor() again.

    uiks_urlsD <- rowURLextractor(uiks_urls[1:200, ], transliterate("Результаты выборов по одномандатному (многомандатному) округу"))
    
Check if uiks_urlsD provide you direct access to the data by copy-pasting one of the URLs to your web browser.

Seventh, now scrape the data using uiks_urls.  We need to keep in mind several things: 

  + *bylevel* parameter defines the level of aggregation for the dataset:  since we are dealing with SMD data, the level must reflect SMD.  Open the uiks_urls and find a variable  "level1" -- this is exactly what we need.
  
  + *typedata="slow"*, i.e. we need to go precinct-page by precinct-page to extract precinct-level data (the faster approach would be to scrape using the "Summary table of electoral results", which contains precinct-level information per rayon, this is a so-called "fast approach".  See below) 
  
  + *dname=TRUE* since we care about the names of SMD Candidates we need to set this paramter to TRUE (sometimes the exact names of the columns are unimportant, so for computational purposes the variable names are are assigned simple codings "C1...Cn" for ballot counts and "P1...Pn" for candidates/parties).
  
  
Let's scrape first 200 rows.

    data_uiks <- dataBuilder(uiks_urlsD,  bylevel="level1", typedata = "slow", dnames = TRUE)
     
    names(data_uiks$data)


Eighth, the data is a list of data frames defined by "level1".  To merge all the data together one needs to apply dataMerger() with parameter byrow = TRUE, i.e. the data needs to be merged by row. 

     data_uiks_merged <- dataMerger(data_uiks, byrow = TRUE) 

To access the webscraped data: 

     edit(data_uiks_merged$data)


Note that this is the decription of one of the webscrapping "paths":  to address your webscraping needs you can use any path and any sequence of functions as long as they are logically consistent.  
For instance, we could start webscrapping a bit differently:

    root_url <- "http://www.vybory.izbirkom.ru/region/izbirkom?action=show&vrn=27720002327736&region=77&prver=0&pronetvd=null".

    root_urlsD <- rowURLextractor(root_url, transliterate("Данные о предварительных итогах голосования по одномандатному (многомандатному) округу"))
    oiks_urls <- listURLextractor(root_urlsD)
    rayons_urls <- listURLextractor(oiks_urls)
    rayons_urls2 <- rowURLextractor(rayons_urls, transliterate("сайт избирательной комиссии субъекта Российской Федерации"))
    uiks_urls <- listURLextractor(rayons_urls2)
    data_uiks <- dataBuilder(uiks_urls[1:100,],  bylevel="level2", typedata = "slow", dnames = TRUE)
    data_uiks_merged <- dataMerger(data_uiks, byrow = TRUE) 

    #an alternative with pipe operator
    library(dplyr)
       
    data_uiks<- rowURLextractor(root_url, transliterate("Данные о предварительных итогах голосования по одномандатному (многомандатному) округу"))%>%
                listURLextractor() %>%
                listURLextractor() %>%
                rowURLextractor(transliterate("сайт избирательной комиссии субъекта Российской Федерации")) %>%
                listURLextractor() %>%
                dataBuilder(uiks_urls,  bylevel="level2", typedata = "slow", dnames = TRUE) %>%
                dataMerger(byrow = TRUE) 

The description of the third webscraping using "fast" method is provided in *Task 3*.
  
    
 

### URL extraction functions:

1. <em>fileURLextractor (html_file, tabextract = NULL, hashid = FALSE)</em>

    This function extracts election-related links and urls from an html page of the Central Election Commission.
    
    + html_file -- html file object.
    +	tabextract -- select the table number to extract in order to override the table selection algorithm.
    +	hashid -- generate unique md5 hash id per election.

2. <em>listURLextractor (x)</em> 
    
    This function extracts election-related links from the lists or menus.
    
    +	x -- link, list of links or data frame with links.

3. <em>rowURLextractor (x, item, select = 1)</em>

    This function extracts the links and urls from the page.
    
    +	x -- the list of urls or MenuListExtractor object.
    +	item -- link/expression that contains the url of interest.
    +	select -- select id of a single match among several identified by the algorithm.

### Data building function:
4.  <em> dataBuilder (x, bylevel = NULL, ttime = FALSE, typedata = "slow", dnames = FALSE, tabextract = NULL, savetodir = "")</em> 

    This function builds the data frame from the webpage or listURLextractor/rowURLextractor objects.
    
    +	x -- link/list/data frame with links.
    +	bylevel -- define data subsets for an output
    + ttime -- checks if extracted data covers reported turnout over election day (TRUE) or not (FALSE).
    +	typedata -- checks whether the data extracted from "svodnya tablitsa"(pivot table) link (fast approach) or "rezultaty vyborov" link (slow approach).
    +	dnames -- assign to column name labels original labels (TRUE) or not (FALSE).
    +	tabextract -- select the table number to extract in order to override the table selection algorithm.
    +	savetodir -- save html data files to specified directory, i.e. "C:/Documents".

### Auxiliary functions:
5.  <em> dataMerger(x, byrow = TRUE) </em> 
    
    This function merges the list of data objects together.
    
    +	x -- the list of data objects.
    +	byrow -- the list is merged by row or by column.  If byrow==FALSE only two objects merged into a list.

6.  <em> eftFormatter(x, Nvalid = "CEC", levels = TRUE) </em> 

    This function reformats the data for its use with Election Forensics Toolkit.
  
    +	x -- the object from Databuilder/DataMerger function.
    +	Nvalid -- valid votes computed using either "CEC" (using the Central Election Comission's formula); "AllVotes" (total number of votes cast for all candidates).
    +	levels -- add information on levels (TRUE) or not (FALSE) to the output.

7.  <em> transliterate(v) </em>

    Returns transliterated expression.
    
    +	v -- expression


## II Extracting data on candidates
8.  <em> scrapeCandidates(x, tabextract = NULL, savetodir = "") </em> 

    This function extracts the candidate-related data information from the webpages.
    
    +	x -- url, list of urls
    +	tabextract -- select the table number to extract in order to override the table selection algorithm.
    +	savetodir -- save html downloaded data files in directory, i.e. "C:/Documents".


## Tips for efficient webscraping

1.	Long-Path Approach vs. Short-Path Approach

    Long-Path approach assumes that the URL accumulation starts with <em>fileURLextractor()</em> function.  In other words, after the initial data search, all the search results must be saved as a single html file for later use by this function.  The short-path approach assumes that the user can feed URLs directly into the list and row extraction functions without resorting to <em>fileURLextractor()</em> function.  The latter approach, however, may prevent the user from getting certain "level" attributes like the date of election or election-specific hashids important for producing unique election identifiers.

2.	Automated Webscraping vs. Prudent Webscraping

    With the help of <em>dataBuilder()</em> it is possible to webscrape literary hundreds of elections and collect the data across thousands of locations, but this can also drastically increase the chances of arbitrary errors in one of the elections and high probability of losing all previously scrapped data.  If the user attempts to webscrape many elections at once, a much more prudent approach would be to place <em>CECscraper</em> functions within the loop and save the data as R objects using <em>saveRDS()</em> in R. 

3.	Slow Webscraping vs. Fast Webscraping

    On the website of the Central Election Commission electoral data is provided in two different formats:  "Rezul'taty vyborov" (Electoral results)      and <em>"Svodnaya tablitsa rezul'tatov vyborov"</em> (<em>"Summary table of electoral results"</em>).  If the webscraping of the first format
    takes relatively long time to implement (it contains deeper data structure with less data per page), the second format is much faster (it
    contains shallower data structure with more information per page).  Both approaches "slow" or "faster" can be used interchangeably, but slow web scrapping is more error      prone.

4.	Table Search Automation vs. Manual Selection

    Both <em>dataBuilder()</em> and  <em>scrapeCandidates()</em> functions contain in-built detection algorithm designed to automatically select the table that is used for data extraction.  The webpage can consist of up to 12 different tables and subtables.  If dataBuilder() and                    scrapeCandidates() produce an error message the algorithm must have failed to detect correct table.  In case of an error, the user need to rely on trial-and-error approach of defining the table number via tabextract parameter.
                                     
5.	Transliteration

    All Cyrillic characters are transliterated.  In order to use rowURLextractor() efficiently, the user needs transliterate Cyrillic text via the  transliterate() function.

# Examples

### *Task 1: Webscrape the region-level electoral data for presidential election 2018 using the slow webscrapping approach.*

Let’s use a short-path approach by feeding the root url into the set of URL extractor functions: 
The sequence of steps is as follows:


![Picture 1](Inst/Task1p1.png)
    
![Picture 2](Inst/Task1p2.png)
  
    #Code Fragment 
    
    library(CECscraper)

    url<-"http://www.vybory.izbirkom.ru/region/izbirkom?action=show&global=1&vrn=100100084849062&region=0&prver=0&pronetvd=null" #See Picture 1
    res1 <- rowURLextractor(url, transliterate("Результаты выборов"), select = 1) #See Picture 2

![Picture 3](Inst/Task1p3.png)

    #Code Fragment
    
    res2 <- listURLextractor(res1)  #See Picture 3
    res3 <- dataBuilder(res2, ttime = FALSE, typedata = "slow", dnames = TRUE)
    View(res3$data)
  
  \ 
  \ 
  \ 
  
    #Full code

    library(CECscraper)

    url<-"http://www.vybory.izbirkom.ru/region/izbirkom?action=show&global=1&vrn=100100084849062&region=0&prver=0&pronetvd=null" #See Picture 1
    res1 <- rowURLextractor(url, transliterate("Результаты выборов"), select = 1) #See Picture 2
    res2 <- listURLextractor(res1)  #See Picture 3
    res3 <- dataBuilder(res2, ttime = FALSE, typedata = "slow", dnames = TRUE)
    View(res3$data)

  \ 
  \ 
  \ 

### *Task 2: Webscrape the region-level data for all gubernatorial elections that occured on September 8, 2019 using slow approach.* 
Since we need to download many elections it is more convenient to use the long-path approach.

Picture 4
![Picture 4](Inst/Task2p1.png)

Picture 5
![Picture 5](Inst/Task2p2.png)
    
    #Code Fragment

    library(CECscraper)
    library(rvest)
     
    webpage <- read_html("Inst/gubernatorial_sept2019.html") #See Picture 4 and Picture 5
    res1 <- fileURLextractor(webpage, hashid = TRUE)


Picture 6
![Picture 6](Inst/Task2p3.png)

    #Code Fragment

    res2 <- rowURLextractor(res1, transliterate("Результаты выборов"), select = 1) #See Picture 6
    res3 <- dataBuilder(res2, ttime = FALSE, typedata = "slow", dnames = TRUE, bylevel="level2")
    names(res3$data)
    
    res4 <- dataMerger(res3, byrow = TRUE) 
    View(res4$data)
  
  \ 
  \ 
  \ 

    #Full code
    
    library(CECscraper)
    library(rvest)
     
    webpage <- read_html("Inst/gubernatorial_sept2019.html") #See Picture 4 and Picture 5
    res1 <- fileURLextractor(webpage, hashid = TRUE)
    res2 <- rowURLextractor(res1, transliterate("Результаты выборов"), select = 1) #See Picture 6
    res3 <- dataBuilder(res2, ttime = FALSE, typedata = "slow", dnames = TRUE, bylevel="level2")
    names(res3$data)
    res4 <- dataMerger(res3, byrow = TRUE) 
    View(res4$data)


### *Task 3: Webscrape the precinct-level data for Moscow election on September 2019 using fast approach. 

Picture 7
![Picture 7](Inst/Task3p1.png)

Picture 8
![Picture 8](Inst/Task3p2.png)

    #Code Fragment
    
    library(CECscraper)
    library(dplyr)
    
    url<-"http://www.vybory.izbirkom.ru/region/izbirkom?action=show&vrn=27720002327736&region=77&prver=0&pronetvd=null" #See Picture 7 and Picture 8
    
    res1 <- rowURLextractor(url, transliterate("Сводная таблица предварительных итогов голосования")) #See Picture 8
 
Picture 9
![Picture 9](Inst/Task3p3.png)

Picture 10
![Picture 10](Inst/Task3p4.png)

    #Code Fragment

    res2 <- listURLextractor(res1)  #See Picture 9
    res3 <- listURLextractor(res2)  #See Picture 10

Picture 11
![Picture 11](Inst/Task3p5.png)

Picture 12
![Picture 12](Inst/Task3p6.png)

    #Code Fragment

    res4 <- rowURLextractor(res3, transliterate("сайт избирательной комиссии субъекта Российской Федерации")) #See Picture 11
    res5 <- dataBuilder(res4, typedata = "fast", dnames = TRUE, bylevel="level3") #See Picture 12

  
  \ 
  \ 
  \ 

    #Full code

    library(CECscraper)
    library(dplyr)

    ###########################
    #extracting electoral data#
    ###########################
    url<-"http://www.vybory.izbirkom.ru/region/izbirkom?action=show&vrn=27720002327736&region=77&prver=0&pronetvd=null" #See Picture 7 and Picture 8
    
    res1 <- rowURLextractor(url, transliterate("Сводная таблица предварительных итогов голосования")) #See Picture 8
    res2 <- listURLextractor(res1)  #See Picture 9
    res3 <- listURLextractor(res2)  #See Picture 10
    res4 <- rowURLextractor(res3, transliterate("сайт избирательной комиссии субъекта Российской Федерации")) #See Picture 11
    res5 <- dataBuilder(res4, typedata = "fast", dnames = TRUE, bylevel="level3") #See Picture 12
    
    #an alternative with pipe operator
    res5 <- rowURLextractor(url, transliterate("Сводная таблица предварительных итогов голосования")) %>%
              listURLextractor() %>%
              listURLextractor() %>%
              rowURLextractor(transliterate("сайт избирательной комиссии субъекта Российской Федерации")) %>%
              dataBuilder(typedata = "fast", dnames = TRUE, bylevel="level3")
    
    #############################
    #extracting time information#
    #############################
    res1t <- rowURLextractor(url, transliterate("Данные об открытии помещений для голосования"))
    res2t <- listURLextractor(res1t)
    res3t <- listURLextractor(res2t)
    res4t <- rowURLextractor(res3t, transliterate("сайт избирательной комиссии субъекта Российской Федерации"))
    res5t <- dataBuilder(res4t, typedata = "fast", ttime = TRUE, dnames = TRUE, bylevel="level3")
    
    #an alternative with pipe operator
    res5t <- rowURLextractor(url, transliterate("Данные об открытии помещений для голосования")) %>%
               listURLextractor() %>%
               listURLextractor() %>% 
               rowURLextractor(transliterate("сайт избирательной комиссии субъекта Российской Федерации")) %>%
               dataBuilder(typedata = "fast", ttime = TRUE, dnames = TRUE, bylevel="level3")
    
    ###############################
    #merging electoral + time data#
    ###############################
    res6 <- dataMerger(list(res5, res5t), byrow = FALSE)
    res7 <- dataMerger(res6, byrow = TRUE) 
    
    #remove extra rows for rayons
    res8 <- res7$data[grepl("^UIK", res7$data$link),]
    
    ###############################
    #extracting info on candidates#
    ###############################
    urlc <- "http://www.moscow_city.vybory.izbirkom.ru/region/region/moscow_city?action=show&root=1&tvd=27720002327740&vrn=27720002327736&region=77&global=null&sub_region=0&prver=0&pronetvd=null&vibid=27720002327736&type=220"
    resc <- scrapeCandidates(urlc)


### *Task 4: Webscrape all available country-level data for federal elections (both presidential and parliamentary). 
    
    library(CECscraper)
    library(dplyr)
    library(rvest)
     
    webpage <- read_html("Inst/elections_federal.html") #See Picture 4 and Picture 5
    res1 <- fileURLextractor(webpage, hashid = TRUE)
    
    filter1<-paste(c(transliterate("Результаты выборов"), transliterate("Итоги голосования по федеральному округу")), collapse="|")
    res2 <- rowURLextractor(res1[c(1:6,8),], filter1, select = 1)
    res3 <- rowURLextractor(res1[7,], transliterate("Результаты выборов по федеральному избирательному округу"), select = 1)
    resm <- rbind(res2,  res3)    
    resm$level5 <-  paste(resm$level1, resm$level4)
    res3 <- dataBuilder(resm, ttime = FALSE, typedata = "slow", dnames = TRUE, bylevel="level5")
    
    names(res3$data)


### *Task 5: Webscrape the SMD electoral data for 2016 State Duma elections, webscrape  candidate's party ID, and then format the data in the CLEA's format (*[CLEA website](http://www.electiondataarchive.org/data-and-documentation.php)).  

    #Full code

    library(CECscraper)
    library(dplyr)
    
    #Scrape SMD data
    url2016<-"http://www.vybory.izbirkom.ru/region/izbirkom?action=show&global=1&vrn=100100067795849&region=0&prver=0&pronetvd=0"
    
    p0<-rowURLextractor(url2016, transliterate("Результаты выборов по одномандатному избирательному округу"), select=1)%>%
        listURLextractor()%>%
        listURLextractor()%>%
        dataBuilder(typedata="slow", bylevel="link", ttime=FALSE, dnames=TRUE)%>%
        dataMerger()
    
    p1 <- t(p0$data)
    p2 <- p1[-c(1:24),]
    oiks <- p1[5,]
    oiks_oik <- unlist(sapply(1:length(oiks), function(i){rep(oiks[i], sum(!is.na(p2[,i])))}))
    oiks_num <- gsub("[^0-9.]", "",  oiks_oik)
    oiks_votes <- unlist(sapply(1:length(oiks), function(i){  p2[,i][!is.na(p2[,i])]}))
    oiks_names <- as.character(names(oiks_votes))
    candidates_data <- data.frame(oiks_oik, oiks_num, oiks_names, oiks_votes, stringsAsFactors = FALSE)
    candidates_data$ind <- paste(candidates_data$oiks_names, candidates_data$oiks_num, sep="_")
    
    oiks_num <- gsub("[^0-9.]", "",  oiks)
    electoral_info <- data.frame(cbind(oiks, oiks_num, t(p1[c(7:24),])), stringsAsFactors = FALSE)
    
    #Scrape the data on candidate's party ID
    candidates="http://www.vybory.izbirkom.ru/region/region/izbirkom?action=show&root=1&tvd=100100067795854&vrn=100100067795849&region=0&global=true&sub_region=0&prver=0&pronetvd=0&vibid=100100067795849&type=220"
    smd.candidates<-scrapeCandidates(candidates)
    smd.candidates2<-subset(smd.candidates$data, select=c("FIO.kandidata", "Nomer.okruga", "Status.kandidata.4", "Sub.yekt.vydvizheniya"))
    smd.candidates3<-smd.candidates$data[,c("FIO.kandidata", "Nomer.okruga", "Status.kandidata.4", "Sub.yekt.vydvizheniya", "Status uchastnika vyborov")]
    smd.candidates3$ind<-paste(apply(smd.candidates3["FIO.kandidata"],1,as.character), 
                               apply(smd.candidates3["Nomer.okruga"],1,as.character), sep="_")
    
    #Merge the data
    merged_data<-merge(candidates_data, smd.candidates3, by="ind")
    merged_data2<-subset(merged_data, select=c("oiks_oik", "oiks_num", "oiks_names", "oiks_votes", "Sub.yekt.vydvizheniya"))
    merged_data3<-merge(electoral_info, merged_data2, by="oiks_num")
    
    #write.csv(merged_data3, "Russia2016_SMD.csv")






