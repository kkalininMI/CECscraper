# CECscraper
CECscraper is an open source tool designed to assist with data web scraping of the Central Election Commission's website.  The pre-defined functions help implement a wide range of web-scrapping tasks related to election results by election, by time and the candidates.  

## I Extracting electoral data
The web-scrapping of electoral data consists of two stages: on the first stage the user "guides" the script to create the target collection of URLs with attributes (called "level"" or "info"), attributes directly linked to URLs (called "link"), on the second stage the final dataset is constructed based on the list of URLs. 

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
c                                     
5.	Transliteration

    All Cyrillic characters are transliterated.  In order to use rowURLextractor() efficiently, the user needs transliterate Cyrillic text via the  transliterate() function.

##Examples

+ Task 1: Webscrape the region-level electoral data for presidential election 2018 using the slow webscrapping approach.  

Let’s use a short-path approach feeding the link into the set of URL extractor functions: 
The sequence of steps is as follows:


![Picture 1](Inst/Task1p1.png)

![Picture 2](Inst/Task1p2.png) 

![Picture 3](Inst/Task1p3.png)
  
    library(CECscraper)

    url<-"http://www.vybory.izbirkom.ru/region/izbirkom?action=show&global=1&vrn=100100084849062&region=0&prver=0&pronetvd=null" #See Picture 1
    res1 <- rowURLextractor(url, transliterate("Результаты выборов"), select = 1) #See Picture 2
    res2 <- listURLextractor(res1)  #See Picture 3
    res3 <- dataBuilder(res2, ttime = FALSE, typedata = "slow", dnames = TRUE)
    View(res3$data)

 \ 
 \ 
 \ 

+ Task 2: Webscrape the region-level data for all gubernatorial elections that occured on September 8, 2019 using slow approach. 
Here it is more convenient to use the long-path approach

![Picture 4](Inst/Task2p1.png)

![Picture 5](Inst/Task2p2.png)

![Picture 6](Inst/Task2p3.png)


    library(CECscraper)
    library(rvest)
     
    webpage <- read_html("Inst/gubernatorial_sept2019.html") #See Picture 4 and Picture 5
    res1 <- fileURLextractor(webpage, hashid = TRUE)
    res2 <- rowURLextractor(res1, transliterate("Результаты выборов"), select = 1) #See Picture 6
    res3 <- dataBuilder(res2, ttime = FALSE, typedata = "slow", dnames = TRUE, bylevel="level2")
    names(res3$data)
    res4 <- dataMerger(res3, byrow = TRUE) 
    View(res4$data)















