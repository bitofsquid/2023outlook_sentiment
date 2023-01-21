# 2023outlook_sentiment
Using text analysis to understand positive/negative sentiment in 2023 investment outlooks by major banks

## Introduction
Each year, major investment banks and other market prognosticators publish in-depth analyses of their views for the upcoming year. Often they share how they think the markets they cover will perform. While accountability for these predictions is rarely emphasized, I thought analyzing their content in aggregate could be a helpful exercise. 

### Methodology
To conduct this analysis, I used R and specifically the Tidyverse, Tidytext, PDFtools, and Lexicon packages. 

```
library(pdftools)
library(tidyverse)
library(tidytext)
library(lexicon)

```

I then sourced the URLs that pointed to the PDF versions of the 2023 market outlooks for 12 major banks. 

```
## Market Outlook URLs by firm
gs.url <- "https://www.goldmansachs.com/insights/pages/gs-research/macro-outlook-2023-this-cycle-is-different/report.pdf"
jpm.url <- "https://am.jpmorgan.com/content/dam/jpm-am-aem/global/en/insights/market-insights/mi-investment-outlook-ce-en.pdf"
ms.url <- "https://www.morganstanley.com/im/publication/insights/articles/article_2023investmentoutlook_enin.pdf"
hsbc.url <- 'https://www.privatebanking.hsbc.com/content/dam/privatebanking/gpb/wealth-insights/investment-insights/house-views/2023/brochure/Q1%202023%20Investment%20Outlook%20-%20Looking%20for%20the%20Silver%20Lining%20-%20Brochure.pdf'
citi.url <- "https://www.privatebank.citibank.com/newcpb-media/media/documents/outlook/outlookwealthreport2023.pdf"
cs.url <- "https://data.stagingmag.nl/2763/issues/39432/494150/downloads/2209270_cs_io_2023_en_rgb_digital.pdf"
db.url <- "https://www.deutschewealth.com/content/dam/deutschewealth/cio-perspectives/cio-insights-assets/q1-2023/CIO-Insights-Outlook-2023-Resilience-versus-recession.pdf"
ing.url <- "https://think.ing.com/uploads/reports/ING_global_outlook_2023_Dec_2022_OT.pdf"
apollo.url <- "https://www.apollo.com/~/media/Files/A/Apollo-V3/documents/2023-apollo-economic-and-capital-markets-outlook-white-paper.pdf"
wf.url <- "https://www08.wellsfargomedia.com/assets/pdf/personal/investing/investment-institute/2023-outlook-report_ADA.pdf"
bny.url <- "https://www.bnymellonwealth.com/content/dam/bnymellonwealth/pdf-library/articles/Final_BNYM2023OutlookMaster12_20_22.pdf"
fid.url <- "https://professionals.fidelity.co.uk/static/master/media/pdf/outlook/annual_outlook_2023.pdf"

```

To ensure this analysis focused on the most meaningful text within each outlook, I utilized the list of functional words (i.e. "the", "at", "and", etc.) and prepositions (i.e. "with", "from", "like", "into") provided by the Lexicon R package to filter out irrelevant content. 

I also created a customized list of other words that needed to be excluded (e.g. the names of the banks publishing the outlooks, the names of the months of the year, and other low value words). 

Lastly, I filtered out any other words with less than 2 letters that may not have been caught by the other filters (for abbreviations, etc.). 

```
prepositions <- pos_preposition
functional <- function_words

exclude <- c("goldman", "sachs", "morgan", "stanley", "research", "analyst", "exhibit", 
             "january", "february", "march", "april", "june", "july", "august", "september", "october",
             "november", "december", "jan", "feb", "mar", "apr","jun","jul","aug","sep","oct","nov",
             "dec", "source",  "jpm", "sachsglobal", "haver", "analytics", "report", "citi", "global", 
             "investment", "performance", "j.p", "year","management", "u.s", "index","investments", "market",
             "markets", "hsbc", "credit suisse", "deutsche bank", "ing", "apollo", "management", "fidelity",
             "bank", "new york", "mellon", "information", "views", "opinions", "document", "herein",
             "credit", "suisse", "private", "banking", "investors", "wells", "fargo")
             
```

Next, I used the Tidytext R package to define a list of positive and negative words that I would ultimately use to classify the relative sentiment of each outlook. 

```
nrc_negative <- get_sentiments("nrc") %>%
  filter(sentiment == "negative")

nrc_positive <- get_sentiments("nrc") %>% 
  filter(sentiment == "positive")

```

Next, I used the PDFtools and Tidytext R packages to download each outlook, extract individual words, and tag them with a positive or negative handle. Finally, I put the summary information together in a combined table that showed the number of total, positive, and negative words contained in each outlook and then plotted the results. 

```
summary <- tibble()

for (i in 1:length(outlooks)){
   
  temp <- as_tibble(gsub("[[:digit:]]", "", pdf_text(url.list[[i]])))
  
  temp <- temp %>%
    rename("text" = value) %>%
    unnest_tokens("word", "text") %>%
    mutate(word = str_replace(word, "'s", "")) %>%
    filter(!word %in% prepositions & !word %in% functional & !word %in% exclude & nchar(word) > 2) %>%
    count(word, sort = TRUE) %>%
    mutate(firm = outlooks[i])

  summary <- rbind(summary, temp)
}

total <- summary %>%
  group_by(firm) %>%
  summarize(sum(n))

negative <- summary %>%
  inner_join(nrc_negative) %>% 
  group_by(firm, sentiment) %>%
  summarize(sum(n)) %>%
  select(!sentiment)

positive <- summary %>%
  inner_join(nrc_positive) %>% 
  group_by(firm, sentiment) %>%
  summarize(sum(n)) %>%
  select(!sentiment)

words <- total %>%
  inner_join(positive, by = "firm") %>% 
  inner_join(negative, by = "firm")

colnames(words) <- c("firm", "total", "negative", "positive")

neg.pos <- words %>%
  mutate(neg.pos = negative/positive) %>% 
  arrange(desc(neg.pos)) 

verbosity <- words %>%
  arrange(desc(total))

barplot(neg.pos$neg.pos, xlab = "Investment Firm", ylab = "Negative / Positive Ratio", col = "red",
        ylim = c(0,2), names.arg = neg.pos$firm, main = "Negative Sentiment in 2023 Investment Outlooks")
abline(h=1)

barplot(verbosity$total, xlab = "Investment Firm", ylab = "Total Number of Words", ylim = c(0,25000), 
        names.arg = verbosity$firm, main = "Verbosity of 2023 Investment Outlooks", col = "blue")

```

## Observations
This was my first attempt at any form of text analysis and though the amount of hard-hitting data science here is rather limited, I thought the final output was still fairly informative. As we would expect given the current economic climate, most outlooks skewed negative with Goldman Sachs being most pessimistic. On the other hand ING was neutral in sentiment with roughly the same amount of positive and negative text found in their outlook. Another interesting observation was the amount of text contained in each outlook. Even after filtering out low value words, Citi took first prize for the most verbose publication writing nearly 25,000 words. In contrast, BNY Mellon was the most concise writing roughly 1,600 words in their outlook.

## Further Analysis
As a next step in this analysis, it would be much more informative to analyze longer segments of text (i.e. phrases) in order to understand more clearly the context, meaning, and sentiment of what the authors may have been saying. In the next iteration of this analysis, I plan to explore this angle more deeply. 

