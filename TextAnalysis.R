library(pdftools)
library(tidyverse)
library(tidytext)
library(lexicon)

## Name the outlooks
outlooks <- c("gs", "jpm", "ms", "hsbc", "citi", "cs", "db", "ing", "apollo", "wf", "bny", "fid")

## Exclude irrelevant or non-meaningful words
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


## Get sentiment categories
nrc_negative <- get_sentiments("nrc") %>%
  filter(sentiment == "negative")

nrc_positive <- get_sentiments("nrc") %>% 
  filter(sentiment == "positive")


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


url.list <- list(gs.url, jpm.url, ms.url, hsbc.url, citi.url, cs.url, db.url, ing.url, apollo.url, wf.url,
                 bny.url, fid.url)

## Create raw table showing word counts for each bank 
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

## Create summary tables for total, negative, and positive words by firm
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

## Create tables with negative to positive ratio and verbosity stats
neg.pos <- words %>%
  mutate(neg.pos = negative/positive) %>% 
  arrange(desc(neg.pos)) 

verbosity <- words %>%
  arrange(desc(total))

## Plot relative prevalence of negative to positive words
par(mfrow = c(1,2))

barplot(neg.pos$neg.pos, xlab = "Investment Firm", ylab = "Negative / Positive Ratio", col = "red",
        ylim = c(0,2), names.arg = neg.pos$firm, main = "Negative Sentiment in 2023 Investment Outlooks")
abline(h=1)

## Plot the number of words by firm
barplot(verbosity$total, xlab = "Investment Firm", ylab = "Total Number of Words", ylim = c(0,25000), 
        names.arg = verbosity$firm, main = "Verbosity of 2023 Investment Outlooks", col = "blue")


