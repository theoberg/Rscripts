library(purrr)
library(rvest)
library(clipr)
library(tidytext)
library(dplyr)
library(ggplot2)

business <- 'zerazza.com'

link <- paste0('https://se.trustpilot.com/review/', business, '?languages=all')
no_pages <- ceiling(read_html(link) %>% html_nodes('.headline__review-count') %>% html_text(trim=TRUE) %>% gsub('\\s','',.) %>% as.numeric()/20)

reviews <- tibble()

for(page_result in 1:no_pages) {
  page <- paste0(link, '&page=', page_result)
  review_card <- read_html(page) %>% html_nodes('.review-card')
  reviews <- rbind(reviews, review_card %>% 
                     map_dfr(~list(
                       verified = html_node(.x, '.review-content-header__review-verified') %>% 
                         html_text(trim=TRUE) %>% 
                         regexpr('isVerified', .) > 0,
                       
                       reviewer = html_node(.x, '.consumer-information__name') %>% 
                         html_text(trim=TRUE) %>% 
                         {if(length(.) == 0) NA else .},
                       
                       reviewerReviews = html_nodes(.x, '.consumer-information__review-count') %>% 
                         html_text(trim=TRUE) %>% 
                         substring(1,1) %>% 
                         as.integer() %>% 
                         {if(length(.) == 0) NA else .},
                       
                       title = html_nodes(.x, '.link--dark') %>% 
                         html_text(trim=TRUE) %>% 
                         {if(length(.) == 0) NA else .},
                       
                       # reviewCreated = html_nodes(.x, '.review-content-header__dates') %>% 
                       # html_text(trim=TRUE) %>% 
                       #   gsub('.+?([0-9-]{10}).+','\\1',.) %>% 
                       #   as.Date(format='%Y-%m-%d') %>% 
                       #   {if(length(.) == 0 ) NA else .},
                       
                       lastUpdated = html_nodes(.x, '.review-content-header__dates') %>% 
                         html_text(trim=TRUE) %>% 
                         gsub('.+([0-9-]{10}).+','\\1',.) %>% 
                         as.Date(format='%Y-%m-%d') %>% 
                         {if(length(.) == 0 ) NA else .},
                       
                       stars = html_nodes(.x, 'img') %>% 
                         html_attr('alt') %>% 
                         substr(1,1) %>%
                         factor(levels=c('1','2','3','4','5')) %>% 
                         {if(length(.) == 0 ) NA else .},
                       
                       review = html_nodes(.x, '.review-content__text') %>% 
                         html_text(trim=TRUE) %>% 
                         gsub('\\t','',.) %>% 
                         {if(length(.) == 0) NA else .},
                       
                       reviewReplied = html_node(.x, '.brand-company-reply__content') %>%
                         regexpr('.+', .) > 0
                      )
                    )
                  )

print(paste('Page: ', page_result, ' of ', no_pages ))
}

readline(prompt='When you\'re ready, press [enter] to continue to Google Sheets and translate the copied text')

write_clip(reviews$review)
browseURL('https://docs.google.com/spreadsheets/d/1C74VU5yBTEGlCL-agcDpRrEhkeYhiQfC6h1AeYNDX-0/edit#gid=0')

readline(prompt='When you\'ve copied the translated text, press [enter] to continue or [esc] to end')

reviews <- mutate(reviews, translatedReview = read_clip(), lengthOgLang = nchar(review) , index = 1:n())

unnested_reviews <- reviews %>%
  group_by(index) %>% 
  ungroup() %>% 
  unnest_tokens(word, translatedReview)

sentiment_reviews <- unnested_reviews %>% 
  inner_join(get_sentiments('afinn')) %>% 
  group_by(index) %>% 
  summarise(sentiment = sum(value)) %>% 
  left_join(reviews,.) %>% 
  subset(select=-c(index,translatedReview)) %>% 
  distinct()

ggplot(sentiment_reviews, aes(stars)) + geom_bar()

ggplot(sentiment_reviews, aes(lastUpdated)) + geom_bar()

ggplot(sentiment_reviews, aes(lastUpdated)) + geom_bar() + facet_wrap(~stars) #+ scale_x_date(limits = as.Date(c('2020-01-01','2020-08-01')))

length_summary <- sentiment_reviews %>% 
  group_by(stars) %>% 
  summarise(meanLength = mean(lengthOgLang, na.rm=TRUE))

ggplot(length_summary, aes(stars, meanLength)) + geom_col()

ggplot(sentiment_reviews, aes(stars, lengthOgLang)) + geom_boxplot()

ggplot(sentiment_reviews, aes(sentiment, lengthOgLang, color = stars)) + geom_point()

ggplot(sentiment_reviews, aes(sentiment, lengthOgLang, color = stars)) + stat_ellipse() + ylim(0,1200)

