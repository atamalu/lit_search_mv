Literature Searches in R
================

# Introduction

Note: Never replace an actual literature search with an automated method
like this. Do not sacrifice quality for a small discount on time spent.
This example is just to show how R can be used to assist in gathering
some preliminary information.

# Gathering data

``` r
rm(list = ls())
library(rvest)
library(xml2)
library(rplos)
```

    ## Warning: package 'rplos' was built under R version 3.6.3

``` r
p.search <- searchplos(q = 'learned helplessness', limit = 10)
dois <- p.search$data$id
txts <- plos_fulltext(dois)
```

It is useful to save the data, so we will also do that

``` r
saveRDS(txts, 'example.rds')
```

# Synthesizing data

When designing criteria for text mining the material you need, it helps
a lot to have a background in the subject. In the case of a subject
that’s novel to you, it may actually be more helpful to do this
<i>post</i> literature search.

### pt. 1: article title

Extracting the article title is relatively easy. It’s the first entry
labeled “article-title” and the alternative title is “alt-title”.

``` r
txt <- read_xml(txts[[1]]) # read the xml object for first article

titl <- txt %>%
  html_nodes('article-title') %>% 
  xml_text() %>%
  .[[1]] # select first item (the actual title)

alt.titl <- txt %>%
  html_nodes('alt-title') %>% 
  xml_text() %>%
  .[[1]]
```

Now we turn it into a function

``` r
get_article_titles <- function(article.object){
  
  txt <- read_xml(article.object)

  titl <- txt %>%
    html_nodes('article-title') %>% 
    xml_text() %>%
    .[[1]] 
  alt.titl <- txt %>%
    html_nodes('alt-title') %>% 
    xml_text() %>%
    .[[1]] 
  
  titls <- list(titl, alt.titl)
  return(titls)
  
}
```

### pt. 2: article classification

Mice are only used in certain labs for learned helplessness; rats are
the primary subject species. So here we extract counts of \[“mice” and
“mouse”\] vs \[“rat” and “rats”\] vs \[“human”, “people”,
“participant”, “participants”, “student”, “students”, “volunteer”,
“volunteers”\] to classify the article. It should be noted that not
every country uses the “subject” vs “participant” distinction, so this
may not always be completely reliable.

We first extract a single article for our example before the loop. We
can do this essentially the same way as extracting the article’s titles.

``` r
article <- txts[[1]] %>%
  read_xml() %>%
  html_nodes('p') %>% # extract in paragraphs
  xml_text()
```

Then classify the article as a participant (human) or subject
(mouse/rat)

``` r
library(stringr)

subject_or_part <- function(article.text){
  
  # count each
  rat <- str_count(article.text, pattern = 'rat|rats')
  mouse <- str_count(article.text, pattern = 'mice|mouse')
  ppl <- str_count(article.text, pattern = 'human|people|participant|participants|student|students|volunteer|volunteers')
  
  # move into list and keep highest count
  ret <- list(rat = sum(rat), 
              mouse = sum(mouse), 
              people = sum(ppl))

  ret <- ret[which.max(ret)]
  return(ret)
  
}

article.type <- names(subject_or_part(article))
```

This code counts the number of occurrences for these words, based on the
aforementioned criteria. We have determined from the results that the
species the first article uses is .

### pt. 3: methodology

One important part of designing an experiment for fear conditioning or
learned helplessness is shock intensity. Too strong of a stressor may
result in physical damage that could confound neural activity measures,
and too weak of a stressor will result in conditioned fear; not learned
helplessness.

For psychological paradigms involving shock, you see shock intensity
measured in milliamperes (mA). We need to gather <i>the numbers
around</i> the suffix “mA”. This is where regex is useful.

``` r
### Extract text
intensity.list <- str_extract_all(string = article, 
                pattern = '[0-9.]{1,5} mA|[0-9.]{1,5}mA')
```

We can break down the expression as such. `[0-9.]` numbers 0-9 or .
`[0-9.]{1,5}` any numbers 0-9 or ., that take up 1-5 spaces `[0-9.]{1,5}
mA` any numbers 0-9 or., that take up 1-5 spaces, that come behind a
space and “mA”

We also include the same expression after the `|` sign (signifies “or”)
without a space for articles that may contain the unit directly after
the number (e.g. 1.6mA)

Now we remove the empty items from our list

``` r
intensity.list <- Filter(length, intensity.list)
print(intensity.list)
```

    ## [[1]]
    ## [1] "0.25 mA" "0.05 mA" "0.25 mA" "0.60 mA"
    ## 
    ## [[2]]
    ## [1] "0.10 mA" "0.30 mA"
    ## 
    ## [[3]]
    ## [1] "0.15 mA"
    ## 
    ## [[4]]
    ## [1] "0.10 mA" "0.30 mA"
    ## 
    ## [[5]]
    ## [1] "0.10 mA" "0.10 mA" "0.30 mA"

and turn our code into a function.

``` r
find_mA <- function(article.text){
  intensity.list <- str_extract_all(string = article.text, 
                pattern = '[0-9.]{1,5} mA|[0-9.]{1,5}mA')
  intensity.list <- Filter(length, intensity.list)
  
  return(intensity.list)
}
```

As you can see, we get many values instead of a single one. There are a
few potential experimental explanations for this (e.g. multiple
experiments, testing ranges) and should be looked into via actual
reading. However, we have our shock intensity values extracted for this
study which is good enough for exploration purposes.

# Applying functions

Now we need to apply the functions we wrote to every article we’ve
gathered and put it into one place.

``` r
### Return all objects as full text objects
all.txts <- lapply(txts, function(x){ 
  x %>%
    read_xml() %>%
    html_nodes('p') %>% # extract in paragraphs
    xml_text()
})

### Apply functions in loop and add to list
big.list <- list()

for(i in 1:length(all.txts)){
  
  subject.type <- names(subject_or_part(all.txts[[i]]))
  mA.list <- find_mA(all.txts[[i]])
  titls <- get_article_titles(txts[[1]])
  
  big.list[[i]] <- list(
    title = titls[[1]],
    alt.title = titls[[2]],
    subject.type = subject.type,
    mA.list = mA.list
  )
}

print(big.list)
```

    ## [[1]]
    ## [[1]]$title
    ## [1] "Dissociation of Learned Helplessness and Fear Conditioning in Mice: A Mouse Model of Depression"
    ## 
    ## [[1]]$alt.title
    ## [1] "Trans-Situational Learned Helplessness in Mice"
    ## 
    ## [[1]]$subject.type
    ## [1] "mouse"
    ## 
    ## [[1]]$mA.list
    ## [[1]]$mA.list[[1]]
    ## [1] "0.25 mA" "0.05 mA" "0.25 mA" "0.60 mA"
    ## 
    ## [[1]]$mA.list[[2]]
    ## [1] "0.10 mA" "0.30 mA"
    ## 
    ## [[1]]$mA.list[[3]]
    ## [1] "0.15 mA"
    ## 
    ## [[1]]$mA.list[[4]]
    ## [1] "0.10 mA" "0.30 mA"
    ## 
    ## [[1]]$mA.list[[5]]
    ## [1] "0.10 mA" "0.10 mA" "0.30 mA"
    ## 
    ## 
    ## 
    ## [[2]]
    ## [[2]]$title
    ## [1] "Dissociation of Learned Helplessness and Fear Conditioning in Mice: A Mouse Model of Depression"
    ## 
    ## [[2]]$alt.title
    ## [1] "Trans-Situational Learned Helplessness in Mice"
    ## 
    ## [[2]]$subject.type
    ## [1] "rat"
    ## 
    ## [[2]]$mA.list
    ## [[2]]$mA.list[[1]]
    ## [1] "0.8 mA" "0.8 mA"
    ## 
    ## 
    ## 
    ## [[3]]
    ## [[3]]$title
    ## [1] "Dissociation of Learned Helplessness and Fear Conditioning in Mice: A Mouse Model of Depression"
    ## 
    ## [[3]]$alt.title
    ## [1] "Trans-Situational Learned Helplessness in Mice"
    ## 
    ## [[3]]$subject.type
    ## [1] "rat"
    ## 
    ## [[3]]$mA.list
    ## list()
    ## 
    ## 
    ## [[4]]
    ## [[4]]$title
    ## [1] "Dissociation of Learned Helplessness and Fear Conditioning in Mice: A Mouse Model of Depression"
    ## 
    ## [[4]]$alt.title
    ## [1] "Trans-Situational Learned Helplessness in Mice"
    ## 
    ## [[4]]$subject.type
    ## [1] "rat"
    ## 
    ## [[4]]$mA.list
    ## [[4]]$mA.list[[1]]
    ## [1] "0.8 mA" "0.8 mA"
    ## 
    ## 
    ## 
    ## [[5]]
    ## [[5]]$title
    ## [1] "Dissociation of Learned Helplessness and Fear Conditioning in Mice: A Mouse Model of Depression"
    ## 
    ## [[5]]$alt.title
    ## [1] "Trans-Situational Learned Helplessness in Mice"
    ## 
    ## [[5]]$subject.type
    ## [1] "people"
    ## 
    ## [[5]]$mA.list
    ## list()
    ## 
    ## 
    ## [[6]]
    ## [[6]]$title
    ## [1] "Dissociation of Learned Helplessness and Fear Conditioning in Mice: A Mouse Model of Depression"
    ## 
    ## [[6]]$alt.title
    ## [1] "Trans-Situational Learned Helplessness in Mice"
    ## 
    ## [[6]]$subject.type
    ## [1] "rat"
    ## 
    ## [[6]]$mA.list
    ## [[6]]$mA.list[[1]]
    ## [1] "0.8 mA"
    ## 
    ## [[6]]$mA.list[[2]]
    ## [1] "1.3 mA"
    ## 
    ## 
    ## 
    ## [[7]]
    ## [[7]]$title
    ## [1] "Dissociation of Learned Helplessness and Fear Conditioning in Mice: A Mouse Model of Depression"
    ## 
    ## [[7]]$alt.title
    ## [1] "Trans-Situational Learned Helplessness in Mice"
    ## 
    ## [[7]]$subject.type
    ## [1] "rat"
    ## 
    ## [[7]]$mA.list
    ## list()
    ## 
    ## 
    ## [[8]]
    ## [[8]]$title
    ## [1] "Dissociation of Learned Helplessness and Fear Conditioning in Mice: A Mouse Model of Depression"
    ## 
    ## [[8]]$alt.title
    ## [1] "Trans-Situational Learned Helplessness in Mice"
    ## 
    ## [[8]]$subject.type
    ## [1] "rat"
    ## 
    ## [[8]]$mA.list
    ## list()
    ## 
    ## 
    ## [[9]]
    ## [[9]]$title
    ## [1] "Dissociation of Learned Helplessness and Fear Conditioning in Mice: A Mouse Model of Depression"
    ## 
    ## [[9]]$alt.title
    ## [1] "Trans-Situational Learned Helplessness in Mice"
    ## 
    ## [[9]]$subject.type
    ## [1] "rat"
    ## 
    ## [[9]]$mA.list
    ## list()
    ## 
    ## 
    ## [[10]]
    ## [[10]]$title
    ## [1] "Dissociation of Learned Helplessness and Fear Conditioning in Mice: A Mouse Model of Depression"
    ## 
    ## [[10]]$alt.title
    ## [1] "Trans-Situational Learned Helplessness in Mice"
    ## 
    ## [[10]]$subject.type
    ## [1] "rat"
    ## 
    ## [[10]]$mA.list
    ## list()

This code takes the `txts` object and applies the function to each list
item. Then, it creates a list and adds the results of the functions we
created to it on each iteration; the number of which is the length of
the `all.txts` object.

# Next up

Part 2 shows you how to visualize the data\!
