# ITBDA4-14 Project 2 = Big Data Analytics with R

R scripts for four big data tasks: MapReduce keyword search, LDA topic modelling,
web text mining with summarisation, and an ETL pipeline into PostgreSQL.

## Folder contents

| File / folder | Description |
|---|---|
| `Question1.2_Mapper.R` | Q1.2 – Mapper: emits the document name with the (row, column) of each keyword match |
| `Question1.3_Reducer.R` | Q1.3 – Reducer: groups the matches per document and counts them |
| `Question1.4_MapReduce.R` | Q1.4 – Hadoop configuration settings and MapReduce job driver |
| `Question2_LDA.R` | Q2 – LDA topic modelling of news articles (2.1, 2.2, 2.3) |
| `Question3.R` | Q3 – Web text mining and summarisation of www.dsti.gov.za (3.1, 3.2, 3.3) |
| `Question4_ETL.R` | Q4 – ETL: web scraping, cleaning and PostgreSQL analysis (4.1, 4.2, 4.3) |
| `map_reduce_test_docs/` | Documents searched in Question 1 |
| `sa_news/` | News articles used in Question 2 |

## Requirements

- R and RStudio
- Internet connection (Questions 3 and 4)
- PostgreSQL with pgAdmin (Question 4)
- Hadoop (only for Question 1.4 in `hadoop` mode)

Any missing R packages are installed automatically the first time a script runs.

## How to run

1. Open RStudio and set the working directory to this folder
   (**Session → Set Working Directory → Choose Directory**).
2. Run each script in the Console with `source()`:

```r
source("Question1.2_Mapper.R")      # type the keyword, e.g. africa
source("Question1.3_Reducer.R")
source("Question1.4_MapReduce.R")   # type the keyword, then: local
source("Question2_LDA.R")
source("Question3.R")
source("Question4_ETL.R")           # type your PostgreSQL password when asked
```

Before Question 4, create the database in pgAdmin:

```sql
CREATE DATABASE itdba_etl;
```

## Output

- Results print in the RStudio Console under a heading for each question part.
- Charts appear in the **Plots** pane.
- All results, tables and charts are also saved in the `outputs/` folder,
  which is created automatically.

## Notes

- The search keyword in Question 1 is entered at run time; it is not hard-coded.
- Questions 3 and 4 respect each website's robots.txt rules and save the downloaded pages,
  so later runs do not contact the websites again.
