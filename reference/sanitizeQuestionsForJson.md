# Sanitize questions for JSON

Replace NA in list\$questions with list(NULL) to avoid empty columns.
This prevents toJSON from dropping empty columns.

## Usage

``` r
sanitizeQuestionsForJson(list)
```

## Arguments

- list:

  A list with a data frame named questions

## Value

A list with NA replaced by list(NULL) in list\$questions
