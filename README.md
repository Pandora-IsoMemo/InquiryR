# InquiryR

An app for creating Inquiry templates, conducting a survey, and downloading the results based on the
[shinysurveys package](https://github.com/jdtrat/shinysurveys).

Please check the [vignette](https://shinysurveys.jdtrat.com/articles/surveying-shinysurveys.html) for more information.

## Release notes:

- see `NEWS.md`

## Notes for developers

When adding information to the _help_ sites, _docstrings_ or the _vignette_ of this 
package, please update documentation locally as follows. The documentation of
the main branch is build automatically via github action.

```R
devtools::document() # or CTRL + SHIFT + D in RStudio
devtools::build_site()
```
