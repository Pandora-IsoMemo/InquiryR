# InquiryR 24.11.1

## New Features
- option to import and modify inquiry templates (#1)
  - set an inquiry _name_ and a _description_ 
  - add _standard_ questions as supported by `shinysurveys` package, see [vignette](https://shinysurveys.jdtrat.com/articles/surveying-shinysurveys.html)
  - remove questions from the template
  - edit cells of the questions data frame
- option to add a password before saving a template (#2)
  - the password is required to load the template: either to modify the template or to respond to the inquiry
  - when a password is set, the template is saved as an encrypted object; the download is a `raw` file instead of a `json` file
  - after import, the file is checked if it is encrypted and if so, the user must enter the password to load it

# InquiryR 24.11.0

## New Features
- First version of the shiny app containing an example inquiry template.
