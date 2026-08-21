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
When testing with a local docker container, please make sure to rebuild the docker image after changes in the R code or dependencies. You can do this from the root of the repository via:

```bash
docker build -t inquiryr-app:latest .
```

or for a full rebuild without cache:

```bash
docker build --no-cache -t inquiryr-app:latest .
```


After that, start the container as usual via:

```bash
docker run -p 3838:3838 inquiryr-app:latest
```

and access the app in your browser at `http://localhost:3838/`. Stop the container with `CTRL + C` in the terminal.

**Optional:**

Add `-it` for interactive mode, or `--rm` to remove the container after stopping.