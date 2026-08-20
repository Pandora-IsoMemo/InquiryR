FROM inwt/r-shiny:4.4.3

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        jags \
        qpdf \
        pandoc \
        libmagick++-dev \
        libsodium-dev \
    && rm -rf /var/lib/apt/lists/*

RUN echo "options(repos = c(getOption('repos'), PANDORA = 'https://Pandora-IsoMemo.github.io/drat/'))" >> /usr/local/lib/R/etc/Rprofile.site

RUN Rscript -e "remotes::install_github('r-lib/httr2@v1.2.3')" \
    && Rscript -e "remotes::install_github('tidyverse/ellmer@v0.4.1')"

WORKDIR /app
COPY . .

RUN installPackage

EXPOSE 3838

CMD ["Rscript", "-e", "library(InquiryR);startApplication(3838)"]
