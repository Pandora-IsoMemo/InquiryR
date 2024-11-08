FROM inwt/r-shiny:4.4.1

ADD . .

RUN apt-get update \
&& apt-get install -y --no-install-recommends \
jags \
qpdf \
pandoc \
libmagick++-dev \
&& echo "options(repos = c(getOption('repos'), PANDORA = 'https://Pandora-IsoMemo.github.io/drat/'))" >> /usr/local/lib/R/etc/Rprofile.site \
&& installPackage

CMD ["Rscript", "-e", "library(InquiryR);startApplication(3838)"]
