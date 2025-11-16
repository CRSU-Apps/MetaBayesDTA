FROM rocker/shiny-verse:latest
LABEL org.opencontainers.image.source="https://github.com/CRSU-Apps/MetaBayesDTA"

RUN apt update && \
    DEBIAN_FRONTEND=noninteractive apt install -y -qq \
        build-essential \
        libfribidi-dev \
        libfreetype6-dev \
        libpng-dev \
        libtiff5-dev \
        libjpeg-dev
# Remove examples
RUN rm -rf /srv/shiny-server/*
# Copy the source code
COPY . /srv/shiny-server/metabayesdta
WORKDIR /srv/shiny-server/metabayesdta/
# Install renv
RUN R -e "install.packages('renv', repos = c(CRAN = 'https://cloud.r-project.org'))"
# Initialise renv
RUN R -s -e "renv::init(bare = TRUE)"
# Run renv restore
RUN R -s -e "renv::restore()"
# Isolate renv
RUN R -s -e "renv::isolate()"
# Build the stan models
RUN Rscript /srv/shiny-server/metabayesdta/compile_models.R