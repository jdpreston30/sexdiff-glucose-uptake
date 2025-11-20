FROM rocker/r-ver:4.5.1

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libxml2-dev \
    libcurl4-openssl-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /project

# Copy DESCRIPTION for package installation
COPY DESCRIPTION .

# Install CRAN packages from Imports section
RUN R -e "desc <- read.dcf('DESCRIPTION'); packages <- gsub('\\s+', '', strsplit(desc[,'Imports'], ',')[[1]]); install.packages(packages)"

# Install Bioconductor packages
RUN R -e "if (!requireNamespace('BiocManager', quietly = TRUE)) install.packages('BiocManager'); desc <- read.dcf('DESCRIPTION'); bioc_pkgs <- gsub('\\s+', '', strsplit(desc[,'Bioconductor'], ',')[[1]]); if (length(bioc_pkgs) > 0 && bioc_pkgs[1] != '') BiocManager::install(bioc_pkgs)"

# Copy all project files
COPY . .

CMD ["/bin/bash"]
