# Dockerfile for Reproducible R Environment
# For maximum reproducibility across different systems
# R version 4.5.1 (2025-06-13)

FROM rocker/r-ver:4.5.1

# Set environment variables
ENV DEBIAN_FRONTEND=noninteractive

# Install system dependencies
# Based on requirements from R/Utilities/Helpers/check_system_dependencies.R
RUN apt-get update && apt-get install -y \
    # Core build tools
    build-essential \
    gfortran \
    # Required system libraries (from check_system_dependencies.R)
    libudunits2-dev \
    # XML and networking
    libxml2-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    # Graphics and fonts
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libcairo2-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    && rm -rf /var/lib/apt/lists/*

# Set up renv for exact package restoration
RUN Rscript -e "install.packages('renv', repos='https://cloud.r-project.org')"

# Copy project files
WORKDIR /analysis
COPY renv.lock renv.lock
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json

# Copy remaining project files before restore (needed for renv to scan dependencies)
COPY DESCRIPTION .
COPY R/ R/
COPY All_Run/ All_Run/
COPY Outputs/ Outputs/

# Restore R packages from renv.lock (this captures exact versions)
RUN Rscript -e "renv::restore()"

# Verify renv status
RUN Rscript -e "renv::status()"

# Default command runs the full pipeline
CMD ["Rscript", "All_Run/run.R"]

