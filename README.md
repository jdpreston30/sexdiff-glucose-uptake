# Sex Differences in Glucose Uptake Analysis

**Reproducible analysis pipeline for academic publication**

## 📖 Citation

This code is associated with the analysis presented in the following manuscript:
> Preston, J., & Adediji, B. (2025). Sex differences in glucose uptake. Submitted to *American Journal of Physiology - Endocrinology and Metabolism*. DOI: pending

**Authors**: Joshua Preston, Adediji et al.  
**Correspondence**: joshua.preston@emory.edu  
**ORCID**: [0000-0001-9834-3017](https://orcid.org/0000-0001-9834-3017)

## 🚀 Quick Start for Reproduction

### Option 1: Using Docker (Recommended for Full Reproducibility)

**Docker ensures identical R versions (4.5.1) and package installations across all systems.**

#### Requirements
- [Install Docker Desktop](https://www.docker.com/products/docker-desktop) (includes Docker and Docker Compose)
  - For Mac with Apple Silicon (M1/M2/M3): Download the Apple Silicon version
  - For Mac with Intel: Download the Intel version

#### Running the Analysis

```bash
# 1. Build the Docker image (first time only)
docker-compose build

# 2. Run the complete analysis
docker-compose run analysis Rscript All_Run/run.R
```

That's it! All results will be saved to the `Outputs/` directory.

### Option 2: Local Installation with renv

**If you prefer running locally, this project uses renv for package management.**

#### Requirements
- R 4.5.1 or later
- System dependencies (see Dockerfile for list)

#### Setup

```r
# 1. Open R in the project directory
# renv will automatically activate

# 2. Install all required packages
source("setup_renv.R")

# 3. Run the analysis
source("All_Run/run.R")
```

The first-time setup will take 10-20 minutes to install all packages. After that, the environment is locked and reproducible via `renv.lock`.

## 📁 Project Structure

```
├── DESCRIPTION                    # Package dependencies (R standard)
├── renv.lock                      # Package version lockfile (renv)
├── .Rprofile                      # R startup configuration (activates renv)
├── setup_renv.R                   # One-time setup script for local installation
├── Dockerfile                     # Docker container configuration
├── docker-compose.yml             # Docker Compose setup
├── All_Run/
│   ├── config_dynamic.yaml        # Dynamic analysis configuration
│   └── run.R                      # Main analysis execution script
├── R/
│   ├── Scripts/
│   │   ├── 00a_environment_setup.R    # Environment setup with renv
│   │   ├── 00b_setup.R                # Additional configuration
│   │   ├── 00c_import.R               # Data import
│   │   ├── 01_phenotypic_data.R       # Phenotypic analysis
│   │   ├── 02_physiologic_data.R      # Physiologic analysis
│   │   ├── 03_glucose_uptake.R        # Glucose uptake analysis
│   │   └── 99_render_figures.R        # Figure compilation
│   └── Utilities/
│       ├── Helpers/                   # Helper functions
│       ├── Analysis/                  # Statistical analysis utilities
│       ├── Preprocessing/             # Data preprocessing utilities
│       └── Visualization/             # Visualization functions
├── Data/                          # Raw data files
├── Outputs/                       # Generated results and figures
└── renv/                          # renv package library (auto-managed)
```

## 🔬 Analysis Workflow

1. **Run complete analysis**: `source("All_Run/run.R")`
2. **View results**: Check `Outputs/` directory
3. **Individual components**: Source specific scripts from `R/Scripts/`

## 💻 System Requirements

### Docker Option
- **Docker Desktop** (required)
- **Disk Space**: ~2GB for Docker image and outputs
- **Memory**: 4GB RAM minimum (8GB recommended)

### Local Installation Option
- **R**: 4.5.1 or later
- **Disk Space**: ~1GB for R packages and outputs
- **Memory**: 4GB RAM minimum (8GB recommended)
- **System Dependencies** (Linux/Mac):
  - libxml2-dev, libcurl4-openssl-dev, libssl-dev
  - libfontconfig1-dev, libharfbuzz-dev, libfribidi-dev
  - libfreetype6-dev, libpng-dev, libtiff5-dev, libjpeg-dev

### What's Included

**Reproducibility features:**
- **renv**: Lockfile-based package management for exact version control
- **Docker**: Complete environment isolation (R version + system dependencies)
- **DESCRIPTION**: Standard R package format for dependency declaration

All analysis dependencies are automatically managed via renv or Docker.

## 🤝 For Collaborators

### Quick Start
1. Clone this repository
2. Choose your method:
   - **Docker**: `docker-compose build && docker-compose run analysis Rscript All_Run/run.R`
   - **Local**: Open R in project folder, run `source("setup_renv.R")`, then `source("All_Run/run.R")`

### Development Workflow
- **Add packages**: Add to `DESCRIPTION` Imports field, then run `renv::snapshot()`
- **Update packages**: Run `renv::update()` then `renv::snapshot()`
- **Sync environment**: Run `renv::restore()` to match the lockfile

## 📧 Contact

For questions about the analysis:
- **First Author**: Joshua Preston (joshua.preston@emory.edu)
  - **ORCID**: [0000-0001-9834-3017](https://orcid.org/0000-0001-9834-3017)
  - **Institution**: Emory University
- **Senior Author**: Kevin Pearson (kevin.pearson@uky.edu)
  - **Institution**: University of Kentucky

## 📄 License

This code is available under the MIT License. See LICENSE file for details.
