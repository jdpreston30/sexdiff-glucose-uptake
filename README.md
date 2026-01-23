# Sex Differences in Glucose Uptake Analysis

## 📖 Citation

This code is associated with the analysis presented in the following manuscript:
> Preston, J., & Adediji, B. (2025). Sex differences in glucose uptake. Submitted to *American Journal of Physiology - Endocrinology and Metabolism*. DOI: pending

**Authors**: Joshua Preston, Adediji et al.  
**Correspondence**: joshua.preston@emory.edu  
**ORCID**: [0000-0001-9834-3017](https://orcid.org/0000-0001-9834-3017)

## 🚀 Quick Start for Reproduction

### Option 1: Using Docker (Recommended for Exact Reproducibility)

**Status**: Placeholder - Docker image will be built upon manuscript acceptance

```bash
# Instructions will be added here
# Expected workflow:
# docker pull [username]/sexdiff-glucose-uptake:latest
# docker run -v $(pwd):/analysis [username]/sexdiff-glucose-uptake:latest
```

### Option 2: Manual Installation (Without Docker)

**Prerequisites**: 
- R >= 4.5.1
- Git (to clone repository)
- System dependencies:
  - **macOS**: `brew install udunits libxml2`
  - **Ubuntu/Debian**: `sudo apt-get install libudunits2-dev libxml2-dev`
  - **RHEL/CentOS/Fedora**: `sudo yum install udunits2-devel libxml2-devel`

**Note**: This project uses `renv` for package management to ensure reproducibility. The `renv.lock` file contains exact versions of all packages used in the manuscript.

```r
# 1. Clone the repository
# (from terminal)
git clone https://github.com/jdpreston30/sexdiff-glucose-uptake.git
cd sexdiff-glucose-uptake

# 2. Start R in the project directory
# (renv automatically activates via .Rprofile)

# 3. Restore all packages at exact versions (first time only, ~10-20 minutes)
renv::restore()

# 4. (Optional) Check system dependencies
source("R/Utilities/Helpers/check_system_dependencies.R")
check_system_dependencies()

# 5. Run the complete analysis pipeline
source("All_Run/run.R")
```

**What happens during `renv::restore()`**:
- Installs all R packages at exact versions from `renv.lock`
- Installs CRAN packages (e.g., ggplot2, dplyr, broom, tidyr, ggpattern)
- Creates isolated project library (doesn't affect your system R packages)
- Only needed once per computer; subsequent runs use installed packages
- Packages are automatically loaded from `DESCRIPTION` file during pipeline execution

## 📁 Project Structure

```
├── DESCRIPTION                    # Package dependencies
├── renv.lock                      # Package version lockfile (renv)
├── .Rprofile                      # R startup configuration (activates renv)
├── Dockerfile                     # Docker container configuration (in progress)
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
│   │   └── 04_render_figures.R        # Figure compilation
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

The complete pipeline executes in sequence:

1. **00a-00c**: Environment setup, configuration, data import
2. **01**: Phenotypic data analysis
3. **02**: Physiologic measurements
4. **03**: Glucose uptake analysis (in vivo and ex vivo)
5. **04**: Render publication figures

## 💻 System Requirements

### Computational Requirements
- **R**: Version 4.5.1 or higher
- **Platform**: Developed on macOS (Apple Silicon) but cross-platform compatible
- **Memory**: Minimum 8 GB RAM recommended
- **Storage**: ~500 MB for analysis outputs

### System Dependencies
- **udunits**: Unit conversion library (required for units and sf R packages)
- **libxml2**: XML parsing library (required for xml2 R package)

*Note: System dependencies will be automatically installed in the Docker container. For manual installation, see installation commands in Quick Start section.*

## 📦 Package Dependencies

All R package dependencies are specified in `DESCRIPTION` and locked at exact versions in `renv.lock`. Key packages include:

- **Data manipulation**: dplyr, tidyr, purrr, readr, readxl
- **Statistics**: emmeans, multcomp, lme4, car, ez, ARTool
- **Visualization**: ggplot2, ggpattern, cowplot
- **Reporting**: knitr, rmarkdown

For complete list with versions, see `renv.lock`.

## 📄 License

This code is licensed under [MIT License](LICENSE). Data availability is subject to IRB and institutional policies.

## 📧 Contact

For questions about the analysis or code:
- Joshua D. Preston: joshua.preston@emory.edu
- ORCID: [0000-0001-9834-3017](https://orcid.org/0000-0001-9834-3017)
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
