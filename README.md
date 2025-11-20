# Sex Differences in Glucose Uptake Analysis

**Reproducible analysis pipeline for academic publication**

## 📖 Citation

This code is associated with the analysis presented in the following manuscript:
> Preston, J., & Adediji, B. (2025). Sex differences in glucose uptake. Submitted to *American Journal of Physiology - Endocrinology and Metabolism*. DOI: pending

**Authors**: Joshua Preston, Adediji et al.  
**Correspondence**: joshua.preston@emory.edu  
**ORCID**: [0000-0001-9834-3017](https://orcid.org/0000-0001-9834-3017)

## 🚀 Quick Start for Reproduction

**This analysis requires Docker for reproducibility.** Docker ensures identical R versions (4.5.1) and package installations across all systems, preventing "works on my machine" issues.

### Requirements

- [Install Docker Desktop](https://www.docker.com/products/docker-desktop) (includes Docker and Docker Compose)
  - For Mac with Apple Silicon (M1/M2/M3): Download the Apple Silicon version
  - For Mac with Intel: Download the Intel version

### Running the Analysis

```bash
# 1. Build the Docker image (first time only)
docker-compose build

# 2. Run the complete analysis
docker-compose run analysis Rscript All_Run/run.R
```

That's it! All results will be saved to the `Outputs/` directory.

## 📁 Project Structure

```
├── DESCRIPTION                    # Package dependencies (R standard)
├── Dockerfile                     # Docker container configuration
├── docker-compose.yml             # Docker Compose setup
├── config_dynamic.yaml            # Dynamic analysis configuration
├── run.R                          # Main analysis execution script
├── R/
│   ├── Scripts/
│   │   ├── 00a_environment_setup.R    # Environment setup
│   │   ├── 00b_setup.R                # Additional configuration
│   │   ├── 00c_clinical_metadata.R    # Load clinical metadata
│   │   └── [Analysis scripts...]
│   └── Utilities/
│       ├── Helpers/                   # Helper functions
│       ├── Preprocessing/             # Data preprocessing utilities
│       └── Visualization/             # Visualization functions
├── Data/                          # Raw data files
└── Outputs/                       # Generated results and figures
```

## 🔬 Analysis Workflow

1. **Run complete analysis**: `source("All_Run/run.R")`
2. **View results**: Check `Outputs/` directory
3. **Individual components**: Source specific scripts from `R/Scripts/`

## 💻 System Requirements

- **Docker Desktop** (required)
- **Disk Space**: ~2GB for Docker image and outputs
- **Memory**: 4GB RAM minimum (8GB recommended)

### What's Included in the Docker Container

The Docker image includes:
- R 4.5.1
- All required packages (see `DESCRIPTION`)
- System dependencies (libxml2-dev, libcurl4-openssl-dev)
- **Containerization**: Dockerfile provided for complete environment isolation

## 🤝 For Collaborators

1. Clone this repository
2. Install R packages: `install.packages(readLines("DESCRIPTION")[grep("Imports:", readLines("DESCRIPTION")):length(readLines("DESCRIPTION"))])`
3. Run analysis: `source("run.R")`

## 📧 Contact

For questions about the analysis:
- **Author**: Joshua Preston (joshua.preston@emory.edu)
- **ORCID**: [0000-0001-9834-3017](https://orcid.org/0000-0001-9834-3017)
- **Institution**: Emory University

## 📄 License

This code is available under the MIT License. See LICENSE file for details.
