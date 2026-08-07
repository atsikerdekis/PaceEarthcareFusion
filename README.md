<p align="center"> <img src="assets/PEF_logo3.png" alt="PEF Logo" width="150" height="150"> </p>
<h1 align="center"> 🛰️  PACE EarthCARE Fusion (PEF) 🛰️ </h1>

This software was developed by Athanasios Tsikerdekis ([KNMI](https://www.knmi.nl/home)). It collocates and compares aerosol and cloud data from PACE (SPEXone) and EarthCARE (ATLID). 
The development was funded by ESA through the [AIRSENSE](https://www.grasp-earth.com/portfolio/airsense/) project. 
PACE data were provided by [SRON](https://www.sron.nl/) and EarthCARE data were provided by [KNMI](https://www.knmi.nl). 
If you use this software in a research publication, please cite it appropriately.

## 📜 Attribution

- Code attribution: [PACE EarthCARE Fusion (PEF)](https://github.com/atsikerdekis/PaceEarthcareFusion)
- Data attribution for EarthCARE (ATLID): [van Zadelhoff et al. (2023)](https://doi.org/10.5194/amt-16-3631-2023) and [Donovan et al. (2024)](https://doi.org/10.5194/amt-17-5301-2024)
- Data attribution for PACE (SPEXone): [Fu et al. (2025)](https://doi.org/10.1029/2024GL113525)

## 📥 Download Test Data

Test data can be downloaded from:  
🔗 https://surfdrive.surf.nl/files/index.php/s/VysQvaLkdjzwFop

## 🛠 Environment

1. Install Miniforge3:  
   🔗 https://github.com/conda-forge/miniforge

2. Create the environment using mamba:  
   `mamba env create -f environment/PEF_environment.yml`

3. Activate the environment:  
   `mamba activate PEF`

## ⚙️ Configuration

Modify the following paths before running the code:  
- `00.start.R` → See variables under section `INPUT` and variable `path_code`
- `01.init.R`  → See all `path_*` variables 

## ▶️ Running PEF

Run the script:  
`Rscript 00.start.R` 

## 🌋 ATLID-only Profiler

Need EarthCARE/ATLID profiles for a specific longitude, latitude and date
(e.g. to check a volcanic plume) without any PACE/SPEXone comparison? See the
standalone [`ATLID_Profiler/`](ATLID_Profiler/README.md) tool:

```
cd ATLID_Profiler
Rscript start.R 167.84 -15.39 20260307 20260317
```

