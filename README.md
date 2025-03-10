<p align="center">
  🛰️ <strong>PaceEarthcareFusion (PEF)</strong> 🛰️
  <img src="assets/PEF_logo3.png" alt="PEF Logo" width="200" height="200">
</p>

PEF reads, collocates, and plots EarthCARE and PACE data.

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

