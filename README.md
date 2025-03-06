# 📌 PaceEarthcareFusion (PEF)

PEF reads, collocates, and plots EarthCARE and PACE data.

---
## 📥 Download Test Data
---

Test data can be downloaded from:  
🔗 [Your Link Here]

---
## 🛠 Installation
---

1. Install Miniforge3:  
   https://github.com/conda-forge/miniforge

2. Create the environment using mamba:  
   `mamba env create -f environment/PEF_environment.yml`

3. Activate the environment:  
   `mamba activate PEF`

---
## ⚙️ Configuration
---

Modify the following paths before running the code:  
- `00.start.R` → See variables under section `INPUT` and variable `path_code`
- `01.init.R`  → See all `path_*` variables 

---
## ▶️ Running PEF
---

Run the script with:  
$ Rscript 00.start.R YYYYMMDD  

Example:  
$ Rscript 00.start.R 20241201  

---
🚀 Happy coding!
---
