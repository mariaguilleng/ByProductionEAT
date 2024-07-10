# By Production Efficiency Analysis Trees (EAT) and By Production Convexified Efficiency Analysis Trees (CEAT)

[![DOI:10.5281/zenodo.12683675](http://img.shields.io/badge/DOI-10.5281/zenodo.12683675-B31B1B.svg)](https://zenodo.org/doi/10.5281/zenodo.12683675)

This repository accompanies the paper by M. Guillen, J. Aparicio, M. Kapelko, M. Esteve "Measuring environmental inefficiency through machine learning: An approach based on Efficiency Analysis Trees and by-production technology ".
The repository was created as a result of the project funded by the National Science Centre in Poland (grant no. 2023/49/B/HS4/02991).

This repository contains the implementation of novel machine learning methods for measuring environmental inefficiency, leveraging regression trees under shape constraints. The developed techniques are based on a by-production framework that separates technologies for pollution generation and good output production. In particular, two new approaches are included in the respository: by-production Efficiency Analysis Trees (EAT) and by-production Convexified Efficiency Analysis Trees (CEAT). These methods address common issues such as overfitting found in traditional methods like Free Disposal Hull (FDH) and Data Envelopment Analysis (DEA). Those traditional methods are also included in the respositoty.

The repository includes the following files: 
* `EAT_ByProdcut_model.R`: The programing codes for mathematical models for by-production EAT, by-production CEAT, by-production FDH and by-production DEA.
* `Mathematical models.docx`: The mathematical models for by-production EAT, by-production CEAT, by-production FDH and by-production DEA.
* `WIOD_JCR_dataset.xls`: A real dataset including comprehensive data from 43 countries over the period from 2000 to 2014, derived from the World Input-Output Database (WIOD) and the Joint Research Centre (JCR) of the European Commission. The dataset assumes a production process that includes two non-polluting inputs: labor (measured by the number of employees in thousands) and capital (represented by gross fixed capital formation at constant prices from the year 2000 in millions of Purchasing Power Parity (PPP)). It also includes one polluting input, which is emission-relevant energy use measured in terajoules (TJ). For outputs, the dataset considers one non-polluting output, which is the gross value added at constant prices from the year 2000 in millions of PPP, and one polluting output, which is CO2 emissions measured in kilotons (kt).
* `example.R`: An example of the use of the models.
