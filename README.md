# XMM-Newton SIXTE Simulations

A pipeline to generate simulated XMM-Newton observations of the Chandra Deep Field South (CDFS) and run source detection.

The goal is to test the performance of the source detection software in repeated (overlapping) XMM-Newton observations.

## Details

The pipeline extracts the parameters of the simulations from real XMM-Newton EPIC observations of the CDFS: e.g. duration, pointing coordinates via the attitude file, data mode, filter, etc.

The simulated observations are performed with the [SIXTE](https://www.sternwarte.uni-erlangen.de/sixte) software distributed by Bamberg.

As of November 2022 the XMM-Newton pn/MOS [instrument files](https://www.sternwarte.uni-erlangen.de/~sixte/downloads/sixte/instruments/instruments_xmm-1.2.1.tar.gz) included in SIXTE take into account the PSF, vignetting, effective area, instrumental background etc.

Dedicated XML files with the detector geometry and configuration of each individual pn/MOS1/MOS2 chip were created for the simulations using the [epic_xmm_files](https://github.com/ruizca/sixtexmm.git) Jupyter Notebook developed by Angel Ruiz.

We are making use of modified versions of [sample SIMPUT](https://www.sternwarte.uni-erlangen.de/~sixte/simput/CDFS_combined_simput.tgz) files distributed by the SIXTE team. They include both point and extended sources and diffuse emission of the CDFS.

The detectors are simulated chip-wise. Several SAS tasks (`evlistcomb`, `edet2sky`, `attcalc`) are then called to merge events and correct the detector and sky coordinates for SAS compliance.

The individual tasks of the source detection chain `eboxdetect, emldetect, eexpmap` etc, or `edetect_stack` directly can then be called upon the simulations.

## Software requirement
- R 3.4.4 (installed packages `FITSio`, `stringr`)
- HEASOFT 6.26.1 or later
- SIXTE 2.7.2
- XMM-Newton SAS (`xmmsas_20211130_0941`)

## Auxiliary data
- [simput files of the CDFS](https://www.sternwarte.uni-erlangen.de/~sixte/simput/CDFS_combined_simput.tgz)
- pn/MOS1/MOS2 event lists and attitude files of the observations to be simulated

## Acknowledgements

Based on previous work by [Angel Ruiz](https://github.com/ruizca/sixtexmm), Iris Traulsen, [Sam Sweere](https://github.com/SamSweere/xmm-epicpn-simulator), plus support from Bamberg (Christian Kirsch, Joern Wilms) through the SIXTE helpdesk.

