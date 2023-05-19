# stew
stew: Spatio-Temporal Analysis of Ecosystem State Changes

## Overview
Ecosystems worldwide are facing unprecedented pressures, with rapid changes in disturbance regimes that can have detrimental effects. However, characterizing the state of an ecosystem and predicting its response to disturbances has proven to be a notoriously challenging task. This study aims to address this issue by leveraging remotely sensed spatio-temporal data to identify early warning signals of forest mortality using satellite images.

## Methodology
To achieve this goal, a novel approach is proposed, which utilizes local spatial autocorrelation measurements at each time point. Specifically, the approach employs the local Moran's I and local Geary's c methods to quantify spatial autocorrelation. These methods have been rigorously tested across multiple study sites and have consistently produced robust results. By analyzing time series of local spatial autocorrelation values, the approach successfully generates early warning signals, predicting forest mortality occurrence in unhealthy study sites up to two years in advance.

## Key Features of the "stew" R Package
To facilitate spatio-temporal analysis of ecosystem state changes, I have developed the "stew" R package. This package empowers users to explore and analyze spatio-temporal data effectively. Key features of the "stew" package include:

1. ** Spatio-Temporal Data Exploration: The package provides functions to visualize and explore spatio-temporal datasets, enabling users to gain insights into ecosystem dynamics and disturbances over time.

2. **Local Spatial Autocorrelation Analysis: Users can leverage the "stew" package to calculate local Moran's I and local Geary's c measurements, quantifying the spatial autocorrelation of ecosystem variables at different time points.

3. **Early Warning Signal Detection: The package includes advanced algorithms to detect early warning signals of forest mortality based on time series analysis of local spatial autocorrelation values. These signals can help identify potential climate-induced forest mortality events up to two years in advance.

4. **Robust and Reproducible Results: The "stew" package utilizes state-of-the-art methodologies that have been extensively tested across various study sites. It ensures the generation of reliable and reproducible results for spatio-temporal analysis.

## Conclusion
This work underscores the potential of spatio-temporal indicators in diagnosing early warning signals and predicting upcoming climate-induced forest mortality events. By utilizing the "stew" R package, researchers and practitioners can delve into comprehensive spatio-temporal analyses of ecosystem state changes, aiding in the proactive management and conservation of vulnerable ecosystems.




![image](https://github.com/alibaks/stew/assets/62399942/a144ed69-4dfd-423a-92f4-f531a7ee622f)

Conceptual diagram representing different approaches used in this study to measure early warning signals of forest mortality; A) analysis of time series represents the dynamics of the whole ecosystem state variable, where the temporal signature is averaged over the whole ecosystem and after that leading indicator of critical transitions has been measured; B) following the approach provided in (A) but applied to the time series obtained from each pixel of a spatio-temporal time series that generates a spatial pattern of early warning signals; C) a new approach proposed in this study that measures local spatial autocorrelation (using local Moran’s I and local Geary’s c) at each time and generates early warning signals from time series of local spatial autocorrelation values at each pixel.


For more information: Spatio-temporal analysis of remote sensing images provides early warning signals of forest mortality
S Alibakhshi -
https://www.biorxiv.org/content/biorxiv/early/2021/09/20/2021.09.18.460897.full.pdf
