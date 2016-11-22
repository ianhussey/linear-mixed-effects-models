---
Title: Mixed effects models
Author: Ian Hussey (ian.hussey@ugent.be)
---

# Mixed effects models

## Overview and purpose

Mixed effects models have utility within factorial experimental designs due to their high power. This extracts sample data from a real experiment which employed a 2 (within: IAT block) X 2 (between: training condition, high vs low prior practice) design, and uses it to demonstrate the implementation of mixed methods models. 

Each of the models follows the general format of `rt ~ block * condition + (1 | participant)`. That is, where we explore the interaction between block and condition (fixed effects) in predicting reaction times, while holding participant as a random effect. This acknowledges the non-independence of the multiple rts produced by each participant by allowing the intercept to vary between participants. 

Three models are included:

1. frequentist linear mixed effects model
2. Bayes factors linear mixed effects model
3. Probabalistic index (semi-parametric) mixed effects model (see Thas et al., 2012)
   1. For the moment this is implemented as a fixed effects model with no random effect for participant, as the PIM model doesn't allow for random effects yet. An alternative implementation using cox proportional hazard models is possible, and might be added in future.
   2. Please note that PI models are computationally complex and scale exponentially, and require a lot of memory resources (several gigabytes, so if you run out of ram the calculation will slow down a lot as you read and write to virtual memory). Execution time for this model can range from minutes to hours depending on sample size and your available processing power and memory. The saved RData object will also be several hundred megabytes in size.

## Licenses

### Code

GPLv3+ https://www.gnu.org/licenses/quick-guide-gplv3.en.html

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

### Data

CC-BY-4.0 https://creativecommons.org/licenses/by/4.0/

Please cite:  Hussey, I., & Hughes, S. (2016). Transitive relations and implicit attitudes. https://osf.io/5nxby/