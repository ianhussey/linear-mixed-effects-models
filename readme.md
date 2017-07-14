---
Title: Mixed effects models
Author: Ian Hussey (ian.hussey@ugent.be)
---

# Mixed effects models

## Overview and purpose

Mixed effects models have utility within factorial experimental designs due to their high power. This extracts sample data from a real experiment which employed a 2 (within: IAT block) X 2 (between: training condition, high vs low prior practice) design, and uses it to demonstrate the implementation of mixed methods models. 

Each of the models follows the general format of `rt ~ block * condition + (1 | participant)`. That is, where we explore the interaction between block and condition (fixed effects) in predicting reaction times, while holding participant as a random effect. This acknowledges the non-independence of the multiple rts produced by each participant by allowing the intercept to vary between participants. 

## Licenses

### Code

GPLv3+ https://www.gnu.org/licenses/quick-guide-gplv3.en.html

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

### Data

CC-BY-4.0 https://creativecommons.org/licenses/by/4.0/

Please cite:  Hussey, I., & Hughes, S. (2016). Transitive relations and implicit attitudes. https://osf.io/5nxby/