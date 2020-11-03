## Further Information

This Shiny app is a complement to our research article **Network Interventions for Managing the COVID-19 Pandemic and Sustaining Economy**, in print in the Proceedings of the National Academy of Sciences of the United States of America (PNAS). The paper can be found [here](link tbd) The app demonstrates the efficacy of the "dividing groups" strategy and the "balancing groups" strategy and a combination of the two strategies using a downscaled version of the simulations we used to generate the results in our article. 

This section of the app will define and further describe terminology used in the microsimulation and in the original manuscript.

## Terminology

**Degree**: The number of connections each node has in the network.

**Network N ($N$)**: The network N is defined as:

$$ N = m + s^2/m$$

and refers to the number of network ties per infected individual. This value differs from the population size as not all individauls in a social network are connected and that individuals with a higher degree have a higher risk of infecting others in the network.

**R<sub>eff</sub>**: The effective reprdouction number. This is the average number of secondary transmissions caused by a single primary case.

## Detailed descriptions of interventions

**No intervention**: No intervention is applied to the network.

**Intermediate lockdown**: In the intermediate lockdown strategy, 3 out of 6 total activity sectors were blocked (educational institutions, restaurants/cafes, sports and leisure). Family ties and ties occuring in medical institutions were not restricted.

**Strict lockdown**: In the strict lockdwon strategy, all 6 activity sectors were blocked (the 3 in the intermediate lockdown strategy, as well as workplaces, grocery stores, and all other groups).

**Dividing groups**: In the dividing groups strategy, social groups are split into two distinct subgroups such that individuals in different subgroups no longer interact.

**Balancing groups**: In the balancing groups strategy, some individuals are rerouted to different locations to equalize the number of people at each location. For example, rerouting individuals at a grocery store which has hit its maximum capacity to another store nearby.

## Data Availability

The original simulation data and code can be found on Akihiro Nishi's [Github page](https://github.com/akihironishi/covid19_pnas) as well as at the UCLA Fielding School of Public Health's [Public Data Portal](https://publicdata.ph.ucla.edu/pages/).

Code for this Shiny app and the contained network visualizations is available on request by emailing the developer, George Dewey [here.](gdewey@ucla.edu)