
# Plans for further development

### Further partisan metrics

 - **Proportionality** -- this metric is used by some other sources; it aims to measure how well projected Dem/Rep seat shares mirror their statewide vote shares. I don't think this is difficult to implement whatsoever -- in the interim `EF GAP` and `DEM SEATS` targets are already strong proxies.
 - **Declination** -- similar to `EF GAP`, this captures how much less efficiently (aka, more packed) one party's voters are compared with the other's. Again, this is not very difficult to implement, but it's so similar to `EF GAP` that I question its benefit.
 - **Use of multiple elections** -- this is a key add once I have made Mosaic more modular; it should be feasible to optimize a map to be fair/stable across different electoral environments, especially in states undergoing significant shifts. This is high priority. One workaround is to supply the shapefile with `DEM` and `REP` values that are averages of different elections, but I acknowledge this is *not* the same.

### New geographic metrics
In the status quo, Mosaic is basically blind -- it does not store or handle any geographic information, except for `cut edges`. While there are a number of advantages to `cut edges` -- it's geographically neutral, computationally lightweight and strongly associated with other measures of compactness -- there are still some obvious reasons to implement formal geographic measurements, like:

- **Polsby-Popper score** -- a measure of perimeter vs. area, penalizing districts with boundaries that wind a lot longer than their area would suggest. This is the lowest-hanging fruit since it can be implemented solely by including boundary lengths in the adjacency graph.
- **Reock score** - this is a smarter measure than Polsby-Popper (comparing a district's area to its minimum bounding circle's area) but has higher computational overhead; I am unlikely to implement this anytime soon. This is also true for **convex hull** based metrics.

### Demographic metrics

In the status quo, Mosaic does not use or track racial/ethnic data. This was a decision made for simplicity.

It is very difficult to rate/identify racial gerrymandering, all other objectives held neutral, but I do plan on adding a modular minority composition score (`PROJ HISP. SEATS` e.g.) using a logistics curve similar to what's currently implemented for `PROJ DEM. SEATS`. After that has been done, it's not too much of a lift to generate a proportionality rating.

### Other technical plans

- **Recombination of n > 2 districts** - Mosaic currently performs recombination on only 2 districts at a time. This constrains the solution space over time (for instance, two districts that form a long rectangle or dumbbell-sized combination will probably end up being split along their short axis back into shapes that resembling their original form, without much of a change). The solution to escape this problem will be to occasionally recombine **groups of 3/4 districts**. However, this is surprisingly difficult to do -- on many-district maps (like a 150n Texas State House) it takes considerable time to just identify neighboring triads compared with sampling from `cut_edges`. After finding a valid group, it also takes exponentially more time to re-cut within `pdev_tolerance`. 
- **Smarter sampling** - choosing `cut_edges` for recombination based on non-random factors. Right now, we have `county_bias` that decreases the likelihood that county edges will be used in the minimum spanning tree (and hence cut). There are a number of other variables that could be introduced to this equation, such as edge length.
-  **Additional municipality/COI protection** - currently, only **counties** have any kind of awareness built into Mosaic (through `weight_county_splits` and `county_bias`). Making Mosaic more modular will allow users to create more -- and overlapping -- regional distinctions, like city or municipality IDs that apply on top of counties. The pro-bunking list system can handle some of these needs on a small scale, but building it into the sampling system is much more powerful.
- **Variable weights and conditions over time** - in the status quo, the weights you set in Mosaic are identical for the first initialization round and the last iteration you do (even 25,000 steps later). Making these more variable will enable specific phases of recombination (like a party-focused stage followed by a county-preservation stage) that mirror how human mapmakers approach redistricting. One key use case is for Mosaic to be able to explore the map with a higher `pdev_tolerance` than it ultimately anneals down to.
