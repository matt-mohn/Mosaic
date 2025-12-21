
# Scoring & Optimization Guide

This guide covers everything you need to know about `run_chain()` parameters, scoring metrics, and optimization strategies. It should be a full explanation of how Mosaic evaluates and improves district maps.

## Table of Contents

1. [Function Overview](#function-overview)
2. [Basic Parameters](#basic-parameters)
3. [Scoring Metrics](#scoring-metrics)
   - [Cut Edges](#cut-edges)
   - [County Splits](#county-splits)
   - [Mean-Median Difference](#mean-median-difference)
   - [Efficiency Gap](#efficiency-gap)
   - [Expected Seats](#expected-seats)
   - [Competitiveness](#competitiveness)
   - [Bunking (Anti & Pro)](#bunking-anti--pro)
4. [How Scoring Works](#how-scoring-works)
5. [Optimization Settings](#optimization-settings)
6. [Output & Snapshots](#output--snapshots)
7. [Practical Tuning Guide](#practical-tuning-guide)

---

## Function Overview

Here's the full `run_chain()` function signature with all parameters:

```r
run_chain(
  # Required
  shapefile_path,
  
  # Basic settings
  num_districts = 5,
  num_steps = 1000,
  pdev_tolerance = 0.05,
  seed = NULL,
  assignment = NULL,
  
  # Optimization control
  use_optimization = TRUE,
  initial_temp_factor = 0.2,
  temp_mode = "PROPORTIONAL",
  temp_nominal = NULL,
  cooling_mode = "GUIDED",
  cooling_guided_params = list(guidepoint_iteration_p = 0.9, guidepoint_temperature = 1),
  cooling_rate = 0.999,
  
  # Scoring weights
  weight_cut_edges = 1,
  weight_county_splits = NA,
  weight_mean_median = NA,
  weight_efficiency_gap = NA,
  weight_dem_seats = NA,
  weight_competitiveness = NA,
  
  # Scoring exponents
  exponent_cut_edges = 1,
  exponent_county_splits = 1,
  exponent_mean_median = 2,
  exponent_efficiency_gap = 2,
  exponent_dem_seats = 2,
  exponent_competitiveness = 1,
  
  # Scoring targets
  target_mean_median = 0,
  target_efficiency_gap = 0,
  target_dem_seats = NA,
  
  # Scoring behavior
  use_robust_efficiency_gap = TRUE,
  election_win_prob_at_55 = 0.9,
  competitiveness_maximize = TRUE,
  
  # Geographic control
  county_bias = 1.0,
  bunking_lists = NULL,
  
  # Output control
  output_dir = NULL,
  save_final_assignment = TRUE,
  capture_full_assignments = FALSE,
  snapshot_assignments = TRUE,
  snapshot_interval = 50,
  
  # Verbosity
  verbose_console = TRUE,
  verbose_initialization = FALSE,
  timing_analysis = FALSE,
  final_analysis = TRUE
)
```

Don't worry - you rarely need to set more than 5-10 of these. Most have sensible defaults.

---

## Basic Parameters

### `shapefile_path` (required)
Path to your shapefile containing precinct boundaries and data.

```r
shapefile_path = "shapefiles/North_Carolina_Simplified.shp"
```

Your shapefile should have:
- **GEOID20** or **GEOID** - Unique precinct identifiers
- **POP** - Population counts
- **CTY** - County identifiers (optional, needed for county splits)
- **DEM**, **REP** - Vote totals (optional, needed for partisan metrics)

---

### `num_districts` (default: 5)
Number of districts to create.

```r
num_districts = 14  # North Carolina congressional map
num_districts = 50  # North Carolina state senate
```

---

### `num_steps` (default: 1000)
Number of ReCom iterations to run. More steps = more optimization time.

```r
num_steps = 1000   # Quick run
num_steps = 3000   # Standard optimization
num_steps = 10000  # Deep optimization
```

The more metrics you are trying to optimize for, the longer you should set the optimization (in number of steps). This gives the algorithm more of a runway to meet different goals.

A longer optimization is also necessary for maps with more districts than others, since the number of different combinations worth exploring grows exponentially.

---

### `pdev_tolerance` (default: 0.05)
Maximum allowed population deviation from ideal district size, as a fraction.

```r
pdev_tolerance = 0.05   # ±5% (typical for testing)
pdev_tolerance = 0.10   # ±10% (very loose)
pdev_tolerance = 0.01   # ±1% (very strict)
```

Different legal standards apply on the state and federal level to keep districts "as equal as practicable." 

In some states, this can mean no more than ±1 person from ideal, but others are more flexible, especially if the discrepancy permits achieving another objective like keeping a county together.

You should expect a lower `pdev_tolerance` to raise your computation time significantly.

At very low district population sizes, and very _high_ precinct population sizes, a strict `pdev_tolerance` will severely constrain the solution space (as well as increase computation time). This happens exponentially as you get closer to 0%.

This is most common when drawing small districts (like a statehouse or city-council map) because each precinct is large enough to severely throw off the district population. And unlike a real mapper, Mosaic cannot break precincts down to smaller levels. It may be worth using Mosaic to draw a "basemap" within 2% of ideal, for instance, and then completing final adjustments on your own in separate software.

---

### `seed` (default: NULL)
Random seed for reproducibility. Use the same seed to get the same results.

```r
seed = 123456  # Fixed seed - same results every time
seed = NULL    # Random seed - different results each time
```

---

## Scoring Metrics

Mosaic's optimization works by scoring each proposed map and accepting or rejecting changes based on whether they improve the score. The total score is a weighted sum of individual metrics.

### Cut Edges

**What it measures:** Number of edges in the graph that cross district boundaries.

**Why it matters:** Fewer cut edges = more compact districts. This is the fundamental compactness metric.

**Parameters:**
```r
weight_cut_edges = 1        # Weight (default: 1)
exponent_cut_edges = 1      # Exponent (default: 1)
```

**How it's calculated:**
For each edge in your precinct adjacency graph, check if it connects two precincts in different districts. Count them up. That's your cut edges score.

**Weighted score contribution:**
```
score_contribution = weight_cut_edges * (cut_edges ^ exponent_cut_edges)
```

**Example:**
```r
# Optimize primarily for compactness
results <- run_chain(
  shapefile_path = SHAPEFILE,
  num_districts = 14,
  weight_cut_edges = 1
)
```

With `weight_cut_edges = 1` and `exponent_cut_edges = 1`, a map with 540 cut edges contributes 540 to the total score.

---

### County Splits

**What it measures:** How many counties are unnecessarily split across districts.

**Why it matters:** Many states have legal requirements to keep counties whole when possible. Even without legal requirements, splitting fewer counties often creates more recognizable, administratively simpler districts.

**Parameters:**
```r
weight_county_splits = 15       # Weight (default: NA = disabled)
exponent_county_splits = 1      # Exponent (default: 1)
county_bias = 10                # Sampling bias toward county boundaries
```

**How it's calculated:**
This is more sophisticated than just counting splits. The algorithm:

1. **Calculates allowance** - Each county gets an "allowance" based on how many districts it should reasonably span:
   ```
   allowance = ceiling(county_population / ideal_district_population)
   ```
   
2. **Counts excess splits** - If a county is split into more districts than its allowance, the excess counts toward the score:
   ```
   excess_splits = max(0, actual_districts - allowance)
   ```

3. **Adds smoothness penalty** - For counties split beyond their allowance, we penalize fragmentation. If a county is split 60%-30%-10%, the 30% and 10% pieces contribute to a smoothness penalty.

4. **Combines them:**
   ```
   county_splits_score = excess_splits + smoothness_penalty
   ```

This means:

- Charlotte (Mecklenburg County, NC) with 1M people in a 750K district state can span 2 districts without penalty
- Wake County, NC (pop 1.1m) split 3 ways when it should span 2 districts gets a penalty of 1
- A county split 90%-5%-5% still gets a higher penalty than one split 50%-50%, because we prioritize lowering the total number of splits faster than the degree of splitting.

**Weighted score contribution:**
```
score_contribution = weight_county_splits * (county_splits ^ exponent_county_splits)
```

**Example:**
```r
# Strongly preserve counties
results <- run_chain(
  shapefile_path = SHAPEFILE,
  num_districts = 14,
  county_bias = 10,
  weight_county_splits = 15
)
```

#### About the County Bias Tool

`county_bias` doesn't affect the score directly - instead, it makes the ReCom algorithm more likely to cut along county boundaries when building spanning trees. A `county_bias` value of 2, for instance, means that Mosaic will be twice as likely to cut an edge that lies between counties than one that lies within one. Think of it as guiding the search space rather than penalizing the result.

---

### Mean-Median Difference

**What it measures:** The difference between the mean Democratic vote share across all districts and the median Democratic vote share. A measure of partisan symmetry.

**Why it matters:** In a fair map, the median district should have roughly the same partisan lean as the state overall. A large mean-median difference may indicate partisan gerrymandering.

**Parameters:**
```r
weight_mean_median = 150        # Weight (default: NA = disabled)
exponent_mean_median = 2        # Exponent (default: 2)
target_mean_median = 0          # Target (default: 0 = no skew)
```

**How it's calculated:**
1. Calculate Democratic vote share for each district:
   ```
   dem_share[i] = dem_votes[i] / (dem_votes[i] + rep_votes[i])
   ```

2. Find the mean across all districts:
   ```
   mean_dem_share = mean(dem_share)
   ```

3. Find the median district:
   ```
   median_dem_share = median(dem_share)
   ```

4. Calculate the difference:
   ```
   mean_median_diff = mean_dem_share - median_dem_share
   ```

5. Measure distance from target:
   ```
   raw_metric = abs(mean_median_diff - target_mean_median)
   ```

**Weighted score contribution:**
```
score_contribution = weight_mean_median * (raw_metric ^ exponent_mean_median) * 100
```

The `* 100` scaling factor converts from fraction (0.05) to percentage-like units (5) for easier tuning.

**Interpretation:**

- **Positive MM difference** (e.g., +0.05 or +5%) = Pro-Republican skew. The median district is 5% more Republican than average.
- **Negative MM difference** (e.g., -0.05 or -5%) = Pro-Democratic skew. The median district is 5% more Democratic than average.
- **Zero MM difference** = Symmetric. The median district mirrors the state.

**Example:**
```r
# Create a fair map
results <- run_chain(
  shapefile_path = SHAPEFILE,
  num_districts = 14,
  target_mean_median = 0,
  weight_mean_median = 150
)

# Create a Republican gerrymander (median district R+15%)
results <- run_chain(
  shapefile_path = SHAPEFILE,
  num_districts = 14,
  target_mean_median = 0.15,
  weight_mean_median = 50
)
```

---

### Efficiency Gap

**What it measures:** The difference in "wasted votes" between parties. Another measure of partisan gerrymandering.

**Why it matters:** In a fair map, both parties should waste similar numbers of votes. A large efficiency gap indicates one party is packing or cracking the other.

**Parameters:**
```r
weight_efficiency_gap = 100         # Weight (default: NA = disabled)
exponent_efficiency_gap = 2         # Exponent (default: 2)
target_efficiency_gap = 0           # Target (default: 0 = symmetric)
use_robust_efficiency_gap = TRUE    # Use robust version (default: TRUE)
```

**How it's calculated (standard version):**

For each district, votes are "wasted" if they're:

- **Surplus votes** for the winner (anything over 50%)
- **All votes** for the loser

Example: District with 60% Dem, 40% Rep

- Dem wasted: 10% (60% - 50%)
- Rep wasted: 40% (all of it, they lost)

Sum across all districts:
```
efficiency_gap = (total_dem_wasted - total_rep_wasted) / total_votes
```

For computational simplicity, Mosaic uses an approximation that assumes each district casts the same total number of votes. (This will be fixed eventually).

**How it's calculated (robust version, default):**

The basic version of the efficiency gap is over-sensitive to the current political environment. A change in the result of a single seat from 49.9% to 50.1% Republican can be enough to throw the efficiency gap 10 points in their favor, since it alters the balance of wasted votes dramatically.

The robust version used by default in Mosaic calculates efficiency gap under multiple swing scenarios:

- Current environment (0% swing)
- ±2%, ±4%, ±6%, ±8% swings

Each scenario is weighted by how likely it is (using a normal distribution with  μ=0%, σ=3%):
```
robust_eg = weighted_average(eg_at_each_swing)
```

This gives you an efficiency gap that's more stable across different electoral environments.

**Weighted score contribution:**
```
score_contribution = weight_efficiency_gap * (|eg - target| ^ exponent) * 100
```

**Interpretation:**

- **Positive EG** (e.g., +0.10 or +10%) = Pro-Republican. Republicans waste 10% fewer votes.
- **Negative EG** (e.g., -0.10 or -10%) = Pro-Democratic. Democrats waste 10% fewer votes.
- **Zero EG** = Symmetric wasted votes.

_Note:_ Most other sources cite the efficiency gap using a flipped sign. In Mosaic this is reversed so that both `MM DIFF` and `EFF GAP` are negative when in Democrats' favor.

**Example:**
```r
# Fair map (symmetric wasted votes)
results <- run_chain(
  shapefile_path = SHAPEFILE,
  num_districts = 14,
  target_efficiency_gap = 0,
  weight_efficiency_gap = 100,
  use_robust_efficiency_gap = TRUE
)

# Republican gerrymander (R advantage)
results <- run_chain(
  shapefile_path = SHAPEFILE,
  num_districts = 14,
  target_efficiency_gap = 0.25,   # Target R+25%
  weight_efficiency_gap = 100
)
```

**Standard vs Robust:**
```r
use_robust_efficiency_gap = TRUE   # Default - stable across swings
use_robust_efficiency_gap = FALSE  # Optimizes for current environment only
```

We recommend keeping this as `TRUE` unless you have a specific reason to optimize for a single electoral environment. `EG ADJ` is what `EFF GAP` prints as to the console when using the robust setup.

---

### Expected Dem Seats

**What it measures:** How many seats each party is expected to win, accounting for the probability of victory in each district.

**Why it matters:** Sometimes you want a specific partisan outcome - "create a 7-7 map" or "maximize Democratic seats." This metric lets you target that directly.

**Parameters:**
```r
weight_dem_seats = 250              # Weight (default: NA = disabled)
exponent_dem_seats = 2              # Exponent (default: 2)
target_dem_seats = 7                # Target (default: NA)
election_win_prob_at_55 = 0.9       # Win probability model (default: 0.9)
```

**How it's calculated:**

1. For each district, calculate the probability Democrats win based on vote share:
   ```
   k = -0.05 / log((1 / election_win_prob_at_55) - 1)
   prob_dem_win[i] = 1 / (1 + exp(-(dem_share[i] - 0.5) / k))
   ```

2. Sum probabilities across all districts:
   ```
   expected_dem_seats = sum(prob_dem_win)
   ```

The `k` parameter controls how "swingy" elections are. With the default `election_win_prob_at_55 = 0.9`:
- A district with 55% Dem vote share → 90% chance Dem wins
- A district with 50% Dem vote share → 50% chance Dem wins
- A district with 60% Dem vote share → ~98% chance Dem wins

**Weighted score contribution:**
```
score_contribution = weight_dem_seats * (|expected_dem_seats - target_dem_seats| ^ exponent)
```

**Example:**
```r
# Target a 7-7 map in a 14-district state
results <- run_chain(
  shapefile_path = SHAPEFILE,
  num_districts = 14,
  target_dem_seats = 7,
  weight_dem_seats = 250
)

# Maximize Democratic seats (no specific target, just push high)
# Set target to num_districts to push toward maximum
results <- run_chain(
  shapefile_path = SHAPEFILE,
  num_districts = 14,
  target_dem_seats = 14,
  weight_dem_seats = 100
)
```

**About election_win_prob_at_55:**
This models electoral volatility. Higher values = more certain outcomes:
- `0.9` (default): 55% vote share → 90% win probability (moderate volatility)
- `0.95`: 55% vote share → 95% win probability (low volatility, incumbency advantage)
- `0.80`: 55% vote share → 80% win probability (high volatility, swing seats)

---

### Competitiveness

**What it measures:** How many districts are competitive (close to 50-50).

**Why it matters:** Competitive districts mean more voter choice and responsive representation. Non-competitive districts are often criticized as "safe seats" where outcomes are predetermined.

**Parameters:**
```r
weight_competitiveness = 5          # Weight (default: NA = disabled)
exponent_competitiveness = 1        # Exponent (default: 1)
competitiveness_maximize = TRUE     # Maximize competitive seats (default: TRUE)
```

**How it's calculated:**

1. For each district, find the win probability for the minority party:
   ```
   minority_win_prob[i] = min(prob_dem_win[i], 1 - prob_dem_win[i])
   ```

2. Scale to 0-1 range where 1 = perfectly competitive:
   ```
   competitiveness_score[i] = minority_win_prob[i] * 2
   ```

3. Sum across all districts:
   ```
   total_competitiveness = sum(competitiveness_score)
   ```

A perfectly competitive district (50-50) contributes 1.0 to the score.
A safe district (30-70) contributes ~0.05 to the score.

**Weighted score contribution:**
```
if competitiveness_maximize:
  comp_fraction = total_competitiveness / num_districts
  score_contribution = weight * ((1 - comp_fraction) ^ exponent) * 100
else:
  comp_fraction = total_competitiveness / num_districts  
  score_contribution = weight * (comp_fraction ^ exponent) * 100
```

With `competitiveness_maximize = TRUE` (default), the score penalizes having *few* competitive districts.
With `competitiveness_maximize = FALSE`, the score penalizes having *many* competitive districts (useful for drawing safe seats).

**Example:**
```r
# Maximize competitive districts
results <- run_chain(
  shapefile_path = SHAPEFILE,
  num_districts = 14,
  weight_competitiveness = 5,
  competitiveness_maximize = TRUE
)

# Minimize competitive districts (draw safe seats)
results <- run_chain(
  shapefile_path = SHAPEFILE,
  num_districts = 14,
  weight_competitiveness = 5,
  competitiveness_maximize = FALSE
)
```

**Typical output:**
In a 14-district state, you might see:
- `competitiveness = 8.2` means about 8 districts are reasonably competitive
- `competitiveness = 2.1` means only about 2 districts are competitive

---

### Bunking (Anti & Pro)

**What it measures:** Whether specific precincts are kept together (pro-bunking) or apart (anti-bunking).

**Why it matters:** 
- **Anti-bunking** - Keep incumbents separated (required by some court remedial maps)
- **Pro-bunking** - Group communities of interest (universities, tribal lands, etc.)

**Parameters:**
```r
bunking_lists = create_bunking_lists(
  anti_bunking = list(list1, list2, ...),
  pro_bunking = list(list3, list4, ...)
)
```

Each list has this format:
```r
my_list <- c(
  1000,              # Weight
  1,                 # Exponent
  "precinct_id_1",   # Precinct GEOIDs to keep together/apart
  "precinct_id_2",
  "precinct_id_3",
  ...
)
```

**How anti-bunking is calculated:**

Goal: Keep precincts in different districts.

1. Count how many districts the precincts span:
   ```
   unique_districts = number of distinct districts containing these precincts
   ```

2. Calculate base metric (0 = best, all in different districts):
   ```
   base_metric = (num_precincts - unique_districts) / (num_precincts - 1)
   ```

3. Add smoothness penalty for clustering:
   - If precincts cluster together (e.g., 3 in one district, 1 in another), penalize that
   - This prevents the algorithm from "hiding" bunking by having 90% in one district and 10% in another

**How pro-bunking is calculated:**

Goal: Keep precincts in the same district.

1. Count how many districts the precincts span:
   ```
   unique_districts = number of distinct districts containing these precincts
   ```

2. Calculate base metric (0 = best, all in same district):
   ```
   base_metric = (unique_districts - 1) / (num_precincts - 1)
   ```

3. Add smoothness penalty for fragmentation:
   - If precincts are fragmented (e.g., 60%-30%-10% split), penalize that
   - This encourages keeping all precincts together rather than having small orphan pieces

**Weighted score contribution:**
```
score_contribution = weight * (raw_metric ^ exponent)
```

**Example:**
```r
# Define incumbent locations (anti-bunking)
incumbent_list <- c(
  1000, 1,
  "37183001-27",   # Rep 1
  "3714701504A",   # Rep 2
  "3713500000H",   # Rep 3
  # ... etc
)

# Define university locations (pro-bunking)
university_list <- c(
  5000, 1,
  "37183004-05",   # NC State
  "37063000047",   # NC Central
  "37135000UNC",   # UNC Chapel Hill
  # ... etc
)

results <- run_chain(
  shapefile_path = SHAPEFILE,
  num_districts = 14,
  bunking_lists = create_bunking_lists(
    anti_bunking = list(incumbent_list),
    pro_bunking = list(university_list)
  )
)
```

See the [North Carolina Examples tutorial](../tutorials/nc-examples.md) for detailed examples of bunking in action (Examples 6 and 7).

**Typical output:**
In console, you'll see:
```
Anti-Bunking List 1: weight=1000.0, exponent=1.0, 14 precincts
Pro-Bunking List 1: weight=5000.0, exponent=1.0, 17 precincts
...
Final Analysis:
  List 1: 14 precincts in 13 districts | metric=0.077
  List 1: 17 precincts in 6 districts | metric=0.312
```

For anti-bunking, you want unique_districts ≈ num_precincts (everyone separated).
For pro-bunking, you want unique_districts ≈ 1 (everyone together).

---

## How Scoring Works

### The Total Score

Your map's total score is the weighted sum of all enabled metrics:

```
total_score = weight_cut_edges * (cut_edges ^ exp_cut_edges)
            + weight_county_splits * (county_splits ^ exp_county_splits)
            + weight_mean_median * (|mm - target_mm| ^ exp_mm) * 100
            + weight_efficiency_gap * (|eg - target_eg| ^ exp_eg) * 100
            + weight_dem_seats * (|seats - target_seats| ^ exp_seats)
            + weight_competitiveness * competitiveness_score * 100
            + weight_anti_bunking_1 * (anti_bunking_1 ^ exp_anti_bunking_1)
            + weight_pro_bunking_1 * (pro_bunking_1 ^ exp_pro_bunking_1)
            + ...
```

Any metric with weight `NA` is disabled and contributes 0.

### Example Score Calculation

Let's walk through a concrete example:

```r
results <- run_chain(
  shapefile_path = SHAPEFILE,
  num_districts = 14,
  weight_cut_edges = 1,
  weight_county_splits = 10,
  target_mean_median = 0,
  weight_mean_median = 100
)
```

Suppose at iteration 1000:
- Cut edges: 540
- County splits: 3.2
- Mean-median difference: 0.018 (1.8%)

Calculate each contribution:
```
cut_edges_score = 1 * (540 ^ 1) = 540
county_splits_score = 10 * (3.2 ^ 1) = 32
mean_median_score = 100 * (|0.018 - 0| ^ 2) * 100 = 100 * 0.000324 * 100 = 3.24

total_score = 540 + 32 + 3.24 = 575.24
```


### Understanding Weights

Weights control how much each metric matters relative to others. Here's how to think about them:

**Absolute values don't matter, only ratios:**
- `weight_cut_edges = 1, weight_county_splits = 10` 
- Is identical to `weight_cut_edges = 10, weight_county_splits = 100`
- What matters: county splits are 10× more important than cut edges

**Start with the default (cut edges = 1) and scale others relative to it:**
- `weight_county_splits = 10` means 1 county split = 10 cut edges
- `weight_mean_median = 100` means 1% MM difference = 100 cut edges (roughly)

**Typical weight ranges:**
- Cut edges: 1 (baseline)
- County splits: 5-15
- Mean-median: 50-500
- Efficiency gap: 50-200
- Expected seats: 100-500
- Competitiveness: 5-20
- Bunking: 500-5000

### Understanding Exponents

Exponents control how the penalty scales:

**Linear (exponent = 1):**
- 10 cut edges → score of 10
- 20 cut edges → score of 20
- Doubling the metric doubles the penalty

**Quadratic (exponent = 2):**
- MM difference of 1% → score of 1
- MM difference of 2% → score of 4
- MM difference of 5% → score of 25
- Doubling the metric quadruples the penalty

**Why use exponent = 2?**
Quadratic penalties encourage "getting close" without obsessing over perfection:
- Being 1% away from target: almost free (penalty of 1)
- Being 5% away from target: expensive (penalty of 25)
- Being 10% away from target: very expensive (penalty of 100)

This creates a "good enough" zone near the target, which helps optimization converge.

**When to use exponent = 1:**
- For metrics where you care about every unit equally (cut edges, bunking)
- When you want proportional penalties

**When to use exponent = 2:**
- For metrics with a target where you want a "good enough" zone (MM, EG, seats)
- When you want to heavily penalize large deviations

---

## Optimization Settings

### Simulated Annealing

Mosaic uses simulated annealing to escape local minima and find better solutions.

**How it works:**
1. Start with high "temperature"
2. Accept some moves that make the score worse (controlled by temperature)
3. Gradually cool down (reduce temperature)
4. Eventually only accept improvements

**Why it helps:**
Without annealing, you'd get stuck in the first decent solution you find. With annealing, you can explore and find much better solutions.

---

### Temperature Settings

**`use_optimization`** (default: TRUE)
Enable simulated annealing. Set to `FALSE` for random walk (no optimization).

```r
use_optimization = TRUE   # Optimize the map
use_optimization = FALSE  # Random walk (for comparison/testing)
```

---

**`temp_mode`** (default: "PROPORTIONAL")
How to set the initial temperature.

```r
temp_mode = "PROPORTIONAL"   # Temperature = initial_score * initial_temp_factor
temp_mode = "NOMINAL"        # Temperature = temp_nominal (fixed value)
```

**PROPORTIONAL** (recommended): Scales temperature to your score, so it works regardless of your weights.

**NOMINAL**: Set temperature directly. Useful if you know exactly what temperature you want.

---

**`initial_temp_factor`** (default: 0.2)
When using PROPORTIONAL mode, initial temperature = initial score × this factor.

```r
initial_temp_factor = 0.2   # Default - accepts ~20% of worsening moves initially
initial_temp_factor = 0.5   # More exploration - accepts ~50% of worsening moves
initial_temp_factor = 0.05  # Less exploration - more greedy
```

Higher values = more exploration, slower optimization.
Lower values = less exploration, faster optimization but might get stuck.

---

**`temp_nominal`** (default: NULL)
When using NOMINAL mode, set temperature to this fixed value.

```r
temp_mode = "NOMINAL"
temp_nominal = 100
```

---

### Cooling Settings

**`cooling_mode`** (default: "GUIDED")
How to decrease temperature over time.

```r
cooling_mode = "GUIDED"    # Automatically calculate cooling rate to reach target
cooling_mode = "NOMINAL"   # Use fixed cooling_rate
```

**GUIDED** (recommended): You specify when you want to reach a target temperature, and Mosaic calculates the rate automatically.

**NOMINAL**: You specify the cooling rate directly (for advanced users).

---

**`cooling_guided_params`** (default: list(guidepoint_iteration_p = 0.9, guidepoint_temperature = 1))
When using GUIDED mode, these control the cooling schedule:

```r
cooling_guided_params = list(
  guidepoint_iteration_p = 0.9,    # Reach target temp at 90% of total steps
  guidepoint_temperature = 1       # Target temperature
)
```

This means: "By 90% of the way through, temperature should be down to 1."

The algorithm calculates the cooling rate needed to achieve this.

**Example:**
```r
# Cool more aggressively (reach target earlier)
cooling_guided_params = list(guidepoint_iteration_p = 0.7, guidepoint_temperature = 1)

# Cool more slowly (reach target later)
cooling_guided_params = list(guidepoint_iteration_p = 0.95, guidepoint_temperature = 1)
```

---

**`cooling_rate`** (default: 0.999)
When using NOMINAL mode, multiply temperature by this factor each step:

```r
cooling_rate = 0.999   # Slow cooling
cooling_rate = 0.99    # Faster cooling
```

Formula: `new_temp = current_temp * cooling_rate`

With `cooling_rate = 0.999` over 1000 steps:
- Initial temp: 100
- After 1000 steps: ~36.8

---

### Practical Cooling Advice

**Use GUIDED mode** - it's simpler and more robust:
```r
cooling_mode = "GUIDED"
cooling_guided_params = list(guidepoint_iteration_p = 0.9, guidepoint_temperature = 1)
```

**Adjust guidepoint_iteration_p** based on run length:
- Short runs (1000 steps): 0.8-0.9
- Medium runs (3000 steps): 0.9
- Long runs (10000 steps): 0.95

This ensures temperature stays high enough for exploration but cools down before the end.

---

## Output & Snapshots

### `output_dir` (default: NULL)
Where to save results. If `NULL`, uses `mosaic_path("output")`.

```r
output_dir = "output"              # Default
output_dir = "results/nc_maps"     # Custom directory
```

---

### `save_final_assignment` (default: TRUE)
Save the final district assignments to CSV.

```r
save_final_assignment = TRUE   # Save final map
save_final_assignment = FALSE  # Don't save (testing only)
```

Output: `output/final_assignment_MMDDYYYY_HHMM.csv`

---

### `capture_full_assignments` (default: FALSE)
Save assignments at every iteration (creates very large files).

```r
capture_full_assignments = FALSE  # Default - don't save every iteration
capture_full_assignments = TRUE   # Save everything (for research/debugging)
```

Output: `output/full_assignments_MMDDYYYY_HHMM.csv`

**Warning:** This creates files with `num_precincts × num_steps` cells. For 2500 precincts and 3000 steps, that's 7.5 million cells. Only enable if you need it.

---

### `snapshot_assignments` (default: TRUE)
Save assignments at regular intervals for GIF creation.

```r
snapshot_assignments = TRUE   # Save snapshots for GIFs
snapshot_assignments = FALSE  # Don't save snapshots
```

Output: `output/snapshot_assignments_MMDDYYYY_HHMM.csv`

---

### `snapshot_interval` (default: 50)
How often to save snapshots.

```r
snapshot_interval = 50    # Save every 50 iterations (default)
snapshot_interval = 100   # Save every 100 iterations (fewer frames)
snapshot_interval = 10    # Save every 10 iterations (more frames, smoother GIF)
```

For a 2000-step run:
- `interval = 50` → 41 snapshots
- `interval = 100` → 21 snapshots
- `interval = 10` → 201 snapshots

More snapshots = smoother GIFs but larger files and slower GIF generation.

---

### Verbosity

**`verbose_console`** (default: TRUE)
Print detailed progress during optimization.

```r
verbose_console = TRUE   # Show iteration table with all metrics
verbose_console = FALSE  # Only show iteration counter
```

---

**`verbose_initialization`** (default: FALSE)
Print detailed information during initial partition creation.

```r
verbose_initialization = FALSE  # Quiet initialization
verbose_initialization = TRUE   # Show district-by-district creation
```

---

**`timing_analysis`** (default: FALSE)
Print detailed performance timing breakdown.

```r
timing_analysis = FALSE  # No timing info
timing_analysis = TRUE   # Show time spent in each operation
```

Useful for performance debugging.

---

**`final_analysis`** (default: TRUE)
Print final summary with district-by-district breakdown.

```r
final_analysis = TRUE   # Show final results table
final_analysis = FALSE  # Skip final summary
```

---

## Practical Tuning Guide




### General Tuning Advice

**Start simple, add complexity:**
1. Compactness only
2. Add county preservation
3. Add partisan metrics
4. Add seat targets or bunking

**Iterate on weights:**
- Run for 1000 steps with initial weights
- Check which metrics are far from target
- Increase weights for metrics that need more attention
- Re-run and repeat

**Watch acceptance rate:**
In verbose mode, you'll see acceptance rate (% ACC column):
- ~80-90% early on = good exploration, the algorithm is accepting many changes to span the map
- ~20-40% mid-run = good optimization, the algorithm is getting choose-y 
- <10% at end = final refining, the algorithm is only accepting outright improvements

Each of these stages are important. If Mosaic's acceptance rate falls extremely quickly, it is unlikely to have ventured very far in the solution set. Simultaneously, if Mosaic's acceptance rate is still hovering in the optimization range (e.g. 25%) at the end of the annealing, it probably could have used more time to perfect its final solution.

**Use consistent seeds for testing:**
Always use the same seed when tuning weights, so you can compare results fairly.

**Increase steps for complex objectives:**
- 1000 steps: Simple compactness
- 3000 steps: Compactness + counties + 1-2 partisan metrics
- 5000+ steps: Complex objectives with multiple metrics


---

## Common Patterns

### Fair Competitive Map
```r
results <- run_chain(
  shapefile_path = SHAPEFILE,
  num_districts = 14,
  county_bias = 5,
  weight_county_splits = 10,
  target_mean_median = 0,
  weight_mean_median = 150,
  target_efficiency_gap = 0,
  weight_efficiency_gap = 100,
  weight_competitiveness = 10,
  num_steps = 3000
)
```

### Gerrymander (Republican)
```r
results <- run_chain(
  shapefile_path = SHAPEFILE,
  num_districts = 14,
  county_bias = 2,
  weight_county_splits = 5,
  target_mean_median = 0.15,      # Median district R+15%
  weight_mean_median = 50,
  target_efficiency_gap = 0.25,   # R+25% efficiency gap
  weight_efficiency_gap = 100,
  target_dem_seats = 3,            # Target only 3 Dem seats
  weight_dem_seats = 250,
  num_steps = 3000
)
```

### Gerrymander (Democratic)
```r
results <- run_chain(
  shapefile_path = SHAPEFILE,
  num_districts = 14,
  county_bias = 2,
  weight_county_splits = 5,
  target_mean_median = -0.10,     # Median district D+10%
  weight_mean_median = 50,
  target_efficiency_gap = -0.20,  # D+20% efficiency gap
  weight_efficiency_gap = 100,
  target_dem_seats = 11,           # Target 11 Dem seats
  weight_dem_seats = 250,
  num_steps = 3000
)
```

### Maximum Compactness (No Politics)
```r
results <- run_chain(
  shapefile_path = SHAPEFILE,
  num_districts = 14,
  county_bias = 10,
  weight_county_splits = 20,
  num_steps = 2000
)
```

### Communities of Interest
```r
university_list <- c(5000, 1, "precinct1", "precinct2", ...)

results <- run_chain(
  shapefile_path = SHAPEFILE,
  num_districts = 14,
  county_bias = 3,
  weight_county_splits = 10,
  bunking_lists = create_bunking_lists(
    pro_bunking = list(university_list)
  ),
  num_steps = 2500
)
```

---

## Next Steps

- **[Mosaic in Action: North Carolina Examples](../tutorials/nc-examples.md)** - See these concepts in practice with 7 detailed examples
- **[Plot Guide](plotting.md)** - Visualize your results
- **[How Mosaic Works](algorithm.md)** - Understand the ReCom algorithm and spanning tree
