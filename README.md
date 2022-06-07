# Basics of the R programming language course

## Table of Contents

- [Introduction](#introduction)
- [Dataset](#dataset)
- [Useful resources](#useful-resources)
- [Directory structure](#directory-structure)

## Introduction

This repository contains the course material for the "Basics of the R
programming language - data wrangling and data visualization" held in May 2022,
and organized within the [COORDINATE](https://www.coordinate-network.eu/)
project.

<a rel="license" href="http://creativecommons.org/licenses/by/4.0/"><img
alt="Creative Commons License" style="border-width:0"
src="https://i.creativecommons.org/l/by/4.0/88x31.png" /></a>
<br />
The materials in this repository are licensed under the
<a rel="license"
href="http://creativecommons.org/licenses/by/4.0/">Creative Commons Attribution
4.0 International License</a>.

## Dataset

This course used the
[Generations & Gender Programme Data](https://www.ggp-i.org/). Specifically, the
data from the *Generations & Gender Survey*, Round 1:
- France: waves 1, 2 and 3

## Useful resources

- [RStudio cheat sheets](https://www.rstudio.com/resources/cheatsheets/) giving
    an overview of many packages from the `tidyverse` ecosystem
- [regular expressions cheat sheet](https://remram44.github.io/regex-cheatsheet/regex.html)
- [R for Data Science](https://r4ds.had.co.nz/) free online book
- [Introduction to Statistical Learning](https://www.statlearning.com/) free
    book, available online. Covers a lot of statistical methods with examples
    coded up in R.
- [Grat series of blog posts](https://suzanbaert.netlify.app/2018/01/dplyr-tutorial-1/) on
    data wrangling using dplyr and other tidyverse packages.

## Directory structure

**Note**: `data/` is where you should put your data files obtained from the
GGP website; not present in the repository.

```
.
├── basics
│   ├── 01_r-basics_live.R
│   ├── 01_r-basics.R
│   ├── 02_regular-expressions_live.R
│   └── 02_regular-expressions.R
├── crossda_r-wrangling-data-visualization.Rproj
├── data
│   ├── France_Wave2_V.1.3.sav
│   ├── GGS_Wave1_France_V.4.4.sav
│   └── GGS_Wave3_France_V.1.0.sav
├── README.md
├── visualization
│   ├── 01_grammar.html
│   ├── 01_grammar.Rmd
│   ├── 02_ggplot_basics.R
│   ├── 03_ggplot_multidimensional.R
│   ├── 04_ggplot_customization.R
│   ├── anscombe.svg
│   ├── RcolorBrewer.png
│   ├── rjs.css
│   └── scale.png
└── wrangling
    ├── 01_read-data_merge_live.R
    ├── 01_read-data_merge.R
    ├── 02_transform-data_live.R
    ├── 02_transform-data.R
    ├── 03_pivot-longer_live.R
    ├── 03_pivot-longer.R
    └── 04_prepare-visualization-data.R
```
