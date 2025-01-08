
<!-- README.md is generated from README.Rmd. Please edit that file -->

# What is it?

This repository contains code for reproducing simulations from the paper
[The spike-and-slab lasso and scalable algorithm to accommodate
multinomial outcomes in variable selection
problems](https://www.tandfonline.com/doi/full/10.1080/02664763.2023.2258301),
published in the Journal of Applied Statistics.

There are two primary folders. The folder “ADNI-analyses” contains
details regarding analysis of ADNI data, while the folder “Simulations”
contains code and details for reproducing the simulation study. The
simulation study was conducted on the cluster at the University of
Alabama at Birmingham (UAB). We include `.sh` scripts and `R` code for
reproducing the analyses using Slurm Workload Manager
(<https://slurm.schedmd.com/overview.html>). If you want to re-run
everything from scratch, you will need to change path names in `R` files
and the Slurm scripts, and if your institution does not use Slurm, then
the shells may not be that useful to you. More generally, we recommend
consulting the `.Rmd` files, which combine code and explanations with
`R` markdown.

We are not authorized to include data from the ADNI study, but details
for access are available at <http://adni.loni.usc.edu/>. We have
nevertheless included our code for analyses using ADNI data, so that
anyone with access could run the analyses if they so choose. Note that
we use the `ADNIMERGE` package version 0.0.1, released on 2020-02-19;
subsequent versions/release dates may result in a different, but
significantly overlapping, data set. The PDF files
“adni_data_management” and “adni_multinomial_analysis” are directly
generated by kniting their respective `.Rmd` files for those users
without access to ADNI data; these files detail how the data was
“cleaned” and analyzed, respectively.

With respect to simulations, the folder “Rcode” contains code for
simulating and analyzing data, while the folder “Scripts” contains `.sh`
files for running simulations and analyses on UAB’s cluster. Each of
these directories has a sub-directory for each simulation scenario
containing required code for generating the simulated data as well as
running the analyses. Files beginning with `sim_data` generate simulated
data while `analysis_en`, `analysis_lasso`, `analysis_ssen`, and
`analysis_ssl` perform analyses for elastic net, lasso, spike-and-slab
elastic net, and spike-and-slab lasso, respectively. Again, the primary
file of interest for most users is the `simulation_results.Rmd` file,
which reproduces tables and figures (and alternatives to these), and
gives some brief details of the simulation scenario and how values were
generated. Due to their size, I have not included simulated data in the
repository, which is needed to reproduced the figure of example data and
observe the balanced in outcome distribution, so you will need to
generate the simulated data on your own. If you’d rather not do this,
you can reference the PDF “simulation_results” generated by the `.Rmd`
file that is included in this repository.
