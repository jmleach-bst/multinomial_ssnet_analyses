#!/bin/bash
#SBATCH --array=1-25
#SBATCH --share
#SBATCH --partition=medium
#SBATCH --job-name=analysis_ssen_M5000_X1k_B6_BV012025_NTR300_NTE100_NCV5
#SBATCH --error=analysis_ssen_M5000_X1k_B6_BV012025_NTR300_NTE100_NCV5_%a.err
#SBATCH --output=analysis_ssen_M5000_X1k_B6_BV012025_NTR300_NTE100_NCV5_%a.out
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mail-type=FAIL
#SBATCH --mail-user=jleach@uab.edu
#SBATCH --time=49:59:00
#SBATCH --mem-per-cpu=50GB

module load R/4.1.0-foss-2018a-X11-20180131-bare
srun R CMD BATCH /data/user/jleach/sim_mn_2021/Rcode/analysis_ssen_M5000_X1k_B6_BV012025_NTR300_NTE100_NCV5.R
