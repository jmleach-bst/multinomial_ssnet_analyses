#!/bin/bash
#SBATCH --array=1-5
#SBATCH --share
#SBATCH --partition=short
#SBATCH --job-name=analysis_en_M5000_X1k_B6_BV012025_NTR300_NTE100_NCV5
#SBATCH --error=analysis_en_M5000_X1k_B6_BV012025_NTR300_NTE100_NCV5.err
#SBATCH --output=analysis_en_M5000_X1k_B6_BV012025_NTR300_NTE100_NCV5.out
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mail-type=FAIL
#SBATCH --mail-user=jleach@uab.edu
#SBATCH --time=11:59:00
#SBATCH --mem-per-cpu=50GB

module load R/4.1.0-foss-2018a-X11-20180131-bare
srun R CMD BATCH /data/user/jleach/sim_mn_2021/Rcode/analysis_en_M5000_X1k_B6_BV012025_NTR300_NTE100_NCV5.R
