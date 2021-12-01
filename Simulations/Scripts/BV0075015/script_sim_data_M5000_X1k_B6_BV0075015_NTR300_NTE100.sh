#!/bin/bash
#SBATCH --share
#SBATCH --partition=short
#SBATCH --job-name=sim_data_M5000_X1k_B6_BV0075015_NTR300_NTE100
#SBATCH --error=sim_data_M5000_X1k_B6_BV0075015_NTR300_NTE100.err
#SBATCH --output=sim_data_M5000_X1k_B6_BV0075015_NTR300_NTE100.out
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mail-type=FAIL
#SBATCH --mail-user=jleach@uab.edu
#SBATCH --time=11:59:00
#SBATCH --mem-per-cpu=50GB

module load R/4.1.0-foss-2018a-X11-20180131-bare
srun R CMD BATCH /data/user/jleach/sim_mn_2021/Rcode/sim_data_M5000_X1k_B6_BV0075015_NTR300_NTE100.R
