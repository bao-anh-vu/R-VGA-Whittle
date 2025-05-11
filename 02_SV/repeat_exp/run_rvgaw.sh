#!/bin/bash

method=$1 

# List of arguments
reps=({11..12})

if [ "$method" == "rvgaw" ]; then
  echo "Running script for R-VGAW"
  for arg in "${reps[@]}"
  do
    Rscript 02_run_rvgaw.R "$arg" &
  done
elif [ "$method" == "hmc" ]; then
  echo "Running script for HMC"
  for arg in "${reps[@]}"
  do
    Rscript 03_run_hmc.R "$arg" &
  done
else
  echo "Unknown method: $method"
  exit 1
fi

# Wait for all background tasks to complete
wait

echo "All R scripts have finished running."
