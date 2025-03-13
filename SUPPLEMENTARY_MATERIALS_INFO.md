# Supplementary Materials for "Visual Disturbances to Avatar Foot Position Increase Step-width Variability in Immersive VR Treadmill Walking"

## Repository Context
This Git repository is part of the supplementary materials for the paper referenced above. In alignment with the paper's commitment to transparency and reproducibility, the datasets, analysis code, and detailed instructions for reproducing the results are publicly accessible. These additional supplementary materials, including this repository and a comprehensive read-me document, are available for download via IEEE Xplore ([http://ieeexplore.ieee.org](http://ieeexplore.ieee.org)). These materials provide further methodological insights and statistical details to support the study's findings.
---

## About the Shallow Clone (Space-Saving Setup)
The version of this repository included in the supplementary materials is a **shallow clone** to minimize storage requirements. This means:
- Only the **latest commit** is included (no full Git history).
- The command used to create this version was:  
  ```bash
  git clone --depth 1 https://gitlab.tudelft.nl/mln-lab-public/r-analysis-alex-van-den-berg-2024-vfd-experiment.git
  ```

### How to Retrieve the Full Git History
If you need the complete commit history (e.g., to review changes over time or collaborate on development), follow these steps:

1. **Check the remote URL** (ensure you have access to the original repository):  
   ```bash
   git remote -v
   ```
   This should show the remote URL (e.g., `https://gitlab.tudelft.nl/...`).

2. **Fetch the full history**:  
   ```bash
   git fetch --unshallow
   ```
   This downloads all missing commits from the remote, converting the shallow clone into a full repository.

3. **Update all branches and tags** (optional but recommended):  
   ```bash
   git fetch --all
   git pull --all
   ```

### If You Cloned Normally
If you cloned this repository **without** the `--depth 1` flag, you already have the full history and can ignore the steps above.

---

## Further Details
For instructions on reproducing analyses, dataset descriptions, and repository structure, see the main documentation:  
[`README.md`](./README.md)