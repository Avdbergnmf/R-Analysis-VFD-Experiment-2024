# **Data Dictionary for VFD Experiment**

This document provides an overview of the variables included in the dataset for the study titled:  
*"Visual Disturbances to Avatar Foot Position Increase Step-width Variability in Immersive VR Treadmill Walking."*  

Each variable is briefly described to help understand its purpose and how it was derived.

If you are using the interface, the sidebar contains many different ways to filter our complete dataset. All figures and calculations in the windows on the right are using the data that is selected in this sidebar. Data can be filtered based on participants, trials, and steps. Values that are selected in the selection boxes are **included** in the dataset. Removing all data may crash some of the calculations and with that the whole interface, so be aware of that. At the bottom there are some settings you can use to finetune the generation of the figures to get the sizes you require.


> NOTE: The initial calculation does an initial outlier selection based on rough threshold values for heelstrike positions. However, this selection is far from correctly identifying all the outliers, and so we implemented a manual outlier removal interface. To load our selected outliers and select them in the dataset, follow the instructions below.

## Remove Outliers
To reproduce the results in the paper, the outliers we manually selected still need to be removed. Follow the steps below:

1. Perform the initial calculation described above.
2. Open the **Manual Outlier Filtering** tab.
3. Find the **Import Outliers from CSV** field, and click `Browse`. Now select the outliers csv under: `./runtime/data_extra/outliers-2024-11-28.csv` and import them.
4. Make sure the *outlier matching threshold value* is still set to `0.1`s (this affects the outlier selection, if set larger, a larget timespan is used to remove outliers around the selected timestamps in the outlier dataset).
5. Click **Overwrite Outliers** to write the outliers to the dataset loaded in the interface.
6. Optional: Click **Save current parameters to RDS** and overwrite the previously calculated rds under `./runtime/results/allGaitParams.rds` so that the next time you load the interface, these outliers will already be marked in the dataset.

---

## **1. Participant and Trial Information**

| Variable Name               | Description                                                   |
|-----------------------------|---------------------------------------------------------------|
| `participant`                | Unique identifier for each participant.                      |
| `trialNum`                   | Trial number within the entire experiment (1-6).                   |
| `VFD`                        | Condition indicator (1 = Visual Feedback Distortion, 0 = Control). |
| `heelStrikes.foot`            | Foot (left or right) associated with the heel strike.        |
| `practice`                   | Whether the trial was a practice run (1 = Yes, 0 = No).      |
| `startedWithNoise`            | Indicates if the participant started with visual noise (=VFD).      |
| `conditionNumber`             | Currently doing the first or second condition? (1-2)           |
| `trialNumWithoutPractice`      | Trial number excluding practice trials (1-4).                     |
| `trialNumWithinCondition`      | Trial number within a specific condition (1-2).                   |
| `noticed`                     | Whether participants noticed the disturbance (1 = Yes, 0 = No). |

---

## **2. Questionnaire Responses**

### **Intrinsic Motivation Inventory (IMI)**  
Responses from selected subscales of the IMI questionnaire:

| Variable Name                | Description                                                   |
|------------------------------|---------------------------------------------------------------|
| `IMI.effort`                  | Participant's perceived effort during the task.              |
| `IMI.interest_enjoyment`       | Enjoyment level of the task.                                 |
| `IMI.percieved_competence`     | Self-reported competence in performing the task.             |
| `IMI.total`                    | Combined IMI score across all subscales.                     |

### **Virtual Embodiment Questionnaire (VEQ)**  
Subjective ratings of virtual embodiment:

| Variable Name                | Description                                                   |
|------------------------------|---------------------------------------------------------------|
| `VEQ.agency`                  | Sense of control over the avatar's movements.                |
| `VEQ.body_ownership`           | Feeling of ownership over the virtual body.                  |
| `VEQ.change`                   | Perceived changes in bodily sensations.                      |
| `VEQ.total`                    | Combined VEQ score across all items.                         |

### **Simulator Sickness Questionnaire (SSQ)**  
Self-reported symptoms related to simulator sickness:

| Variable Name                | Description                                                   |
|------------------------------|---------------------------------------------------------------|
| `SSQ.disorientation`          | Disorientation symptoms (e.g., dizziness).                   |
| `SSQ.nausea`                   | Nausea symptoms.                                              |
| `SSQ.oculomotor`               | Oculomotor symptoms (eye strain, difficulty focusing).        |
| `SSQ.total`                    | Combined SSQ score across all subscales.                     |

---

## **3. Step Kinematics**

Step duration (rough estimate, requires further processing if it is to be used) and spatial parameters:

| Variable Name                | Description                                                   |
|------------------------------|---------------------------------------------------------------|
| `stepTimes.mean/sd/cv`        | Mean, standard deviation, and coefficient of variation of step duration (s). |
| `stepLengths.mean/sd/cv`      | Mean, standard deviation, and coefficient of variation of step length (m). |
| `stepWidths.mean/sd/cv`       | Mean, standard deviation, and coefficient of variation of step width (m). |

### **Centered Step Data**  
Values adjusted for participant+trial-specific means:

| Variable Name                      | Description                                                   |
|------------------------------------|---------------------------------------------------------------|
| `centered_stepLengths.mean/sd/cv`   | Centered values for step length (m).                              |
| `centered_stepWidths.mean/sd/cv`    | Centered values for step width (m).                               |

---

## **4. Speed Metrics**
Speed depends on stepTime (duration) and is therefore more of a rough estimate. Before it can be used properly, stepTime should be processed better.
| Variable Name                 | Description                                                   |
|-------------------------------|---------------------------------------------------------------|
| `speed.mean/sd/cv`             | Mean, standard deviation, and coefficient of variation of walking speed (m/s). |

---

## **5. Heel Strike Data**

Measurements related to heel strikes during walking:

| Variable Name                           | Description                                                   |
|-----------------------------------------|---------------------------------------------------------------|
| `heelStrikes.time.mean/sd/cv`            | Timing of heel strikes (mean, standard deviation, CV, in s).        |
| `heelStrikes.pos_x/y/z.mean/sd/cv`       | Heel position in X, Y, Z axes (m).  |
| `heelStrikes.rot_x/y/z.mean/sd/cv`       | Heel rotation in X, Y, Z axes (m).  |
| `heelStrikes.offset_x/y/z.mean/sd/cv`    | VFD offset in each direction (m).                         |
| `heelStrikes.magnitude.mean/sd/cv`       | Magnitude of the applied disturbance (m).                         |
| `heelStrikes.final_pos_x/y/z.mean/sd/cv` | Final foot poition after summing with the offset (m).                        |
s
---

## **6. Toe-Off Data**

Similar to heel strikes, toe-off data measures are available (note that these are not correctly filtered and are a rough estimate and require additional processing if they are to be used for further analyses):

| Variable Name                 | Description                                                   |
|-------------------------------|---------------------------------------------------------------|
| `toeOffs.time.mean/sd/cv`      | Timing of toe-offs (mean, standard deviation, CV, in s).            |
| `toeOffs.pos_x/y/z.mean/sd/cv` | Position at toe-off in X, Y, Z axes (m).                           |
| `toeOffs.rot_x/y/z.mean/sd/cv` | Rotational data at toe-off (degrees, in euler angles).                                    |

---

## **7. Relative Heel Strike Data**

These variable represent the positions of the heelStrikes and offset (VFD sizes) relative to the previous step with the same foot. They may be used to get a better understanding of how these metrics change from step to step:

| Variable Name                           | Description                                                   |
|-----------------------------------------|---------------------------------------------------------------|
| `relHeelStrikes.pos_x/y/z.mean/sd/cv`    | Relative position of heel strikes.                            |
| `relHeelStrikes.offset_x/y/z.mean/sd/cv` | Relative offset of foot positions.                            |

---

## **8. Target Stepping Performance**

Performance metrics related to stepping onto targets. All these metrics were calculated based on the final position of the virtual foot, after summing with the VFD offsets:

| Variable Name                    | Description                                                   |
|----------------------------------|---------------------------------------------------------------|
| `target.score.mean/sd/cv`         | Mean, standard deviation, and coefficient of variation of target scores. |
| `target.targetDist.mean/sd/cv`    | Distance from foot placement to target.                       |
| `target.rel_x/z.mean/sd/cv`       | Distance from foot placement to target along X and Z axes.        |

---

## **9. Difference Metrics**

The diff of each of the metrics (how much they changed from one step to the next, with both feet included):

| Variable Name                     | Description                                                   |
|-----------------------------------|---------------------------------------------------------------|
| `diffData.stepWidth.mean/sd/cv`    | Differences in step width across conditions.                  |
| `diffData.stepLength.mean/sd/cv`   | Differences in step length across conditions.                 |

---

This data dictionary provides a general understanding of the variables. For more detailed explanations and usage, please refer to the accompanying scripts and readme's in the repository.