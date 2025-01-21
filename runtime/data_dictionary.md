# **Data Dictionary for VFD Experiment**

This document provides an overview of the variables included in the dataset for the study titled:  
*"Visual Disturbances to Avatar Foot Position Increase Step-width Variability in Immersive VR Treadmill Walking."*  

Each variable is briefly described to help understand its purpose and how it was derived.

If you are using the interface, the sidebar contains many different ways to filter our complete dataset. All figures and calculations in the windows on the right are using the data that is selected in this sidebar. Data can be filtered based on participants, trials, and steps. Values that are selected in the selection boxes are **included** in the dataset. Removing all data may crash some of the calculations and with that the whole interface, so be aware of that. At the bottom there are some settings you can use to finetune the generation of the figures to get the sizes you require.


> NOTE: The initial calculation does an initial outlier selection based on rough threshold values for heelstrike positions. However, this selection is far from correctly identifying all the outliers, and so we implemented a manual outlier removal interface. To load our selected outliers and select them in the dataset, follow the instructions below.

> NOTE 2: Scroll down to find an explanation of the tracker files in the dataset and the variables found within them (column names).

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

## **Tracker Data Files**

**File Naming Convention**
All tracker files follow the pattern:

```
[object_name]_movement_T[trial_number].csv
```

Example filenames:

- `camera_movement_T001.csv` → Camera movement data for trial 1.
- `leftfoottracker_movement_T003.csv` → Left foot tracker data for trial 3.

The dataset includes multiple CSV files that contain tracking data recorded during the experiment. These files are named based on the tracked object and trial number (e.g., `camera_movement_T001.csv`). The tracked objects and corresponding variables within each file type are described below.

### **Tracked Objects and Variables**

#### **General Tracker Files:**
These files contain positional and rotational data over time for different tracked objects:

- **Tracked Objects:**  
  - `camera`
  - `controllerleft` (left controller)
  - `controllerright` (right controller)
  - `lefthandtracker`
  - `righthandtracker`
  - `hiptracker`
  - `leftfoottracker`
  - `rightfoottracker`
  - `treadmillleft`, `treadmillright`, `treadmillrightback` (used to determine treadmill position in the virtual environment)

- **Variables:**
  ```plaintext
  time, pos_x, pos_y, pos_z, rot_x, rot_y, rot_z
  ```

  - `time`: Timestamp of the recorded frame.
  - `pos_x, pos_y, pos_z`: Position coordinates of the tracked object in the virtual environment (m).
  - `rot_x, rot_y, rot_z`: Rotation angles of the tracked object (degrees, in euler angles).

---

#### **Step Target Tracker Files:**
These files provide information about participants' foot placements relative to visual targets.

- **Filename Example:** `steptargetsmanager_targetsteps_T001.csv`
- **Variables:**
  ```plaintext
  time, foot, score, destroyed, target_x, target_y, target_z, 
  foot_x, foot_y, foot_z, foot_rot_x, foot_rot_y, foot_rot_z, foot_rot_w
  ```

  - `time`: Timestamp of the target event.
  - `foot`: Indicates which foot was used (left/right).
  - `score`: Points awarded based on target accuracy.
  - `destroyed`: Boolean value; `TRUE` if the target was missed (this never happened).
  - `target_x, target_y, target_z`: Target position in the virtual environment.
  - `foot_x, foot_y, foot_z`: (Disturbed) foot position displayed in VR.
  - `foot_rot_x, foot_rot_y, foot_rot_z, foot_rot_w`: Foot orientation in VR.

**Note:** The `foot_x`, `foot_y`, and `foot_z` values represent the disturbed foot position shown in VR, which may differ from the actual foot position if VFD was enabled.

---

#### **Eye Tracker Files:**
Eye tracking data recorded throughout the trials.

- **Filename Example:** `eyetracking_EyeGaze_T001.csv`
- **Variables:**
  ```plaintext
  time, collider, hit_pos_x, hit_pos_y, hit_pos_z, pos_x, pos_y, pos_z, 
  dir_x, dir_y, dir_z, left_openness, left_diam, left_pupil_x, left_pupil_y, 
  left_validity, right_openness, right_diam, right_pupil_x, right_pupil_y, 
  right_validity, combined_conv_dist, combined_conv_validity, combined_openness
  ```

  - `time`: Timestamp of the gaze event.
  - `collider`: Object hit by gaze ray.
  - `hit_pos_x, hit_pos_y, hit_pos_z`: Coordinates of the gaze hit point.
  - `pos_x, pos_y, pos_z`: Position of the eye-tracking device in the virtual environment.
  - `dir_x, dir_y, dir_z`: Direction of the gaze.
  - `left_openness, right_openness`: Eye openness values.
  - `left_diam, right_diam`: Pupil diameters.
  - `left_pupil_x, left_pupil_y, right_pupil_x, right_pupil_y`: Pupil center positions.
  - `left_validity, right_validity`: Validity flags for left and right eye tracking.
  - `combined_conv_dist, combined_conv_validity, combined_openness`: Combined gaze convergence metrics.

**Note:** This data was not used in the final analysis but is included for completeness.

---

#### **Disturbance Noise Tracker Files:**
These files track the disturbances applied to foot positions during the experiment.

- **Filename Example:** `leftfoot_disturbance_noise_T001.csv`
- **Variables:**
  ```plaintext
  time, is_active, is_grounded, offset_x, offset_y, offset_z
  ```

  - `time`: Timestamp of the disturbance event.
  - `is_active`: Boolean indicating whether the disturbance is currently applied.
  - `is_grounded`: Boolean indicating whether the foot is below a certain threshold and considered grounded.
  - `offset_x, offset_y, offset_z`: Offset values applied to the foot position in the VR environment.

---