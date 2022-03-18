# ODsee
R Script to process batch of microbial growth curves for both OD and fluorescence

## Why ODsee?
This script was developped for the Samuel Lab in order to process large batch of growth curves files. The lab uses a Bioteck Cytation 5 reader and a Biospa incubator. The whole setup can ouput multiple excel reports with OD and fluorescence measurment over time. We often have several excel reading report files to process and needed a quick way to summarize and visualize our results.

## How to use it?
The script relies on information provided in the `Manifest.xlsx` file to look for the details of the experiment(s) you are trying to summarize.

In your folder of interest, create a `data` folder and place in it your `growth measurment` files and associated `metadata`. The same `metadata` file can be used for multiple `growth measurment` files if they are replicate.
