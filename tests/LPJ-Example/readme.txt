Example of the folder structure required by rLPJGUESS to run.

The results have been created with the code provided in appendix 2 of Bagnara et al., and include specifically the outputs from the simulation on F. sylvatica only.

More in detail:

- in the LPJrunTest folder, you will find the weather input data (.nc 
files, not empty) to run the model, the location file (gridlist.txt, not 
empty), and the model executable (the working version of my computer). 
You will have to re-create an empty executable called "guess" if you 
work on a mac, the package will look specifically for that.

- in the crudata folder there are more input files (originally large, 
now empty). Their names fit the ones provided in the appendix.

- in the runDirectory folder there are all the files created during the 
model run on beech alone: the .ins file storing settings and parameter 
values, the location file, the model log file, and the output folder.

- the outDirectory folder contains the actual model outputs, created 
before they are read by R (.out files, a normal text editor will do, or 
a read.table from R)