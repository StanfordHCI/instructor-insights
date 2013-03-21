Instructor Insights
============

How well are students in your peer-assessed class grading? How would it compare to grades that staff would award? Are there particular questions that are harder or easier for students to grade? How can I improve grading?

These scripts (will soon be) a collection of tools that help instructor answers questions like these. These scripts is aimed at TAs/instructors for a MOOC that runs on Coursera.

Current Capabilities
--------------------

Right now, the script produces three graphs. Here are sample graphs from the first three assignments of the HCI class. 

How do I get these charts for my own class?
----

It's easy. Use our script!

1. Install R. It's easy to do for most platforms. [Download R](http://cran.cnr.berkeley.edu/). Then, on most platforms, you should just be able to double-click the installer.

2. Once you have installed R, create a new directory. We're going to say it's called `instructor-insights`. Copy the `grading-accuracy.R` file to this folder. (You can also `git clone` this repository to do this.)

3. Go to your Coursera class page, and download your class' Peer Assessment data. The zip file you download for each assignment has two files in it: `submissions.csv` and `evaluations.csv`. Copy these files to a directory under `instructor-insights`. If you `git clone` this repository, we create placeholder folders for three assignments.

4. Open up a terminal, and navigate to the `instructor-insights` folder. Then type:
	Rscript grading-accuracy.R
 This will create graphs like ours in the `output` folder.

Support and Questions
-----

Ask questions and report bugs at the [instructor-insights](https://mailman.stanford.edu/mailman/listinfo/instructor-insights) mailing group.
