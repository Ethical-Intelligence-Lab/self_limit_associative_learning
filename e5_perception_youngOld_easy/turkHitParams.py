# This file must be named turkHitParams.py
# Use this file to specify the parameters of your turk hit.
#
#
# To use this file to post a hit, do the following (assumes you have the
# mechanical turk "boto" toolbox for python installed.

# 1. login to scorsese.wjh.harvard.edu
# 2. to post to the SANDBOX, open the terminal and run these commands:
#		$ if you're not already running python 2, change to python 2: source activate python2
# 		$ cd /Volumes/julian/boto3
#		$ python postHitSandbox.py ../../turk/experiments/jdf/e21_singleIdentity_perception_alternatives_easy/turkHitParams.py
#
# 3. to post to "actual" MTURK, open the terminal and run these commands
# 		$ cd /Volumes/julian/boto3
#		$ python postHitActual.py ../../turk/experiments/jdf/e21_singleIdentity_perception_alternatives_easy/turkHitParams.py

# alvarezlab, konklab
lab = "defreitas"
# Describe your Hit to Workers
titleForWorkers = "Duplication Judgment"
descriptionForWorkers = "React quickly to briefly presented shapes. Takes up to 20 min. Please only participate if you have normal or corrected-to-normal vision, and if you haven't done Duplication Judgment before"
keywordsForWorkers = "short, fun, easy"

# title for you (workers don't see this, but you will in the "manage hits" interface)
projectNameForRequesters = "jdf-singleIdentity-e21_singleIdentity_perception_alternatives_easy"

# Setting up your HI
# note that mturk prices are different for hits with more vs. less than 9 assignments
# so if you post less than 9 assignments, you cannot then "Add Assignments" later
# (e.g., we often test a small batch, then when things are working, add more
#  assignments to get a full batch. If you plan to do this, start with 10 assignments).
pay = '2.00' #must be a string
numAssignmentsToPost = 20 # turk calls this "Number of assignments per HIT"
minutesBeforeHitExpires = 600 # 10 hours
minutesForUsersToFinish = 40
minutesBeforeAutoApproved = 300 # 5 hours

# Additional Worker Qualifications
percentAssignmentsApprovedRequirement = 95
localeRequirement = "US"
qualificationID = "NONE" # or 'qualification code', e.g., 3M0XATTAEM8E2MNWTTR8MZJEE2Z957, see https://requestersandbox.mturk.com/qualification_types
notifyWorkerOfQualification = True # True/False, only matters if qualification code given
# Design Layout
urlForHIT = "https://scorsese.wjh.harvard.edu/turk/experiments/jdf/e21_singleIdentity_perception_alternatives_easy/landingPage.html"
frameHeight = 675
