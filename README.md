# ABAP Lockwaiter
A utility class to check if an object is locked

## Motivation
Inspired by [this blog](https://blogs.sap.com/2016/09/14/waiting-for-lock-objects-to-release-using-lock-modes-u-v-w/) about the need to wait for lock objects to release,
I created a utility class using the function ENQUEUE_XXX with parameter `_wait = abap_true` and check lock mode performing collision check (as explained [here](https://help.sap.com/saphelp_nwes72/helpdata/en/56/2639d058ea4843ab4ada04b466951b/frameset.htm)).