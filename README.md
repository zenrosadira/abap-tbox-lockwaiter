# ABAP Lockwaiter
A utility class to check if an object is locked and wait until it is released.

## Motivation
Inspired by [this blog](https://blogs.sap.com/2016/09/14/waiting-for-lock-objects-to-release-using-lock-modes-u-v-w/) about the need to wait for lock objects to release,
I created a utility class using the function ENQUEUE_XXX with parameter `_wait = abap_true` and lock mode performing a collision check (as explained [here](https://help.sap.com/saphelp_nwes72/helpdata/en/56/2639d058ea4843ab4ada04b466951b/frameset.htm)).
Called like this, the function waits for 5 seconds with a retry of every one second by default (these parameters can be changed using RZ11 transaction).
** Warning: the standard function performs a WAIT, so database commit is invoked.**

## Usage
Create an object of class ZTBOX_CL_LOCKWAITER passing the table representing the object to check, and call method SET OBJECT to set keys that identify an object
```
DATA(lock_waiter) = NEW ztbox_cl_lockwaiter( 'VBAK' ).
lock_waiter->set_object( VALUE vbak( vbeln = '123' ) ).
```

Call method WAIT_FOR to check if the object is free or is locked
```
lock_waiter->wait_for( i_endlessly = abap_true ).
```
If the optional parameter `i_endlessly` is true, ABAP session will remain waiting for the object to be released for an unlimited period of time, otherwise the default standard behaviour set a wait of 5 seconds.

You can also use this tool to lock or unlock objects using LOCK and UNLOCK methods.

## Installation
Install this project using [abapGit](https://abapgit.org/) ![abapGit](https://docs.abapgit.org/img/favicon.png)
