# ABAP Lockwaiter
A utility class to check if an object is locked and wait until it is released.

## Motivation
Inspired by [this blog](https://blogs.sap.com/2016/09/14/waiting-for-lock-objects-to-release-using-lock-modes-u-v-w/) about the need to wait for lock objects to release,
I created a utility class using the function ENQUEUE_XXX with parameter `_wait = abap_true` and lock mode performing a collision check (as explained [here](https://help.sap.com/saphelp_nwes72/helpdata/en/56/2639d058ea4843ab4ada04b466951b/frameset.htm)).
Called like this, the function waits for 5 seconds with a retry of every one second by default (these parameters can be changed using RZ11 transaction).

**Warning: the standard function performs a WAIT, so a database commit is invoked.**

## Quick Start
```abap
* First, create an instance passing the table representing the object to check
DATA(lock_waiter) = NEW ztbox_cl_lockwaiter( 'VBAK' ).

* Call SET_OBJECT to set the keys identifying an object
lock_waiter->set_object( VALUE vbak( vbeln = '123' ) ).

* Call method WAIT to check if the object is released or is locked, 
* and the methods IS_LOCKED/LOCK_MESSAGE to get a message about the lock in the latter case.
lock_waiter->wait( ).
IF lock_waiter->is_locked( ).
  WRITE: lock_waiter->lock_message( ).
ENDIF.

* If the optional parameter `i_endlessly` is true, ABAP session will remain waiting for the object to be released 
* for an unlimited period of time (until timeout for foreground processing)
* otherwise the default standard behaviour set a wait of 5 seconds.
lock_waiter->wait( i_endlessly = abap_true ).
WRITE: |If this statemet is executed, the object is definitely not locked.|.
```

You can also use this tool to lock or unlock objects using LOCK and UNLOCK methods.

## Installation
Install this project using [abapGit](https://abapgit.org/) ![abapGit](https://docs.abapgit.org/img/favicon.png)

## Dependencies
In order to use this tool you must first install these packages:
* [ABAP Dynamic Function Module](https://github.com/zenrosadira/abap-tbox-fmoduler)
