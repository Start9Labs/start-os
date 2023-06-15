import { Runtime } from "./Runtime.mjs"

// @ts-check
new Runtime()

/**

So, this is going to be sent into a running comtainer along with any of the other node modules that are going to be needed and used.

Once the container is started, we will go into a loading/ await state.
This is the init system, and it will always be running, and it will be waiting for a command to be sent to it.

Each command will be a stopable promise. And an example is going to be something like an action/ main/ or just a query into the types.

A command will be sent an object which are the effects, and the effects will be things like the file system, the network, the process, and the os.


 */
// So OS Adapter
// ==============

/**
* Why: So when the we call from the os we enter or leave here? 
    
 */

/**
Command: This is a command that the 

There are 
 */

/**
TODO:
Should I seperate those adapter in/out?
 */
