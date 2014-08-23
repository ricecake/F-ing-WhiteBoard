F-ing-WhiteBoard
================

It's an F-ing Whiteboard app.


Installing:
-----------

0. ensure you have erlang installed, preferably from erlang solutions.
1. ./rebar get-deps
2. ./rebar compile generate


Using
-------

### To Run 
    ./rel/fingwb/bin/fingwb start

### To Attach to Running Process
    ./rel/fingwb/bin/fingwb attach

### To Stop 
    ./rel/fingwb/bin/fingwb stop

### To View
    http://127.0.0.1:8080

Alternate Method
------------
## First Step
  * Rename main directory from "F-ing Whiteboard" to "fingwb"

## To Start

* launch the app:

<!-- comment to stop list -->

    erl -pa `pwd`/deps/*/ebin -pa `pwd`/ebin

  * Start the Whiteboard (from within the fingwb directory):

<!-- comment to stop list -->
  
    application:ensure_all_started(fingwb).

### To View Status

    i().
    
### To Quit

    q().


