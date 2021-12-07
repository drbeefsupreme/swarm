/-  *swarm
/+  default-agent, dbug
|%
+$  versioned-state
    $%  state-0
    ==
::
++  state-0  $:  %0
                 config=(map @ud phases)
                 best=(map @ud [pos=loc val=@rd])
                 swarm=(set ship)
                 cur-step=@ud
                 next-phase=phases
                 start=_|
             ==
::
+$  card  card:agent:gall
--
::
%-  agent:dbug
=|  state-0
=*  state  -
^-  agent:gall
=<
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
    hc    ~(. +> bowl)
::
++  on-init
  ^-  (quip card _this)
  ~&  >>  '%swarm-coord init'
  `this
::
++  on-save
  ~&  >  '%swarm-coord on-save'
  ^-  vase
  !>(state)
::
++  on-load
  ~&  >  '%swarm-coord on-load'
  on-load:def
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  =^  cards  state
    ?+    mark  (on-poke:def mark vase)
        %swarm-coord
      =+  !<(poke=coord vase)
      (on-coord:hc poke)
      ::
    ==
  [cards this]
::
++  on-watch  on-watch:def
  ::  TODO: subscription of new step for swarm members
++  on-leave  on-leave:def
++  on-peek  on-peek:def
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  ?+    wire  (on-agent:def wire sign)
      [%phase ~]
    ?+    -.sign  (on-agent:def wire sign)
        %watch-ack
      ?~  p.sign
        ((slog '%swarm-coord: Subscribe succeeded!' ~) `this)
      ((slog '%swarm-coord: Subscribe failed!' ~) `this)
    ::
      ::    %kick
      ::  %-  (slog '%swarm-coord: Got kick, resubscribing...' ~)
      ::  :_  this
      ::  :~  [%pass /todos %agent [src.bowl %todo] %watch /updates]
      ::  ==
    ::
        %fact
      ?+    p.cage.sign  (on-agent:def wire sign)
          %swarm-update
        ~&  !<(update q.cage.sign)  :: TODO why is this not working?
        `this
      ==
    ==
  ==
::
++  on-arvo  on-arvo:def
++  on-fail  on-fail:def
--
::
|_  bowl=bowl:gall
::
++  on-coord
  |=  =coord
  ^-  (quip card _state)
  ?-  -.coord
    %join-swarm  join-swarm
    %leave-swarm  leave-swarm
    %start-run  start-run
  ==
::
++  join-swarm
  ^-  (quip card _state)
  ~|  'join-swarm failed'
  :_  state(swarm (~(put in swarm) src.bowl))
  :~  :*  %pass  /phase  %agent  [src.bowl %swarm]  %watch  /some/path
  ==  ==
::  =/  swarm-list=(list ship)  ~(tap in swarm)
::  =|  cards=(list card)
::  |-
::  ?~  swarm-list
::    [cards state(swarm (~(put in swarm) src.bowl))]
  ::  =.  cards  %+  weld
  ::               cards
  ::             :~  :*  %pass  /some/wire  %agent
  ::                     [src.bowl %swarm] %watch
  ::                     /phase
  ::             ==  ==

::
++  leave-swarm
  ::  TODO: remove from subscribers
  ^-  (quip card _state)
  `state(swarm (~(del in swarm) src.bowl))
::
++  start-run
  ^-  (quip card _state)
  ~|  'start-run failed'
  ?>  =(src.bowl our.bowl)  :: only the coordinator should be able to start
  ::  TODO: send pokes to all swarm members letting them know the run has begun.
  ::  this should probably be done as a thread, and the run doesn't actually start
  ::  until getting acks from everybody in the swarm
  `state(start %.y)
--
