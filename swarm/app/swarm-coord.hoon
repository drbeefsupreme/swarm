/-  *swarm
/+  default-agent, dbug
|%
+$  versioned-state
    $%  state-0
    ==
::
++  state-0  $:  %0
                 config=(map @ud phases)
                 group-best=[pos=loc val=@rd]
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
  =.  group-best.state  [[.~0 .~0] .~-15593972089999]
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
  ~&  '%swarm-coord on-agent'
  ?+    wire  (on-agent:def wire sign)
      [%some %wire ~]
    ~&  'some-wire'
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
        ~&  '%swarm-coord %fact received'
        =/  paz=phase  +:!<(update q.cage.sign)
        =.  group-best.state  ?.  (gte:rd val.bes.paz val.group-best.state)
                                group-best.state
                              [pos.paz val.bes.paz]
        =.  next-phase.state  (~(put by next-phase.state) src.bowl paz)
        `this
      ==
    ==
  ==
::
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
    %join-swarm   join-swarm
    %leave-swarm  leave-swarm
    %start-run    start-run
    %step         step
    %check-next   check-next
  ==
::
++  join-swarm
  ^-  (quip card _state)
  ~|  'join-swarm failed'
  :_  state(swarm (~(put in swarm) src.bowl))
  =/  who  `@`src.bowl
  :~  :*  %pass  /some/wire  %agent  [src.bowl %swarm]  %watch  /phase
  ::
  ==  ==
::
++  check-next
  ^-  (quip card _state)
  ~|  'check-next failed'
  =/  left=(set ship)  (~(dif in swarm.state) ~(key by next-phase.state))
  ?~  left
    ~&  >>  'got response from all swarmers'
    =.  config.state  (~(put by config.state) cur-step next-phase.state)
    =.  next-phase.state  *phases
    =/  swarm-list  ~(tap in swarm)
    =|  cards=(list card)
    |-
    ?^  swarm-list
      =.  cards  %+  weld  cards
                 :~  ^-  card  ::  why won't it compile without this?
                     :*  %pass  /best/wire  %agent  [i.swarm-list %swarm]  %poke
                     %swarm-action  !>([%update-best group-best.state])
                 ==  ==
      $(swarm-list t.swarm-list)
    [cards state]
  ~&  >>  'not all swarmers have responded, do nothing'
  `state
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
  `state(start %.y)
::
::  TODO: make a thread that waits until it has responses from everybody before
::  updating config.state and then do it again
++  step
  ^-  (quip card _state)
  ~|  'step failed'
  ?>  =(src.bowl our.bowl)
  =/  swarm-list=(list ship)  ~(tap in swarm.state)
  =|  cards=(list card)
  |-
  ?~  swarm-list
    [cards state(cur-step +(cur-step))]
  =/  sam=@p  i.swarm-list
  =.  cards  %+  weld  cards
             :~  ^-  card  ::  why won't it compile without this?
                 :*  %pass  /some/wire  %agent  [sam %swarm]  %poke
                 %swarm-action  !>(~[%update-self])
             ==  ==
  $(swarm-list t.swarm-list)
--
