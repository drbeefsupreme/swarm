::  TODO
::  add peeks
::  make random factor in update a gaussian rather than uniform
::  use lagoon
::
/-  *swarm
/+  default-agent, dbug
|%
+$  versioned-state
    $%  state-0
    ==
::
++  state-0  $:  %0
                 phase=[pos=loc vel=loc bes=[pos=loc val=@rd]]
                 group-bes=[pos=loc val=@rd]
                 swarm=(set ship)
                 steps=(map @ud phases)
                 started=_|  :: has the run started yet
                 coordinator=@p
             ==
::
+$  card   card:agent:gall
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
  ~&  >  '%swarm init'
  =*  pos  pos.phase.state
  =*  vel  vel.phase.state
  =*  bes  bes.phase.state
  =*  gbs  group-bes.state
  =/  rng  ~(. og eny.bowl)
  =^  rapx  rng  (rads:rng 1.000)
  =^  rapy  rng  (rads:rng 1.000)
  =^  ravx  rng  (rads:rng 10)
  =^  ravy  rng  (rads:rng 10)
  =.  pos   [(sub:rd (sun:rd rapx) .~500) (sub:rd (sun:rd rapy) .~500)]
  =.  vel   [(sub:rd (sun:rd ravx) .~5) (sub:rd (sun:rd ravy) .~5)]
  =.  bes   [pos (objective:hc pos)]
  =.  gbs   [pos (objective:hc pos)]
  `this
  ::
++  on-save
  ^-  vase
  !>(state)
::
++  on-load
  ~&  >  'on-load'
  on-load:def
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  =^  cards  state
    ?+    mark  (on-poke:def mark vase)
        %swarm-action
      =+  !<(poke=action vase)
      (on-action:hc poke)
    ==
  [cards this]
::
::  TODO: subscription of phase for coordinator
++  on-watch
  |=  =path
  ~&  'swarm on-watch'
  ^-  (quip card _this)
  ?+    path  (on-watch:def)
    [%phase ~]  ::
      ::?>  (~(has in ships) src.bowl)
      `this
  ==
::
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  =/  who  `@`our.bowl
  ?+    wire  (on-agent:def wire sign)
      [%some %wire ~]
    ?+    -.sign  (on-agent:def wire sign)
        %watch-ack
      ?~  p.sign
        ((slog '%swarm: Subscribe succeeded!' ~) `this)
      ((slog '%swarm: Subscribe failed!' ~) `this)
    ::
      ::    %kick
      ::  %-  (slog '%swarm-coord: Got kick, resubscribing...' ~)
      ::  :_  this
      ::  :~  [%pass /todos %agent [src.bowl %todo] %watch /updates]
      ::  ==
    ::
      ::    %fact
      ::  ?+    p.cage.sign  (on-agent:def wire sign)
      ::      %swarm-update
      ::    ~&  !<(update q.cage.sign)  :: TODO why is this not working?
      ::    `this
      ::  ==
    ==
  ==

++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--
::  helper core
::
|_  bowl=bowl:gall
::
++  on-action
  |=  =action
  ^-  (quip card _state)
  ?-  -.action
    %print-state  print-state
    ::%update-all   update-all
    ::%update-ship  (update-ship +.action)
    %join-swarm   (join-swarm +.action)
 ::   %leave-swarm  (leave-swarm +.action)
    %update-self  update-self
    %start-run    start-run
    %update-best  (update-best +.action)
    ::%update-ship-phase  (update-ship-phase +.action)
  ==
::
++  print-state
  ^-  (quip card _state)
  ~&  >>  state
  ~&  >>>  bowl
  `state
::
++  start-run
  ^-  (quip card _state)
  ~|  'start-run failed'
  ?>  =(src.bowl coordinator.state)
  `state(started %.y)
::
++  join-swarm
  |=  =ship  :: ship of swarm coordinator
  ^-  (quip card _state)
  ~|  'join swarm failed'
  :_  state
  :~  :*  %pass  /phase  %agent  [ship %swarm-coord]  %poke  %swarm-coord  !>([%join-swarm ~])
  ==  ==
::
::  ++  leave-swarm
::    |=  =ship
::    ^-  (quip card _state)
::    ~|  'leave swarm failed'
::    :-  ~
::    ?.  (~(has in swarm.state) ship)
::      state
::    state(swarm (~(del in ships) ship))
::
++  update-self
  ^-  (quip card _state)
  ?>  |(=(src.bowl coordinator.state) =(src.bowl our.bowl))
  =.  phase.state  %-  update-phase  phase.state
::  =.  state  (update-ship-phase our.bowl phase)
  ::  ~&  >>>  ['phase' !>(phase)]
  :_  state
  :~  [%give %fact ~[/phase] %swarm-update !>([phase+phase.state])]
  ==
::
++  update-best
  |=  [pos=loc val=@rd]
  ^-  (quip card _state)
  =.  group-bes.state  [pos val]
  `state
::
::  ++  update-ship
::    |=  =ship
::    ^-  (quip card _state)
::    ~|  'update-ship failed'
::    =/  phase  %^    update-phase
::                   (~(got by pos.state) ship)
::                 (~(got by vel.state) ship)
::               (~(got by bes.state) ship)
::    =.  state  (update-ship-phase ship phase)
::    ::  if new objective is better than group best, update group best
::    =.  group-bes.state  ?.  (gte:rd val.bes.phase val.group-bes.state)
::                           group-bes.state
::                         [pos.phase val.bes.phase]
::    `state
::
::  ++  update-all
::    ^-  (quip card _state)
::    ~|  'update-all failed'
::    =/  i=@  0
::    |-
::    ?:  =(i ship-num.state)
::      ~&  >  group-bes.state
::      `state  :: fix when steps updated
::      ::`state(steps +(steps))
::    =.  state  +:(update-ship `@p`i)  ::  TODO this looks wrong
::    $(i +(i))
::
::  after here are helper functions for various cards
++  objective
  |=  loc
  ^-  @rd
  (sub:rd .~0 (add:rd (mul:rd x x) ;:(mul:rd x x y y)))  ::  -(x^2+x^2y^2)
::
::  ++  update-ship-phase
::    |=  [=ship =phase]
::    ^+  state
::    ~|  'update-ship-phase failed'
::    =.  pos.state  (~(put by pos.state) ship pos.phase)
::    =.  vel.state  (~(put by vel.state) ship vel.phase)
::    =.  bes.state  (~(put by bes.state) ship bes.phase)
::    state
::
++  update-phase
  |=  paz=^phase
  ^+  paz
  ~|  'update-phase failed'
  ::  x_i - position, v_i - velocity, b_i - best position so far
  ::  b - best group position, c_1 - cognitive coefficient
  ::  c_2 - social coefficient, r_1, r_2 random number between 0 and 1
  ::  w - inertia weight
  =/  rng  ~(. og eny.bowl)
  =^  ran1  rng  (rads:rng 1.000)
  =^  ran2  rng  (rads:rng 1.000)
  =/  [px=@rd py=@rd vx=@rd vy=@rd bx=@rd by=@rd gbx=@rd gby=@rd]
    :*  x.pos.paz  y.pos.paz
        x.vel.paz  y.vel.paz
        x.pos.bes.paz  y.pos.bes.paz
        x.pos.group-bes.state
        y.pos.group-bes.state
    ==
  =/  [c1=@rd c2=@rd w=@rd r1=@rd r2=@rd]
    :*  .~0.3  .~0.2  .~0.12
        (div:rd (sun:rd ran1) .~1000)
        (div:rd (sun:rd ran2) .~1000)
    ==
  ::  velocity is updated according to the following rule:
  ::  v_i(t+1) = w * v_i(t) + c_1 * r_1(t) * (b_i(t) - x_i(t))
  ::  + c_2 * r_2(t) * (b(t) - x_i(t))
  ::
  =/  [dif=loc bes-dif=loc]
    :-  [(sub:rd bx px) (sub:rd by py)]
    [(sub:rd gbx px) (sub:rd gby py)]
  =/  newvel=loc   :-  ;:  add:rd
                       (mul:rd w vx)
                       ;:(mul:rd c1 r1 x.dif)
                       ;:(mul:rd c2 r2 x.bes-dif)
                       ==
                   ;:  add:rd
                       (mul:rd w vy)
                       ;:(mul:rd c1 r1 y.dif)
                       ;:(mul:rd c2 r2 y.bes-dif)
                       ==
  =/  newpos=loc   :-  (add:rd x.pos.paz x.newvel)
                   (add:rd y.pos.paz y.newvel)
  =/  objectives=[prev=@rd cur=@rd]  :-  (objective pos.bes.paz)
                                     (objective pos.paz)
  ?:  (gth:rd prev.objectives cur.objectives)  :: is the previous best better than the current value?
    [newpos newvel bes.paz]  :: best does not get updated
  [newpos newvel [newpos cur.objectives]]
--
