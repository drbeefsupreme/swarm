::  TODO
::  add networking
::  make random factor in update a gaussian rather than uniform
::  use lagoon
/+  default-agent, dbug
|%
+$  versioned-state
    $%  state-0
    ==
::
++  state-0  $:  %0
                 pos=(map @p [x=@rd y=@rd])
                 vel=(map @p [x=@rd y=@rd])
                 bes=(map @p [pos=[x=@rd y=@rd] val=@rd])
                 group-bes=[pos=[x=@rd y=@rd] val=@rd]
                 ships=(list ship)
                 steps=@ud
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
    default  ~(. (default-agent this %|) bowl)
    hc       ~(. +> bowl)
::
++  on-init
  ^-  (quip card _this)
  =/  ship-num=@  100
  =/  i=@  0
  =*  pos  pos.state
  =*  vel  vel.state
  =*  bes  bes.state
  =/  rng  ~(. og 'swarm')
  |-
  ?:  =(i ship-num)
    ~&  '%swarm init'
    =.  group-bes.state  (~(got by bes) `@p`0)
    `this
  =^  rapx  rng  (rads:rng 1.000)      :: for individual best
  =^  rapy  rng  (rads:rng 1.000)
  =^  ravx  rng  (rads:rng 10)
  =^  ravy  rng  (rads:rng 10)
  =/  startpx=@rd  (sub:rd (sun:rd rapx) .~500)
  =/  startpy=@rd  (sub:rd (sun:rd rapy) .~500)
  =/  startvx=@rd  (sub:rd (sun:rd ravx) .~5)
  =/  startvy=@rd  (sub:rd (sun:rd ravy) .~5)
  %=  $
    i    +(i)
    pos  (~(put by pos) `@p`i [startpx startpy])
    vel  (~(put by vel) `@p`i [startvx startvy])
    bes  (~(put by bes) `@p`i [[startpx startpy] (objective:hc [startpx startpy])])
  ==
  ::
++  on-save
  ^-  vase
  !>(state)
++  on-load
  ~&  >  'on-load'
  on-load:default
++  on-poke
  |=  [=mark =vase]
  |^
  ^-  (quip card _this)
  ~&  group-bes.state
  `this(state (update state))
::  (on-poke:default mark vase)
  ::`this
  ::  ?+    mark  (on-poke:def mark vase)
  ::      %noun
  ::    ~&  'noun'
  ::  ==
  ++  update
    |=  a=_state
    ^+  state
    =*  bes  bes.a
    =*  pos  pos.a
    =*  vel  vel.a
    =*  grp  group-bes.a
    =/  i=@  0
    |-
    ?:  =(i 10)
      a(steps +(steps))
    =/  phase  %^    update-phase
                   (~(got by pos) `@p`i)
                 (~(got by vel) `@p`i)
               (~(got by bes) `@p`i)
    ?:  (gte:rd val.bes.phase val.grp)
      ::  if new objective is better than group best, update group best
      %=  $
        i    +(i)
        pos  (~(put by pos) `@p`i pos.phase)
        vel  (~(put by vel) `@p`i vel.phase)
        bes  (~(put by bes) `@p`i bes.phase)
        grp  [pos.phase val.bes.phase]
      ==
    %=  $
      i    +(i)
      pos  (~(put by pos) `@p`i pos.phase)
      vel  (~(put by vel) `@p`i vel.phase)
      bes  (~(put by bes) `@p`i bes.phase)
    ==
  ::
  ++  update-phase
    |=  [pos=[x=@rd y=@rd] vel=[x=@rd y=@rd] bes=[pos=[x=@rd y=@rd] val=@rd]]
    ^-  [pos=[x=@rd y=@rd] vel=[x=@rd y=@rd] bes=[pos=[x=@rd y=@rd] val=@rd]]
    ::  x_i - position, v_i - velocity, b_i - best position so far
    ::  b - best group position, c_1 - cognitive coefficient
    ::  c_2 - social coefficient, r_1, r_2 random number between 0 and 1
    ::  w - inertia weight
    =*  px  x.pos
    =*  py  y.pos
    =*  bx  x.pos.bes
    =*  by  y.pos.bes
    =*  vx  x.vel
    =*  vy  y.vel
    =*  gbx  x.pos.group-bes.state
    =*  gby  y.pos.group-bes.state
    =/  c1=@rd  .~0.3
    =/  c2=@rd  .~0.2
    =/  rng  ~(. og (add steps.state `@`x.pos))
    =^  ran1  rng  (rads:rng 1.000)
    =^  ran2  rng  (rads:rng 1.000)
    =/  r1=@rd  (div:rd (sun:rd ran1) .~1000)
    =/  r2=@rd  (div:rd (sun:rd ran2) .~1000)
    =/  w=@rd  .~0.12
    ::  velocity is updated according to the following rule:
    ::  v_i(t+1) = w * v_i(t) + c_1 * r_1(t) * (b_i(t) - x_i(t))
    ::  + c_2 * r_2(t) * (b(t) - x_i(t))
    =/  dif=[x=@rd y=@rd]     [(sub:rd bx px) (sub:rd by py)]
    =/  bes-dif=[x=@rd y=@rd]  [(sub:rd gbx px) (sub:rd gby py)]
    =/  newvel=[x=@rd y=@rd]  :-  (add:rd (add:rd (mul:rd w vx) (mul:rd (mul:rd c1 r1) x.dif)) (mul:rd (mul:rd c2 r2) x.bes-dif))
                              (add:rd (add:rd (mul:rd w vy) (mul:rd (mul:rd c1 r1) y.dif)) (mul:rd (mul:rd c2 r2) y.bes-dif))
    =/  newpos=[x=@rd y=@rd]  :-  (add:rd x.pos x.newvel)
                            (add:rd y.pos y.newvel)
    =/  objectives=[prev=@rd cur=@rd]  :-  (objective:hc pos.bes)
                                     (objective:hc pos)
    ?:  (gth:rd prev.objectives cur.objectives)  :: is the previous best better than the current value?
      [newpos newvel bes]  :: best does not get updated
    [newpos newvel [newpos cur.objectives]]
  --
::
++  on-watch  on-watch:default
++  on-leave  on-leave:default
++  on-peek   on-peek:default
++  on-agent  on-agent:default
++  on-arvo   on-arvo:default
++  on-fail   on-fail:default
--
::  helper core
|_  bowl=bowl:gall
++  objective
  |=  [x=@rd y=@rd]
  ^-  @rd
  (sub:rd .~0 (add:rd (mul:rd x x) (mul:rd y y)))  ::  -(x^2+y^2)
--
