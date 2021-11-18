::  TODO
::  add networking
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
                 pos=(map @p loc)
                 vel=(map @p loc)
                 bes=(map @p [pos=loc val=@rd])
                 phases=(map @p phase)
                 group-bes=[pos=loc val=@rd]
                 ships=(set ship)
                 ship-num=@ud
                 steps=@ud
             ==
::
+$  card   card:agent:gall
+$  loc    [x=@rd y=@rd]
+$  phase  [pos=loc vel=loc bes=[pos=loc val=@rd] step=@ud]
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
  =.  ship-num.state  10
  =/  i=@  0
  =*  pos  pos.state
  =*  vel  vel.state
  =*  bes  bes.state
  =*  gbs  group-bes.state
  =/  rng  ~(. og eny.bowl)
  |^
  ?:  =(i ship-num.state)
    `this
  =.  ships.state  (~(put in ships) `@p`i)
  =^  rapx  rng  (rads:rng 1.000)
  =^  rapy  rng  (rads:rng 1.000)
  =^  ravx  rng  (rads:rng 10)
  =^  ravy  rng  (rads:rng 10)
  =/  startp=loc   [(sub:rd (sun:rd rapx) .~500) (sub:rd (sun:rd rapy) .~500)]
  =/  startv=loc   [(sub:rd (sun:rd ravx) .~5) (sub:rd (sun:rd ravy) .~5)]
  =/  obj=@rd    (objective:hc startp)
  ?:  =(i 0)
    =.  gbs  [startp obj]  (rec startp startv obj)
  ?:  (gte:rd obj val.gbs)
    =.  gbs  [startp obj]  (rec startp startv obj)
  (rec startp startv obj)
  ++  rec
    |=  [startp=loc startv=loc obj=@rd]
    %=  ^$
      i    +(i)
      pos  (~(put by pos) `@p`i startp)
      vel  (~(put by vel) `@p`i startv)
      bes  (~(put by bes) `@p`i [startp obj])
    ==
  --
  ::
++  on-save
  ^-  vase
  !>(state)
++  on-load
  ~&  >  'on-load'
  on-load:def
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
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-agent  on-agent:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--
::  helper core
|_  bowl=bowl:gall
++  on-action
  |=  =action
  ^-  (quip card _state)
  ?-  -.action
    %print-state  print-state
    %update-all   [~ (update-all state)]
    %update-ship  [~ (update-ship state +.action)]
  ==
::
++  print-state
  ^-  (quip card _state)
  ~&  >>  state
  ~&  >>>  bowl
  `state
::
++  objective
  |=  loc
  ^-  @rd
  (sub:rd .~0 (add:rd (mul:rd x x) ;:(mul:rd x x y y)))  ::  -(x^2+x^2y^2)
::
++  update-ship
  |=  [a=_state =ship]
  ^+  a
  =*  bes  bes.a
  =*  pos  pos.a
  =*  vel  vel.a
  =*  grp  group-bes.a
  =/  phase  %^    update-phase
                 (~(got by pos) ship)
               (~(got by vel) ship)
             (~(got by bes) ship)
  =.  pos  (~(put by pos) ship pos.phase)
  =.  vel  (~(put by vel) ship vel.phase)
  =.  bes  (~(put by bes) ship bes.phase)
  ::  if new objective is better than group best, update group best
  =.  grp  ?.  (gte:rd val.bes.phase val.grp)
           grp
         [pos.phase val.bes.phase]
  a
::
++  update-all
  |=  a=_state
  ^+  a
  =/  i=@  0
  |-
  ?:  =(i ship-num.state)
    ~&  >  group-bes.a
    a(steps +(steps))
  %=  $
    a  (update-ship a `@p`i)
    i  +(i)
  ==
::
++  update-phase
  |=  [pos=loc vel=loc bes=[pos=loc val=@rd]]
  ^-  [pos=loc vel=loc bes=[pos=loc val=@rd]]
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
  =/  rng  ~(. og eny.bowl)
  =^  ran1  rng  (rads:rng 1.000)
  =^  ran2  rng  (rads:rng 1.000)
  =/  r1=@rd  (div:rd (sun:rd ran1) .~1000)
  =/  r2=@rd  (div:rd (sun:rd ran2) .~1000)
  =/  w=@rd  .~0.12
  ::  velocity is updated according to the following rule:
  ::  v_i(t+1) = w * v_i(t) + c_1 * r_1(t) * (b_i(t) - x_i(t))
  ::  + c_2 * r_2(t) * (b(t) - x_i(t))
  =/  dif=loc      [(sub:rd bx px) (sub:rd by py)]
  =/  bes-dif=loc  [(sub:rd gbx px) (sub:rd gby py)]
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
  =/  newpos=loc   :-  (add:rd x.pos x.newvel)
                   (add:rd y.pos y.newvel)
  =/  objectives=[prev=@rd cur=@rd]  :-  (objective pos.bes)
                                     (objective pos)
  ?:  (gth:rd prev.objectives cur.objectives)  :: is the previous best better than the current value?
    [newpos newvel bes]  :: best does not get updated
  [newpos newvel [newpos cur.objectives]]
--
