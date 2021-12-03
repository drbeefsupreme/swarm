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
  =|  cards=(list card)
  |^
  ?:  =(i ship-num.state)
    [cards this]
  =.  cards  %+  weld
               cards
             :~  ^-  card  :: why do i need this cast here?
                 :*  %pass  /  %agent
                     [our.bowl %swarm]
                     %poke  %swarm-action
                     !>(`action`[%join-swarm `@p`i])
             ==  ==
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
    ::
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
::
|_  bowl=bowl:gall
::
++  objective
  |=  loc
  ^-  @rd
  (sub:rd .~0 (add:rd (mul:rd x x) ;:(mul:rd x x y y)))  ::  -(x^2+x^2y^2)
::
++  on-action
  |=  =action
  ^-  (quip card _state)
  ?-  -.action
    %print-state  print-state
    %update-all   update-all
    %update-ship  (update-ship +.action)
    %join-swarm   (join-swarm +.action)
    %leave-swarm  (leave-swarm +.action)
  ==
::
++  print-state
  ^-  (quip card _state)
  ~&  >>  state
  ~&  >>>  bowl
  `state
::
++  join-swarm
  |=  =ship
  ^-  (quip card _state)
  ~|  'join swarm failed'
  :-  ~
  ?:  (~(has in ships.state) ship)
    state
  state(ships (~(put in ships) ship))
::
++  leave-swarm
  |=  =ship
  ^-  (quip card _state)
  ~|  'leave swarm failed'
  :-  ~
  ?.  (~(has in ships.state) ship)
    state
  state(ships (~(del in ships) ship))
::
++  update-ship
  |=  =ship
  ^-  (quip card _state)
  ~|  'update-ship failed'
  =*  bes  bes.state
  =*  pos  pos.state
  =*  vel  vel.state
  =*  grp  group-bes.state
  =/  phase  %^    update-phase
                 (~(got by pos) ship)
               (~(got by vel) ship)
             (~(got by bes) ship)
  =.  pos  (~(put by pos) ship pos.phase)
  =.  vel  (~(put by vel) ship vel.phase)
  =.  bes  (~(put by bes) ship bes.phase)
  ::  if new objective is better than group best, update group best
  ::
  =.  grp  ?.  (gte:rd val.bes.phase val.grp)
           grp
         [pos.phase val.bes.phase]
  `state
::
++  update-all
  ^-  (quip card _state)
  ~|  'update-all failed'
  =/  i=@  0
  |-
  ?:  =(i ship-num.state)
    ~&  >  group-bes.state
    `state(steps +(steps))
  =.  state  +:(update-ship `@p`i)  ::  TODO this looks wrong
  $(i +(i))
::
++  update-phase
  |=  [pos=loc vel=loc bes=[pos=loc val=@rd]]
  ^-  [pos=loc vel=loc bes=[pos=loc val=@rd]]
  ~|  'update-phase failed'
  ::  x_i - position, v_i - velocity, b_i - best position so far
  ::  b - best group position, c_1 - cognitive coefficient
  ::  c_2 - social coefficient, r_1, r_2 random number between 0 and 1
  ::  w - inertia weight
  =/  rng  ~(. og eny.bowl)
  =^  ran1  rng  (rads:rng 1.000)
  =^  ran2  rng  (rads:rng 1.000)
  =/  [px=@rd py=@rd vx=@rd vy=@rd bx=@rd by=@rd gbx=@rd gby=@rd]
    :*  x.pos  y.pos
        x.vel  y.vel
        x.pos.bes  y.pos.bes
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
  =/  newpos=loc   :-  (add:rd x.pos x.newvel)
                   (add:rd y.pos y.newvel)
  =/  objectives=[prev=@rd cur=@rd]  :-  (objective pos.bes)
                                     (objective pos)
  ?:  (gth:rd prev.objectives cur.objectives)  :: is the previous best better than the current value?
    [newpos newvel bes]  :: best does not get updated
  [newpos newvel [newpos cur.objectives]]
--
