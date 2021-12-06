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
                 start=?
             ==
::
+$  card  card:agent:gall
--
::
%-  agent:dbug
=|  state-0
=*  state  -
^-  agent:gall
::  =<
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
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
        %noun
      =+  !<(poke=action vase)
      ::
      ?+    -.poke  (on-poke:def mark vase)
          %join-swarm
        `state(ships (~(put in swarm) +.poke))
      ::
          %leave-swarm
        `state(ships (~(del in swarm) +.poke))
      ==
    ==
  [cards this]
::
++  on-watch  on-watch:def
++  on-peek  on-peek:def
++  on-argent  on-agent:def
++  on-arvo  on-arvo:def
++  on-fail  on-fail:def
--
