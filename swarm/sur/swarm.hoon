|%
+$  action
  $%  [%update-all ~]
      [%print-state ~]
      [%update-ship =ship]
      [%join-swarm =ship]
      [%leave-swarm =ship]
      [%update-self ~]
      [%start-run ~]
::      [%update-ship-phase =ship =phase]
  ==
::
+$  coord
  $%  [%join-swarm ~]
      [%leave-swarm ~]
      [%start-run ~]
  ==
::
+$  update
  $%  [%phase =phase]
  ==
::
+$  loc    [x=@rd y=@rd]
+$  phase  [pos=loc vel=loc bes=[pos=loc val=@rd]]
+$  phases  (map @p phase)
--
