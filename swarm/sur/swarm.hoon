|%
+$  action
  $%  [%update-all ~]
      [%print-state ~]
      [%update-ship =ship]
      [%join-swarm =ship]
      [%leave-swarm =ship]
      [%update-ship-phase =ship =phase]
  ==
+$  loc    [x=@rd y=@rd]
+$  phase  [pos=loc vel=loc bes=[pos=loc val=@rd]]
+$  phases  (map @p phase)
--
