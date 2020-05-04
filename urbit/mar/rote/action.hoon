/-  rote
|_  act=action:rote
++  grow
  |%
  ++  tank  >act<
  --
::
++  grab
  |%
  ++  noun  action:rote
  ++  json
    |=  jon=^json
    %-  action:rote
    =<  (action jon)
    |%
    ++  action
      %-  of:dejs:format
      :~  import+import
      ==
    ::
    ++  import
      %-  ot:dejs:format
      :~  who+(su:dejs:format fed:ag)
          deck+so:dejs:format
      ==
    --
  --
--