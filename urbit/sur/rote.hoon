|%
::
:: This is our core data structure, representing a flashcard deck. This is the
:: structure that we'll parse our udon files into, and the type of object we'll
:: share with other ships.
::
+$  deck
  $:  author=@p
      title=@t
      path=@t
      cards=@t
  ==
::
:: This type defines possible poke actions. Currently there is only one action,
:: for importing a deck on another ship.
::
+$  action
  $%
     [%import who=@p deck=@tas]
  ==
--