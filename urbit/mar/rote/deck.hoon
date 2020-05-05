/-  rote
|_  deck=deck:rote
++  grab
  |%
  ++  noun  deck:rote
  --
++  grow
  |%
  ++  json
    =,  enjs:format
    %-  pairs
    :~  path+s+path.deck
        title+s+title.deck
        ship+s+(scot %p author.deck)
        cards+s+cards.deck
    ==
  --
--
