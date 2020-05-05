::
:: This is the backend for 'rote', a simple flashcard app. It also serves as a
:: walkthrough that will help you write your own app. I erred on the side of
:: providing too much information, rather than too little; feel free to skip
:: over stuff you already know. The intended audience is fresh-out-of-101
:: Hooners, i.e. people who have a decent grasp of Hoon and Arvo at a high
:: level, but haven't memorized all the runes and are missing a lot of context.
:: As a supplement, I recommend reading the official Gall tutorials as well;
:: they'll fill some parts that aren't covered here, and when you're learning,
:: it never hurts to have the core concepts explained to you multiple times in
:: different ways.
::
:: For context, I'll provide a high-level overview of how the app works. It's
:: basically a pared-down version of publish, with flashcard decks instead of
:: notebooks. These decks consistute the app's :: state. Each deck is stored
:: as an udon (i.e. Markdown) file in your desk (filesystem). When requested,
:: the backend provides these decks to the frontend (for rendering), and also
:: to other ships (for sharing). Other than that, the only thing the backend
:: needs to do is serve static HTML/JS/CSS resources; it's nothing fancy!
::
::
:: We begin by importing some files via Ford. (Ford runes can be identified by
:: their leading slash.) '/-  foo' reads the file /sur/foo.hoon and makes its
:: contents available via the 'foo' identifier. The /sur directory contain molds
:: (type definitions) that need to be shared across multiple files. In our case,
:: /sur/rote.hoon contains our central 'deck' mold, as well as an 'action' mold,
:: which is used by both the backend and by the frontend.
::
:: We can refer to our imported mold as 'deck:rote'. If we had written '/-
:: *rote' here, there would be no namespacing; we would refer to 'deck:rote'
:: as 'deck'. My personal preference is to always use explicit namespacing,
:: but it's not uncommon to expose one or two namespaces, as long as it doesn't
:: cause too much ambiguity.
::
:: (By the way, ':' isn't some special namespace accessor syntax; it's just the
:: irregular form of '=<', which composes two expressions. 'deck:rote' means
:: "evaluate the expression 'deck', using 'rote' as the subject.")
::
/-  rote
::
:: Next, we import some "library files." /+ is pretty much identical to /-,
:: except that it looks in the /lib directory. Again, we use explicit
:: namespacing here, but Gall apps often import *server for convenience.
::
:: server provides various HTTP server utilities, such as handling
:: authentication and converting JSON values to raw octets. cram is used for
:: parsing udon metadata; don't worry about it. default-agent provides defaults
:: (or 'stubs') for the 10 Gall agent arms.
::
/+  server, cram, default-agent
::
:: Lastly, '/=' is used to load non-Hoon data. Here, we're loading the static
:: resources that we'll serve to the frontend.
::
/=  index                  :: bind the contents to the face 'index'
  /^  octs                 :: cast the following expression to the 'octs' type (not strictly necessary here, but good practice)
  /;  as-octs:mimes:html   :: call 'as-octs:mimes:html' (from the standard library) on the file data
  /:  /===/app/rote/index  :: the path to the file
    /html/                 :: the mark (extension) of the file
::
:: Same as above, just more compact
::
/=  tile-js  /;  as-octs:mimes:html  /:  /===/app/rote/js/tile  /js/
/=  script   /;  as-octs:mimes:html  /:  /===/app/rote/js/index  /js/
/=  style    /;  as-octs:mimes:html  /:  /===/app/rote/css/index  /css/
::
:: Here, we add *all* png files in the img directory using the /_ rune, which is
:: a basically a wildcard -- this is analogous to app/rote/img/*.png in Unix.
:: Since we're loading multiple paths, the result is a map from path to data.
::
/=  rote-png
  /^  (map knot @)
  /:  /===/app/rote/img  /_  /png/
::
:: The last thing we load is our flashcard deck files. This is a bit unorthodox;
:: the Right Way To Do It would be to query Clay for the contents of /decks, and
:: then parse each listed file and add it to our app state. Unfortunately, I
:: wasn't able to get that working in time, so instead, we just punt it to Ford.
:: This works better than you might think, as Ford will automatically rebuild
:: our app whenever we modify the /decks directory! But it does mean that local
:: decks and remote decks are stored in two different places, which is ugly.
::
/=  local-decks-udon
  /^  (map knot @)
  /:  /===/app/rote/decks  /_  /udon/
::
::
:: Okay, done with importing. Our next step is to define a few local types.
:: These don't need to go in /sur, because they're only used in this file.
::
|%
::
:: For convenience, we define an alias for the Gall 'card' type, since we'll be
:: using it a lot. A 'card' is an effect, such as requesting data from Arvo or
:: commanding another agent to do something.
::
+$  card  card:agent:gall
::
:: More importantly, we need to define our agent's state. By convention, the
:: state is a tagged union, comprising the different state types in each
:: version of the app. For example, maybe in the v0 release of your app, its
:: state was a single '@ud'. Then in v1, you added a '@tas', so your state is
:: now a '[@ud @tas]'. Instead of redefining your state type, you would add the
:: new type to the versioned-state union. This allows you to easily upgrade from
:: one version to the next, which we'll see in the on-load arm.
::
:: (You don't need to start defining separate types like this until your app is
:: out in the wild. If you're just rapidly iterating on an MVP, it's fine to
:: redefine 'state-zero'.)
::
:: Our app's state is a set of decks, keyed by their owner and name.
::
+$  state-zero  decks=(map [@p @ta] deck:rote)
+$  versioned-state
  $%
    [%0 state-zero]
  ==
--
::
:: Now that we have a state type, we need a state value -- the thing we'll
:: actually mutate. It doesn't really matter what value we use for 'state' here,
:: since it will be initialized later, so we use the bunt value.
::
:: Some Gall apps will add a state value to the subject without a face (so that
:: they can refer to e.g. 'decks' instead of 'decks.state'), but as usual, my
:: preference is to be explicit.
::
=|  state=versioned-state
::
:: Finally, we come to the definition of the agent itself. What we're actually
:: doing here is composing two expressions: our agent, and a set of helper
:: functions. Composing them allows us to reference the helper functions from
:: within the agent. Gall agents must have a specific set of 10 arms -- no more,
:: no less -- so we can't add any helper arms to the agent core itself.
::
^-  agent:gall
=<
  ::
  :: Agents are doors; their sample is a "bowl," which contains useful OS stuff
  :: like entropy, the current time, and the name of our ship.
  ::
  |_  =bowl:gall
  ::
  :: First we define a few aliases using '+*', which is kind of a strange rune.
  :: Technically, it's not a rune at all; it's better to think of it as a macro
  :: that inserts '=*' expressions into each of the door's arms. That's why it
  :: doesn't prevent our door from being a Gall agent; it's not actually adding
  :: any arms, just modifying the arms that are already there.
  ::
  +*  this  .                                  :: our agent
      rc    ~(. +> bowl)                       :: +> resolves to our helper door, which we turn into a core by passing it our bowl
      def   ~(. (default-agent this %|) bowl)  :: our default/'stub' arms, constructed by passing our door and bowl to the default-agent library
  ::
  :: on-init is called when the app is started for the first time. It is
  :: responsible for setting the initial state and emitting any initialization
  :: effects. It is *not* called every time the app is restarted (e.g. with
  :: ':goad %force'); as far as I can tell, it is only ever called once, when
  :: you run '|start %app'.
  ::
  ++  on-init
    ::
    :: The "return type" of this arm, and most others, is a list of cards
    :: (effects) and a new agent. ('_foo' is an irregular form that means "the
    :: type of 'foo'.")
    ::
    ^-  (quip card _this)
    ::
    :: Our agent doesn't need to do anything to initialize its state: recall
    :: that 'state' is already initialized to the the bunt value of a map (i.e.
    :: an empty map), which is what we want. We do, however, need to emit two
    :: effects: we need to tell Eyre (%e) to route HTTP requests to /~rote to
    :: our app, and we need to tell the launch agent where to find our tile, so
    :: it can include it in the Landscape home screen.
    ::
    :_  this
    :~
      ::
      :: Cards passed to Arvo have a different structure than cards passed to
      :: other agents. The Arvo structure is:
      ::
      ::    [%pass /my/wire %arvo %vane task]
      ::
      :: where %vane is one of %a,%b,%c..., standing for Ames, Behn, Clay, etc.
      :: A 'task' generally consists of a "mark" (symbol) indicating the
      :: desired action, followed by any associated data, which varies based
      :: on the task.
      ::
      :: In this case, '%connect' expects a domain and URL, followed by the name
      :: of the app to connect.
      ::
      [%pass /bind %arvo %e %connect [~ /'~rote'] %rote]
      ::
      :: The structure for an agent card is necessarily more generic than an
      :: Arvo card. Since it has to work with arbitrary agents, it uses a
      :: dynamic "cage" value rather than a tagged union of all legal values.
      :: The structure is:
      ::
      ::    [%pass /my/wire %agent [~ship %app-name] task]
      ::
      :: where 'task' is a tagged union of %poke, %watch, and %leave. %poke is
      :: the most common and flexible way of interacting with agents, whereas
      :: %watch and %leave deal specifically with subscription.
      ::
      :: In this case, we're poking the %launch app. The %launch app defines yet
      :: another tagged union, this one of possible actions we can request. We
      :: construct an action that tells %launch to add our tile to Landscape;
      :: then we wrap the action in its type to produce a vase; then we pair the
      :: vase with the %launch-action mark to form a cage. (=cage is shorthand
      :: for cage=cage; it's a common idiom when you're defining a value of a
      :: certain type and you know you won't need to refer to that type again.)
      ::
      =/  action  [%add %rote /rotetile '/~rote/js/tile.js']
      =/  =cage  [%launch-action !>(action)]
      [%pass /tile %agent [our.bowl %launch] %poke cage]
    ==
  ::
  :: These arms are called when the app is upgraded. This will occur any time
  :: you modify your app and commit the desk: first, on-save will be called to
  :: store the previous state; then the app will be reloaded; then the previous
  :: state will be passed to the new on-load arm.
  ::
  :: on-save rarely needs to be more sophisticated than '!>(state)', i.e. the
  :: current state wrapped in its type.
  ::
  :: on-load should type-switch on the value it's given, so that you can upgrade
  :: appropriately based on what version you're upgrading from.
  ::
  ++  on-save  !>(state)
  ++  on-load
    |=  =vase
    ^-  (quip card _this)
    =/  prev  !<(versioned-state vase)
    ?-  -.prev
        %0
      ::
      :: `this is a common expression in Gall apps: it's an irregular form that
      :: expands to [~ this], i.e. returning a new agent, but no cards. In this
      :: case, %0 is the latest version, so we pass the previous state through
      :: unmodified.
      ::
      `this(state prev)
    ==
  ::
  :: on-poke is the main 'event handler' for your app. It's where you'll receive
  :: input from the user (via either Dojo or Landscape) and from other ships.
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    ::
    :: Switch on the mark; in other words, dispatch based on the type of action
    :: we're being asked to perform. If we don't recognize the action, default
    :: to the on-poke arm from def. (Note that 'mark' does not mean a Clay
    :: filetype here; it's just an alias for @tas.)
    ::
    ?+    mark  (on-poke:def mark vase)
        ::
        :: The %noun mark is generally used for handling Dojo commands. If you
        :: run:
        ::
        ::    > :rote %foo 1
        ::
        :: It will be sent as '[%noun [%foo 1]]'. If you need to send a
        :: different mark, use:
        ::
        ::    > :rote &foo 1
        ::
        :: This will be sent as '[%foo 1]'.
        ::
        %noun
      ::
      :: Here we assert that the source of the poke is "on the same team" as us,
      :: i.e. that the request is either coming from our own ship, or from one
      :: of our moons. If the assert fails, we crash -- but don't worry,
      :: crashing isn't fatal here. The crash will be caught by Gall and
      :: forwarded to our on-fail arm, so that we can process the error if we so
      :: desire. (Usually, though, such errors are simply ignored.)
      ::
      ?>  (team:title our.bowl src.bowl)
      ::
      :: The only %noun poke we recognize is %print-state, which...prints the
      :: current state. This isn't very useful in production, but I left it in
      :: because it's useful to have when developing. When I need to debug
      :: something, I often do so by adding a new case here and poking my app
      :: manually from Dojo.
      ::
      ?+    q.vase  (on-poke:def mark vase)
          %print-state
        ~&  state
        `this
      ::
          %reset-state
        `this(state *versioned-state)
      ==
        ::
        :: %rote-action is where we handle pokes from other agents. Somewhat
        :: unintuitively, that includes the frontend! When the frontend is just
        :: accessing resources via GET requests, those pokes will be routed to
        :: the '%handle-http-request' arm below; but when it needs to initiate
        :: some kind of action, it uses PUT requests to send arbitrary pokes.
        ::
        :: Like the %launch app, we've defined a set of possible actions, so
        :: we'll do a type-switch to dispatch on those. But since this tends to
        :: involve a lot of logic, we call out to one of our helper arms to avoid
        :: cluttering the agent definition.
        ::
        :: Here, we use the '=^' rune, which is a little complicated, but also
        :: frequently used in Gall apps. Much like how an agent's arms produce a
        :: new agent and a list of effects, '=^' pins a new face to the subject
        :: and rebinds an existing face. The values of these faces come from the
        :: third argument. So in this expression, we're calling
        :: 'poke-rote-action' on the action, which produces a list of cards and
        :: a new state; 'state' is then rebound to the new state, and the cards
        :: are returned from 'on-poke' along with 'this'.
        ::
        %rote-action
      =^  cards  state  (poke-rote-action:rc !<(action:rote vase))
      [cards this]
        ::
        :: %handle-http-request is where we handle HTTP requests from the
        :: frontend. The server library performs most of the heavy lifting here,
        :: and the rest is extracted into one of our helper functions.
        ::
        %handle-http-request
      ::
      :: HTTP requests don't affect our state.
      ::
      :_  this
      ::
      :: Assert the vase's type and extract its data, binding it to two faces,
      :: and pull 'app:server' into the subject for convenience.
      ::
      =+  !<([eyre-id=@ta =inbound-request:eyre] vase)
      =+  app=app:server
      ::
      :: 'require-authorization' is a higher-order function that first
      :: authenticates the request, then calls our gate on it; and
      :: 'give-simple-payload' takes the resulting payload and turns it into
      :: cards addressed to Eyre. Gall hands the cards to Arvo, which routes the
      :: cards to Eyre, and Eyre translates them into an HTTP response and
      :: writes it to our frontend.
      ::
      %+  give-simple-payload:app  eyre-id
      %+  require-authorization:app  inbound-request
      poke-handle-http-request:rc
    ==
  ::
  :: on-watch handles subscriptions, which we handle by switching on the
  :: subscription path.
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card _this)
    ?+    path  (on-watch:def path)
        [%http-response *]  :: TODO: not clear what the function of this is
      `this
    ::
        [%rotetile ~]  :: TODO: not clear what the function of this is
      `this
    ::
    :: This is the path that the frontend subscribes on for deck updates.
    :: When we receive a deck from another ship, we'll use this subscription
    :: to tell the frontend to refresh.
    ::
    :: TODO: better name?
    ::
        [%primary ~]
      `this
    ::
    :: Another ship is subscribing to one of our decks! As with %rote-action,
    :: we'll implement the logic for this in one of our helper arms.
    ::
        [%deck @ ~]
      =/  name  i.t.path
      =^  cards  state  (watch-deck:rc name)
      [cards this]
    ==
  ::
  :: on-agent receives "signs" from other agents (possibly on other ships),
  :: addressed to a particular wire. A sign is another tagged union; it either
  :: notifies us that something happened (%poke-ack, %watch-ack, or %kick), or
  :: delivers a cage (%fact).
  ::
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    ?+  -.sign  (on-agent:def wire sign)
        ::
        :: After we subscribe to a deck, the other ship will send us the deck
        :: data. We extract the deck's name from the wire, and then hand it off
        :: to 'handle-import-deck', which adds it to our state.
        ::
        %fact
      ?+  wire  (on-agent:def wire sign)
          [%import @ @ ~]
        ?>  ?=(%rote-deck p.cage.sign)
        =/  name  i.t.t.wire
        =/  deck  !<(deck:rote q.cage.sign)
        =^  cards  state  (handle-import-deck:rc name deck)
        [cards this]
      ==
    ==
  ::
  :: on-arvo receives signs from Arvo. In our case, the only sign we receive
  :: is from Eyre, acknowledging that it completed the %connect task we issued
  :: in 'on-init'.
  ::
  ++  on-arvo
    |=  [=wire =sign-arvo]
    ^-  (quip card _this)
    ?+  wire  (on-arvo:def wire sign-arvo)
      [%bind ~]  `this
    ==
  ::
  :: We don't need to do anything in these arms, so we use the default
  :: implementations. Phew, that's it for our agent definition!
  ::
  ++  on-leave  on-leave:def
  ++  on-peek   on-peek:def
  ++  on-fail   on-fail:def
  --
::
:: Remember, the expression above is composed with this one. The above
:: expression was our agent; this one is our helper door. When we compose the
:: two, the helper door becomes the subject of the agent.
::
|_  bowl=bowl:gall
::
:: The decks that Ford loaded for us are stored as udon, so we need to parse
:: them into our 'deck' type, which we achieve by calling 'parse-deck' on each
:: entry of the map.
::
++  local-decks
  ^-  (map @ta deck:rote)
  (~(rut by local-decks-udon) parse-deck)
::
:: The handler for '%rote-action' pokes.
::
++  poke-rote-action
  |=  =action:rote
  ^-  (quip card _state)
  ?-    -.action
      ::
      :: This is a request to subscribe to a deck on another ship. We construct
      :: a '%watch' card that contains the subscription path and is identified
      :: by a unique wire. Gall and Arvo will route this card to the %rote app
      :: on the other ship, where it will be handled by their 'on-watch' arm.
      :: Finally, 'on-watch' will send a '%fact' sign back to us, which will be
      :: handled by our 'on-agent' arm.
      ::
      %import
    ?>  (team:title our.bowl src.bowl)
    =/  =wire  /import/(scot %p who.action)/[deck.action]
    :_  state
    [%pass wire %agent [who.action %rote] %watch /deck/[deck.action]]~
  ==
::
:: The handler for subscription events. Pretty simple: we fetch the named deck
:: from local-decks, and send it as a '%fact' to the subscriber. We immediately
:: '%kick' the subscriber afterward, because we don't support true subscription
:: yet -- just one-off request-response exchanges.
::
++  watch-deck
  |=  name=@ta
  ^-  (quip card _state)
  =/  =deck:rote  (~(got by local-decks) name)
  :_  state
  ::
  :: %rote-deck is not an arbitrary symbol; it must refer to a Clay mark.
  :: When there's a hyphen in the name, it's interpreted as a directory, so
  :: Arvo will look for this mark in /mar/rote/deck.hoon.
  ::
  :: TODO: explain the '~'s
  ::
  :~  [%give %fact ~ %rote-deck !>(deck)]
      [%give %kick ~ ~]
  ==
::
:: Importing a deck -- again, pretty simple, we just need to insert the deck
:: into our state. We also send an update to the '%primary' subscriber, i.e.
:: our frontend, so that it can refresh and display the new deck.
::
++  handle-import-deck
  |=  [name=@ta =deck:rote]
  ^-  (quip card _state)
  =/  ds  (~(put by decks.state) [author.deck name] deck)
  :_  state(decks ds)
  ::
  :: '[foo]~' is irregular syntax for '[foo ~]', i.e. a list of one element.
  ::
  [[%give %fact [/primary]~ %rote-deck !>(deck)]]~
::
:: The handler for HTTP requests. There are two major cases we need to handle:
:: static resources (HTML/JS/CSS/images) and deck data.
::
++  poke-handle-http-request
  |=  =inbound-request:eyre
  ^-  simple-payload:http
  =+  gen=gen:server
  =/  url  (parse-request-line:server url.request.inbound-request)
  ::
  :: Switch on the URL. For static resources, match the URL to one of the files
  :: we loaded from Ford.
  ::
  ?+  site.url  not-found:gen
      [%'~rote' %css %index ~]  (css-response:gen style)
      [%'~rote' %js %tile ~]    (js-response:gen tile-js)
      [%'~rote' %js %index ~]   (js-response:gen script)
      [%'~rote' %img @t *]
    =/  name=@t  i.t.t.site.url
    =/  img  (~(get by rote-png) name)
    ?~  img  not-found:gen
    (png-response:gen (as-octs:mimes:html u.img))
  ::
  :: For deck data, we need to transform our decks into JSON.
  ::
      [%'~rote' %decks ~]
    ::
    :: We use '=,' to expose the namespace of 'enjs:format', which provides arms
    :: for JSON encoding. We then call two functions on the following
    :: expression; these will turn the JSON into raw octets, and then turn those
    :: raw octets into an HTTP response with the appropriate metadata.
    ::
    =,  enjs:format
    %-  json-response:gen
    %-  json-to-octs:server
    ::
    :: Symbol tags like %a, %o, and %s tell the JSON encoder how a particular
    :: value should be encoded. We want to produce a JSON array, so we use the
    :: %a tag.
    ::
    :-  %a
    ::
    :: Here we concatenate our local and remote decks and map a gate over them.
    :: The gate turns a deck into a JSON object using the 'pairs' arm from
    :: 'enjs:format'.
    ::
    %+  turn  (weld ~(val by local-decks) ~(val by decks.state))
    |=  =deck:rote
    %-  pairs
    ::
    :: 'foo+bar+~zod' produces '[%foo [%bar ~zod]]'; it's an irregular form
    :: that's frequently used to tag a values with a symbol/mark. Here, we're
    :: tagging each value with an appropriate JSON type -- '%s' means string --
    :: and then further tagging that pair with the name that 'pairs' should use
    :: in the resulting object.
    ::
    :~  path+s+path.deck
        title+s+title.deck
        ship+s+(scot %p author.deck)
        cards+s+cards.deck
    ==
  ::
  :: This match needs to be defined last; otherwise it would be matched before
  :: the others.
  ::
      [%'~rote' *]  (html-response:gen index)
  ==
::
:: And finally, our udon deck file parser. This function is fairly dense and not
:: terribly relevant to other apps, so feel free to skip it.
::
++  parse-deck
  |=  [path=@tas udon=@t]
  ^-  deck:rote
  =/  front-idx  (add 3 (need (find ";>" (trip udon))))
  =/  front-matter
    (cat 3 (end 3 front-idx udon) 'dummy text\0a')
  =/  body  (cut 3 [front-idx (met 3 udon)] udon)
  ::
  :: Interpret the header as Hoon code, producing either a map of
  :: symbol->string, or an error.
  ::
  =/  meta=(each (map term knot) tang)
    %-  mule  |.
    %-  ~(run by inf:(static:cram (ream front-matter)))
    |=  a=dime  ^-  cord
    ?+  (end 3 1 p.a)  (scot a)
      %t  q.a
    ==
  ::
  :: For various fields, extract the field from the header if it exists, using a
  :: default value otherwise.
  ::
  =/  author=@p  our.bowl
  =?  author  ?=(%.y -.meta)
    %+  fall
      (biff (~(get by p.meta) %author) (slat %p))
    our.bowl
  ::
  =/  title=@t  path
  =?  title  ?=(%.y -.meta)
    (fall (~(get by p.meta) %title) path)
  ::
  :*  author
      title
      path
      body
  ==
::
:: You're done! Hopefully you have a better picture of how to create your own
:: Gall app now. If there are things you're still confused by, or parts that
:: could be worded more clearly, don't hesitate to open an issue or pull
:: request:
::
::    https://github.com/lukechampine/rote
::
--
