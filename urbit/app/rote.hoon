::
:: This is the backend for 'rote', a simple flashcard app. It also serves as a
:: walkthrough that will help you write your own app. I erred on the side of
:: providing too much information, rather than too little; feel free to skip
:: over stuff you already know. The intended audience is fresh-out-of-101
:: Hooners, i.e. people who have a decent grasp of Hoon and Arvo at a high
:: level, but haven't memorized all the runes and are missing a lot of context.
:: As a supplement, I recommend reading the official Gall tutorials as well;
:: they'll fill in some gaps that aren't covered here. When you're learning, it
:: never hurts to have core concepts explained to you multiple times in multiple
:: ways.
::
:: For context, I'll provide a high-level overview of how the app works. It's
:: basically a pared-down version of publish, with flashcard decks instead of
:: notebooks. These decks consistute the agent's state. Each deck is stored as
:: an udon (i.e. Markdown) file in your desk (filesystem). When requested, the
:: backend provides these decks to the frontend (for rendering), and also to
:: other ships (for sharing). Other than that, the only thing the backend needs
:: to do is serve static HTML/JS/CSS resources; it's nothing fancy!
::
::
:: We begin by importing some files via Ford. (Ford runes can be identified by
:: their leading slash.) '/-  foo' reads the file /sur/foo.hoon and makes its
:: contents available via the 'foo' identifier. The /sur directory contain molds
:: (type definitions) that need to be shared across multiple files. In our case,
:: /sur/rote.hoon contains our central 'deck' mold, as well as an 'action' mold,
:: which is used by both the backend and by the frontend.
::
:: We can refer to our deck mold as 'deck:rote'. If we had written '/- *rote'
:: here, there would be no namespacing; we would refer to 'deck:rote' as
:: 'deck'. My personal preference is to always use explicit namespacing, but
:: it's not uncommon to expose one or two namespaces, as long as it doesn't
:: cause too much ambiguity.
::
:: (By the way, ':' isn't some special namespace accessor syntax; it's just the
:: irregular form of '=<', which composes two expressions. 'deck:rote' means
:: "evaluate the expression 'deck', using 'rote' as the subject.")
::
/-  rote
::
:: Next, we import some library files. '/+' is pretty much identical to '/-',
:: except that it looks in the /lib directory. Again, we use explicit
:: namespacing here, but Gall agents often import '*server' for convenience.
::
:: 'server' provides various HTTP server utilities, such as handling
:: authentication and converting JSON values to raw octets. 'cram' is used for
:: parsing udon metadata; don't worry about it. 'default-agent' provides
:: defaults (aka stubs) for the 10 Gall agent arms.
::
:: The '/-' and '/+' runes also support custom faces; we could write e.g.
:: '/+  srv=server' if we wanted to refer to 'server' as 'srv'. This is often
:: done when your agent has both a type definition file and a library file, to
:: prevent a naming collision.
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
:: Same as above, just more compact:
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
:: The '===' in these paths is shorthand for your "beak." A beak is a filesystem
:: path prefix, comprising your ship name, desk name, and a revision identifier.
:: Later on, we'll construct a beak manually.
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
:: version of the agent. For example, maybe in the v0 release of your app, your
:: agent's state was a single '@ud'. Then in v1, you added a '@tas', so your
:: state is now a '[@ud @tas]'. Instead of redefining your state type, you would
:: add the new type to the 'versioned-state' union. This allows you to easily
:: upgrade from one version to the next, which we'll see in the 'on-load' arm.
::
:: (You don't need to start defining separate types like this until your app is
:: out in the wild. If you're just rapidly iterating on an MVP, it's fine to
:: redefine 'state-zero'.)
::
:: Our agent's state consists of two maps: one for local decks (which are also
:: stored in Clay as raw .udon files), and one for remote decks (which we
:: requested from other ships). The latter is keyed by both the ship name and
:: deck name, and the former by just the deck name.
::
+$  state-zero
  $:  local-decks=(map @ta deck:rote)
      remote-decks=(map [@p @ta] deck:rote)
  ==
+$  versioned-state
  $%
    [%0 state-zero]
  ==
--
::
:: Before we move on, I want to clear up one thing that confused me. We just
:: defined a core: the '--' terminated the expression. But now we're going to
:: add another expression below, which will reference the core we defined above.
:: Normally, we would have to explicitly compose these two expressions with the
:: '=>' rune. But as it turns out, .hoon files built by Ford are parsed as a
:: *list* of expressions, which are implicitly composed. Here, I'll prove it:
::
.  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .
::
:: Composing '.' with anything is a no-op, so we can do it as many times as we
:: want without changing the final result. But if you try to enter something
:: like this in Dojo, you'll get a syntax error, because Dojo only accepts a
:: single expression at a time.
::
:: Anyway...
::
:: Now that we have a state type, we need a state value -- the thing we'll
:: actually "mutate." It doesn't really matter what value we use for 'state'
:: here, since it will be initialized later, so we use the bunt value.
::
:: Some Gall apps will add a state value to the subject without a face, so that
:: they can refer to e.g. 'decks' instead of 'decks.state'. But as usual, my
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
  :: like entropy, the current time, and the name of our ship. Here we use some
  :: irregular syntax: '=foo' means 'foo=foo'. This is a common idiom when you
  :: know that you won't need to reference the 'foo' type again.
  ::
  |_  =bowl:gall
  ::
  :: First we define a few aliases using '+*', which is kind of a strange rune.
  :: Technically, it's not a rune at all; it's better to think of it as a macro
  :: that inserts '=*' expressions into each of the door's arms. That's why it
  :: doesn't prevent our door from being a Gall agent: it's not actually adding
  :: any arms, just modifying the arms that are already there.
  ::
  +*  this  .                                  :: our agent
      rc    ~(. +> bowl)                       :: +> resolves to our helper door, which we turn into a core by passing it our bowl
      def   ~(. (default-agent this %|) bowl)  :: our default/"stub" arms, constructed by passing our door and bowl to the default-agent library
  ::
  :: on-init is called when the agent is started for the first time. It is
  :: responsible for setting the initial state and emitting any initialization
  :: effects. It is *not* called every time the agent is restarted (e.g. with
  :: ':goad %force'); as far as I can tell, it is only ever called once, when
  :: you run '|start %foo'.
  ::
  ++  on-init
    ::
    :: The "return type" of this arm, and most others, is a list of cards
    :: (effects) and a new agent. ('_foo' is an irregular form that means "the
    :: type of 'foo'.")
    ::
    ^-  (quip card _this)
    ::
    :: Our agent doesn't need to do anything to initialize its state; recall
    :: that 'state' is already initialized to the the bunt value of its type
    :: (i.e. two empty maps) which is what we want. We do, however, need to emit
    :: effects: we need to tell Eyre (%e) to route HTTP requests to /~rote to
    :: our agent, we need to ask Clay (%c) for the list of deck files, and we need
    :: to tell the launch agent where to find our tile.
    ::
    :_  this  :: ':_' constructs a cell, inverted
    :~
      ::
      :: Cards passed to Arvo have a different structure than cards passed to
      :: other agents. The Arvo structure is:
      ::
      ::    [%pass /my/wire %arvo <vane> task]
      ::
      :: where <vane> is one of %a,%b,%c..., standing for Ames, Behn, Clay, etc.
      :: A 'task' generally consists of a "mark" (symbol) indicating the
      :: desired action, followed by any associated data, which varies based
      :: on the task.
      ::
      :: In this case, '%connect' expects a domain and URL, followed by the name
      :: of the agent to connect.
      ::
      [%pass /bind %arvo %e %connect [~ /'~rote'] %rote]
      ::
      :: This card queries Clay, asking it for the contents of the directory
      :: where we store our own decks. Since we reuse this card in a few places,
      :: it's defined in our helper core.
      ::
      clay-sing:rc
      ::
      :: The structure for an agent card is necessarily more generic than an
      :: Arvo card. Since it has to work with arbitrary agents, it uses a
      :: dynamic "cage" rather than a tagged union of all legal values. The
      :: structure is:
      ::
      ::    [%pass /my/wire %agent [<ship> <agent-name>] task]
      ::
      :: where 'task' is a tagged union of %poke, %watch, and %leave. %poke is
      :: the most common and flexible way of interacting with agents, whereas
      :: %watch and %leave deal specifically with subscription.
      ::
      :: In this case, we're poking the %launch agent. The %launch agent defines
      :: yet another tagged union, this one of possible actions we can request.
      :: We construct an action that tells %launch to add our tile to Landscape;
      :: then we wrap the action in its type to produce a vase; then we pair the
      :: vase with the %launch-action mark to form a cage.
      ::
      =/  action  [%add %rote /rotetile '/~rote/js/tile.js']
      =/  =cage  [%launch-action !>(action)]
      [%pass /tile %agent [our.bowl %launch] %poke cage]
    ==
  ::
  :: These arms are called when the agent is upgraded. This will occur any time
  :: you modify your agent and commit the desk: first, 'on-save' will be called
  :: to store the previous state; then the agent will be reloaded; then the
  :: previous state will be passed to the new 'on-load' arm.
  ::
  :: 'on-save' rarely needs to be more sophisticated than '!>(state)', i.e. the
  :: current state wrapped in its type. Note that 'on-save' doesn't take any
  :: arguments or emit any effects.
  ::
  :: 'on-load' should type-switch on the value it's given, so that you can
  :: upgrade appropriately based on what version you're upgrading from.
  ::
  ++  on-save
    ^-  vase
    !>(state)
  ::
  ++  on-load
    |=  =vase
    ^-  (quip card _this)
    ::
    :: Regardless of what we load, we also need to emit the same Clay card as
    :: in 'on-init', because if the agent is upgraded, we'll want to fetch the
    :: current directory contents again.
    ::
    :: We use ':-' here rather than ':_' so that the shorter branch comes first,
    :: and we use the irregular syntax '[foo]~', which means '[foo ~]'.
    :: 
    :-  [clay-sing:rc]~
    ::
    :: This agent only has one state version, so the "upgrade" logic is trivial:
    :: just set 'state' equal to the contents of the provided vase.
    ::
    =/  prev  !<(versioned-state vase)
    ?-  -.prev
        %0
      this(state prev)
    ==
  ::
  :: 'on-poke' is one of the main "event handlers" for your agent. It's where
  :: you'll receive input from the user (via either Dojo or Landscape) and from
  :: other ships.
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    ::
    :: Switch on the mark; in other words, dispatch based on the type of action
    :: we're being asked to perform. If we don't recognize the action, default
    :: to the 'on-poke' arm from 'def'.
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
        :: This will be sent as '[%foo 1]'. Note, however, that this requires
        :: a 'foo' mark to exist in the /mar directory.
        ::
        %noun
      ::
      :: Here we assert that the source of the poke is "on the same team" as us,
      :: i.e. that the request is either coming from our own ship, or from one
      :: of our moons. If the assert fails, we crash -- but don't worry,
      :: crashing isn't fatal here. The crash will be caught by Gall and
      :: forwarded to our 'on-fail' arm, so that we can process the error if we
      :: so desire. (Usually, though, such errors are simply ignored.)
      ::
      ?>  (team:title our.bowl src.bowl)
      ::
      :: So, these pokes aren't very useful in production, but I left them in
      :: because they're useful to have when developing/debugging. When I need
      :: to debug something, I often do so by adding a new case here and poking
      :: my agent manually from Dojo.
      ::
      ?+    q.vase  (on-poke:def mark vase)
          %print-state
        ~&  state
        `this  :: irregular syntax for '[~ this]'
      ::
          %reset-state
        `this(state *versioned-state)  :: irregular syntax for bunt value
      ==
        ::
        :: %rote-action is where we handle pokes from other agents. Somewhat
        :: unintuitively, that includes the frontend! When the frontend is just
        :: accessing resources via GET requests, those pokes will be routed to
        :: the '%handle-http-request' arm below; but when it needs to initiate
        :: some kind of action, it uses PUT requests to send arbitrary pokes.
        ::
        :: Like the %launch agent, we've defined a set of possible actions, so
        :: we'll do a type-switch to dispatch on those. But since this tends to
        :: involve a lot of logic, we call out to one of our helper arms to avoid
        :: cluttering the agent definition.
        ::
        :: Here, we use the '=^' rune, which is a little complicated. Much like
        :: how an agent's arms produce a new agent and a list of effects, '=^'
        :: pins a new face to the subject and rebinds an existing face. The
        :: values of these faces come from the third argument. So here, we're
        :: calling 'poke-rote-action' on the action, which produces a list of
        :: cards and a new state; 'state' is then rebound to the new state, and
        :: the cards are returned from 'on-poke' along with 'this'.
        ::
        %rote-action
      =^  cards  state  (poke-rote-action:rc !<(action:rote vase))
      [cards this]
        ::
        :: %handle-http-request is where we handle HTTP GET requests from the
        :: frontend. The server library performs most of the heavy lifting here,
        :: and the rest is extracted into one of our helper functions.
        ::
        %handle-http-request
      :_  this  :: GET requests don't affect our state
      ::
      :: Assert the vase's type and extract its data, binding it to two faces.
      ::
      =+  !<([eyre-id=@ta =inbound-request:eyre] vase)
      ::
      :: 'require-authorization' is a higher-order function that first
      :: authenticates the request, then calls our gate on it; and
      :: 'give-simple-payload' takes the resulting payload and turns it into
      :: cards addressed to Eyre. Gall hands the cards to Arvo, which routes the
      :: cards to Eyre, and Eyre translates them into an HTTP response and
      :: writes it to our frontend.
      ::
      %+  give-simple-payload:app:server  eyre-id
      %+  require-authorization:app:server  inbound-request
      poke-handle-http-request:rc
    ==
  ::
  :: 'on-watch' handles subscriptions, which we handle by switching on the
  :: subscription path. The path serves two purposes: it tells us the "type"
  :: of the request, and it tells us where to send updates. In the general
  :: case, you would want to store these paths, so that you can reply to them
  :: later; but this is usually unnecessary, as the path is a single hard-coded
  :: element (e.g. '%primary') and can simply be inlined wherever it's needed,
  :: or is constructed deterministically from information that can be found
  :: elsewhere (such as '/deck/[deck-name]').
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card _this)
    ?+    path  (on-watch:def path)
    ::
    :: This subscription is requested by Eyre. I *think* the purpose of this
    :: is to tell us what path to send our response data to, but this is
    :: handled for us anyway in 'give-simple-payload' -- so we don't need to
    :: do anything here.
    ::
        [%http-response *]
      `this
    ::
    :: Similarly, this subscription is requested by the Launch agent; it's
    :: telling us where to send tile-related updates. In fact, *we* told
    :: Launch to subscribe to this path, back in our 'on-init' arm. Since our
    :: app doesn't have a fancy tile (yet) we don't need to do anything here.
    ::
        [%rotetile ~]
      `this
    ::
    :: This is the path that the frontend subscribes on for deck updates.
    :: When we receive a deck from another ship, we'll use this subscription
    :: to tell the frontend to refresh. ('primary' is an arbitrary name; it
    :: just needs to match the name we use in the frontend.)
    ::
        [%primary ~]
      `this
    ::
    :: Lastly, a more interesting path: another ship is subscribing to one
    :: of our decks! Unlike the previous cases, we will react to this
    :: subscription immediately, by sending the agent the requested deck. As
    :: with our pokes, we'll implement the logic for this in a helper arm.
    ::
    :: What's happening here is that we're matching the value 'path' against
    :: the type '[%deck @ ~]', i.e. a list with two elements (the first being
    :: '%deck', and the second being an arbitrary atom). After matching, the
    :: type system knows that 'path' is a list, so we can access 'i.t.path' to
    :: get the second element of the list.
    ::
        [%deck @ ~]
      ::
      :: Notice that we use '=*' here, rather than '=/'. The latter adds a new
      :: noun to the subject, whereas the former is more like an alias or a
      :: macro. The exact semantics are worth exploring on your own later, but
      :: for now, a simple rule of thumb suffices: use '=/' when computing new
      :: nouns, and '=*' when referencing existing nouns.
      ::
      =*  name  i.t.path
      =^  cards  state  (watch-deck:rc name)
      [cards this]
    ==
  ::
  :: 'on-agent' receives "signs" from other agents (possibly on other ships),
  :: addressed to a particular "wire." A sign is another tagged union; it either
  :: notifies us that something happened (%poke-ack, %watch-ack, or %kick), or
  :: delivers a cage (%fact).
  ::
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    ::
    :: It's best practice to switch on the wire first, then the sign; refer to
    :: https://urbit.org/blog/precepts-discussion
    ::
    ?+  wire  (on-agent:def wire sign)
        ::
        :: After we subscribe to a deck, the other ship will send us the deck
        :: data. We extract the deck's name from the wire, and the actual deck
        :: data from the sign; then we hand them off to 'handle-import-deck',
        :: which adds the deck to our state and pushes it to the '%primary'
        :: subscriber.
        ::
        [%import @ @ ~]
      =*  name  &3.wire :: equivalent to 'i.t.t.wire'
      ?+  -.sign  (on-agent:def wire sign)
          %fact
        ?>  ?=(%rote-deck p.cage.sign)
        =/  deck  !<(deck:rote q.cage.sign)
        =^  cards  state  (handle-import-deck:rc name deck)
        [cards this]
      ==
    ==
  ::
  :: 'on-arvo' receives signs from Arvo. Whenever you pass a card to Arvo, you
  :: specify a "wire," which will be included in the response that Arvo sends
  :: back to you, much like an agent subscription path.
  ::
  ++  on-arvo
    |=  [=wire =sign-arvo]
    ^-  (quip card _this)
    ?+  wire  (on-arvo:def wire sign-arvo)
      ::
      :: In a few places, we pass a card to Clay, asking it for a directory
      :: listing; here, we handle the response that Clay sends. We pass the
      :: list to our 'reload-decks' helper function to read each file and add
      :: it to our state.
      ::
        [%reload ~]
      ?>  ?=([?(%b %c) %writ *] sign-arvo)
      =*  riot  +>.sign-arvo
      ?>  ?=(^ riot)
      =/  paths  !<((list path) q.r.u.riot)
      =^  cards  state  (reload-decks:rc paths)
      [cards this]
      ::
      :: We also need to handle the case where an existing deck file is updated.
      :: Here, the response we receive is the data of a single file, rather than
      :: a directory listing.
      ::
        [%update ~]
      ?>  ?=([?(%b %c) %writ *] sign-arvo)
      =*  riot  +>.sign-arvo
      ?~  riot  `this  :: file was deleted
      =^  cards  state  (update-deck:rc u.riot)
      [cards this]
      ::
      :: The Eyre sign is trivial, simply acknowledging that it completed the
      :: '%connect' task we issued in 'on-init'.
      ::
        [%bind ~]
      `this
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
:: The handler for '%rote-action' pokes.
::
++  poke-rote-action
  |=  =action:rote
  ^-  (quip card _state)
  ?-    -.action
      ::
      :: This is a request to subscribe to a deck on another ship. We construct
      :: a '%watch' card that contains the subscription path and is identified
      :: by a unique wire. Gall and Arvo will route this card to the %rote agent
      :: on the other ship, where it will be handled by their 'on-watch' arm.
      :: Finally, 'on-watch' will send a '%fact' sign back to us, which Arvo
      :: and Gall will pair with our unique wire; this pair is then passed to
      :: our 'on-agent' arm.
      ::
      %import
    ?>  (team:title our.bowl src.bowl)
    =/  =wire  /import/(scot %p who.action)/[deck.action]
    :_  state
    [%pass wire %agent [who.action %rote] %watch /deck/[deck.action]]~
  ==
::
:: The handler for subscription events. Pretty simple: we fetch the named deck,
:: and send it as a '%fact' to the subscriber. We immediately
:: '%kick' the subscriber afterward, because we don't support true subscription
:: yet -- just one-off request-response exchanges.
::
++  watch-deck
  |=  name=@ta
  ^-  (quip card _state)
  =/  =deck:rote  (~(got by local-decks.state) name)
  :_  state
  ::
  :: The delivery of cards is somewhat nuanced, so bear with me. Cards are
  :: addressed to a set of subscription paths on a set of ships. In practice,
  :: you almost always address cards to a single path or to '~', and to a single
  :: ship or to '~'. This sentinel, '~', means either "return to sender" or
  :: "broadcast," depending on whether the card is returned from 'on-watch' or
  :: another arm. Let's break it down by card type.
  ::
  :: The '%fact' card looks like this:
  ::
  ::   [%give %fact <paths> <mark> <vase>]
  ::
  :: You cannot specify the set of recipient ships in a '%fact' card. This is
  :: because the recipients are always implied by the context. If the card is
  :: returned from 'on-watch' and 'paths' is '~', then the card is sent to a
  :: single ship -- the ship who sent the '%watch' that triggered the 'on-watch'
  :: -- and the card will be addressed to the same path that the ship subscribed
  :: to. Outside of 'on-watch', '~' is illegal, and will probably cause a crash.
  :: You must provide an explicit set of paths, and the '%fact' will be sent to
  :: all ships subscribed to those paths. It's ok to use explicit paths in
  :: 'on-watch'; they will be broadcast to all ships as usual.
  ::
  :: The '%kick' card looks like this:
  ::
  ::   [%give %kick <paths> <ships>]
  ::
  :: Unlike '%fact', the set of recipients is not implied by context. For
  :: example, you can easily imagine a "ban" command, handled by 'on-poke',
  :: that kicks a specific ship. The sentinel value '~' can be used for either
  :: the paths or the ships, or both. Within 'on-watch', the first '~' is short
  :: for the subscription path (as in '%fact'), and the second '~' is short for
  :: the subscribing ship. Outside 'on-watch', '~' means "all paths" and "all
  :: ships," respectively. Thus, you can kick all paths on a particular ship,
  :: or all ships on a particular path, or all paths on all ships.
  ::
  :: So: here, we are using '~' with '%fact' to send a deck to the subscriber,
  :: down the same path they subscribed to; and then using '~' with '%kick' to
  :: immediately kick the subscribing ship from that same path.
  ::
  :: Lastly: '%rote-deck' is not an arbitrary symbol; it's a proper Clay mark.
  :: When there's a hyphen in the name, everything before the first hyphen is
  :: interpreted as a directory, so Arvo will look for this mark at
  :: /mar/rote/deck.hoon.
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
  =/  ds  (~(put by remote-decks.state) [author.deck name] deck)
  :_  state(remote-decks ds)
  [[%give %fact [/primary]~ %rote-deck !>(deck)]]~
::
:: The handler for HTTP GET requests. There are two major cases we need to
:: handle: static resources (HTML/JS/CSS/images) and deck data.
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
    =*  name  i.t.t.site.url
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
    %+  turn  (weld ~(val by local-decks.state) ~(val by remote-decks.state))
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
:: Okay, everything after this point concerns loading deck files from Clay. But
:: before we get any deeper into Clay-land, I want to make one thing clear: your
:: app probably doesn't need Clay!
::
:: Coming from Unix, this is counterintuitive: Clay is the Urbit filesystem, and
:: pretty much every app needs to talk to the filesystem, right? But remember:
:: Urbit is a single-level store. That means there isn't a hard distinction
:: between RAM (fast, small, ephemeral) and disk (slow, large, permanent). When
:: we return a new agent from one of our arms, Gall ensures that that value is
:: committed durably to the single-level store. It sounds a little insane, but
:: imagine if you had a Unix binary with all its state embedded inside it, and
:: whenever you ran the binary, it would perform some action, mutate its state,
:: and then write itself back to disk. That's kinda what's going on here!
::
:: Anyway: since your state is already being stored durably, that's usually all
:: you need. The most common reason to use Clay is so that you can interface
:: with Unix: for rote (and publish), this allows users to write Markdown files
:: with their favorite Unix editor, and then import them into the app. But if
:: rote had a built-in deck editor, I would probably strip out all the Clay
:: code. So: if your app doesn't need to interop with a Unix filesystem, you
:: can safely skip this section.
::
:: Moving on...
::
:: 'clay-sing' and 'clay-next' aren't gates; they're just cards that we pass to
:: Clay. I defined them here because they're used in multiple places, and to
:: avoid cluttering the other code with Clay-specific details.
::
++  clay-sing
  ^-  card
  ::
  :: There's a lot going on here! '%sing' is short for "single" -- we're
  :: querying the state at a particular instant, rather than an ongoing
  :: subscription. '%t' means we want a directory listing (rather than, e.g.,
  :: the contents of a file), and 'now.bowl' is the current time. We also
  :: specify '/reload', the wire we saw in 'on-arvo'.
  ::
  =/  =rave:clay  [%sing %t [%da now.bowl] /app/rote/decks]
  [%pass /reload %arvo %c %warp our.bowl q.byk.bowl `rave]
::
++  clay-next
  ^-  card
  ::
  :: This card is identical to 'clay-sing', except that we specify '%next'
  :: instead of '%sing'. '%next' means "notify me the next time this thing
  :: changes," so unlike '%sing', it won't produce a response immediately.
  ::
  =/  =rave:clay  [%next %t [%da now.bowl] /app/rote/decks]
  [%pass /reload %arvo %c %warp our.bowl q.byk.bowl `rave]
::
:: Unlike the cards above, 'clay-update' is a gate (parameterized on a path),
:: and it requests the contents of a single file, rather than a directory. We
:: return a card like this for every deck file, so that we'll be notified if
:: any of them change.
::
++  clay-update
  |=  =path
  ^-  card
  =/  =rave:clay  [%next %x [%da now.bowl] path]
  [%pass /update %arvo %c %warp our.bowl q.byk.bowl `rave]
::
:: 'reload-decks' handles reading the deck udon files, parsing them, and adding
:: them to our state.
::
++  reload-decks
  |=  paths=(list path)
  ^-  (quip card _state)
  ::
  :: '|^' composes an expression (the first argument) with a core (the second).
  :: It's useful when you need to define one or more helper functions for
  :: computing a particular value.
  ::
  |^
  =/  next-cards  (turn paths clay-update)
  :-  (snoc next-cards clay-next)
  ::
  :: 'molt' takes a list of pairs and turns them into a map. We want a map from
  :: deck names to decks, so 'read-deck' takes a single path and turns it into
  :: a pair of name and deck.
  ::
  state(local-decks (molt (turn paths read-deck)))
  ++  read-deck
    |=  =path
    ^-  [@ta deck:rote]
    ?>  ?=([%app %rote %decks @ %udon ~] path)
    =*  name  &4:path
    ::
    :: Here we construct a "beak," which we saw earlier in the Ford imports as
    :: '==='. We have to build it manually here, because we need the current
    :: time; '===' will expand to a beak whose current time is the *build* time
    :: of the agent, rather than the time in 'bowl'.
    ::
    :: We concatenate our beak with 'path' to construct the full path, and then
    :: use the '.^' rune to scry it. '@t' specifies the aura of the data, and
    :: '%cx' means we're scrying file data ('x') from Clay ('c').
    ::
    =/  our-beak  /(scot %p our.bowl)/[q.byk.bowl]/(scot %da now.bowl)
    =/  =deck:rote  (parse-deck name .^(@t %cx (welp our-beak path)))
    [name deck]
  --
::
:: 'update-deck' parses a deck from a 'rant' (a Clay structure containing the
:: data at a particular node in the filesystem), and stores it in the state.
:: It also produces a 'clay-update' card, subscribing to the next change made
:: to that particular file.
::
++  update-deck
  |=  =rant:clay
  ^-  (quip card _state)
  =*  path  q.rant
  =*  name  &4.path
  =/  udon  !<(@t q.r.rant)
  =/  ds  (~(put by local-decks.state) name (parse-deck name udon))
  :_  state(local-decks ds)
  [(clay-update path)]~
::
:: 'parse-deck' is our udon deck file parser. It's fairly dense and not
:: terribly relevant to other apps, so feel free to skip it.
::
++  parse-deck
  |=  [path=@tas udon=@t]
  ^-  deck:rote
  =/  front-idx  (add 3 (need (find ";>" (trip udon))))
  =/  front-matter  (cat 3 (end 3 front-idx udon) 'dummy text\0a')
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
    (fall (biff (~(get by p.meta) %author) (slat %p)) our.bowl)
  =/  title=@t  path
  =?  title  ?=(%.y -.meta)
    (fall (~(get by p.meta) %title) path)
  :*  author
      title
      path
      body
  ==
--
::
:: You're done! Hopefully you have a better picture of how to create your own
:: Gall agent now. If there are things you're still confused by, or parts that
:: could be worded more clearly, don't hesitate to open an issue or pull
:: request:
::
::    https://github.com/lukechampine/rote
::
