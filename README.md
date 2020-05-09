# rote

A flashcard app for Landscape.

## Installation

Create an `.urbitrc` file:

```js
module.exports = {
  URBIT_PIERS: [
    "/path/to/ship/home",
  ]
};
```

Then, in Unix:

```
yarn
yarn run build
```

Then, in Dojo:

```
|commit %home
|start %rote
```

You should then be able to see the `rote` tile on your Landscape home.

For Hoon practice, try importing these decks!

```
~watter-parter/hoon-runes
~watter-parter/common-hoon-runes
~watter-parter/hoon-irregular-forms
```

## Walkthrough

This repository also functions as a walkthrough for creating your own Gall
app! Open up [rote.hoon](urbit/app/rote.hoon) and read the whole thing.