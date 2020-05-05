import { Map, Record, List } from 'immutable'

const DeckRecord = Record({
  id: "<?>",
  path: "<?>",
  title: "<?>",
  ship: "<?>",
  cards: List([]),
  cardIndex: 0,
  answerShown: false,
})

const CardRecord = Record({
  front: "<?>",
  back: "<?>",
})

function deckToRecord(deck) {
  return DeckRecord({
    id: `${deck.ship}/${deck.path}`,
    path: deck.path,
    title: deck.title,
    ship: deck.ship,
    cards: List(deck.cards.split('===').map(section => {
      const i = section.indexOf('---')
      return CardRecord({
        front: section.slice(0, i).trim(),
        back: section.slice(i+3, -1).trim(),
      })
    })),
    cardIndex: 0,
    answerShown: false,
  })
}

class UrbitApi {
  setAuthTokens(authTokens) {
    this.authTokens = authTokens;
  }

  setSelected(selected) {
    // TODO
  }

  bind(path, method, ship = this.authTokens.ship, appl = "rote", success, fail) {
    window.urb.subscribe(ship, appl, path,
      fail,
      event => success({
        data: event,
        from: {ship, path},
      }),
      fail,
    );
  }

  action(appl, mark, data) {
    return new Promise((resolve, reject) => {
      window.urb.poke(ship, appl, mark, data, resolve, reject);
    });
  }

  handleErrors(response) {
    if (!response.ok) throw Error(response.status);
    return response;
  }

  initBindings(onEvent, onErr) {
    if (!api.authTokens) {
      console.error("~~~ ERROR: Must set api.authTokens before operation ~~~")
      return
    }
    this.bind('/primary', 'PUT', api.authTokens.ship, 'rote',
      e => onEvent(deckToRecord(e.data)),
      onErr,
    )
  }

  fetchDecks(callback) {
    fetch('/~rote/decks.json')
    .then((response) => response.json())
    .then(json => {
      callback(json.reduce((decks, raw) => {
        const deck = deckToRecord(raw)
        return decks.set(deck.id, deck)
      }, Map({})))
    })
  }

  importDeck(path, callback) {
    let action = {
      import: {
        who: path.split('/')[0].slice(1),
        deck: path.split('/')[1],
      }
    }
    this.action("rote", "rote-action", action)
        .catch(console.log)
        .then(callback)
  }
}

export let api = new UrbitApi();
window.api = api;
