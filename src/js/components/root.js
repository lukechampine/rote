
import React, { useReducer, useEffect } from 'react'
import { BrowserRouter, Route } from 'react-router-dom'
import { Map, Record } from 'immutable'
import { HeaderBar } from './header-bar.js'
import { Sidebar } from './sidebar.js'
import { ImportDeck } from './import.js'
import { DeckViewer } from './deck-viewer.js'

const StateRecord = Record({
  decks: Map({}),
})

function reducer(state, action) {
  if (action.type === 'init') {
    return state.set('decks', action.payload)
  } else if (action.type === 'subscribe-event') {
    const deck = action.payload
    return state.setIn(['decks', deck.id], deck)
  }

  const deck = state.decks.get(action.payload.id)
  const len = deck.cards.size
  const setIndex = (i) => {
    return state.mergeIn(['decks', deck.id], {
      cardIndex: i,
      answerShown: false, // hide answer when changing cards
    })
  }

  switch (action.type) {
  case 'next-card':
    return setIndex((deck.cardIndex+1) % len)
  
  case 'prev-card':
    return setIndex((deck.cardIndex+len-1) % len)
  
  case 'rand-card':
    // make sure we select a different card -- unless there's only one, of course
    let newIndex
    do {
        newIndex = Math.floor(Math.random() * len)
    } while (len > 1 && newIndex === deck.cardIndex)
    return setIndex(newIndex)
  
  case 'reveal-answer':
    return state.setIn(['decks', deck.id, 'answerShown'], true)

  default:
    console.error(`unrecognized action type '${action.type}'`)
    console.error(action.payload)
  }
}

export function Root() {
  const [state, dispatch] = useReducer(reducer, StateRecord({}));

  // set up subscriptions
  useEffect(() => {
    api.initBindings(
      e => dispatch({type: 'subscribe-event', payload: e}),
      e => dispatch({type: 'subscribe-error', payload: e}),
    )
  }, [])

  // fetch initial state
  useEffect(() => {
    window.api.fetchDecks(decks => dispatch({type: 'init', payload: decks}))
  }, [])

  return (
    <BrowserRouter>
      <div className='absolute h-100 w-100 bg-gray0-d ph4-m ph4-l ph4-xl pb4-m pb4-l pb4-xl'>
      <HeaderBar/>
      <Route exact path='/~rote' render={ () => {
        return (
          <div className='ba-xl flex h-100'>
            <Sidebar decks={state.decks}/>
            <div className='center h-100 w-100 overflow-x-hidden flex flex-column bg-white bg-gray0-d dn db-ns'>
                <div className='dt pb3 w-100 h-100'>
                  <div className='dtc v-mid f9 gray2 tc lh-copy'>
                    <p>Select a deck to begin.</p>
                    <br/>
                    <p>PROTIP: You can navigate between cards with ← and →,</p>
                    <p>and reveal a card's answer with Enter↵.</p>
                  </div>
                </div>
            </div>
          </div>
        )}}
      />

      <Route exact path='/~rote/import' render={ (props) => {
        return (
          <div className='cf w-100 flex ba-m ba-l ba-xl b--gray4 b--gray1-d br1 h-100 h-100-minus-40-m h-100-minus-40-l h-100-minus-40-xl'>
            <Sidebar decks={state.decks}/>
            <ImportDeck />
          </div>
        )
      }}/>

      <Route exact path='/~rote/deck/:ship/:path' render={ (props) => {
        const id = `${props.match.params.ship}/${props.match.params.path}`
        const deck = state.decks.get(id)
        if (!deck) {
          return <p>Loading decks...</p>
        }
        return (
          <div className='cf w-100 flex ba-m ba-l ba-xl b--gray4 b--gray1-d br1 h-100 h-100-minus-40-m h-100-minus-40-l h-100-minus-40-xl'>
            <Sidebar decks={state.decks}/>
            <DeckViewer deck={deck} dispatch={dispatch}/>
          </div>
        )}}
      /> 
      </div>
    </BrowserRouter>
  )
}
