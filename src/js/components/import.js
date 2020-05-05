import React, { useState } from 'react'
import { Link, useHistory } from 'react-router-dom'

export function ImportDeck() {
  const history = useHistory()
  const [deckPath, setDeckPath] = useState('')
  const importDeck = () => {
    window.api.importDeck(deckPath, () => {
      history.push('/~rote')
    })
  }
  return (
    <div className='h-100 w-100 pt4 overflow-x-hidden flex flex-column bg-gray0-d white-d pa3'>
      <div
        className='w-100 dn-m dn-l dn-xl inter pt1 pb6 f8'>
        <Link to='/~rote/'>{'⟵ All Decks'}</Link>
      </div>
      <h2 className='mb3 f8'>Import a Deck</h2>
      <div className='w-100'>
        <p className='f8 lh-copy mt3 db'>Enter a <span className='mono'>~ship/deck-name</span></p>
        <textarea
          className='f7 mono ba bg-gray0-d white-d pa3 mb2 db focus-b--black focus-b--white-d b--gray3 b--gray2-d nowrap'
          placeholder='~zod/times-tables'
          spellCheck='false'
          rows={1}
          onChange={e => setDeckPath(e.target.value)}
          onKeyPress={e => {
            if (e.key === 'Enter') {
              e.preventDefault()
              importDeck()
            }
          }}
          style={{
            resize: 'none',
          }} />
        <br />
        <button
          onClick={importDeck}
          className='db f9 green2 ba pa2 b--green2 bg-gray0-d pointer'>Import</button>
        <br />
        <p className='gray3'>You may need to refresh after importing, sorry! Working on it...</p>
      </div>
    </div>
  )
}
