import React from 'react'
import { Link, useLocation } from 'react-router-dom';

function SidebarItem({deck}) {
  const location = useLocation()
  const linkURL = `/~rote/deck/${deck.ship}/${deck.path}`
  const selectedClass = (location.pathname === linkURL) ? "bg-gray5 bg-gray1-d c-default" : "pointer hover-bg-gray5 hover-bg-gray1-d";
  return (
    <Link to={linkURL}>
      <div className={"w-100 v-mid f9 ph4 pv1 " + selectedClass}>
        <p className="dib f9">{deck.title}</p>
      </div>
    </Link>
  )
}

export function Sidebar({decks, selected}) {
  const deckList = Object.values(decks).sort((a, b) => a.title.localeCompare(b.title))
  return (
    <div className="bn br-m br-l br-xl b--gray4 b--gray1-d lh-copy min-w14 h-100 flex-shrink-0 pt3 pt0-m pt0-l pt0-xl relative overflow-y-hidden dn-s flex-basis-100-s flex-basis-250-ns">
      <div className="w-100 f9">
        <Link to="/~rote/import" className="dib f9 green2 pa4">
          Import Deck
        </Link>
      </div>
      <div className="overflow-y-auto pb1">
        {deckList.map((deck, i) => (
          <SidebarItem
            key={i}
            deck={deck}
            selected={deck.path === selected}
          />
        ))}
      </div>
    </div>
  )
}
