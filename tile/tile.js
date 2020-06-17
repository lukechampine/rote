import React from 'react';

export function roteTile() {
  return (
    <div className={"w-100 h-100 relative bg-white bg-gray0-d ba b--black b--gray1-d"}>
      <a className="w-100 h-100 db bn" href="/~rote">
        <p className="black white-d f9 absolute" style={{ left: 8, top: 8 }}>
           Flashcards
        </p>
        <img className="absolute invert-d" src="/~rote/img/Tile.png"
          style={{ left: 22, top: 32, height: 75, width: 75 }}
        />
      </a>
    </div>
  )
}

window.roteTile = roteTile;
