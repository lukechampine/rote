import React from 'react'
import useMousetrap from "react-hook-mousetrap"
import ReactMarkdown from 'react-markdown'

function Card({card, answerShown, reveal}) {
    return (
        <div className={"ba-xl pa4 mw6 w5 white-d " + (answerShown ? "" : "pointer")} onClick={reveal}>
            <ReactMarkdown className="pa4 tc" source={card.front} linkTarget={"_blank"} />
            <div style={{visibility: answerShown ? "" : "hidden"}}>
                <ReactMarkdown className="pa4 gray3 tc" source={card.back} linkTarget={"_blank"} />
            </div>
        </div>
    )
} 

export function DeckViewer({deck, dispatch}) {
    const nextCard = () => dispatch({type: 'next-card', payload: {id: deck.id}})
    const prevCard = () => dispatch({type: 'prev-card', payload: {id: deck.id}})
    const randCard = () => dispatch({type: 'rand-card', payload: {id: deck.id}})
    const reveal = () => dispatch({type: 'reveal-answer', payload: {id: deck.id}})
    useMousetrap('left', prevCard)
    useMousetrap('right', nextCard)
    useMousetrap('enter', reveal)

    return (
        <div className="center mw6 h-100 pt0 pt8-m pt8-l pt8-xl">
            <div className="flex justify-between mb4">
                {
                    [
                        {label: "← prev ", onClick: prevCard},
                        {label: "⚁ random", onClick: randCard},
                        {label: "next →", onClick: nextCard},
                    ].map(b => (
                        <div key={b.label} className="flex">
                            <a
                                className="gray3 pointer"
                                onClick={b.onClick}> 
                                {b.label}
                            </a>
                        </div>
                    ))
                }
            </div>
            <div className="flex-col center lh-solid h-100">
                <Card
                    card={deck.cards.get(deck.cardIndex)}
                    answerShown={deck.answerShown}
                    reveal={reveal}
                />
            </div>
        </div>
    ) 
}
