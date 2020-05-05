import React, { Component } from 'react'

export class IconHome extends Component {
  render() {
    const classes = this.props.classes ? this.props.classes : ''
    return (
      <img
        className={'invert-d ' + classes}
        src='/~rote/img/Home.png'
        width={16}
        height={16} />
    )
  }
}
