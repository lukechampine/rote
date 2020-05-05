import React from 'react';
import ReactDOM from 'react-dom';
import { Root } from '/components/root';
import { api } from '/api';

api.setAuthTokens({
  ship: window.ship
});

ReactDOM.render((
  <Root />
), document.querySelectorAll('#root')[0]);
