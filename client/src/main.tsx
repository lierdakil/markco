import * as React from 'react'
import {Doc} from './views'
import * as ReactDOM from 'react-dom'
import initSyntax from './syntax'

async function init () {
  initSyntax()
  ReactDOM.render(
    <Doc name="project"/>,
    document.getElementById('root')
  )
}

init()
