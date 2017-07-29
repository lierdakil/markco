import * as React from 'react'
import {Main} from './views'
import * as ReactDOM from 'react-dom'
import initSyntax from './syntax'

async function init () {
  initSyntax()
  ReactDOM.render(
    <Main/>,
    document.getElementById('root')
  )
}

init()
