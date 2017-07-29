import * as React from 'react'
import {Doc} from './views'
import * as ReactDOM from 'react-dom'

async function init () {
  ReactDOM.render(
    <Doc name="project"/>,
    document.body
  )
}

init()
