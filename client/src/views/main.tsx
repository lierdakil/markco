import * as React from 'react'
import {Doc} from './doc'
import {ProjectList} from './project-list'
import {
  BrowserRouter as Router,
  Route
} from 'react-router-dom'

export class Main extends React.Component {
  constructor () {
    super()
  }

  public render () {
    return (
      <Router>
        <div>
          { <Route exact path="/docs/:name" component={Doc} /> }
          <ProjectList />
        </div>
      </Router>
    )
  }
}
