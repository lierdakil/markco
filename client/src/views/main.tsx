import * as React from 'react'
import {Doc} from './doc'
import {FileBrowser} from './file-browser'
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
        <div className="container">
          <ProjectList />
          { <Route exact path="/docs/:name" component={Doc} /> }
          { <Route exact path="/docs/:name" component={FileBrowser} /> }
        </div>
      </Router>
    )
  }
}
