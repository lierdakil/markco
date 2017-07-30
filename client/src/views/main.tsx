import * as React from 'react'
import {Doc} from './doc'
import {FileBrowser} from './file-browser'
import {ProjectList} from './project-list'
import {Login} from './login'
import {
  BrowserRouter as Router,
  Route, Switch, Redirect
} from 'react-router-dom'

export class Main extends React.Component {
  constructor () {
    super()
  }

  public render () {
    if (!sessionStorage.getItem('auth')) {
      sessionStorage.setItem('return-url', location.href)
    }
    return (
      <Router>
          <Switch>
            <Route path="/login" component={Login} />
            <Route path="/">
              { sessionStorage.getItem('auth') ?
                <div className="container">
                  <ProjectList />
                  <Route exact path="/docs/:name" component={Doc} />
                  <Route exact path="/docs/:name" component={FileBrowser} />
                </div>
                : <Redirect to="/login" />
              }
            </Route>
          </Switch>
      </Router>
    )
  }
}
