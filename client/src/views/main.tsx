import * as React from 'react'
import {Doc} from './doc'
import {FileBrowser} from './file-browser'
import {ProjectList} from './project-list'
import {Login} from './login'
import {
  BrowserRouter as Router,
  Route, Switch, Redirect
} from 'react-router-dom'
import {Grid, Row, Col, Collapse, Button, Glyphicon} from 'react-bootstrap'

export interface State {
  leftPanel: boolean
  rightPanel: boolean
}

export class Main extends React.Component<{}, State> {
  private fileBrowser?: FileBrowser
  constructor () {
    super()
    this.state = {
      leftPanel: false,
      rightPanel: true
    }
  }

  public render () {
    if (!sessionStorage.getItem('auth')) {
      sessionStorage.setItem('return-url', location.href)
    }
    return (
      <Grid>
        <Router>
          <Switch>
            <Route path="/login">
              {sessionStorage.getItem('auth') ?
              <Redirect to="/"/> :
              <Row>
                <Col md={12}>
                  <Login/>
                </Col>
              </Row>
              }
            </Route>
            <Route path="/">
              {!sessionStorage.getItem('auth') ?
              <Redirect to="/login"/> :
              <Switch>
                <Route path="/" exact>
                  <Row>
                    <Col md={12}>
                      <ProjectList/>
                    </Col>
                  </Row>
                </Route>
                <Route path="/docs/:name">
                  { ({match}) => match &&
                    <Row>
                      <Button className="btn-panel btn-panel-left"
                        onClick={() => {this.setState({leftPanel: !this.state.leftPanel})}}>
                        <Glyphicon
                          glyph={`chevron-${this.state.leftPanel ? 'left' : 'right'}`}
                          />
                      </Button>
                      <Button className="btn-panel btn-panel-right"
                        onClick={() => {this.setState({rightPanel: !this.state.rightPanel})}}>
                        <Glyphicon
                          glyph={`chevron-${this.state.rightPanel ? 'right' : 'left'}`}
                          />
                      </Button>
                      <Collapse dimension="width" in={this.state.leftPanel}>
                        <Col md={3}>
                          <ProjectList/>
                        </Col>
                      </Collapse>
                      <Col md={
                          12
                          - (this.state.leftPanel ? 3 : 0)
                          - (this.state.rightPanel ? 3 : 0)
                        }>
                        <Doc
                          name={match.params.name}
                          onPaste={() => {this.fileBrowser && this.fileBrowser.update()}}
                        />
                      </Col>
                      <Collapse dimension="width" in={this.state.rightPanel}>
                        <Col md={3}>
                          <FileBrowser
                            ref={(fb) => { fb && (this.fileBrowser = fb) }}
                            name={match.params.name}/>
                        </Col>
                      </Collapse>
                    </Row>
                  }
                </Route>
              </Switch>}
            </Route>
          </Switch>
        </Router>
      </Grid>
    )
  }
}
