import * as React from 'react'
import * as api from '../api'
import { NavLink } from 'react-router-dom'

export interface PLState {
  projects: string[]
}

export class ProjectList extends React.Component<{}, PLState> {
  constructor () {
    super()
    this.state = { projects: [] }
    this.update()
  }

  public render () {
    return (
      <div className="project-list">
        <ul>
          { this.state.projects.map((prjName) =>
            <li>
              <NavLink to={`/docs/${prjName}`}>
                {prjName}
              </NavLink>
            </li>) }
        </ul>
      </div>
    )
  }

  public async update () {
    this.setState({ projects: await api.listProjects() })
  }
}
