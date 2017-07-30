import * as React from 'react'
import * as api from '../api'
import sha1 = require('sha1')

export class Login extends React.Component {
  public refs: {
    'login': HTMLInputElement
    'password': HTMLInputElement
  }
  constructor () {
    super()
    this.state = { invalid: false }
  }

  public render () {
    return (
      <div className="login-form-container">
        <form className="login-form" onSubmit={this.submit.bind(this)}>
          <label htmlFor="login">Login:</label>
          <input ref="login" name="login" type="text" />
          <label htmlFor="password">Password:</label>
          <input ref="password" name="password" type="password" />
          <button type="submit">Login</button>
        </form>
      </div>
    )
  }

  public async submit (ev: React.FormEvent<HTMLFormElement>) {
    ev.preventDefault()
    const u = this.refs.login.value,
          p = this.refs.password.value,
          hpw = sha1(`${u}:${p}`).toString()
    const skey = await api.login(u, hpw)
    sessionStorage.setItem('auth', skey)
    const returnUrl = sessionStorage.getItem('return-url') || '/'
    sessionStorage.removeItem('return-url')
    location.href = returnUrl
  }
}
