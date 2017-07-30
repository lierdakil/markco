import sha1 = require('sha1')

// tslint:disable: no-null-keyword
export async function asyncRequest<T> (
  type: string, dest: string, body: {} | null = null, bodyType: string = 'application/json'
): Promise<T> {
  const xhr = new XMLHttpRequest()
  xhr.open(type, `/api${dest}`, true)
  xhr.setRequestHeader('Accept', 'application/json')
  const auth = sessionStorage.getItem('auth')
  if (auth) {
    xhr.setRequestHeader('X-Markco-Authentication', auth)
  }
  const res = new Promise<T>((resolve, reject) => {
    xhr.onreadystatechange = (e) => {
      if (xhr.readyState === 4) {
        if (xhr.status === 204 || xhr.status === 205) {
          resolve()
        } else if (xhr.status >= 200 && xhr.status < 300) {
          resolve(JSON.parse(xhr.responseText))
        } else if (xhr.status === 401 || xhr.status === 403) {
          const u = prompt('user?'), p = prompt('pass?')
          sessionStorage.setItem('auth', `${u}:${sha1(`${u}:${p}`)}`)
          resolve(asyncRequest(type, dest, body, bodyType))
        } else {
          reject(xhr.responseText && JSON.parse(xhr.responseText))
        }
      }
    }
    if (body != null && bodyType === 'application/json') {
      xhr.setRequestHeader('Content-Type', 'application/json;charset=UTF-8')
      xhr.send(JSON.stringify(body))
    } else if (body != null) {
      xhr.setRequestHeader('Content-Type', bodyType)
      xhr.send(body)
    } else {
      xhr.send(null)
    }
  })
  return res
}
