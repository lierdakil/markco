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
        } else {
          if (auth && (xhr.status === 401 || xhr.status === 403)) {
            sessionStorage.removeItem('auth')
            sessionStorage.setItem('return-url', location.href)
            location.reload()
          }
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
