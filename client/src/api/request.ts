// tslint:disable: no-null-keyword
export async function asyncRequest<T> (
  type: string, dest: string, body: {} | null = null, bodyType: string = 'application/json'
): Promise<T> {
  const xhr = new XMLHttpRequest()
  xhr.open(type, dest, true)
  xhr.setRequestHeader('Accept', 'application/json')
  const res = new Promise<T>((resolve, reject) => {
    xhr.onreadystatechange = (e) => {
      if (xhr.readyState === 4) {
        if (xhr.status === 204 || xhr.status === 205) {
          resolve()
        } else if (xhr.status >= 200 && xhr.status < 300) {
          resolve(JSON.parse(xhr.responseText))
        } else {
          reject(JSON.parse(xhr.responseText))
        }
      }
    }
    if (body != null && bodyType === 'application/json') {
      xhr.setRequestHeader('Content-Type', 'application/json;charset=UTF-8')
      xhr.send(JSON.stringify(body))
    } else if (body != null) {
      xhr.setRequestHeader('Content-Type', 'application/octet-stream')
      xhr.send(body)
    } else {
      xhr.send(null)
    }
  })
  return res
}
