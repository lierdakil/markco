// tslint:disable: no-null-keyword
export async function asyncRequest<T> (
  type: string, dest: string, body: {} | null = null, result: string = 'application/json'
): Promise<T> {
  const xhr = new XMLHttpRequest()
  xhr.open(type, dest, true)
  xhr.setRequestHeader('Accept', result)
  const res = new Promise<T>((resolve, reject) => {
    xhr.onreadystatechange = (e) => {
      if (xhr.readyState === 4) {
        if (xhr.status === 204 || xhr.status === 205) {
          resolve()
        } else if (xhr.status >= 200 && xhr.status < 300) {
          if (result === 'application/json') {
            resolve(JSON.parse(xhr.responseText))
          } else {
            resolve(xhr.responseText as any)
          }
        } else {
          if (result === 'application/json') {
            reject(JSON.parse(xhr.responseText))
          } else {
            reject(xhr.responseText)
          }
        }
      }
    }
    if (body != null) {
      xhr.setRequestHeader('Content-Type', 'application/json;charset=UTF-8')
      xhr.send(JSON.stringify(body))
    } else {
      xhr.send(null)
    }
  })
  return res
}
