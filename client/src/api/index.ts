import {asyncRequest} from './request'

export async function listProjects () {
  return asyncRequest<string[]>('GET', `/projects`)
}

export async function render (name: string) {
  return asyncRequest<string[]>('GET', `/projects/${name}/render`)
}

export async function getSource (name: string, chunk: number) {
  return asyncRequest<string>('GET', `/projects/${name}/source/${chunk}`)
}

export async function update (name: string, chunk: number, value: string) {
  return asyncRequest<string[]>('PATCH', `/projects/${name}/update/${chunk}`, value)
}

export async function upload (name: string, content: ArrayBuffer) {
  return asyncRequest<string>('POST', `/projects/${name}/upload`, content, 'application/octet-stream')
}

export interface FileInfo {
  fileName: string
  fileURI: string
}

export async function fileList (name: string) {
  return asyncRequest<FileInfo[]>('GET', `/projects/${name}/files`)
}
