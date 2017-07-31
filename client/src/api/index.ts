import {asyncRequest} from './request'

export async function listProjects () {
  return asyncRequest<string[]>('GET', `/projects`)
}

export async function createProject (name: string, content: string) {
  return asyncRequest<void>('POST', `/projects/${name}`, content)
}

export async function deleteProject (name: string) {
  return asyncRequest<void>('DELETE', `/projects/${name}`)
}

export interface ChunkData {
  chunkHtml: string
  chunkSrc: string
  chunkNum: number
}

export async function render (name: string) {
  return asyncRequest<ChunkData[]>('GET', `/projects/${name}`)
}

export async function getSource (name: string, chunk: number) {
  return asyncRequest<string>('GET', `/projects/${name}/${chunk}`)
}

export async function update (name: string, chunk: number, value: string) {
  return asyncRequest<void>('PATCH', `/projects/${name}/${chunk}`, value)
}

export async function appendChunk (name: string, value: string) {
  return asyncRequest<void>('PATCH', `/projects/${name}`, value)
}

export async function upload (name: string, content: ArrayBuffer) {
  return asyncRequest<string>('POST', `/projects/${name}/files`, content, 'application/octet-stream')
}

export interface FileInfo {
  fileName: string
  fileURI: string
}

export async function fileList (name: string) {
  return asyncRequest<FileInfo[]>('GET', `/projects/${name}/files`)
}

export async function deleteFile (name: string, file: string) {
  return asyncRequest<string>('DELETE', `/projects/${name}/files/${file}`)
}

export async function login (authLogin: string, authHashedPassword: string) {
  return asyncRequest<string>('POST', `/login`, { authLogin, authHashedPassword })
}
