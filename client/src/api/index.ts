const host = 'http://localhost:8081'

import {asyncRequest} from './request'

export async function render (name: string) {
  return asyncRequest<string[]>('GET', `${host}/projects/${name}/render`)
}

export async function getSource (name: string, chunk: number) {
  return asyncRequest<string>('GET', `${host}/projects/${name}/source/${chunk}`)
}

export async function update (name: string, chunk: number, value: string) {
  return asyncRequest<string[]>('PATCH', `${host}/projects/${name}/update/${chunk}`, value)
}
