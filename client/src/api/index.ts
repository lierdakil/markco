import {asyncRequest} from './request'

export async function render (name: string) {
  return asyncRequest<string[]>('GET', `/projects/${name}/render`)
}

export async function getSource (name: string, chunk: number) {
  return asyncRequest<string>('GET', `/projects/${name}/source/${chunk}`)
}

export async function update (name: string, chunk: number, value: string) {
  return asyncRequest<string[]>('PATCH', `/projects/${name}/update/${chunk}`, value)
}
