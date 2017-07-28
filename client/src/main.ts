import {Doc} from './views'

async function init () {
  const doc = new Doc({name: 'project'})
  // document.addEventListener('dblclick', runEdit)
  document.body.appendChild(doc.element)
  // doc.update({name: 'project'})
}

init()
