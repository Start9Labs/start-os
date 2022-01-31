import { parse } from 'node-html-parser'
import * as fs from 'fs'

let index = fs.readFileSync('./dist/ui/index.html').toString('utf-8')

const root = parse(index)
for (let elem of root.querySelectorAll('link')) {
  if (elem.getAttribute('rel') === 'stylesheet') {
    const sheet = fs
      .readFileSync('./dist/ui/' + elem.getAttribute('href'))
      .toString('utf-8')
    index = index.replace(elem.toString(), '<style>' + sheet + '</style>')
  }
}
fs.writeFileSync('./dist/ui/index.html', index)
