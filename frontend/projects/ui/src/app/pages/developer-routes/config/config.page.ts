import { Component } from '@angular/core'
import { CodeModel } from '@ngstack/code-editor'

@Component({
  selector: 'config',
  templateUrl: 'config.page.html',
  styleUrls: ['config.page.scss'],
})
export class ConfigPage {
  theme = 'vs-dark'

  model: CodeModel = {
    language: 'json',
    uri: 'main.json',
    value: '{}',
  }

  options = {
    contextmenu: true,
    minimap: {
      enabled: true,
    },
  }

  onCodeChanged(value) {
    console.log('CODE', value)
  }
}
