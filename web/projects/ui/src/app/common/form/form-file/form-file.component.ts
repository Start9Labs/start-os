import { Component } from '@angular/core'
import { TuiFileLike } from '@taiga-ui/kit'
import { CT } from '@start9labs/start-sdk'
import { Control } from '../control'

@Component({
  selector: 'form-file',
  templateUrl: './form-file.component.html',
  styleUrls: ['./form-file.component.scss'],
})
export class FormFileComponent extends Control<CT.ValueSpecFile, TuiFileLike> {}
