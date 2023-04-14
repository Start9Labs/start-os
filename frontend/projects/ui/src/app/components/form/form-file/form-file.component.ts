import { Component } from '@angular/core'
import { TuiFileLike } from '@taiga-ui/kit'
import { ValueSpecFile } from 'start-sdk/lib/config/configTypes'
import { Control } from '../control'

@Component({
  selector: 'form-file',
  templateUrl: './form-file.component.html',
  styleUrls: ['./form-file.component.scss'],
})
export class FormFileComponent extends Control<ValueSpecFile, TuiFileLike> {}
