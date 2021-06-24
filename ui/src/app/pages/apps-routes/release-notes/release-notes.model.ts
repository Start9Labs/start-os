import { Injectable } from '@angular/core'

@Injectable({
  providedIn: 'root',
})
export class ReleaseNoteModel {
  releaseNotes: { [version: string]: string}

  constructor () { }
}


