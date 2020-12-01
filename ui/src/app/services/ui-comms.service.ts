import { Injectable } from '@angular/core'
import { BehaviorSubject } from 'rxjs'
import { AppInstalledPreview } from '../models/app-types'

@Injectable({
  providedIn: 'root',
})
export class UiComms {
  $isViewingUi$ = new BehaviorSubject(false)
}