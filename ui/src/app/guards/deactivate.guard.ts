import { Injectable, Directive } from '@angular/core'
import { CanDeactivate } from '@angular/router'
import { HostListener } from '@angular/core'

@Directive()
export abstract class PageCanDeactivate {
  abstract canDeactivate (): boolean

  @HostListener('window:beforeunload', ['$event'])
  unloadNotification (e: any) {
    console.log(e)
    if (!this.canDeactivate()) {
      e.returnValue = true
    }
  }
}

@Injectable({
  providedIn: 'root',
})
export class CanDeactivateGuard implements CanDeactivate<PageCanDeactivate> {

  canDeactivate (page: PageCanDeactivate): boolean {
    return page.canDeactivate() || confirm('You have unsaved changes. Are you sure you want to leave the page?')
  }
}