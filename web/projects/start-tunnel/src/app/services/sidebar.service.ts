import { Injectable, signal } from '@angular/core'

@Injectable({
  providedIn: 'root',
})
export class SidebarService {
  readonly start = signal(false)
}
