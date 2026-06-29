import { Injectable, signal } from '@angular/core'

@Injectable({ providedIn: 'root' })
export class PluginsService {
  public readonly enabled = signal(false)
  public readonly size = signal(100)
}
