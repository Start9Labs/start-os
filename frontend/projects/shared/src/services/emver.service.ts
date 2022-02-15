import { Injectable } from '@angular/core'
import * as emver from '@start9labs/emver'

@Injectable({
  providedIn: 'root',
})
export class Emver {
  constructor() {}

  compare(lhs: string, rhs: string): number {
    if (!lhs || !rhs) return null
    return emver.compare(lhs, rhs)
  }

  satisfies(version: string, range: string): boolean {
    return emver.satisfies(version, range)
  }
}
