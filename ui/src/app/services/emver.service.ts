import { Injectable } from '@angular/core'

@Injectable({
  providedIn: 'root',
})
export class Emver {
  private e: typeof import('@start9labs/emver')
  constructor () { }

  async init () {
    this.e = await import('@start9labs/emver')
  }

  compare (lhs: string, rhs: string): number {
    return this.e.compare(lhs, rhs)
  }

  satisfies (version: string, range: string): boolean {
    return this.e.satisfies(version, range)
  }
}