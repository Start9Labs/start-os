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
    console.log('EMVER', this.e)
    const compare = this.e.compare(lhs, rhs)
    console.log('COMPARE', compare)
    return compare
  }

  satisfies (version: string, range: string): boolean {
    return this.e.satisfies(version, range)
  }
}