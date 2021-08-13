import { Injectable } from '@angular/core'
import * as emver from '@start9labs/emver'


@Injectable({
  providedIn: 'root',
})
export class Emver {
  constructor () { }

  compare (lhs: string, rhs: string): number {
    console.log('EMVER', emver)
    const compare = emver.compare(lhs, rhs)
    console.log('COMPARE', compare)
    return compare
  }

  satisfies (version: string, range: string): boolean {
    return emver.satisfies(version, range)
  }
}