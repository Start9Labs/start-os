import { Subject, BehaviorSubject } from 'rxjs'
import { PropertySubject, initPropertySubject, complete, peekProperties, PropertySubjectId } from './property-subject.util'
import { NgZone } from '@angular/core'
import { both, diff } from './misc.util'

export type Update<T extends { id: string }> = Partial<T> & {
  id: string
}
export type Delta<T> = { action: 'add' | 'delete', id: string } | { action: 'update', id: string, effectedFields: Partial<T> }

export class MapSubject<T extends { id: string }> {
  contents: { [id: string]: PropertySubject<T> } = { }
  $delta$ = new Subject<Delta<T>>()

  constructor (
    private readonly zone: NgZone = new NgZone({ shouldCoalesceEventChangeDetection: true }),
  ) { }

  get ids () : string[] { return Object.keys(this.contents) }
  get all () : T[] { return this.ids.map(id => this.peek(id) as T) }

  getContents () : PropertySubjectId<T>[] {
    return Object.entries(this.contents).map( ([k, v]) => ({ id: k, subject: v }))
  }

  add (t: T): void {
    this.contents[t.id] = initPropertySubject(t)
    this.$delta$.next({ action: 'add', id: t.id })
  }

  delete (id: string): void {
    const t$ = this.contents[id]
    if (!t$) return
    complete(t$)
    delete this.contents[id]
    this.$delta$.next({ action: 'delete', id })
  }

  update (newValues: Update<T>): void {
    const t$ = this.contents[newValues.id] as PropertySubject<T>

    if (!t$) {
      this.contents[newValues.id] = initPropertySubject(newValues) as PropertySubject<T>
      return
    }

    const effectedFields = { }
    const oldKeys = Object.keys(t$)
    const newKeys = Object.keys(newValues)

    const newKeysInUpdate = diff(newKeys, oldKeys)
    newKeysInUpdate.forEach(keyToAdd => {
      t$[keyToAdd] = new BehaviorSubject(newValues[keyToAdd])
      effectedFields[keyToAdd] = newValues[keyToAdd]
    })

    const keysToUpdate = both(newKeys, oldKeys)
    keysToUpdate.forEach(keyToUpdate => {
      const valueToUpdate = newValues[keyToUpdate]
      if (JSON.stringify(t$[keyToUpdate].getValue()) !== JSON.stringify(valueToUpdate)) {
        this.zone.run(() => t$[keyToUpdate].next(valueToUpdate))
        effectedFields[keyToUpdate] = newValues[keyToUpdate]
      }
    })

    if (Object.keys(effectedFields).length) {
      this.$delta$.next({
        action: 'update',
        id: newValues.id,
        effectedFields,
      })
    }
  }

  watch (id: string): undefined | PropertySubject<T> {
    return this.contents[id]
  }

  peek (id: string): T | undefined {
    return this.contents[id] && peekProperties(this.contents[id])
  }
}
