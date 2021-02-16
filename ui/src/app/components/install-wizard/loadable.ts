import { BehaviorSubject, Subject } from 'rxjs'

export interface Loadable {
  load: (prevResult?: any) => void
  result?: any // fill this variable on slide 1 to get passed into the load on slide 2. If this variable is falsey, it will skip the next slide.
  loading$: BehaviorSubject<boolean> // will be true during load function
  cancel$: Subject<void> // will cancel load function
}

