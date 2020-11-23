import { BehaviorSubject, Subject } from 'rxjs'

export interface Loadable {
  load: () => void
  $loading$: BehaviorSubject<boolean> //will be true during load function
  $cancel$: Subject<void> //will cancel load function
}

export interface Colorable {
  $color$: BehaviorSubject<string>
}
