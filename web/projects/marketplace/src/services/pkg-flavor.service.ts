import { BehaviorSubject, Observable } from 'rxjs'

export abstract class AbstractPkgFlavorService {
  readonly flavorActive$ = new BehaviorSubject<boolean>(false)
  abstract getFlavorStatus$(): Observable<boolean>
  abstract toggleFlavorStatus(status: boolean): void
}
