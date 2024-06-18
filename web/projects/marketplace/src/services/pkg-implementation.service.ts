import { BehaviorSubject, Observable } from 'rxjs'

export abstract class AbstractPkgImplementationService {
  readonly altImplActive$ = new BehaviorSubject<boolean>(false)
  abstract getAltStatus$(): Observable<boolean>
  abstract toggleAltStatus(status: boolean): void
}
