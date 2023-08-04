import { BehaviorSubject, Observable } from 'rxjs'

export abstract class AbstractCategoryService {
  readonly category$ = new BehaviorSubject<string>('all')
  readonly query$ = new BehaviorSubject<string>('')

  abstract getCategory$(): Observable<string>

  abstract changeCategory(category: string): void

  abstract setQuery(query: string): void

  abstract getQuery$(): Observable<string>

  abstract resetQuery(): void
}
