import { Injectable, inject } from '@angular/core'
import { Observable } from 'rxjs'
import { AbstractCategoryService } from '@start9labs/marketplace'
import { Router } from '@angular/router'

@Injectable()
export class CategoryService extends AbstractCategoryService {
  private readonly router = inject(Router)

  getCategory$(): Observable<string> {
    return this.category$
  }

  changeCategory(category: string) {
    this.category$.next(category)
  }

  setQuery(query: string) {
    this.query$.next(query)
  }

  getQuery$(): Observable<string> {
    return this.query$
  }

  resetQuery() {
    this.query$.next('')
  }

  handleNavigation() {
    this.router.navigate([])
  }
}
