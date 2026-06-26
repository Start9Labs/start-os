import { CommonModule } from '@angular/common'
import { Component, input } from '@angular/core'
import { RouterLink } from '@angular/router'
import { i18nPipe } from '@start9labs/shared'
import { TuiCardLarge, TuiHeader } from '@taiga-ui/layout'
import { MarketplacePkgBase } from '../types'
import { MarketplaceDependencyComponent } from './dependency.component'

@Component({
  selector: 'marketplace-dependencies',
  template: `
    <header tuiHeader>{{ 'Dependencies' | i18n }}</header>
    @for (dep of pkg().dependencyMetadata | keyvalue; track $index) {
      <a
        marketplaceDependency
        queryParamsHandling="merge"
        [dep]="dep"
        [pkg]="pkg()"
        [routerLink]="[]"
        [queryParams]="{ id: dep.key }"
      ></a>
    }
  `,
  hostDirectives: [TuiCardLarge],
  imports: [
    CommonModule,
    MarketplaceDependencyComponent,
    i18nPipe,
    TuiHeader,
    RouterLink,
  ],
})
export class MarketplaceDependenciesComponent {
  readonly pkg = input.required<MarketplacePkgBase>()
}
