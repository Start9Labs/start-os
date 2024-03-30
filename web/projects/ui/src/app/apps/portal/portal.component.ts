import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import {
  ActivatedRoute,
  NavigationEnd,
  Router,
  RouterOutlet,
} from '@angular/router'
import { tuiDropdownOptionsProvider } from '@taiga-ui/core'
import { PatchDB } from 'patch-db-client'
import { filter, map } from 'rxjs'
import { TabsComponent } from 'src/app/apps/portal/components/tabs.component'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { HeaderComponent } from './components/header/header.component'
import { BreadcrumbsService } from './services/breadcrumbs.service'

@Component({
  standalone: true,
  template: `
    <header appHeader>{{ name$ | async }}</header>
    <main [attr.data-dashboard]="tab$ | async"><router-outlet /></main>
    <app-tabs />
  `,
  styles: [
    `
      :host {
        // TODO: Theme
        background: url(/assets/img/background_dark.jpeg);
        background-size: cover;
      }

      main {
        flex: 1;
        overflow: hidden;
      }
    `,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [CommonModule, RouterOutlet, HeaderComponent, TabsComponent],
  providers: [
    // TODO: Move to global
    tuiDropdownOptionsProvider({
      appearance: 'start-os',
    }),
  ],
})
export class PortalComponent {
  private readonly breadcrumbs = inject(BreadcrumbsService)
  // TODO: Refactor to (activate) on <router-outlet> when routing structure becomes flat
  private readonly _ = inject(Router)
    .events.pipe(
      filter((event): event is NavigationEnd => event instanceof NavigationEnd),
      takeUntilDestroyed(),
    )
    .subscribe(e => {
      this.breadcrumbs.update(e.url.replace('/portal/service/', ''))
    })

  readonly name$ = inject(PatchDB<DataModel>).watch$('ui', 'name')
  readonly tab$ = inject(ActivatedRoute).queryParams.pipe(
    map(params => params['tab']),
  )
}
