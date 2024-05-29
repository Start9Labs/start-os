import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { NavigationEnd, Router, RouterOutlet } from '@angular/router'
import { TuiScrollbarModule } from '@taiga-ui/core'
import { PatchDB } from 'patch-db-client'
import { filter } from 'rxjs'
import { TabsComponent } from 'src/app/routes/portal/components/tabs.component'
import { BreadcrumbsService } from 'src/app/services/breadcrumbs.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { HeaderComponent } from './components/header/header.component'

@Component({
  standalone: true,
  template: `
    <header appHeader>{{ name$ | async }}</header>
    <main>
      <tui-scrollbar [style.max-height.%]="100">
        <router-outlet />
      </tui-scrollbar>
    </main>
    <app-tabs />
  `,
  styles: [
    `
      :host {
        height: 100%;
        display: flex;
        flex-direction: column;
        // TODO: Theme
        background: url(/assets/img/background_dark.jpeg) fixed center/cover;
      }

      main {
        flex: 1;
        overflow: hidden;
        margin-bottom: var(--bumper);
      }
    `,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    CommonModule,
    RouterOutlet,
    HeaderComponent,
    TabsComponent,
    TuiScrollbarModule,
  ],
})
export class PortalComponent {
  private readonly breadcrumbs = inject(BreadcrumbsService)
  private readonly _ = inject(Router)
    .events.pipe(
      filter((event): event is NavigationEnd => event instanceof NavigationEnd),
      takeUntilDestroyed(),
    )
    .subscribe(e => {
      this.breadcrumbs.update(e.url.replace('/portal/service/', ''))
    })

  readonly name$ = inject(PatchDB<DataModel>).watch$('ui', 'name')
}
