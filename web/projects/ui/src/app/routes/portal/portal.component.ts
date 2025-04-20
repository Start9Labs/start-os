import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { RouterOutlet } from '@angular/router'
import { TuiScrollbar } from '@taiga-ui/core'
import { PatchDB } from 'patch-db-client'
import { TabsComponent } from 'src/app/routes/portal/components/tabs.component'
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
      @import '@taiga-ui/core/styles/taiga-ui-local';

      :host {
        height: 100%;
        display: flex;
        flex-direction: column;
        // @TODO Theme
        background: url(/assets/img/background_dark.jpeg) fixed center/cover;
      }

      main {
        flex: 1;
        overflow: hidden;
        margin: 0 var(--bumper) var(--bumper);
        filter: grayscale(1) brightness(0.75);

        @include transition(filter);

        header:has([data-status='success']) + &,
        header:has([data-status='neutral']) + & {
          filter: none;
        }
      }
    `,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    CommonModule,
    RouterOutlet,
    HeaderComponent,
    TabsComponent,
    TuiScrollbar,
  ],
})
export class PortalComponent {
  readonly name$ = inject<PatchDB<DataModel>>(PatchDB).watch$('ui', 'name')
}
