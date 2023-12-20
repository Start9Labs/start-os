import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { RouterOutlet } from '@angular/router'
import { tuiDropdownOptionsProvider } from '@taiga-ui/core'
import { PatchDB } from 'patch-db-client'
import { HeaderComponent } from './components/header/header.component'
import { NavigationComponent } from './components/navigation.component'
import { DrawerComponent } from './components/drawer/drawer.component'
import { DataModel } from 'src/app/services/patch-db/data-model'

@Component({
  standalone: true,
  template: `
    <header appHeader>{{ name$ | async }}</header>
    <nav appNavigation></nav>
    <main><router-outlet /></main>
    <app-drawer />
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
  imports: [
    CommonModule,
    RouterOutlet,
    HeaderComponent,
    NavigationComponent,
    DrawerComponent,
  ],
  providers: [
    // TODO: Move to global
    tuiDropdownOptionsProvider({
      appearance: 'start-os',
    }),
  ],
})
export class PortalComponent {
  readonly name$ = inject(PatchDB<DataModel>).watch$('ui', 'name')
}
