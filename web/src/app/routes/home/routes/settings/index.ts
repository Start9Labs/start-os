import { ChangeDetectionStrategy, Component } from '@angular/core'
import {
  RouterLink,
  RouterLinkActive,
  RouterOutlet,
  Routes,
} from '@angular/router'
import { TuiTitle } from '@taiga-ui/core'
import { TuiTabs } from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { ToCamelPipe } from 'src/app/pipes/to-camel.pipe'

@Component({
  template: `
    <header tuiHeader>
      <hgroup tuiTitle><h2>System Settings</h2></hgroup>
    </header>
    <tui-tabs>
      @for (tab of tabs; track $index) {
        <a tuiTab routerLinkActive [routerLink]="tab | toCamel">
          {{ tab }}
        </a>
      }
    </tui-tabs>
    <router-outlet />
  `,
  styles: `
    :host {
      height: stretch;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    RouterOutlet,
    RouterLink,
    RouterLinkActive,
    TuiHeader,
    TuiTitle,
    TuiTabs,
    ToCamelPipe,
  ],
})
class Settings {
  protected readonly tabs = ['General', 'Security', 'Logs']
}

export default [
  {
    path: '',
    component: Settings,
    children: [
      {
        path: 'general',
        loadComponent: () => import('./routes/general'),
      },
      {
        path: 'security',
        loadComponent: () => import('./routes/security'),
      },
      {
        path: 'logs',
        loadComponent: () => import('./routes/logs'),
      },
      {
        path: '**',
        redirectTo: 'general',
      },
    ],
  },
] satisfies Routes
