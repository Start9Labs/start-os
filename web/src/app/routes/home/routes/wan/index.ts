import { ChangeDetectionStrategy, Component } from '@angular/core'
import {
  RouterLink,
  RouterLinkActive,
  RouterOutlet,
  Routes,
} from '@angular/router'
import { TuiButton, TuiTitle } from '@taiga-ui/core'
import { TuiTabs } from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { Help } from 'src/app/directives/help.directive'
import { ToCamelPipe } from 'src/app/pipes/to-camel.pipe'

import { WanAside } from './aside'

@Component({
  template: `
    <wan-aside *help />
    <header tuiHeader>
      <hgroup tuiTitle><h2>Internet (WAN)</h2></hgroup>
    </header>
    <tui-tabs>
      @for (tab of tabs; track $index) {
        <a tuiTab routerLinkActive [routerLink]="tab | toCamel">
          {{ tab }}
        </a>
      }
    </tui-tabs>
    <router-outlet />
    <footer class="g-footer">
      <button tuiButton appearance="flat-grayscale">Cancel</button>
      <button tuiButton>Save</button>
    </footer>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    RouterOutlet,
    RouterLink,
    RouterLinkActive,
    TuiHeader,
    TuiTitle,
    TuiTabs,
    TuiButton,
    Help,
    WanAside,
    ToCamelPipe,
  ],
})
class Wan {
  protected readonly tabs = ['IPv4', 'IPv6', 'MAC Address', 'Dynamic DNS']
}

export default [
  {
    path: '',
    component: Wan,
    children: [
      {
        path: 'ipv4',
        loadComponent: () => import('./routes/ipv4'),
      },
      {
        path: 'ipv6',
        loadComponent: () => import('./routes/ipv6'),
      },
      {
        path: 'mac-address',
        loadComponent: () => import('./routes/mac'),
      },
      {
        path: 'dynamic-dns',
        loadComponent: () => import('./routes/dns'),
      },
      {
        path: '**',
        redirectTo: 'ipv4',
      },
    ],
  },
] satisfies Routes
