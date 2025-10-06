import { ChangeDetectionStrategy, Component } from '@angular/core'
import { RouterOutlet, Routes } from '@angular/router'

@Component({
  template: '<router-outlet />',
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [RouterOutlet],
})
export class Outbound {}

export default [
  {
    path: '',
    component: Outbound,
    children: [
      {
        path: '',
        loadComponent: () => import('./routes/table'),
      },
      {
        path: ':label',
        loadComponent: () => import('./routes/vpn'),
      },
    ],
  },
] satisfies Routes
