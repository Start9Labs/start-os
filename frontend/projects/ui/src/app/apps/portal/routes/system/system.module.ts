import { NgModule } from '@angular/core'
import { RouterModule, Routes } from '@angular/router'
import { systemTabResolver } from '../../utils/system-tab-resolver'
import { toDesktopItem } from '../../utils/to-desktop-item'

const ROUTES: Routes = [
  {
    title: systemTabResolver,
    path: 'backups',
    loadComponent: () =>
      import('./backups/backups.component').then(m => m.BackupsComponent),
    data: toDesktopItem('/portal/system/backups'),
  },
  {
    title: systemTabResolver,
    path: 'snek',
    loadComponent: () =>
      import('./snek/snek.component').then(m => m.SnekComponent),
    data: toDesktopItem('/portal/system/snek'),
  },
]

@NgModule({
  imports: [RouterModule.forChild(ROUTES)],
  declarations: [],
  exports: [],
})
export class SystemModule {}
