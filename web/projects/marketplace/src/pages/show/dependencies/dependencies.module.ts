import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { RouterModule } from '@angular/router'
import { EmverPipesModule, ResponsiveColDirective } from '@start9labs/shared'
import { DependenciesComponent } from './dependencies.component'
import { TuiAvatarModule } from '@taiga-ui/experimental'
@NgModule({
  imports: [
    CommonModule,
    RouterModule,
    ResponsiveColDirective,
    TuiAvatarModule,
    EmverPipesModule,
  ],
  declarations: [DependenciesComponent],
  exports: [DependenciesComponent],
})
export class DependenciesModule {}
