import { NgModule } from '@angular/core'
import { RouterModule, Routes } from '@angular/router'
import { InitializingModule } from '@start9labs/shared'
import { LoadingPage } from './loading.page'

const routes: Routes = [
  {
    path: '',
    component: LoadingPage,
  },
]

@NgModule({
  imports: [InitializingModule, RouterModule.forChild(routes)],
  declarations: [LoadingPage],
})
export class LoadingPageModule {}
