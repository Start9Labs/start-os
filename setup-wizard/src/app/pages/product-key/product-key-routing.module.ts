import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { ProductKeyPage } from './product-key.page';

const routes: Routes = [
  {
    path: '',
    component: ProductKeyPage,
  }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class ProductKeyPageRoutingModule {}
