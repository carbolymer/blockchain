import {NgModule} from '@angular/core';
import {RouterModule, Routes} from "@angular/router";
import {AccountsComponent} from "./accounts/accounts.component";
import {NodesComponent} from "./nodes/nodes.component";
import {TransactionsComponent} from "./transactions/transactions.component";
import {NotFoundComponent} from "./not-found.component";
import {AccountDetailComponent} from "./account-detail/account-detail.component";


const routes: Routes = [
  {path: 'account/:id', component: AccountDetailComponent, data: {title: 'Account details - Blockchain'}},
  {path: 'accounts', component: AccountsComponent, data: {title: 'List of accounts - Blockchain'}},
  {path: 'nodes', component: NodesComponent, data: {title: 'Network nodes - Blockchain'}},
  {path: 'transactions', component: TransactionsComponent, data: {title: 'Transactions - Blockchain'}},
  {path: '', redirectTo: '/nodes', pathMatch: 'full'},
  {path: '**', component: NotFoundComponent}
];

@NgModule({
  exports: [RouterModule],
  imports: [RouterModule.forRoot(routes)]
})
export class AppRoutingModule {
}

