import {MaterializeModule} from 'angular2-materialize';
import {BrowserModule} from '@angular/platform-browser';
import {NgModule} from '@angular/core';

import {AppComponent} from './app.component';
import {FormsModule} from "@angular/forms";
import {AppRoutingModule} from './/app-routing.module';
import {HttpClientModule} from "@angular/common/http";
import {AccountsComponent} from './accounts/accounts.component';
import {NodesComponent} from './nodes/nodes.component';
import {TransactionsComponent} from './transactions/transactions.component';
import {NodeService} from "./node.service";
import {NotFoundComponent} from './not-found.component';
import {TransactionService} from "./transaction.service";
import {AccountService} from "./account.service";
import {AccountDetailComponent} from './account-detail/account-detail.component';
import { MessageService } from './message.service';

@NgModule({
  declarations: [
    AppComponent,
    AccountsComponent,
    NodesComponent,
    TransactionsComponent,
    NotFoundComponent,
    AccountDetailComponent
  ],
  imports: [
    BrowserModule,
    FormsModule,
    AppRoutingModule,
    HttpClientModule,
    MaterializeModule
  ],
  providers: [NodeService, TransactionService, AccountService, MessageService],
  bootstrap: [AppComponent]
})
export class AppModule {
}


