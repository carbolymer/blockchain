import {Component, OnDestroy, OnInit} from '@angular/core';
import {Observable} from "rxjs/Observable";
import {timer} from "rxjs/observable/timer";
import {Subscription} from "rxjs/Subscription";
import {AccountService} from "../account.service";
import {Account} from "../Account"

@Component({
  selector: 'app-accounts',
  templateUrl: './accounts.component.html',
  styleUrls: ['./accounts.component.css']
})
export class AccountsComponent implements OnInit, OnDestroy {

  private static refreshTrigger: Observable<number> = timer(0, 1000);
  private refreshSubscription: Subscription;
  accounts: Account[] = [];

  constructor(private accountService: AccountService) { }

  ngOnInit() {
    this.refreshSubscription = AccountsComponent.refreshTrigger
      .subscribe(_ => this.getAccounts());
  }

  ngOnDestroy(): void {
    this.refreshSubscription.unsubscribe();
  }

  private getAccounts(): void {
    this.accountService.getAccounts()
      .subscribe(accounts => this.accounts = accounts)
  }
}
