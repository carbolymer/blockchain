import {Component, OnDestroy, OnInit} from '@angular/core';
import {Transaction} from "../Transaction";
import {TransactionService} from "../transaction.service";
import {Observable} from "rxjs/Observable";
import {timer} from "rxjs/observable/timer";
import {Subscription} from "rxjs/Subscription";

@Component({
  selector: 'app-transactions',
  templateUrl: './transactions.component.html',
  styleUrls: ['./transactions.component.css']
})
export class TransactionsComponent implements OnInit, OnDestroy {
  private static refreshTrigger: Observable<number> = timer(0, 1000);

  transactions: Transaction[] = [];
  private refreshSubscription: Subscription;

  constructor(private transactionService: TransactionService) {
  }

  ngOnInit() {
    this.refreshSubscription = TransactionsComponent.refreshTrigger
      .subscribe(_ => this.getTransactions());
  }

  ngOnDestroy(): void {
    this.refreshSubscription.unsubscribe();
  }

  private getTransactions(): void {
    this.transactionService.getTransactions()
      .subscribe(transactions => this.transactions = transactions);
  }
}
