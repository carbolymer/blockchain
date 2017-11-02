import {Component, OnDestroy, OnInit} from '@angular/core';
import {ActivatedRoute} from "@angular/router";
import {AccountService} from "../account.service";
import {TransactionService} from "../transaction.service";
import {Transaction, TransactionHistoryEntry} from "../Transaction";
import {Account, getTransactionHistory} from "../Account";
import {Location} from "@angular/common";
import {StatusMessage} from "../StatusMessage";

@Component({
  selector: 'app-account-detail',
  templateUrl: './account-detail.component.html',
  styleUrls: ['./account-detail.component.css']
})
export class AccountDetailComponent implements OnInit, OnDestroy {
  allAccounts: Account[] = [];
  account: Account = new Account();
  recipientId: string = '';
  transferAmount?: number = null;
  transactions: TransactionHistoryEntry[] = [];

  constructor(private route: ActivatedRoute,
              private accountService: AccountService,
              private transactionService: TransactionService,
              private location: Location) {
  }

  ngOnInit() {
    this.resetNewTransactionForm();
    const id = this.route.snapshot.paramMap.get('id');
    this.accountService.getAccounts()
      .subscribe(accounts => {
        const thisAccount = accounts.filter(account => account.accountId === id);
        if (thisAccount.length === 1) {
          this.account = thisAccount[0];
          this.transactions = getTransactionHistory(this.account)
            // descending
            .sort((tx1, tx2) => tx2.time.getTime() - tx1.time.getTime());
          this.allAccounts = accounts;
        } else if (thisAccount.length == 0) {
          this.location.back();
        } else {
          throw `unexpected number of accounts: ${thisAccount}`;
        }
      });
  }

  resetNewTransactionForm() {
    this.recipientId = '';
    this.transferAmount = null;
  }

  ngOnDestroy(): void {

  }

  createTransaction(): void {
    // TODO generate date on backend to avoid spoofing
    this.transactionService.newTransaction(new Transaction(Number(this.transferAmount), this.account.accountId, this.recipientId, new Date()))
      .subscribe((statusMessage: StatusMessage) => console.log(statusMessage));
  }
}
