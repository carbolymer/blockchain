import {Transaction, TransactionHistoryEntry} from "./Transaction";
import {Array} from "util"

export class Account {
  accountId: string;
  balance: number;
  accountTransactions: Array<Transaction>;
}

export function getTransactionHistory(account: Account): TransactionHistoryEntry[] {
  return (<Array<Transaction>> account.accountTransactions).flatMap(transaction => {
    let entries: Array<TransactionHistoryEntry> = [];
    if (transaction.recipient === account.accountId
      && transaction.sender === account.accountId) {
      const shiftedDate = new Date(new Date(transaction.time).getTime() - 1);
      entries.push(new TransactionHistoryEntry(account.accountId, -transaction.amount, shiftedDate));
    }

    let otherAddress: string, amount: number;
    if (transaction.recipient === account.accountId) {
      otherAddress = transaction.sender;
      amount = transaction.amount;
    } else if (transaction.sender === account.accountId) {
      otherAddress = transaction.recipient;
      amount = -transaction.amount;
    } else {
      throw `Unexpected transaction in account: ${transaction}`;
    }
    entries.push(new TransactionHistoryEntry(otherAddress, amount, new Date(transaction.time)));
    return entries;
  });
}
