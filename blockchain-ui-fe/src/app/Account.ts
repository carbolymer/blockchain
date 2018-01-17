import {Transaction, TransactionHistoryEntry} from "./Transaction";
import {Array} from "util"

export class Account {
  accountId: string;
  balance: number;
  accountTransactions: Array<Transaction>;
}

export function getTransactionHistory(account: Account): TransactionHistoryEntry[] {
  return (<Array<Transaction>> account.accountTransactions).flatMap(dto => {
    let transaction: Transaction = Transaction.fromTransactionDto(dto);
    let entries: Array<TransactionHistoryEntry> = [];
    if (transaction.recipient === account.accountId
      && transaction.sender === account.accountId) {
      const shiftedDate = new Date(new Date(transaction.time).getTime() - 1);
      entries.push(new TransactionHistoryEntry(account.accountId, -transaction.amount, 0, shiftedDate));
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
    entries.push(new TransactionHistoryEntry(otherAddress, amount, 0, new Date(transaction.time)));
    return entries;
  })
    .sort((tx1, tx2) => tx2.time.getTime() - tx1.time.getTime())
    .reduceRight((accumulator: Array<TransactionHistoryEntry>, element: TransactionHistoryEntry) => {
      element.balance = element.amount;
      if (accumulator.length > 0) {
        let pv: TransactionHistoryEntry = accumulator[accumulator.length - 1];
        element.balance += pv.balance;
      }
      accumulator.push(element);
      return accumulator;
    }, []);
}
