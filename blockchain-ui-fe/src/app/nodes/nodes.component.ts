import {Component, OnDestroy, OnInit} from '@angular/core';
import {Node} from '../Node'
import {timer} from "rxjs/observable/timer";
import {NodeService} from "../node.service";
import {Observable} from "rxjs/Observable";
import {Subscription} from "rxjs/Subscription";
import {StatusMessage} from "../StatusMessage";
import {MessageService} from "../message.service";

@Component({
  selector: 'app-nodes',
  templateUrl: './nodes.component.html',
  styleUrls: ['./nodes.component.css']
})
export class NodesComponent implements OnInit, OnDestroy {
  private static refreshTrigger: Observable<number> = timer(0, 1000);

  nodes: Node[] = [];
  currentlyMiningNodesIds: Set<string> = new Set();
  private refreshSubscription: Subscription;


  constructor(private nodeService: NodeService,
              private messageService: MessageService) {
  }

  ngOnInit() {
    this.refreshSubscription = NodesComponent.refreshTrigger
      .subscribe(_ => this.getNodes());
  }

  ngOnDestroy(): void {
    this.refreshSubscription.unsubscribe();
  }

  private getNodes(): Observable<Node[]> {
    const nodesObservable: Observable<Node[]> = this.nodeService.getNodes();
    nodesObservable.subscribe(nodes => {
      this.nodes = nodes;
    });
    return nodesObservable;
  }

  mine(node: Node): void {
    this.currentlyMiningNodesIds.add(node.id);
    this.nodeService.mine(node)
      .flatMap((statusMessage: StatusMessage) => {
        this.messageService.add(statusMessage);
        return this.getNodes();
      })
      .subscribe(_ => {
        this.currentlyMiningNodesIds.delete(node.id);
      });
  }
}
