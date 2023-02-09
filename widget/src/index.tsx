import * as React from 'react';
import { Position } from 'vscode-languageserver-protocol';
import { InteractiveCode, useAsync, RpcContext, CodeWithInfos, RpcSessionAtPos, DocumentPosition } from '@leanprover/infoview';

interface Item {
  name : string
  score : number
  expr?: CodeWithInfos
  error?: string
}

interface Props {
  items: Item[]
  pos : DocumentPosition
}

interface GetItemsArgs {
  items : Item[]
  pos : DocumentPosition
}

async function getItems(rs : RpcSessionAtPos, args : GetItemsArgs) : Promise<Item[]> {
  return rs.call<GetItemsArgs, Item[]>('PremiseSelection.getItems', args)
}

export default function (props: Props) {
  const pos = props.pos
  if (props.items.length === 0) {
    return <div>No premises found!</div>
  }
  const rs = React.useContext(RpcContext)
  const res = useAsync(() => getItems(rs, props), [rs, pos])
  let items : Item[] = []
  let msg : any | undefined = undefined
  if (res.state === 'loading') {
    // items = props.items
    msg = <>Loading</>
  } else if (res.state === 'rejected') {
    msg = <>Error: {JSON.stringify(res.error)}</>
    items = []
  } else if (res.state === 'resolved') {
    items = res.value
    msg = <>Loaded</>
  }
  return <div>
    Hello!
    <table>
      <tbody>
        {items.slice(0, 10).map(x => <ViewItem key={x.name} {...x}/>)}
      </tbody>
    </table>
    {msg && <span>{msg}</span>}
  </div>
}

function ViewItem(props : Item) {
  let n;
  if (props.expr) {
    n = <code style={{color: "var(--vscode-textLink-activeForeground)"}}><InteractiveCode fmt={props.expr}/></code>
  } else {
    n = <code style={{ overflowWrap: "anywhere" }}>{props.name}</code>
  }
  return <tr>
    <td>{n}</td>
    <td>{props.score}</td>
    <td>{props.error}</td>
  </tr>
}