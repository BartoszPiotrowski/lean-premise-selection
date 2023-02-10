import * as React from 'react';
import { Position } from 'vscode-languageserver-protocol';
import { InteractiveCode, useAsync, RpcContext, CodeWithInfos, RpcSessionAtPos, DocumentPosition } from '@leanprover/infoview';

interface Item {
  name : string
  score : number
  expr?: CodeWithInfos
  error?: string
  tactic?: string
  tacticResult?: CodeWithInfos
}

interface Props {
  items: Item[]
  pos : DocumentPosition
}

interface GetItemArgs {
  item : Item
  pos : DocumentPosition
}

async function getItem(rs : RpcSessionAtPos, args : GetItemArgs) : Promise<Item> {
  const items =  rs.call<GetItemArgs, Item>('PremiseSelection.getItem', args)
  // await new Promise(r => setTimeout(r, 2000));
  return items;
}

interface Update {
  kind: 'update'
  index : number
  item : Item
}

interface Reset {
  kind:'reset'
  items : Item[]
}

function reducer(state : Item[], action : Update | Reset) {
  if (action.kind === 'update') {
    state = [...state];
    state[action.index] = action.item;
    return state;
  } else {
    return action.items
  }
}

export default function (props: Props) {
  const pos = props.pos
  if (props.items.length === 0) {
    return <div>No premises found!</div>
  }
  const [items, update] = React.useReducer(reducer, props.items || [])
  const [status, setStatus] = React.useState('init')
  const r = React.useRef(0)
  const rs = React.useContext(RpcContext)
  async function e() {
    r.current += 1
    const id = r.current
    update({kind: 'reset', items: props.items})
    setStatus(`loading ${id}`)
    for (let i = 0; i < items.length; i++) {
      const item = await getItem(rs, {item : props.items[i], pos})
      if (r.current !== id) {
        return
      }
      update({kind: 'update', index: i, item})
    }
    setStatus(`done ${id}`)
  }
  React.useEffect(() => {e()}, [rs, props.items.map(i => `${i.name}-${i.score}`).join(',')])
  let msg : any | undefined = <>{status}</>
  return <div>
    <table>
      <tbody>
        {items.map(x => <ViewItem key={x.name} {...x}/>)}
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
    <td>{props.score}</td>
    <td>{n}</td>
    <td>{props.error ?? <code>{props.tactic}</code>}</td>
    <td>{props.tacticResult && <code style={{color: "var(--vscode-textLink-activeForeground)"}}><InteractiveCode fmt={props.tacticResult}/></code>}</td>
  </tr>
}
