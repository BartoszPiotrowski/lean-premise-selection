import * as React from 'react';

interface Props {
  items: {
    name: string
    score: number
  }[]
}

export default function (props: Props) {
  if (props.items.length === 0) {
    return <div>No premises found!</div>
  }
  return <div>
    Here are the premises:
    <table>
      <tbody>
        {props.items.map(x => <tr><td><code style={{ overflowWrap: "anywhere" }}>{x.name}</code></td> <td>{x.score}</td></tr>)}
      </tbody>
    </table>
  </div>
}
