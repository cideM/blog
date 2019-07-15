---
title: Hooks are Hard
date: '2019-07-15'
publish: true
---

## Introduction

## Intervals

### Basic Building Blocks

```jsx
function Child({ time }) {
  return <p>Time: {time}</p>
}

function Input({ onChange, id, type, text }) {
  return (
    <input
      type={type}
      id={id}
      value={text}
      onChange={e => {
        onChange(e.target.value)
      }}
    />
  )
}
```

![Screenshot of the interface](./screen1.png 'Screenshot')

Timer starts running when the component mounts. User can adjust the amount by which the value is increased every second. Timer needs to be precise. Adjusting the value should not change the timer interval.

### Pit of Despair

```js
function App() {
  const [input, setInput] = React.useState(`1`);
  const [time, setTime] = React.useState(0);

  React.useEffect(() => {
    const id = setInterval(() => {
      setTime(time + Number(input));
    }, 1000);

    return () => clearInterval(id);
  });
```

Seems to work, but there's a catch: changing the value increases the timer interval to more than a second. Reason is that the `useEffect` hook runs on every render. This also restarts the timer on every render. If we start typing at the end of a second, the interval is thrown away, a new timer is started and so the new duration of the interval is `remaining duration before render + 1s`.

```js
    return () => clearInterval(id);
    // eslint-disable-next-line
  }, []);
```

Breaks app entirely because the values inside the `useEffect` body always refer to the initial state (~ snapshot, talk about that concept).

So how are we going to fix this? We need to somehow refer to the most recent value from within the `useEffect` hook while at the same time not re-running the `useEffect` hooks on each render. References!

```js
const savedCallback = React.useRef()

React.useEffect(() => {
  savedCallback.current = () => {
    setTime(time + Number(input))
  }
})

React.useEffect(() => {
  let id = setInterval(() => {
    savedCallback.current()
  }, 1000)
  return () => clearInterval(id)
}, [])
```

Store the callback in a ref so that we can access the most recent version through that reference, without having to re-render the component. Update that ref on each render, so that the callback always uses new values.

Mention `setState` function signature and why it won't always work.

[link overreacted](https://overreacted.io/making-setinterval-declarative-with-react-hooks/)

### Pit of Success

Write the same thing with classes.

## Same Function, Different State

Use example from coding challenge to illustrate problem.
