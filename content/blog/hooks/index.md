---
title: Hooks are Hard
date: '2019-07-15'
publish: true
---

## Introduction

When hooks were first introduced, many people found them to look fairly magical. Especially the constraint that the order of hooks must not change across renders seemed pretty wild. On the other hand, getting rid of the divide between class based and functional components, and making sharing of functionality between components easier, seemed really appealing. That was especially true for me, since I saw them as an opportunity to stop using `recompose` for absolutely everything in our codebase at work.

So I took one of our features and rewrote it using hooks. The goal was to see what using hooks feels like, what issues arise and how solutions are available, so that we're well prepared when we start using hooks throughout the entire codebase.

This post is an experience report, not a review.

## Creating a Timer

To me the trickiest part of hooks is the balancing act of making sure your callbacks don't reference stale data while at the same time minimizing rerenders. One example that highlights this issue is creating a timer that runs every second and increases a value. The twist is that the user can adjust the amount by which the value is increaased, without this affecting the timer in any way. No slowdowns, no speedups. The code for this example can be found in [this repl.it](https://repl.it/@cideM/timer).

### Basic Building Blocks

The components are all very simple. There's a component displaying the current value and another that renders the input field, through which you can adjust the amount by which the value increases.

```js
function SomeChild({ x }) {
  return <p>Time: {x}</p>
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

The timer starts running as soon as the component mounts. Go ahead and try it out in the repl! The version in the repl actually works. Adjust the value and see the value increase by that amount, without any hiccups. I'll now walk you through the process and pitfalls of arriving at that solution.

### Pit of Despair

Below is the first solution that I came up with. I store both text input and value with the `useState` hook. Additionally, I start a timer in `useEffect` and in the body of that function I increase the current value. The timer is cleared when the component unmounts thanks to the cleanup function returned from `useEffect`. So far, so good. And this actually works! Sort of...

You can copy & paste the code into the repl to observe the issue. If you type anything into the input field just as _the next tick of `setInterval` is about to kick in_, that next tick is delayed. What gives?

```js
function App() {
  const [input, setInput] = React.useState(`1`);
  const [x, setX] = React.useState(0);

  React.useEffect(() => {
    const id = setInterval(() => {
      setX(x + Number(input));
    }, 1000);

    return () => clearInterval(id);
  });
```

To understand this quirky behavior you need to understand how `useEffect` works. It will always run on mount and unmount, much like `componentDidMount` and `componentWillUnmount` [^1]. Additionally, it runs **after every completed render, including the cleanup function**. In the above example, the following happens:

1. Component mounts, `useEffect` runs.
2. User types into input field and state updates, triggering a render of `<App />`
3. `useEffect` runs. It first cleans up the previous interval, then starts a new interval.

If the preceding interval is cleaned up just as it's about to tick, and a new interval is then started, the pausethat the user sees is `duration of the old interval until cleanup + duration of the new interval`.

So what's the solution to this problem? Pass an array of values, that the effect depends on, as a second argument to `useEffect`. We want our interval to remain active for the entire lifetime of the component, therefore we pass an empty array to `useEffect`. Now it only runs on mount and unmount. Wohoo!

```js
    return () => clearInterval(id);
    // eslint-disable-next-line
  }, []);
```

Except that now everything is broken. The interval ticks along nicely, but the value stays at 1.

The new issue is that the function passed to `setInterval` always refers to the initial state. So on each tick it increments 0 to 1 and that's it. You can imagine that on each render, all the state and props of a component is saved in a snapshot. On subsequent renders, rather than mutating the old snapshot, a new snapshot is created. React compares the virtual DOM that would result from old and new snapshot, and update the real DOM accordingly.

But that also means that `x` and `input` in the function passed to `setInterval` will always refer to the values from the very initial snapshot, since `useEffect` is **only run once, after the component rendered the first time**.

How can we solve this issue for good? Our requirements are:

- Start an interval that lives as long as the component
- Store the values in a way that we can always reference the most recent values

The solution below is more or less a 1:1 copy from Dan Abramov's [overreacted blog](https://overreacted.io/making-setinterval-declarative-with-react-hooks/), where he happened to talk about almost precisely this issue. I just inlined the function rather than creating a custom hook from it.

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

The solution here is to store the function called in `setInterval` in a reference, with `useRef`[^2]. The interval is only created once, as can be seen from the empty dependency array. But instead of refering to a function from the first render of `<App />`, we access (and call), the most recent version of that function through the reference. On each render, we simply update that stored function, by reassigning `savedCallback.current` to a new function using the most recent state. We're mutating the reference in place! Even though `useEffect` only runs once, it will can access the (mutated) updated function through the reference.

Fun fact, there's a [tweet](https://twitter.com/dan_abramov/status/1099842565631819776?lang=en) by Dan Abramov where he states that:

> useRef() is basically useState({current: initialValue })[0]

Meaning you can do this:

```js
const ref = useState({ current: 0 })[0]
ref.current = 2
console.log(ref) // 2
```

It's just JS after all. Nothing prevents you from mutating stuff willy nilly.

### Pit of Success

Write the same thing with classes.

## Same Function, Different State

Use example from coding challenge to illustrate problem.

[^1] Unlike the lifecycle methods, the function passed to `useEffect` runs _after_ layout and paint.
[^2] Alternatively you could also store input and value in references, but that would make things pretty unergonomical and would force all users of those values to reach them through `.current`.
