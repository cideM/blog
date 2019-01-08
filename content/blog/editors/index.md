---
title: Choosing a Text Editor
date: '2018-09-10T22:12:03.284Z'
---

_I update this post every so often with new insights. That might make for a jarring read, I'm sorry._

Ever since I started learning to code I've been constantly switching editors. I've used Vim, Neovim, Atom, Visual Studio Code, Intellij IDEA Ultimate and Kakoune. I've spent most time with Visual Studio Code and Neovim.

I think that I was always looking for an editor that would magically help me focus. In that sense, I was chasing some vague zen-like editing experience which, I assumed, could only be found in bare bones editors. You know, less is more and digital minimalism and whatnot.

Unfortunately I'm still just as easily distracted as always and not even Kakoune, the most unixy of the bunch, could cure that affliction.

Looking back, I did learn a thing or two, such as doing things in the CLI, but I mostly wasted a ton of time. I only ever looked at editors from a consumer point of view, I never sat down and developed plugins. In that sense, my editor journey reminds me of my recent video game history.

After graduating, I became mildly addicted to an MMORPG (GuildWars 2). On the positive side, it was through that game that I really started using reddit and developed a more in-depth interest in video games. I also have fond memories of raid nights and dungeon runs. But it was mostly precious lifetime down the drain. Contrast this with a friend of mine, who spent as much time as I did in GW2, but coupled it with creating YouTube videos. He eventually became one of the most well known content creators and got a lot of valuable skills out of his time in GW2. That's pretty much my editor experience too: wasted a lot of time, never really got much value out of it.

I am genuinely interested in both text editors and terminals, but I never turned that interest into knowledge or skills.

So first lesson to my future self: **if you must use some old fashioned and slightly inconvenient technology, at least make sure you've got something to show for at the end. Learn Vimscript, write or contribute to some plugins, you get the idea.**

The primary reason why I keep coming back to modal editors is the flow-inducing editing experience, which stems primarily from always having your hands on the home row. In other editors you frequently have to reach for the mouse or awkwardly angle your hand so you can hit a modifier or arrow key. Even with sophisticated shortcuts, the editing flow of modal editors is very hard to reproduce in non-modal editors.

What I don't really buy into is the belief that modal editors let you achieve
so much more with far fewer actions - think vim golf. For example, slash search and edit requires the following actions in Neovim: `/` followed by the word you're looking for, then e.g, `c` to change it. In VSC the same sequence would be `cmd+f`, again followed by the word, `esc` (or `cmd+1`) to focus on the editor, and then you can start typing and it changes the highlighted word. I'd say that's about the same amount of actions (note that I'm comparing actions not keystrokes). Neovim and Kakoune both have the very nice "do something between delimiter" motions, whereas Sublime and VSC only offer expand region. If you further customize Neovims target object (or use the `targets` plugin) it's a serious beast when it comes to flying around the codebase, changing all the code between all the delimiters. Going back to VSC feels like chopping off your legs. On the other hand typing speed is overrated and it's not exactly make or break whether you can change `[foo, bar]` to `[bar]` in under 100ms.

Second lesson: **Sublime and VSC don't hold a candle to Neovim in terms of editing, but it probably doesn't matter.**

Speaking of features, there are of course many things an editor like VSC brings to the table: It has an amazing ESLint plugin which runs circles around every other implementation. As a front end developer, I should care about that. In Rust being able to click on "test" to run a single test that's right there in front of me is also really nice. In languages where there's no tooling I really like (Haskell), I can just run my usual `rg --files -t hs | entr -c ...`. I'm not losing anything by using VSC in that area, I'm only gaining something. Symbol search is something I don't use very often as it's only available in code bases that VSC (and its plugins) understand. But if it works it's without a doubt an improvement over `ripgrep` et al. since it skips comments and things that, well, aren't symbols. Memo to myself: use more symbol search. No, actually, symbol search all the things!

Linter and compiler feedback in general is a surprisingly deep topic. Both Haskell and Rust often have lengthy error messages, even with interleaved ASCII art. Some or all of that formatting is lost in VSC's pop-up window for errors, which sucks. In these cases it might be more productive to just run `ghcid` in a terminal and look at the plain output. On the other hand, with many errors, seeing where in your file those errors occur and only ever looking at one of them at a time, can be a major producitivy boost. Ultimately I think VSC has a lot of room for improvements in that area but again, I can just run whatever command I want in VSC's integrated terminal (or switch to a dedicated terminal instance). Doesn't mean I need to abandon VSC entirely just for those edge cases.

In general, VSCs autoformatting and linting is pretty fast. In Neovim I use `ALE` for both these days and while it's a great plugin (memo to myself: contribute... all the things?) it's just not as fast an snappy as VSC.

There is a part of me that chases the lofty ideal of a modern, bare bones, unixy editor (think Kakoune marries VSC and they both have an affair with Neovim). An editor that integrates perfectly with an equally elegant terminal emulator, which has just the right number of features and otherwise lets me integrate nicely with the CLI (Kakoune does that exceptionally well). But that editor doesn't exist. So the question is: do chase that ideal (Neovim) or just go with the pragmatic choice (e.g., VSC)?

I mentioned it already but the one positive thing about using a more minimalistic text editor is that it forces you to learn more about CLI tools. That can end up being less efficient than an IDE, but knowing more about those tools (that the IDE is often using, but exposed through a GUI) can come in really handy in situations that the predetermined GUI just doesn't handle.

A prime example is git, which has a lot of ~~confusing~~ potentially helpful options to explore the index and which are absent in many VSC git plugins.

Lesson three: VSC offers nice features. CLI tools are powerful. **Do you invest in gettings things done quicker now or maybe having a stronger CLI fu in the future?** Is that going to be worth it? Burrito or Sandwich?

Lastly, there's performance. Sublime (and the terminal editors) are without a doubt less resource hungry. But terminal editors are also slower at scrolling and generally uglier, in my humble opinion. Sublime however is both incredibly fast and also very nice to look at. Although that kind of falls apart when you realize that syntax highlighting is not a solved problem in 2018 (like... seriously? why doesn't every language ship with a syntax parser that adheres to some common standard _grumble_). But I have an expensive Mac Book Pro. I also have an 8 core Ryzen CPU in my big ass desktop PC. I have 16GB of RAM. I don't care if VSC requires more resources. And a lot of the CPU cycle hogging stuff is actually plugins (linters, compilers, watchers, servers). I can trim down VSC to some essentials and it's a fast and snappy editor. I've used VSC for long coding sessions in a car and I don't notice the battery drain. I do have a battery pack after all. It's not like I'll be stuck in the desert with my life depending on a 9h coding session. VSC's electron powered GUI is often faster than Neovim. For example, doing a search in VSC is pretty much as fast as running ripgrep in the CLI. Neovim, using ripgrep, takes longer and I assume it's because it has to populate and open the quickfix window.

Lesson four: **VSC is fast. Sublime is fast. Neovim so-so.** :(

So, you've made it this far future me. It's probably late, you're listening to low key electronic music, and you have at least 6 tabs open, with reddit and HN posts about why Neovim Is The Light and that you should Praise The Sun - no that was just a reminder that you bought DS3 and you haven't finished DS2 yet so get going.

If you want to use Neovim, do it. But have a good reason. Read this post. **If you feel like using a customizable editor, really learn to customize it.** Make sure you get something out of it beyond just learning org-mode or something.

Your Past Self.
